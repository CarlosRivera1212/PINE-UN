f_ds_3 <- function(df_grid,
                   df_emi,
                   df_temp,
                   nom_coordx = 'CX',
                   nom_coordy = 'CY',
                   nom_cont = 'NO2',
                   pond_esp = c(0.6, 0.3, 0.1)){
  
  options(dplyr.summarise.inform = FALSE)
  cat('INICIO', ' - ', nom_cont,' - ')
  
  ncp = 'PRIMARIA'
  ncs = 'SECUNDARIA'
  nct = 'TERCIARIA'
  
  
  p1 = pond_esp[1] # 60
  p2 = pond_esp[2] # 30
  p3 = pond_esp[3] # 10
  
  df_grid = df_grid %>% 
    group_by(DEPARTAMENTO, TIPO, GRID_ID, CX, CY) %>% 
    summarise(ELEV = mean(ELEV),
              LARGOTOT_1 = PRIMARIA + SECUNDARIA + TERCIARIA,
              LARGOTOT_2 = p1*PRIMARIA + p2*SECUNDARIA + p3*TERCIARIA) %>% 
    group_by(DEPARTAMENTO) %>% 
    mutate(LARGOTOT_1_porc = LARGOTOT_1/sum(LARGOTOT_1),
           LARGOTOT_2_porc = LARGOTOT_2/sum(LARGOTOT_2)) %>% 
    ungroup()
  
  df_emi = df_emi %>% 
    pivot_longer(BC:SO2,
                 names_to = 'CONTAMINANTE',
                 values_to = 'VAL_CONT') %>% 
    filter(CONTAMINANTE == nom_cont)
  
  # TABLA ESPACIAL
  df_vc = left_join(df_grid, df_emi,
                    c('DEPARTAMENTO','TIPO'),
                    multiple='all')
  
  # TABLA TEMPORAL
  df_vct = df_vc %>% 
    group_by(GRID_ID, TIPO, CONTAMINANTE, CX, CY) %>% 
    summarise(
      LARGOTOT_1 = sum(LARGOTOT_1),
      LARGOTOT_2 = sum(LARGOTOT_2),
      LARGOTOT_1_porc = sum(LARGOTOT_1_porc),
      LARGOTOT_2_porc = sum(LARGOTOT_2_porc),
      ELEV = mean(ELEV),
      VAL_CONT = sum(VAL_CONT, na.rm=T)) %>%
    ungroup() %>% 
    left_join(df_temp,
              'TIPO',
              multiple='all')
  
  
  fmm <- function(x){ (x-min(x))/(max(x)-min(x)) }
  
  
  # MODELO ESPACIAL Y TEMPORAL
  dfmod_st = df_vct %>%
    filter(!is.na(VAL_CONT)) %>%
    # mutate(Y_1 = fmm(Y_1),
    #        Y_2 = fmm(Y_2),
    #        ELEV = fmm(ELEV)) %>%
    mutate(Y_1 = LARGOTOT_1_porc * VAL_CONT * POND_TEMP,
           Y_2 = LARGOTOT_2_porc * VAL_CONT * POND_TEMP)
  
  # dfmod_st = pre_dfmod_st %>% 
  #   sample_frac(0.5)
    
  xy = as.matrix(dfmod_st[, c(nom_coordx, nom_coordy)])
  contnb = dnearneigh(coordinates(xy), 0, 10000, longlat = T)
  dlist = nbdists(contnb, xy)
  dlist = lapply(dlist, function(x) ifelse(x==0, 0, 1/x))
  Wve = nb2listw(contnb, glist=dlist, style = "W")
  
  
  cat(' - creando modelos - ')
  try({
    mod1 = errorsarlm(
      Y_1 ~ ELEV + POND_TEMP,
      data = dfmod_st,
      listw = Wve
    )
    
    # pre_dfmod_st$Yf_1 = predict(mod1, newdata=pre_dfmod_st)[,1]
    fv1 = mod1$fitted.values
    fv1[fv1<=0] = min(fv1[fv1>0])/sum(fv1<=0)
    dfmod_st$Yf_1 = fv1
  })
  
  try({
    mod2 = errorsarlm(
      Y_2 ~ ELEV + POND_TEMP,
      data = dfmod_st,
      listw = Wve
    )
    fv2 = mod2$fitted.values
    fv2[fv2<0] = 0
    dfmod_st$Yf_2 = fv2
  })
  
  
  cat('FIN\n')
  # RETURN
  # return(pre_dfmod_st)
  return(dfmod_st)
}
