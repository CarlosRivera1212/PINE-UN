# FUNCTION PARA DESAGREGACION ESPACIAL
# primero filtrar TIPO: URBANO - INTERURBANO
# primero filtrar DEPARTAMENTO
f_ds_2 <- function(df_grid,
                   df_emi,
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
  
  fmm <- function(x){ (x-min(x))/(max(x)-min(x)) }
  
  
  # MODELO ESPACIAL
  
  dfmod_s = df_vc %>% 
    filter(!is.na(VAL_CONT)) %>% 
    # mutate(Y_1 = fmm(Y_1),
    #        Y_2 = fmm(Y_2),
    #        ELEV = fmm(ELEV)) %>% 
    mutate(Y_1 = LARGOTOT_1_porc * VAL_CONT,
           Y_2 = LARGOTOT_2_porc * VAL_CONT) %>% 
    group_by(TIPO, GRID_ID, CX, CY, CONTAMINANTE) %>% 
    summarise(Y_1 = mean(Y_1),
              Y_2 = mean(Y_2),
              ELEV = mean(ELEV)) %>% 
    ungroup()
  
  xy = as.matrix(dfmod_s[, c(nom_coordx, nom_coordy)])
  contnb = dnearneigh(coordinates(xy), 0, 10000, longlat = T)
  dlist = nbdists(contnb, xy)
  dlist = lapply(dlist, function(x) ifelse(x==0, 0, 1/x))
  Wve = nb2listw(contnb, glist=dlist, style = "W")
  
    # modelo autoregresivo: Y suma
  # try({
  #   dfmod_s$Yf_1_0 = errorsarlm(
  #     Y_1 ~ 1,
  #     data = dfmod_s,
  #     listw = Wve
  #   )$fitted.values
  # })

    # modelo autoregresivo: Y suma ponderada
  # try({
  #   dfmod_s$Yf_2_0 = errorsarlm(
  #     Y_2 ~ 1,
  #     data = dfmod_s,
  #     listw = Wve
  #   )$fitted.values
  # })
  
    # modelo elev: Y suma
  try({
    mod1 = errorsarlm(
      Y_1 ~ ELEV,
      data = dfmod_s,
      listw = Wve
    )
    
    fv1 = mod1$fitted.values
    fv1[fv1<=0] = min(fv1[fv1>0])/sum(fv1<=0)
    dfmod_s$Yf_1 = fv1
  })
  
    # modelo elev: Y suma ponderada
  try({
    mod2 = errorsarlm(
      Y_2 ~ ELEV,
      data = dfmod_s,
      listw = Wve
    )
    
    fv2 = mod2$fitted.values
    fv2[fv2<0] = 0
    dfmod_s$Yf_2 = fv2
  })
  
  cat('FIN\n')
  # RETURN
  return(dfmod_s)
}
