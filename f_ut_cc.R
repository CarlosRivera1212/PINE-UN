# Funcion Union Tablas Consumo Combustible

f_ut_cc <- function(acti, parq, catv){
  require(readxl)
  require(dplyr)
  require(reshape2)
  # acti: Tabla ACTIVIDAD (KM ANUALES)
  # parq: Tabla PARQUE AUTOMOTOR
  # catv: Tabla CATEG VEHIC Y CARACT
  # vent: Tabla VENTAS
  
  acti_parq = left_join(
    acti %>% 
      melt('DEPARTAMENTO') %>% 
    rename(CATEGORIA = variable, ACTIVIDAD = value),
    parq %>% 
      melt('DEPARTAMENTO') %>% 
      rename(CATEGORIA = variable, PARQUE = value),
    c('DEPARTAMENTO', 'CATEGORIA')
  )
  
  acti_parq_catv = left_join(
    acti_parq,
    catv,
    c('CATEGORIA')
  )
  
  # BALANCE ENERGÉTICO BECO - TRANSPORTE CARRETERO
  acti_parq_catv_sum = acti_parq_catv %>% 
    group_by(DEPARTAMENTO, CATEGORIA) %>% 
    summarise(PROD = ACTIVIDAD * PARQUE * RENDIMIENTO) %>% 
    dcast('DEPARTAMENTO ~ CATEGORIA', value.var = 'PROD') %>% 
    group_by(DEPARTAMENTO) %>% 
    summarise(
      G_ESTI = sum(A_G,B_G,C_G,M_G,T_G,TR_G,V_G, na.rm = T),
      D_ESTI = sum(A_D,B_D,C_D,T_D,TR_D,V_D, na.rm = T),
      GNVC_EST = sum(A_GNVC,B_GNVC,C_GNVC,M_GNVC,T_GNVC, na.rm = T)
    )
  
  VKT_acti_parq = acti_parq_catv %>% 
    group_by(DEPARTAMENTO, CATEGORIA) %>% 
    summarise(VKT = ACTIVIDAD * PARQUE) %>% 
    dcast('DEPARTAMENTO ~ CATEGORIA', value.var = 'VKT')
  
  return(list(acti_parq_catv_sum = acti_parq_catv_sum,
              VKT_acti_parq = VKT_acti_parq))
}
