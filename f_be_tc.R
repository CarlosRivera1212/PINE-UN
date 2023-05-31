# Funcion BALANCE ENERGÉTICO BECO - TRANSPORTE CARRETERO

f_be_tc <- function(cons, vent){
  require(readxl)
  require(dplyr)
  require(reshape2)
  
  balance = left_join(cons, vent, c('DEPARTAMENTO')) %>% 
    mutate(G_VAR = abs(G_VENTAS_BECO-G_ESTI)/G_VENTAS_BECO,
           G_RES = ifelse(G_VENTAS_BECO<G_ESTI, 'SOBREESTIMADO', 'SUBESTIMADO'),
           D_VAR = abs(D_VENTAS_BECO-D_ESTI)/D_VENTAS_BECO,
           D_RES = ifelse(D_VENTAS_BECO<D_ESTI, 'SOBREESTIMADO', 'SUBESTIMADO'),
           GNVC_VAR = abs(GNVC_TEORICO-GNVC_EST)/GNVC_TEORICO,
           GNVC_RES = ifelse(GNVC_TEORICO<GNVC_EST, 'SOBREESTIMADO', 'SUBESTIMADO'))
  
  return(balance)
}
