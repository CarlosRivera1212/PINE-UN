# FUNCION CLASIFICACION EURO
library(dplyr)


f_ceuro <- function(df, col_clase, col_tipo_serv,
                    col_tipo_comb, col_mod, file_reglas){
  
  load(file_reglas)
  
  df_sel = df %>% 
    select('CLASE'=col_clase,
           'TIPO_SERVICIO'=col_tipo_serv,
           'TIPO_COMBUSTIBLE'=col_tipo_comb,
           'MODELO'=col_mod)
  
  df_clas_euro = left_join(
    df_sel, df_reg_euro,
    c('CLASE','TIPO_SERVICIO','TIPO_COMBUSTIBLE','MODELO')
  )
  
  df_sal = df %>% 
    mutate(CAT_EURO = df_clas_euro$CAT_EURO)
  
  return(df_sal)
}
