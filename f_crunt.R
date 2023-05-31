# FUNCION CLASIFICACION RUNT

f_crunt <- function(clase, cil, tipo_motor=NULL, tipo_comb=NULL, cant_eje = NULL){
  
  # Automóvil, camioneta, campero
  if (clase %in% c("AUTOMOVIL", "CAMIONETA", "CAMPERO")) {
    # cilindraje < 1,200 cc
    if (cil <= 1200) { c_s = c('Passenger cars', 'Mini') }
    # cilindraje < 1,600 cc
    else if (cil <= 1600) { c_s = c('Passenger cars', 'Small') }
    # cilindraje < 2,000 cc
    else if (cil <= 2000) { c_s = c('Passenger cars', 'Medium') }
    # cilindraje > 2,000 cc
    else if (cil > 2000) { c_s = c('Passenger cars', 'Large-SUV-Executive') }
  }
  
  # Mototriciclo, motocarro, ciclomotor, cuadriciclo, cuatrimoto
  else if(clase %in% c("MOTOTRICICLO", "MOTOCARRO", "CICLOMOTOR", "CUADRICICLO", "CUATRIMOTO")){
    c_s = c('L-Category', 'Quad & ATVs')
  }
  
  # Motocicletas
  else if(clase %in% c("MOTOCICLETA")){
    if (tipo_motor == '2 TIEMPOS') {
      if(cil <= 50) { c_s = c('L-Category', 'Mopeds 2-stroke < 50cc') }
      else if(cil > 50) { c_s = c('L-Category', 'Motorcycles 2-stroke > 50cc') }
    }
    else if (tipo_motor == '4 TIEMPOS') {
      # Cilindraje < 50 cc
      if(cil <= 50) { c_s = c('L-Category', 'Mopeds 4-stroke < 50cc') }
      # 50 cc < Cilindraje < 250 cc
      else if(cil <= 250) { c_s = c('L-Category', 'Motorcycles 4-stroke < 250cc') }
      # 250 cc < Cilindraje < 750 cc
      else if(cil <= 750) { c_s = c('L-Category', 'Motorcycles 4-stroke 250cc - 750cc') }
      # Cilindraje > 750 cc
      else if(cil > 750) { c_s = c('L-Category', 'Motorcycles 4-stroke > 750cc') }
    }
  }
  # Microbús, buseta, bus
  else if(clase %in% c("MICROBUS", "BUSETA", "BUS")){
    if (tipo_comb == 'GNV') { c_s = c('Buses', 'Urban CNG Buses') }
    else if (tipo_comb == 'DIES ELECT') { c_s = c('Buses', 'Urban Buses Diesel Hybrid') }
    else if (tipo_comb == 'DIESEL') {
      if(cil <= 3000) { c_s = c('Buses', 'Urban Buses Midi <= 15 t') }
      else if(cil <= 6000) { c_s = c('Buses', 'Urban Buses Midi 15 – 18 t') }
      else if(cil > 6000) {
        if (cant_eje <= 3) { c_s = c('Buses', 'Coaches Standard <= 18 t') }
        else if (cant_eje > 3) { c_s = c('Buses', 'Coaches Articulated > 18 t') }
      }
    }
  }
  # Camiones - Volquetas
  else if(clase %in% c("CAMION", "VOLQUETA")) {
    if (tipo_comb == 'GASOLINA') { c_s = c('Light Commercial Vehicles', 'N1-III') }
    else if (tipo_comb == 'DIESEL') {
      if (cil <= 5000) {  c_s = c('Heavy Duty Vehicles', 'Rigid <= 7.5 t') }
      else if (cil <= 6400) {  c_s = c('Heavy Duty Vehicles', 'Rigid 7.5 - 12 t') }
      else if (cil <= 7300) {  c_s = c('Heavy Duty Vehicles', 'Rigid 12 - 14 t') }
      else if (cil <= 8200) {  c_s = c('Heavy Duty Vehicles', 'Rigid 14 - 20 t') }
      else if (cil <= 11000) {  c_s = c('Heavy Duty Vehicles', 'Rigid 20 - 26 t') }
      else if (cil > 11000) {  c_s = c('Heavy Duty Vehicles', 'Rigid 26 - 28 t') }
    }
  }
  # Tractocamiones
  else if(clase %in% c("TRACTOCAMION")){
    if(cil <= 14000) { c_s = c('Heavy Duty Vehicles', 'Articulated 28 - 34 t') }
    else if(cil > 14000) { c_s = c('Heavy Duty Vehicles', 'Articulated 50 - 60 t') }
  }
  
  else { c_s = c(NULL, NULL) }
  
  return(c_s)
}