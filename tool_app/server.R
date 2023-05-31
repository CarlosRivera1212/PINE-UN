shinyServer(function(input, output, session) {
  
  # TAB 1 - CONSUMO COMBUSTIBLE
  observe({
    req(input$t1_file_id)
    
    pag = excel_sheets(input$t1_file_id$datapath)
    print(pag)
    
    updateSelectInput(session, 't1_hoja1_id', choices = pag)
    updateSelectInput(session, 't1_hoja2_id', choices = pag)
    updateSelectInput(session, 't1_hoja3_id', choices = pag)
    updateSelectInput(session, 't1_hoja4_id', choices = pag)
  })
  
  observeEvent(input$t1_btn1_id, {
    df1 = read_excel(input$t1_file_id$datapath, input$t1_hoja1_id)
    df2 = read_excel(input$t1_file_id$datapath, input$t1_hoja2_id)
    df3 = read_excel(input$t1_file_id$datapath, input$t1_hoja3_id)
    df4 = read_excel(input$t1_file_id$datapath, input$t1_hoja4_id)
    
    df5 = f_ut_cc(df1, df2, df3)
    df6 = f_be_tc(df5$acti_parq_catv_sum, df4)
    
    output$t1_tbl1_id <- renderDataTable({
      df6
    })
    
    output$t1_ui_dwnld_btn_id <- renderUI({
      downloadButton("t1_dwnld_tbl_id", label = "Descargar tabla")
    })
    
    output$t1_dwnld_tbl_id <- downloadHandler(
      filename = function(){
        fecha_file = format(Sys.time(), "%Y-%m-%d %H.%M.%S")
        paste("TABLA_VALIDACION_", fecha_file, ".xlsx", sep = "")
      },
      content = function(file){
        write_xlsx(df6, file)
      }
    )
    
  })
  
  
  # TAB 2 - CLASIFICACION RUNT-EURO
  observeEvent(input$t2_btn1_id, {
    df_ori = read_excel(input$t2_file_id$datapath)
    
    # CATEGORIA EURO
    df_inp = df_ori %>% 
      mutate(TIPO_SERVICIO_2 = ifelse(TIPO_SERVICIO == 'Publico',
                                      'PUBLICO', 'OTROS'),
             MODELO_2 = ifelse(MODELO>1990,MODELO,1990),
             MODELO_2 = as.factor(MODELO_2))
    
    # reglas_euro = '../df_reg_euro_032823.rda'
    df_euro = f_ceuro(df = df_inp,
                      col_clase = 'CLASE',
                      col_tipo_serv = 'TIPO_SERVICIO_2',
                      col_tipo_comb = 'TIPO_COMBUSTIBLE_FINAL3',
                      col_mod = 'MODELO_2',
                      file_reglas = reglas_euro)
    
    # CATEGORIA RUNT
    df_euro_runt = df_euro
    clas_runt = mapply(
      f_crunt,
      clase = df_euro_runt$CLASE,
      cil = df_euro_runt$CILINDRAJE,
      tipo_motor = df_euro_runt$TIPO_MOTOR_MOTO2,
      tipo_comb = df_euro_runt$TIPO_COMBUSTIBLE_FINAL3,
      cant_eje = df_euro_runt$CANTIDAD_EJES
    )
    df_euro_runt$CAT_RUNT = clas_runt[1,]
    df_euro_runt$SEG_RUNT = clas_runt[2,]
    
    output$t2_tbl1_id <- renderDataTable({
      df_euro_runt
    })
    
    # DESCARGAR TABLA
    output$t2_ui_dwnld_btn_id <- renderUI({
      downloadButton("t2_dwnld_tbl_id", label = "Descargar tabla")
    })
    
    output$t2_dwnld_tbl_id <- downloadHandler(
      filename = function(){
        fecha_file = format(Sys.time(), "%Y-%m-%d %H.%M.%S")
        paste("TABLA_CLASIFICACION_", fecha_file, ".xlsx", sep = "")
      },
      content = function(file){
        write_xlsx(df_euro_runt, file)
      }
    )
  })
  
  
  # TAB 3 - DESAGREGACION
  
  observeEvent(input$t3_file_emis_id, {
    df_emi = read_excel(input$t3_file_emis_id$datapath)
    updateSelectInput(session, 't3_dpto_id', choices = unique(df_emi$DEPARTAMENTO))
  })
  
  
  observeEvent(input$t3_btn1_id, {

    df_grid = read_excel(input$t3_file_grid_id$datapath) %>%
      filter(TIPO == input$t3_urban_id,
             !is.na(ELEV))

    df_emi = read_excel(input$t3_file_emis_id$datapath) %>%
      filter(TIPO == input$t3_urban_id)

    df_temp = read_excel(input$t3_file_temp_id$datapath) %>%
      filter(TIPO == input$t3_urban_id)


    print('PROCESANDO')
    
    res_ds = f_ds_2(
      df_grid,
      df_emi,
      'CX', 'CY', input$t3_cont_id,
      c(0.6, 0.3, 0.1)
    )
    
    res_dst = left_join(
      res_ds,
      filter(df_temp, TIPO==input$t3_urban_id),
      'TIPO',
      multiple='all') %>%
      mutate(HORA_cut = cut(HORA, seq(0,24,4), include.lowest = TRUE)) %>% 
      group_by(CX, CY, HORA_cut) %>% 
      mutate(Yf_1 = Yf_1*POND_TEMP,
             Yf_2 = Yf_2*POND_TEMP)
    
    
    # RESULTADOS
    print('FIN')
    
    # SIN PONDERAR
    output$t3_plt1_1_id <- renderPlot({
      res_ds %>%
        ggplot()+
        aes(CX, CY, fill=Y_1)+
        geom_tile()+
        coord_equal()+
        labs(
          title = 'Des Espacial Sin Modelo',
          subtitle = 'Sin Ponderacion'
        )+
        theme_bw()
    })
    output$t3_plt1_2_id <- renderPlot({
      res_ds %>%
        ggplot()+
        aes(CX, CY, fill=Yf_1)+
        geom_tile()+
        coord_equal()+
        labs(
          title = 'Des Espacial Con Modelo',
          subtitle = 'Sin Ponderacion'
        )+
        theme_bw()
    })
    
    # PONDERADO
    output$t3_plt2_1_id <- renderPlot({
      res_ds %>%
        ggplot()+
        aes(CX, CY, fill=Y_2)+
        geom_tile()+
        coord_equal()+
        labs(
          title = 'Des Espacial Sin Modelo',
          subtitle = 'Ponderacion: 60% - 30% - 10%'
        )+
        theme_bw()
    })
    output$t3_plt2_2_id <- renderPlot({
      res_ds %>%
        ggplot()+
        aes(CX, CY, fill=Yf_2)+
        geom_tile()+
        coord_equal()+
        labs(
          title = 'Des Espacial Con Modelo',
          subtitle = 'Ponderacion: 60% - 30% - 10%'
        )+
        theme_bw()
    })
    
    # ESPACIAL
    output$t3_plt3_1_id <- renderPlot({
      res_dst %>% 
        ggplot()+
        aes(CX,CY, fill=Yf_1)+
        geom_tile()+
        coord_equal()+
        facet_wrap(~HORA_cut, nrow=1)+
        labs(
          title = 'Des Temporal Con Modelo',
          subtitle = 'Sin Ponderacion'
        )+
        theme_bw()
    })
    
    output$t3_plt3_2_id <- renderPlot({
      res_dst %>% 
        ggplot()+
        aes(CX,CY, fill=Yf_2)+
        geom_tile()+
        coord_equal()+
        facet_wrap(~HORA_cut, nrow=1)+
        labs(
          title = 'Des Temporal Con Modelo',
          subtitle = 'Ponderacion: 60% - 30% - 10%'
        )+
        theme_bw()
    })
    
    output$t3_tbl1_id <- renderDataTable({
      res_ds
    })
    
    output$t3_ui_dwnld_btn_id <- renderUI({
      downloadButton("t3_dwnld_tbl_id", label = "Descargar tabla")
    })
    
    output$t3_dwnld_tbl_id <- downloadHandler(
      filename = function(){
        fecha_file = format(Sys.time(), "%Y-%m-%d %H.%M.%S")
        paste("TABLA_DESAGREGACION_", fecha_file, ".xlsx", sep = "")
      },
      content = function(file){
        write_xlsx(res_ds, file)
      }
    )
  })
})
