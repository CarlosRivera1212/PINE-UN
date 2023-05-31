shinyUI(fluidPage(
  titlePanel("Proyecto Inventario Nacional Emisiones"),
  
  tabsetPanel(
    tabPanel(title = 'Consumos Combustible',
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 fileInput('t1_file_id', 'Archivo Validación Combustible'),
                 
                 selectInput('t1_hoja1_id', 'Hoja - ACTIVIDAD (KM ANUALES)', choices = NULL),
                 selectInput('t1_hoja2_id', 'Hoja - PARQUE AUTOMOTOR', choices = NULL),
                 selectInput('t1_hoja3_id', 'Hoja - CATEG VEHIC Y CARACT', choices = NULL),
                 selectInput('t1_hoja4_id', 'Hoja - VENTAS', choices = NULL),
                 
                 actionButton('t1_btn1_id', 'Cargar')
               ),
               mainPanel(
                 width = 9,
                 uiOutput('t1_ui_dwnld_btn_id'),
                 tags$hr(),
                 dataTableOutput('t1_tbl1_id')
               )
             )),
    tabPanel(title = 'Clasificacion',
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 fileInput('t2_file_id', 'Archivo'),
                 actionButton('t2_btn1_id', 'Cargar')
               ),
               mainPanel(
                 width = 9,
                 uiOutput('t2_ui_dwnld_btn_id'),
                 tags$hr(),
                 dataTableOutput('t2_tbl1_id')
               )
             )),
    tabPanel(title = 'Desagregacion',
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 fileInput('t3_file_grid_id', 'Archivo Grid'),
                 fileInput('t3_file_emis_id', 'Archivo Emisiones'),
                 fileInput('t3_file_temp_id', 'Archivo Pond. Temporal'),
                 radioButtons('t3_urban_id', 'Tipo',
                              c('URBANO', 'INTERURBANO')),
                 selectInput(
                   't3_cont_id',
                   'Contaminante',
                   c('BC', 'CO', 'CO2', 'NO2', 'PM 2.5', 'SO2')
                 ),
                 # checkboxInput('t3_udpto_id', 'Usar Departamento', FALSE),
                 # conditionalPanel(
                 #   condition = "input.t3_udpto_id == true",
                 #   selectInput('t3_dpto_id', 'Departamento', NULL)
                 # ),
                 actionButton('t3_btn1_id', 'Cargar'),
                 tags$hr(),
                 uiOutput('t3_ui_dwnld_btn_id')
               ),
               mainPanel(width = 10,
                         tabsetPanel(
                           tabPanel(
                             title = 'Espacial',
                             fluidRow(
                               conditionalPanel(
                                 condition = "$('html').hasClass('shiny-busy')",
                                 # tags$div("Cargando...",id="loadmessage")
                                 textOutput('t3_load') %>%
                                   withSpinner(type = 5, size = 2)
                               ),
                               # column(width = 5, uiOutput('t3_ui_plt1_id')),
                               column(width = 5, plotOutput('t3_plt1_1_id', height = '600px')),
                               column(width = 5, plotOutput('t3_plt1_2_id', height = '600px')),
                               column(width = 5, plotOutput('t3_plt2_1_id', height = '600px')),
                               column(width = 5, plotOutput('t3_plt2_2_id', height = '600px'))
                             )
                           ),
                           tabPanel(title = 'Temporal',
                                    plotOutput('t3_plt3_1_id'),
                                    plotOutput('t3_plt3_2_id')),
                           tabPanel(title = 'Tabla',
                                    dataTableOutput('t3_tbl1_id'))
                         ))
             ))
  )
))
