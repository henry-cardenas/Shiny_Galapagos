library(shiny)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(dygraphs)
library(highcharter)
library(stats)
library(vars)
library(graphics)
library(forecast)
library(xts)
library(lubridate)
library(FactoMineR) #para el PCA
library(factoextra) #para el PCA
library(broom) #para ajuste lineal ideal
library(dplyr) #para ajuste lineal ideal
library(sf)
library(leaflet)
library(leaflet.extras)

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Galapagos"),
                    dashboardSidebar(
                        div(style="padding-top:10px;padding-left:10px;text-align:center;", img(src = "logo_instituto_v2.png", height = 78, width = 202)),
                        # tags$p("    Intituto de Geografía"),
                        # tags$p(""),
                        # tags$p("hum = Humedad Relativa [%]"),
                        # tags$p("rad = Radiacion Solar [w/m2]"),
                        # tags$p("pres = Presion Atmosferica [hPa]"),
                        # tags$p("vviento = Velocidad del Viento [m/s]"),
                        # tags$p("dviento = Direccion del Viento [grados]"),
                        # tags$p("precip = Precipitacion [mm]"),
                        selectizeInput(
                            'estacion_id', 'Seleccionar estacion', size = 12,choices = c("UPS_UPS", "Cebollar_ETAPA", "CTS_UPS", 
                                                                                         "Cumbe_UPS", "El_Labrado_ETAPA","Izhcayrrumi_ETAPA",
                                                                                         "Machangara_Chulco_ETAPA", "Quingeo_UPS", "Sayausi_PTAP_ETAPA",
                                                                                         "Tixan_ETAPA","Tixan_UPS","Ucubamba_ETAPA")
                        ),
                        selectizeInput(
                                'variable_dos_id', 'Seleccionar segunda variable', size = 7,choices = c("hum", "rad", "pres", "vviento", "dviento","precip","temperatura")
                        )
                        #sliderInput("dias_id","Numero dias de prediccion:",min = 1,max = 30,value = 7,step = 1),
                        #actionButton("boton_aplicar", "Aplicar"),
                    ),
                    dashboardBody(
                        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                        fluidRow(
                            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                            box(title = "Visualizador geografico de Estaciones Meteorologicas", solidHeader = T,
                                width = 6, height = "25%",collapsible = F,
                                leafletOutput("mymap",height = "440px")
                            ),
                            box(title = "Visualizador general", solidHeader = T,
                                width = 6, collapsible = F,
                                tabsetPanel(
                                    tabPanel("Predicciones Temperatura",
                                             #h4("Visualizador y predicciones de Temperatura"),
                                             dygraphOutput("predictPlot")
                                    ),
                                    tabPanel("Temperatura horaria",
                                             #h4("Visualizador y predicciones de Temperatura"),
                                             highchartOutput("horarioPlot")
                                    ),
                                    tabPanel("Meteorologia",
                                             h4("Boxplots de variables meteorologicas"),
                                             plotOutput("plot_resumen_meteo")
                                    ),
                                    tabPanel("PCA",
                                             h4("Analisis de Componentes Principales"),
                                             plotOutput("plot_pca")
                                    ),
                                    tabPanel("Estadisticas del modelo",
                                             h4("Medidas de Bondad de Ajuste del modelo de prediccion de temperatura"),
                                             tableOutput("tabla_evaluacion")
                                    ),
                                    tabPanel("Ajuste",
                                             h4("Ajuste Simulados vs. Observados"),
                                             highchartOutput("plot_dispersion")
                                    )
                                )
                            ),
                            box(title = "Extremos climaticos", solidHeader = T,
                                width = 6, height = "950px",collapsible = T,
                                tabsetPanel(
                                        tabPanel("CWN_CWD",
                                                 h4("Olas de Frio"),
                                                 img(src = "indices_extremos/CWN_y_CWD.png", height = "800px", width = 650)
                                        ),
                                        tabPanel("HWN_y_HWD",
                                                 h4("Olas de Calor"),
                                                 img(src = "indices_extremos/HWN_y_HWD.png", height = "800px", width = 650)
                                        ),
                                        tabPanel("TX10p",
                                                 h4("Dias Frios"),
                                                 img(src = "indices_extremos/TX10p_referencia.png", height = "800px", width = 650)
                                        ),
                                        tabPanel("TX90p",
                                                 h4("Dias calidos"),
                                                 img(src = "indices_extremos/TX90p_referencia.png", height = "800px", width = 650)
                                        )
                                )
                            ),
                            box(title = "Variables meteorológicas", solidHeader = T,
                                width = 6, height = "100%",collapsible = F,
                                tabsetPanel(
                                        tabPanel("Radiacion",
                                                 #h4("Visualizador y predicciones de Temperatura"),
                                                 dygraphOutput("plot_segunda_variable")
                                        ),
                                        tabPanel("Humedad",
                                                 #h4("Visualizador y predicciones de Temperatura"),
                                                 dygraphOutput("plot_segunda_variable")
                                        ),
                                        tabPanel("Presion",
                                                 dygraphOutput("plot_segunda_variable")
                                        ),
                                        tabPanel("Precipitacion",
                                                 dygraphOutput("plot_segunda_variable")
                                        ),
                                        tabPanel("V_Viento",
                                                 dygraphOutput("plot_segunda_variable")
                                        ),
                                        tabPanel("D_Viento",
                                                 dygraphOutput("plot_segunda_variable")
                                        )
                                )
                            )
                        ) # row
                    ) # body
)