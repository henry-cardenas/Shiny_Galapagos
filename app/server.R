
df_evaluacion <- data.frame(read_csv("www/tabla_evaluacion/tabla_evaluacion_temperatura.csv"))

#lectura shape estaciones
estaciones_shape <- st_read("www/Estaciones_meteo/galapagos_meteo.shp")

################### Lectura bases de datos de las estaciones ###################
UPS_UPS_basehoraria <- read_csv("www/Bases_usar/UPS_UPS_basehoraria_usar.csv", 
                                col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Cebollar_ETAPA_basehoraria <- read_csv("www/Bases_usar/Cebollar_ETAPA_basehoraria.csv", 
                                       col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

CTS_UPS_basehoraria <- read_csv("www/Bases_usar/CTS_UPS_basehoraria.csv", 
                                col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Cumbe_UPS_basehoraria <- read_csv("www/Bases_usar/Cumbe_UPS_basehoraria.csv", 
                                  col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

ElLabrado_ETAPA_basehoraria <- read_csv("www/Bases_usar/ElLabrado_ETAPA_basehoraria.csv", 
                                        col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Izcayrrumi_ETAPA_basehoraria <- read_csv("www/Bases_usar/Izcayrrumi_ETAPA_basehoraria.csv", 
                                         col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

MachangaraChulco_ETAPA_basehoraria <- read_csv("www/Bases_usar/MachangaraChulco_ETAPA_basehoraria.csv", 
                                               col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Quingeo_UPS_basehoraria <- read_csv("www/Bases_usar/Quingeo_UPS_basehoraria.csv", 
                                    col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

sayausiPTAP_ETAPA_basehoraria <- read_csv("www/Bases_usar/sayausiPTAP_ETAPA_basehoraria.csv", 
                                          col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Tixan_ETAPA_basehoraria <- read_csv("www/Bases_usar/Tixan_ETAPA_basehoraria.csv", 
                                    col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Tixan_UPS_basehoraria <- read_csv("www/Bases_usar/Tixan_UPS_basehoraria.csv", 
                                  col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

Ucubamba_ETAPA_basehoraria <- read_csv("www/Bases_usar/Ucubamba_ETAPA_basehoraria.csv", 
                                       col_types = cols(fecha = col_datetime(format = "%m/%d/%Y %H:%M")))

#### Reemplazo la columna de fechas de la siguiente manera para que sea reconocido en la imagen docker porque daba problemas
fechas = seq.POSIXt(from = as.POSIXct("06/01/2015 00:59", format = "%m/%d/%Y %H:%M"),
                    to = as.POSIXct("12/31/2018 23:59", format = "%m/%d/%Y %H:%M"), 
                    by = "1 hour")

UPS_UPS_basehoraria[1] <- fechas
Cebollar_ETAPA_basehoraria[1] <- fechas
CTS_UPS_basehoraria[1] <- fechas
Cumbe_UPS_basehoraria[1] <- fechas
ElLabrado_ETAPA_basehoraria[1] <- fechas
Izcayrrumi_ETAPA_basehoraria[1] <- fechas
MachangaraChulco_ETAPA_basehoraria[1] <- fechas
Quingeo_UPS_basehoraria[1] <- fechas
sayausiPTAP_ETAPA_basehoraria[1] <- fechas
Tixan_ETAPA_basehoraria[1] <- fechas
Tixan_UPS_basehoraria[1] <- fechas
Ucubamba_ETAPA_basehoraria[1] <- fechas

###

lista_estaciones <- list(UPS_UPS = UPS_UPS_basehoraria,
                         Cebollar_ETAPA = Cebollar_ETAPA_basehoraria,
                         CTS_UPS = CTS_UPS_basehoraria,
                         Cumbe_UPS = Cumbe_UPS_basehoraria,
                         El_Labrado_ETAPA = ElLabrado_ETAPA_basehoraria,
                         Izhcayrrumi_ETAPA = Izcayrrumi_ETAPA_basehoraria,
                         Machangara_Chulco_ETAPA = MachangaraChulco_ETAPA_basehoraria,
                         Quingeo_UPS = Quingeo_UPS_basehoraria,
                         Sayausi_PTAP_ETAPA = sayausiPTAP_ETAPA_basehoraria,
                         Tixan_ETAPA = Tixan_ETAPA_basehoraria,
                         Tixan_UPS_basehoraria = Tixan_UPS_basehoraria,
                         Ucubamba_ETAPA = Ucubamba_ETAPA_basehoraria)

###############################################################################################

shinyServer(function(input, output,session) {
    
    output$tabla_evaluacion <- renderTable(df_evaluacion[,1:5])
    
    output$mymap <- renderLeaflet({
        leaflet(data = estaciones_shape) %>% 
            addProviderTiles(providers$OpenTopoMap)  %>%
            addMarkers(~Longitud , ~Latitud, label= ~NOMBRE, popup = paste0("Altitud: ",estaciones_shape$ALTITUD," m s.n.m.")) %>%
            addMiniMap(toggleDisplay = TRUE,mapOptions = leafletOptions(minZoom = 1, maxZoom = 15)) %>%
            addScaleBar(position = "bottomleft") %>%
            addFullscreenControl() # adds full screen button
    })
    
    output$horarioPlot <- renderHighchart({
            nombre_estacion = input$estacion_id
            datos = data.frame(lista_estaciones[(names(lista_estaciones) %in% c(nombre_estacion))][[1]])
            colnames(datos)<- c("fecha","temperatura","precip", "hum", "rad", "pres", "vviento", "dviento")
            
            hcboxplot(x = datos$temperatura, var = as.POSIXlt(datos$fecha)$hour,
                      name = "Temperatura", color = "#2980b9") %>%
                    hc_add_theme(hc_theme_ffx()) %>% 
                    hc_chart(type = "column") %>%
                    hc_yAxis(labels = list(format = "{value} C"), min = 0) %>%
                    hc_title(text = paste0("Temperaturas por hora - Estacion ",input$estacion_id), align = "center")
    })
    
    output$predictPlot <- renderDygraph({
            
            nombre_estacion = input$estacion_id
            
            datos = data.frame(lista_estaciones[(names(lista_estaciones) %in% c(nombre_estacion))][[1]])
            colnames(datos)<- c("fecha","temperatura","precip", "hum", "rad", "pres", "vviento", "dviento")
            
            #####################################################
            # Convert each variable as time series
            firstHour <- 24*(as.Date("2015-06-01 00:59:00")-as.Date("2015-01-01 00:59:00"))
            
            temp_ts=ts(datos$temperatura,start=c(2015,firstHour),frequency=24*365)
            precip_ts=ts(datos$precip,start=c(2015,firstHour),frequency=24*365)
            hum_ts=ts(datos$hum,start=c(2015,firstHour),frequency=24*365)
            rad_ts=ts(datos$rad,start=c(2015,firstHour),frequency=24*365)
            pres_ts=ts(datos$pres,start=c(2015,firstHour),frequency=24*365)
            vviento_ts=ts(datos$vviento,start=c(2015,firstHour),frequency=24*365)
            dviento_ts=ts(datos$dviento,start=c(2015,firstHour),frequency=24*365)
            
            # #####################################################
            
            x=cbind(temp_ts,hum_ts,vviento_ts);colnames(x)=c("Temperatura","Humedad",'VViento')
            numero_lags = 168 #168 PRO
            #VARselect(x, lag.max=1, type="both")
            # summary(fit <- VAR(x, p=3, type="both")) #Quitar summary que le vuelve muy lento alcodigo
            fit <- vars::VAR(x, p= numero_lags, type="both") #
            
            # #####################################################
            
            alerta_calor = boxplot(datos$temperatura)$stats[5]
            alerta_frio = boxplot(datos$temperatura)$stats[1]
            nhoras_predichas = 168
            forecastingarima = forecast::forecast(fit, h= nhoras_predichas)$forecast$Temperatura
            
            lower <- forecastingarima$lower[,2] #1 for 80 %; 2 for 95% confidence interval
            upper <- forecastingarima$upper[,2] #1 for 80 %; 2 for 95% confidence interval
            pforecasting <- forecastingarima$mean
            #View(forecastingarima$x) #Serie de tiempo original
            ts_graph<- cbind(forecastingarima$x, lower, upper,
                             pforecasting, alerta_calor, alerta_frio)
            # ##########################################
            ts_graph2 <- data.frame(ts_graph)
            # fechas_predichas = seq.POSIXt(from = datos$fecha[nrow(datos)] + hours(1), by = "hour", length.out = nhoras_predichas)
            # fecha_completas = c(datos$fecha, fechas_predichas)
            serie_tiempo = xts(ts_graph2, order.by= seq.POSIXt(from = datos$fecha[1], by = "hour", length.out = 31608)) #serie_tiempo = xts(fecha = fecha_completas,order.by= fecha_completas, ts_graph2) 
            # #########################################
            
            dygraph(serie_tiempo, group = "grupo_variables", main = paste0("Temperatura [C]- ","Estacion ",nombre_estacion)) %>%
                    dyRangeSelector(height = 40,
                                    dateWindow = c("2018-12-25", "2019-01-08")) %>%
                    dySeries('alerta_calor', strokePattern = 'dashed') %>%
                    dySeries('alerta_frio', strokePattern = 'dashed') %>%
                    dySeries(name = "forecastingarima.x", label = "Observado") %>%
                    dySeries(c("lower","pforecasting","upper"), label = "Predicho") %>%
                    dyLegend(show = "always", hideOnMouseOut = FALSE, width = 550) %>%
                    dyHighlight(highlightCircleSize = 5,
                                highlightSeriesOpts = list(strokeWidth = 2)) %>%
                    dyOptions(axisLineColor = "navy", gridLineColor = "green") #%>% #default gray
            #dyAxis("y", label = "[ C]")
            
            # serie_tiempo_uno = xts(fecha = datos$fecha, order.by = datos$fecha, temp_ts)
            # dygraph(serie_tiempo_uno, group = "grupo_variables")
            
    })
    
    output$plot_segunda_variable <- renderDygraph({
        
        nombre_estacion = input$estacion_id
        nombre_variable = input$variable_dos_id
        datos = data.frame(lista_estaciones[(names(lista_estaciones) %in% c(nombre_estacion))][[1]])
        colnames(datos)<- c("fecha","temperatura","precip", "hum", "rad", "pres", "vviento", "dviento")
        nhoras_predichas = 168
        datos_anadidos = data.frame(rbind(as.matrix(datos[, (names(datos) %in% c(nombre_variable))]), as.matrix(rep(NA,nhoras_predichas))))
        
        # Convert each variable as time series
        firstHour <- 24*(as.Date("2015-06-01 00:59:00")-as.Date("2015-01-01 00:59:00"))
        
        variable_ts=ts(datos_anadidos[,1],start=c(2015,firstHour),frequency=24*365)
        
        fechas_predichas = seq.POSIXt(from = datos$fecha[nrow(datos)] + hours(1), by = "hour", length.out = nhoras_predichas)
        fecha_completas = c(datos$fecha, fechas_predichas)
        serie_tiempo_dos = xts(fecha = fecha_completas, order.by = fecha_completas, variable_ts)
        
        dygraph(serie_tiempo_dos, group = "grupo_variables",
                main = paste0(nombre_variable," - Estacion ",nombre_estacion)) %>%
            dyRangeSelector(height = 40, dateWindow = c("2018-12-25", "2019-01-08"))
    })
    
    output$plot_resumen_meteo <- renderPlot({
        nombre_estacion = input$estacion_id
        datos = data.frame(lista_estaciones[(names(lista_estaciones) %in% c(nombre_estacion))][[1]])
        colnames(datos)<- c("fecha","temperatura","precip", "hum", "rad", "pres", "vviento", "dviento")
        
        par(mfrow=c(2,3))
        boxplot(datos$temperatura, main = "Temperatura", ylab = " C")
        boxplot(datos$hum, main = "Humedad", ylab = "%")
        boxplot(datos$rad, main = "Radiacion", ylab = "W/m2")
        boxplot(datos$pres, main = "Presion", ylab = "hPa")
        boxplot(datos$vviento, main = "V.viento", ylab = "m/s")
        boxplot(datos$dviento, main = "D.Viento", ylab = "grados")
    })
    
    output$plot_pca <- renderPlot({
        nombre_estacion = input$estacion_id
        datos = data.frame(lista_estaciones[(names(lista_estaciones) %in% c(nombre_estacion))][[1]])
        colnames(datos)<- c("fecha","temperatura","precip", "hum", "rad", "pres", "vviento", "dviento")
        datosC<-scale(datos[-1])
        #Analisis de Factores 
        res.pca <- PCA(datosC,scale.unit = TRUE,ncp=8,graph = FALSE)
        #Reprsentacion de obervaciones y variables
        fviz_pca_var(res.pca, col.var = "black")
    })
    
    output$plot_dispersion <- renderHighchart({
        nombre_estacion = input$estacion_id
        datos = data.frame(lista_estaciones[(names(lista_estaciones) %in% c(nombre_estacion))][[1]])
        colnames(datos)<- c("fecha","temperatura","precip", "hum", "rad", "pres", "vviento", "dviento")
        
        #####################################################
        # Convert each variable as time series
        firstHour <- 24*(as.Date("2015-06-01 00:59:00")-as.Date("2015-01-01 00:59:00"))
        
        temp_ts=ts(datos$temperatura,start=c(2015,firstHour),frequency=24*365)
        hum_ts=ts(datos$hum,start=c(2015,firstHour),frequency=24*365)
        vviento_ts=ts(datos$vviento,start=c(2015,firstHour),frequency=24*365)
        
        # ########################### EVALUACION
        # ########### eliminar ultimas 168 observaciones de las series de tiempo
        temp_ts_dos <- temp_ts[1:(length(temp_ts)-168)]
        hum_ts_dos <- hum_ts[1:(length(hum_ts)-168)]
        vviento_ts_dos <- vviento_ts[1:(length(vviento_ts)-168)]
        
        #####################################################
        # Convert each variable as time series
        
        temp_ts_dos = ts(temp_ts_dos,start=c(2015,firstHour),frequency=24*365)
        hum_ts_dos = ts(hum_ts_dos,start=c(2015,firstHour),frequency=24*365)
        vviento_ts_dos =ts(vviento_ts_dos,start=c(2015,firstHour),frequency=24*365)
        
        # #####################################################
        
        x=cbind(temp_ts_dos,hum_ts_dos,vviento_ts_dos);colnames(x)=c("Temperatura","Humedad",'VViento')
        numero_lags = 168 #168 PRO
        fit <- VAR(x, p= numero_lags, type="both") #
        
        # #####################################################
        
        nhoras_predichas = 168
        forecastingarima = forecast(fit, h= nhoras_predichas)$forecast$Temperatura
        
        pforecasting <- forecastingarima$mean
        observados =  datos$temperatura[(length(datos$temperatura)-168+1):(length(datos$temperatura))]
        df_evaluar <- data.frame(pforecasting,observados)
        
        colnames(df_evaluar) <- c("Simulados","Observados")
        
        minimo = min(df_evaluar[,1], df_evaluar[,2])-0.5
        maximo = max(df_evaluar[,1], df_evaluar[,2])+0.5
        
        df_linea_ideal = data.frame(c((minimo-1):(maximo+1)),c((minimo-1):(maximo+1)))
        colnames(df_linea_ideal)<- c("ideal1","ideal2")
        modelo_ideal = lm(ideal1 ~ ideal2, data = df_linea_ideal) #add regression line
        fit_line <- broom::augment(modelo_ideal) %>% dplyr::arrange(ideal1)
        
        df_evaluar %>% hchart('scatter', hcaes(x = Observados, y = Simulados)) %>%
            hc_add_series(
                fit_line, type = "line", hcaes(x = ideal2, y = .fitted),
                name = "Ideal", id = "ideal") %>%
            hc_title(text = paste0("Estacion: ",nombre_estacion)) %>%
            hc_add_theme(hc_theme_ffx()) 
    })
    
    output$downloadData <- downloadHandler(
            filename = function() {paste("dataset_galapagos_", Sys.Date(), ".csv", sep="")},
            content = function(file) {write.csv(data.frame(UPS_UPS_basehoraria), 
                                                file,
                                                row.names = FALSE)})
})
