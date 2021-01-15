library(shiny)

shinyServer(function(input, output, session) {

    
    updatePickerInput(session,
                      inputId = "selector_comunas_barras",
                      choices = datos_casen %>% select(comuna) %>% distinct() %>% pull() %>% as.character(),
                      selected = c("La Florida", "Puente Alto", "La Cisterna", "Cerrillos", "Ñuñoa", "Vitacura", "Providencia", "Maipú", "Santiago")
    )

    source("filtrador_casen.R", local = TRUE)

    #Output tabla ----
    output_tabla_casen <- reactive({

      # #Base normal sin expansión
      # if (input$expansion_casen==FALSE) {
        output <- datos_filtrados() %>%
          group_by(comuna) %>%
          count(name="cantidad") %>%
          ungroup() %>%
          mutate(porcentaje = cantidad/sum(cantidad))

      #   #Cálculo con expansión
      # } else if (input$expansion_casen==TRUE) {
      #   output <- datos_filtrados() %>%
      #     group_by(comuna) %>%
      #     summarise(cantidad = n()) %>%
      #     #mutate(cantidad = cantidad*expc) %>%
      #     mutate(porcentaje = cantidad/sum(cantidad))
      # }

      output
    })


    selector_tipo_casen <- renderText({
      as.character(input$selector_tipo_casen)
    })

    #Output gráfico ----
    output_grafico_casen <- reactive({
      grafico_g <- output_tabla_casen() %>%
        filter(comuna %in% input$selector_comunas_barras) %>%
        ggplot(aes(forcats::fct_reorder(comuna, cantidad, .desc=T),
                   cantidad,
                   fill=cantidad)) +
        geom_col(width=0.7) +
        geom_text(aes(label = stringr::str_trim(format(cantidad, big.mark =  ".", decimal.mark = ","))),
                  vjust=-0.8,
                  size=4) +
        labs(y="Cantidad",
             x="") +
        theme_minimal(base_size = 15) +
        viridis::scale_fill_viridis() +
        scale_y_continuous(expand = expansion(mult=c(0, 0.2))) +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5),
              axis.title.y = element_text(margin=margin(r=10)))

      grafico_g

    })

    output$output_grafico_casen <- renderPlot({
      output_grafico_casen()
    })


    #Output densidad ----
    output_densidad_casen <- reactive({

      if (input$selector_densidad_pobreza_casen==TRUE) {

        densidad_g <- datos_filtrados() %>%
          filter(!is.na(pobreza)) %>%
          ggplot(aes(x = ytotcor,
                     fill = pobreza,
                     col = pobreza)) +
          geom_density(kernel = "gaussian",
                       bw = 50000, n=20000,
                       alpha = 0.9) +
          scale_x_continuous(labels = function(x) stringr::str_trim(format(x, big.mark = ".", decimal.mark=",")),
                             breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1250000, 1500000, 1750000, 2000000, 2500000, 3000000, 3500000, 4000000, 5000000, 6000000, 7000000, 8000000, 9000000, 10000000)) +
          theme_minimal(base_size=15) +
          scale_fill_viridis_d(aesthetics = c("fill", "col")) +
          coord_cartesian(xlim = c(0, 3000000),
                          #ylim = c(0, 0.000004),
                          expand = FALSE) +
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                legend.margin = margin(0, 20, 20, 0),
                legend.text=element_text(margin=margin(r=10)),
                axis.text.x = element_text(angle=-90, hjust=0,
                                           margin=margin(t=5)),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank())

      } else if (input$selector_densidad_pobreza_casen==FALSE) {

        densidad_g <- datos_filtrados() %>%
          filter(!is.na(pobreza)) %>%
          ggplot(aes(x = ytotcor,
                     fill = "pobreza",
                     col = "pobreza")) +
          geom_density(kernel = "gaussian",
                       bw = 50000, n=20000,
                       alpha = 0.9) +
          scale_x_continuous(labels = function(x) stringr::str_trim(format(x, big.mark = ".", decimal.mark=",")),
                             breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1250000, 1500000, 1750000, 2000000, 2500000, 3000000, 3500000, 4000000, 5000000, 6000000, 7000000, 8000000, 9000000, 10000000)) +
          theme_minimal(base_size=15) +
          scale_fill_viridis_d(aesthetics = c("fill", "col")) +
          coord_cartesian(xlim = c(0, 4000000),
                          #ylim = c(0, 0.000004),
                          expand = FALSE) +
          theme(legend.position = "none",
                legend.title = element_blank(),
                legend.margin = margin(0, 20, 20, 0),
                legend.text=element_text(margin=margin(r=10)),
                axis.text.x = element_text(angle=-90, hjust=0,
                                           margin=margin(t=5)),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank())
      }

      if (input$selector_densidad_genero_casen==TRUE) {
        densidad_g <-  densidad_g +
          facet_wrap(~sexo) +
          theme(panel.spacing.x =unit(1, "cm"))
      } else {
        densidad_g <- densidad_g
      }


      densidad_g

    })

    output$output_densidad_casen <- renderPlot({
      output_densidad_casen()
    })







    #Output mapa ----
    output$output_mapa_casen <- renderGirafe({

      #Obtener mapa
      mapa_regional <- chilemapas::mapa_comunas %>%
        left_join(
          chilemapas::codigos_territoriales %>%
            select(matches("comuna"))
        ) %>%
        rename(comuna=nombre_comuna) %>%
        mutate(comuna = recode(comuna,
                               "San Jose de Maipo" = "San José de Maipo",
                               "Alhue" = "Alhué",
                               "Curacavi" = "Curacaví",
                               "Maria Pinto" = "María Pinto",
                               "Nunoa" = "Ñuñoa",
                               "Penalolen" = "Peñalolén",
                               "Maipu" = "Maipú",
                               "Penaflor" = "Peñaflor",
                               "San Ramon" = "San Ramón",
                               "San Joaquin" = "San Joaquín",
                               "Estacion Central" = "Estación Central",
                               "Conchali" = "Conchalí")) %>%
        left_join(output_tabla_casen() ) %>%
        filter(codigo_region=="13") #%>%
        # filter(comuna != "San Jose de Maipo",
        #        comuna != "San Pedro",
        #        comuna != "Alhue")

      #Graficar
      #Frecuencia ----
      if (input$selector_tipo_casen=="Frecuencia") {
        mapa_regional_g <- mapa_regional %>%
          ggplot(aes(geometry = geometry,
                     fill = cantidad )) +
          geom_sf_interactive(col="white",
                              aes(tooltip = paste(comuna, "\n",
                                                  stringr::str_trim(format(cantidad, big.mark =  ".", decimal.mark = ","))
                                                  ))) +
          # geom_sf_label(aes(label = comuna),
          #               size=4,
          #               label.padding = unit(0.15, "lines"),
          #               fill="white",
          #               color="#999999") +
          # geom_sf_label(aes(label = stringr::str_trim(format(cantidad, big.mark =  ".", decimal.mark = ","))),
          #               size=4,
          #               label.padding = unit(0.15, "lines"),
          #               nudge_y=-0.08,
          #               fill="white",
          #               color="#999999") +
          viridis::scale_fill_viridis(name="Frecuencia",
                                      labels = function(x) format(x, big.mark =  ".", decimal.mark = ",") ) +
          coord_sf(expand = FALSE) +
          theme_minimal(base_size = 15) +
          theme(legend.position = "right",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank())

        #Porcentaje ----
      } else if (input$selector_tipo_casen=="Porcentaje") {
        mapa_regional_g <- mapa_regional %>%
          ggplot(aes(geometry = geometry,
                     fill = porcentaje )) +
          geom_sf_interactive(col="white",
                              aes(tooltip = paste(comuna, "\n",
                                                  stringr::str_trim(paste0(round(porcentaje*100, digits=1),"%"))
                              ))) +
          # geom_sf_label(aes(label = comuna),
          #               size=4,
          #               label.padding = unit(0.15, "lines"),
          #               fill="white",
          #               color="#999999") +
          # geom_sf_label(aes(label = ifelse(comuna!="Colchane",
          #                                  paste0(round(porcentaje*100, digits=1),"%"),
          #                                  "NA")),
          #               size=4,
          #               label.padding = unit(0.15, "lines"),
          #               nudge_y=-0.08,
          #               fill="white",
          #               color="#999999") +
          viridis::scale_fill_viridis(name="Porcentaje",
                                      labels = function(x) paste0(round(x*100, digits=1),"%") ) +
          coord_sf(expand = FALSE) +
          theme_minimal(base_size = 15) +
          theme(legend.position = "right",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank())
      }


      girafe(ggobj = mapa_regional_g,
             pointsize = 18,
             #bg = color_verde_oficial_medio,
             width_svg = 12, height_svg = 8,
             fonts = list(sans = "Open Sans"),
             options = list(
               opts_sizing(rescale = FALSE, width = 1),
               opts_toolbar(position = "topright", saveaspng = FALSE)))
    })

    # output$output_mapa_casen <- renderPlot({
    #   output_mapa_casen()
    # })
    
  #Texto filtro ----
    
    output$texto_filtro <- renderText({
      
      texto <- vector() #crear vector
      
      
      #pobreza por ingresos
      if (is.null(input$pobreza_ingresos_casen)) { 
        
        } else if (input$pobreza_ingresos_casen == "Pobreza extrema" & input$pobreza_ingresos_casen == "Pobreza no extrema") {
        texto <-  append(texto, "en situación de pobreza extrema")
        
      } else if (input$pobreza_ingresos_casen == "Pobreza no extrema") {
        texto <-  append(texto, "en situación de pobreza no extrema")
        
      } else if (input$pobreza_ingresos_casen == "Pobreza extrema") {
        texto <-  append(texto, "en situación de pobreza extrema")
      
      }
      
      #pobreza multidimensional
      if (!is.null(input$pobreza_multi_casen)) {
        texto <-  append(texto, "en situación de pobreza multidimensional")
      }
      
      #slider percentil
      if (is.null(input$percentil_slider_casen)) { 
        
      } else if (input$percentil_slider_casen != 100) {
       
        texto <-  append(texto, 
                         paste0("perteneciente al ", 
                                input$percentil_slider_casen, "% de los ingresos más bajos"))
      }
      
      #mujeres
      if (!is.null(input$mujeres_casen)) {
        texto <-  append(texto, "de género femenino")
      }
      
      
      if (!is.null(input$migrantes_casen)) {
        texto <-  append(texto, "en condición de migrantes")
      }
      
      
      if (input$edad_slider_casen != 0) {
        texto <-  append(texto, paste0("mayores de ",
                                       input$edad_slider_casen, " años"))
      }
      
      
      if (input$escolaridad_slider_casen < 25) {
        texto <-  append(texto, paste0("con menos de ",
                                       input$escolaridad_slider_casen, " años de educación"))
      }
      
      
      if (!is.null(input$indigena_casen)) {
        texto <-  append(texto, "que pertenecen a un pueblo originario")
      }
      
      
      if (!is.null(input$inactivos_casen)) {
        texto <-  append(texto, "que no integran la fuerza de trabajo (inactividad)")
      }
      
      
      if (!is.null(input$informalidad_casen)) {
        texto <-  append(texto, "que carecen de contrato laboral (informalidad)")
      }
      
      
      if (!is.null(input$trabajo_domestico_casen)) {
        texto <-  append(texto, "que se dedican al trabajo doméstico remunerado (trabajadoras/es de casa particular)")
      }
      
      
      if (is.null(input$zona_casen)) { 
        
      } else if (input$zona_casen == "Rural" & input$zona_casen == "Urbano") {
        texto <- texto
        
      } else if (input$zona_casen == "Urbano") {
        texto <-  append(texto, "que viven en zona urbana")
        
      } else if (input$zona_casen == "Rural") {
        texto <-  append(texto, "que viven en zona rural")
        }
      
      
      if (!is.null(input$hacinamiento_casen)) {
        texto <-  append(texto, "que viven en condición de hacinamiento")
      }
      
      
      if (!is.null(input$servicios_casen)) {
        texto <-  append(texto, "en situación de aceso deficiente a servicios básicos")
      }
      
      
      if (is.null(input$vivienda_calidad_casen)) { 
        
      } else if (input$vivienda_calidad_casen == "Recuperable" & input$vivienda_calidad_casen == "Irrecuperable") {
        texto <- texto
        
      } else if (input$vivienda_calidad_casen == "Recuperable") {
        texto <-  append(texto, "cuya calidad de su vivienda es inaceptable pero recuperable")
      
      } else if (input$vivienda_calidad_casen == "Irrecuperable") {
        texto <-  append(texto, "cuya calidad de su vivienda es inaceptable y considerada irrecuperable")
      }
      
      
      if (!is.null(input$malnutricion_casen)) {
        texto <-  append(texto, "que vive con menores de edad en situación de malnutrición")
      }
      
      
      if (!is.null(input$comida_sana_casen)) {
        texto <-  append(texto, "con dificultades para acceder a alimentación saludable")
      }
      
      
      if (!is.null(input$comida_insuficiencia_casen)) {
        texto <-  append(texto, "que experimenta insuficiencia de alimentos debido a su falta de recursos")
      }
      
      
      texto_listo <- texto
      
      #poner texto por defecto si no se ha seleccionado nada
      if (length(texto) == 0) {
        texto_listo <- append(texto_listo, "mostrando toda la población")
      }
      
      #unir textos de filtros activos y separar con comas
      texto_2 <- paste(texto_listo, collapse = ", ") 
      
      #poner título y punto al final
      if (length(texto) == 0) {
        texto_3 <- paste0("<b>Población filtrada:</b> ", texto_2, ". ") 
      } else {
      texto_3 <- paste0("<b>Población filtrada:</b> personas ", texto_2, ". ") 
      }
      
      return(texto_3)
    })
    
    
    
    
    
    
    
})
