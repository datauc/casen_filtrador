library(shiny)

shinyServer(function(input, output) {

    datos_casen <- reactive({
      load("data/Casen Tarapacá 2017.Rdata")

      casen
    })

    source("filtrador_casen.R", local = TRUE)

    #Output tabla ----
    output_tabla_casen <- reactive({

      #Base normal sin expansión
      if (input$expansion_casen==FALSE) {
        output <- datos_filtrados() %>%
          group_by(comuna) %>%
          count(name="cantidad") %>%
          ungroup() %>%
          mutate(porcentaje = cantidad/sum(cantidad))

        #Cálculo con expansión
      } else if (input$expansion_casen==TRUE) {
        output <- datos_filtrados() %>%
          group_by(comuna) %>%
          summarise(cantidad = n(),
                    expc = mean(expc)) %>%
          mutate(cantidad = cantidad*expc) %>%
          mutate(porcentaje = cantidad/sum(cantidad))
      }

      output
    })


    selector_tipo_casen <- renderText({
      as.character(input$selector_tipo_casen)
    })

    #Output gráfico ----
    output_grafico_casen <- reactive({
      grafico_g <- output_tabla_casen() %>%
        ggplot(aes(forcats::fct_reorder(stringr::str_wrap(comuna, 4), cantidad, .desc=T),
                   cantidad,
                   fill=cantidad)) +
        geom_col(width=0.7) +
        geom_text(aes(label = stringr::str_trim(format(cantidad, big.mark =  "."))),
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
          scale_x_continuous(labels = function(x) stringr::str_trim(format(x, big.mark = ".")),
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
          scale_x_continuous(labels = function(x) stringr::str_trim(format(x, big.mark = ".")),
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
    output_mapa_casen <- reactive({

      #Obtener mapa
      mapa_regional <- chilemapas::mapa_comunas %>%
        left_join(
          chilemapas::codigos_territoriales %>%
            select(matches("comuna"))
        ) %>%
        rename(comuna=nombre_comuna) %>%
        mutate(comuna = recode(comuna,
                               "Camina"="Camiña")) %>%
        left_join(output_tabla_casen() ) %>%
        filter(codigo_region=="01")

      #Graficar
      #Frecuencia ----
      if (input$selector_tipo_casen=="Frecuencia") {
        mapa_regional_g <- mapa_regional %>%
          ggplot(aes(geometry = geometry,
                     fill = cantidad )) +
          geom_sf(col="white") +
          geom_sf_label(aes(label = comuna),
                        size=4,
                        label.padding = unit(0.15, "lines"),
                        fill="white",
                        color="#999999") +
          geom_sf_label(aes(label = stringr::str_trim(format(cantidad, big.mark =  "."))),
                        size=4,
                        label.padding = unit(0.15, "lines"),
                        nudge_y=-0.08,
                        fill="white",
                        color="#999999") +
          viridis::scale_fill_viridis(name="Frecuencia",
                                      labels = function(x) format(x, big.mark =  ".") ) +
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
          geom_sf(col="white") +
          geom_sf_label(aes(label = comuna),
                        size=4,
                        label.padding = unit(0.15, "lines"),
                        fill="white",
                        color="#999999") +
          geom_sf_label(aes(label = ifelse(comuna!="Colchane",
                                           paste0(round(porcentaje*100, digits=1),"%"),
                                           "NA")),
                        size=4,
                        label.padding = unit(0.15, "lines"),
                        nudge_y=-0.08,
                        fill="white",
                        color="#999999") +
          viridis::scale_fill_viridis(name="Porcentaje",
                                      labels = function(x) paste0(round(x*100, digits=1),"%") ) +
          coord_sf(expand = FALSE) +
          theme_minimal(base_size = 15) +
          theme(legend.position = "right",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank())
      }


      mapa_regional_g
    })

    output$output_mapa_casen <- renderPlot({
      output_mapa_casen()
    })
    
    
    
    
    
    
    
    
    
    
    
    
})
