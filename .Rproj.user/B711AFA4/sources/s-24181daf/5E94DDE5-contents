#Filtrador ----
datos_filtrados <- reactive({
  
  filtrado <- datos_casen()
  
  #Mujeres ----
  if (!is.null(input$mujeres_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(sexo=="Mujer")
  } else if (is.null(input$mujeres_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  #Migrantes ----
  if (!is.null(input$migrantes_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(r1a=="Otra nacionalidad. Especifique país")
  } else if (is.null(input$migrantes_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  
  
  #Pobreza ingresos ----
  if (is.null(input$pobreza_ingresos_casen)) { #FALSE
    filtrado <- filtrado
  } else if (length(input$pobreza_ingresos_casen) > 1 ) { #TRUE 1+2
    filtrado <- filtrado %>%
      filter(pobreza != "No pobres")
  } else if (input$pobreza_ingresos_casen == "Pobreza no extrema") { #TRUE 1
    filtrado <- filtrado %>%
      filter(pobreza == "Pobres no extremos")
  } else if (input$pobreza_ingresos_casen == "Pobreza extrema") { #TRUE 2
    filtrado <- filtrado %>%
      filter(pobreza == "Pobres extremos")
  } else if (!is.null(input$pobreza_ingresos_casen)) { #FALSE?
    filtrado <- filtrado
  }
  
  #Pobreza multi ----
  if (!is.null(input$pobreza_multi_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(pobreza_multi_4d == "Pobre")
  } else if (is.null(input$pobreza_multi_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  #Percentil ----
  percentil_valor <- reactive({
    as.integer(input$percentil_slider_casen_casen)/100
  })
  
  #Cálculo del percentil
  percentil <- reactive({
    
    
    percentil <- quantile(datos_casen()$ytrabajocorh, 
                          probs = percentil_valor() )
    percentil
  })
  
  #Output del tramo de corte
  output$percentil_elegido <- renderText({
    paste0("Ingresos por hogar menores a $", format(percentil(), big.mark = "."))
  })
  
  #Lógica
  if (input$percentil_slider_casen==100) {
    filtrado <- filtrado
  } else if (input$percentil_slider_casen!=100) {
    filtrado <- filtrado %>%
      filter(ytrabajocorh < percentil() )
  }
  
  
  #Escolaridad ----
  escolaridad_valor <- reactive({
    as.integer(input$escolaridad_slider_casen)
  })
  
  if(input$escolaridad_slider_casen==25){
    filtrado <- filtrado
  } else if (input$escolaridad_slider_casen!=25) {
    filtrado <- filtrado %>%
      filter(esc <= escolaridad_valor()  )
  }
  
  
  #Edad ----
  edad_valor <- reactive({
    as.integer(input$edad_slider_casen)
  })
  
  
  if(input$edad_slider_casen==0){
    filtrado <- filtrado
  } else if (input$edad_slider_casen!=0) {
    filtrado <- filtrado %>%
      filter(edad >= edad_valor()  )
  }
  
  
  #Pueblos originarios ----
  if (!is.null(input$indigena_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(r3 != "No pertenece a ningún pueblo indígena")
  } else if (is.null(input$indigena_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  
  
  #Inactivos ----
  if (!is.null(input$inactivos_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(activ=="Inactivos")
  } else if (is.null(input$inactivos_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  #Sin contrato ----
  if (!is.null(input$informalidad_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(o17=="No tiene")
  } else if (is.null(input$informalidad_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  #Trabajadoras domésticas ----
  if (!is.null(input$trabajo_domestico_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(stringr::str_detect(oficio4, "doméstico"))
  } else if (is.null(input$trabajo_domestico_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  #Zona ----
  if (is.null(input$zona_casen)) { #FALSE
    filtrado <- filtrado
  } else if (length(input$zona_casen) > 1 ) { #TRUE 1+2
    filtrado <- filtrado
  } else if (input$zona_casen == "Urbano") { #TRUE 1
    filtrado <- filtrado %>%
      filter(zona == "Urbano")
  } else if (input$zona_casen == "Rural") { #TRUE 2
    filtrado <- filtrado %>%
      filter(zona == "Rural")
  } else if (!is.null(input$zona_casen)) { #FALSE?
    filtrado <- filtrado
  }
  
  
  #Hacinamiento ----
  if (!is.null(input$hacinamiento_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(hacinamiento!="Sin hacinamiento (2,49 y menos)")
  } else if (is.null(input$hacinamiento_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  
  # Servicios básicos  ----
  if (!is.null(input$servicios_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(hh_d_servbas==1)
  } else if (is.null(input$servicios_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  #Calidad vivienda ----
  if (is.null(input$vivienda_calidad_casen)) { #FALSE
    filtrado <- filtrado
  } else if (length(input$vivienda_calidad_casen) > 1 ) { #TRUE 1+2
    filtrado <- filtrado %>%
      filter(calglobviv != "Aceptable",
             calglobviv != "Sin dato")
  } else if (input$vivienda_calidad_casen == "Recuperable") { #TRUE 1
    filtrado <- filtrado %>%
      filter(calglobviv == "Recuperable")
  } else if (input$vivienda_calidad_casen == "Irrecuperable") { #TRUE 2
    filtrado <- filtrado %>%
      filter(calglobviv == "Irrecuperable")
  } else if (!is.null(input$vivienda_calidad_casen)) { #FALSE?
    filtrado <- filtrado
  }
  
  
  
  
  # Malnutrición de menores  ----
  if (!is.null(input$malnutricion_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(hh_d_mal==1)
  } else if (is.null(input$malnutricion_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  # Dificultad para acceder a alimentación sana ----
  if (!is.null(input$comida_sana_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(r8b=="Sí")
  } else if (is.null(input$comida_sana_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  # Insuficiencia de alimentos ----
  if (!is.null(input$comida_insuficiencia_casen)) { #TRUE
    filtrado <- filtrado %>%
      filter(r8e=="Sí")
  } else if (is.null(input$comida_insuficiencia_casen)) { #FALSE
    filtrado <- filtrado
  }
  
  
  #Output filtro ----
  filtrado
  
})