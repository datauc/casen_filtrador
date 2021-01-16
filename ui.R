library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  
  titlePanel(title = NULL,
             windowTitle = "Filtrador Casen"),
  
  #tipografías
  tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Open Sans');")), # Importar Open Sans
  
  fluidRow(
    column(12,
           h1("Filtrador Casen"),
           p("Esta herramienta entrega datos socioeconómicos y poblacionales de grupos sociales que viven condiciones específicas de desigualdad y vulnerabilidad en la Región Metropolitana de Chile."),
           p("Utilice los botones presentados a continuación para ir filtrando o reduciendo la población total, que cumple con las condiciones seleccionadas.
        Por ejemplo, seleccionar 'Mujeres' y 'Pobreza extrema' resultará en la cantidad de población que corresponde a dicho grupo social, junto a otros datos tales como sus ingresos."),
           htmlOutput("texto_filtro"),
           hr(),
    )
  ),
  
  fluidRow(
    #(columna 1) ----
    column(4,
           h3("Socioeconómico"),
           #br(),
           # POBREZA ----
          
           # Estratos de ingresos bajos y medios-bajos ----
           h4("Pobreza por ingresos",
              actionLink(
                inputId = "ayuda_pobreza_ingresos_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           conditionalPanel(
             condition = "input.ayuda_pobreza_ingresos_casen != 0 && input.ayuda_pobreza_ingresos_casen % 2 != 0",
             p("El valor de la línea de pobreza por ingresos se estima sobre la base del gasto total de los hogares pertenecientes al estrato de referencia utilizado para la determinación de la canasta básica de alimentos, excluyendo los gastos en alcohol y tabaco, además del gasto en bienes y servicios adquiridos por menos del 10% de los hogares."),
             p("El valor de la línea de pobreza extrema por ingresos equivale a dos tercios del valor correspondiente a la línea de pobreza"),
           ),
           
           checkboxGroupButtons(
             inputId = "pobreza_ingresos_casen",
             #label = "Pobreza por ingresos:",
             label=NULL,
             choices = c("Pobreza no extrema",
                         "Pobreza extrema"),
             justified = TRUE
           ),
           
           # Pobreza multidimensional ----
           h4("Pobreza multidimensional", 
              actionLink(
                inputId = "ayuda_pobreza_multi_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )
           ),
           
           conditionalPanel(
             condition = "input.ayuda_pobreza_multi_casen != 0 && input.ayuda_pobreza_multi_casen % 2 != 0",
             p("La medición de pobreza multidimensional implementada por el Ministerio de Desarrollo Social y Familia toma en cuenta un conjunto de 5 dimensiones y 15 indicadores (3 indicadores en cada dimensión) que buscan identificar si los hogares alcanzan un determinado umbral de bienestar."),
             p("Los hogares que se encuentran en situación de pobreza multidimensional son aquellos que acumulan un porcentaje de 22,5% ó más de carencias en los 15 indicadores individuales que se utilizan para la medición, ponderados de acuerdo al peso que les corresponde en cada dimensión."),
           ),
           
           checkboxGroupButtons(
             inputId = "pobreza_multi_casen",
             #label = "Pobreza multidimensional:",
             label=NULL,
             choices = c("Pobreza multidimensional"),
             justified = TRUE
           ),
           
           hr(), 
           #Percentil ingresos ----
           h4("Percentil de ingresos", 
              actionLink(
                inputId = "ayuda_percentil_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )
           ),
           
           conditionalPanel(
             condition = "input.ayuda_percentil_casen != 0 && input.ayuda_percentil_casen % 2 != 0",
             p("Hogares que se encuentran por debajo del porcentaje especificado de ingresos; por ejemplo, seleccionar 40% para elegir a los hogares debajo del 40% de ingresos más bajos."),
           ),
           
           sliderInput("percentil_slider_casen", 
                       label=NULL,
                       width="97%",
                       min = 0, max = 100, value = 100,
                       step=1, round=TRUE,
                       post="% inferior",
                       ticks=FALSE),
           
           textOutput("percentil_elegido"),
           
           # LABORAL ----
           hr(),
           h3("Situación laboral"),
           
           # Inactividad ----
           checkboxGroupButtons(
             inputId = "inactivos_casen",
             #label = "Trabajo informal",
             label=NULL,
             choices = c("Personas inactivas (no trabajan ni estudian)"="Inactivas"),
             justified = TRUE
           ),
           
           # Trabajadores/as informal ----
           checkboxGroupButtons(
             inputId = "informalidad_casen",
             #label = "Trabajo informal",
             label=NULL,
             choices = c("Trabajadores/as sin contrato laboral"="Sin contrato"),
             justified = TRUE
           ),
           
           # # Trabajadores/as domésticos/as ----
           # checkboxGroupButtons(
           #   inputId = "trabajo_domestico_casen",
           #   #label = "Trabajo doméstico",
           #   label=NULL,
           #   choices = c("Trabajadoras/es domésticas/es"="Trabajadoras domésticas"),
           #   justified = TRUE
           # ),
           
    ),
    #(columna 2) ----
    column(4,
           #DEMOGRAFÍA ----
           h3("Demografía"),
           
           
           #Mujeres ----
           #h3("Género"),
           checkboxGroupButtons(
             inputId = "mujeres_casen",
             #label = "Mujeres:",
             choices = c("Mujeres"),
             #             "Todos"),
             #selected = "Todos",
             justified = TRUE
           ),
           
           #Migrantes ----
           checkboxGroupButtons(
             inputId = "migrantes_casen",
             #label = "Mujeres:",
             choices = c("Migrantes"),
             #             "Todos"),
             #selected = "Todos",
             justified = TRUE
           ),
           
           # Pueblos indígenas ----
           checkboxGroupButtons(
             inputId = "indigena_casen",
             #label = "Pertenencia a pueblos originarios:",
             label=NULL,
             choices = c("Pertenece a un pueblo originario"),
             justified = TRUE
           ),
           
           # Zona ----
           #h4("Zona de residencia"),
           checkboxGroupButtons(
             inputId = "zona_casen",
             #label = "Zona de residencia:",
             label=NULL,
             choices = c("Urbano",
                         "Rural"),
             justified = TRUE
           ),
           
           #h3("Edad"),
           
           #column(12, style = "padding: 0px;",
                  
                  # Edad slider ----
                  #column(6, #style = "padding: 0px;",
           h4("Edad"),
           p("Seleccionar edad mínima a considerar (por ejemplo, mayores de 40 años)"),
                         sliderInput("edad_slider_casen", 
                                     #label="Edad",
                                     label = NULL,
                                     width="100%",
                                     ticks=FALSE,
                                     pre="≥",
                                     min = 0, max = 110, value = 0),
                  #),
                  br(),
                  
                  #Escolaridad slider ----
           h4("Educación"),
           p("Seleccionar un máximo de años de educación formal a considerar (por ejemplo, 8 años de escolaridad, equivalente a enseñanza media)"),
                         sliderInput("escolaridad_slider_casen", 
                                     #label="Años de escolaridad",
                                     label=NULL,
                                     width="100%",
                                     min = 0, max = 25, value = 25,
                                     step=1,
                                     pre="≤",
                                     ticks=FALSE),

    ), #end column
    
    #— ----
    column(4,
           #VIVIENDA ----
           #
           h3("Vivienda"),
           
          
           
           
           
           # Hacinamiento ----
           h4("Hogares con hacinamiento",
              actionLink(
                inputId = "ayuda_hacinamiento_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           
           conditionalPanel(
             condition = "input.ayuda_hacinamiento_casen != 0 && input.ayuda_hacinamiento_casen % 2 != 0",
             p("Razón entre el número de personas residentes en la vivienda y el número de dormitorios de la misma, considerando piezas de uso exclusivo o uso múltiple. Contempla las categorías: sin hacinamiento, medio bajo (2,5 personas), medio alto (3,5 personas) y crítico (5 personas)."),
           ),
           
           checkboxGroupButtons(
             inputId = "hacinamiento_casen",
             label = NULL,
             choices = c("Hacinamiento"),
             justified = TRUE
           ),
           
           
           
           # Acceso a servicios básicos ----
           h4("Acceso a servicios básicos de la vivienda",
              actionLink(
                inputId = "ayuda_servicios_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           
           conditionalPanel(
             condition = "input.ayuda_servicios_casen != 0 && input.ayuda_servicios_casen % 2 != 0",
             p("Índice sintético que clasifica al parque habitacional ocupado de acuerdo con el tipo y calidad de acceso a servicios básicos de las viviendas en que residen los hogares sobre la base de tres variables: (a) origen del agua de la vivienda (diferenciando entre soluciones adecuadas e inadecuadas según se trate de zona urbana y rural); (b) sistema de distribución de agua en la vivienda; y, (c) sistema de eliminación de excretas en la vivienda."),
           ),
           
           
           checkboxGroupButtons(
             inputId = "servicios_casen",
             label = NULL,
             choices = c("Acceso deficiente a servicios básicos"),
             justified = TRUE
           ),
           
           hr(),
           
           # Calidad global de la vivienda ----
           h4("Calidad global de la vivienda",
              actionLink(
                inputId = "ayuda_calidad_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           
           conditionalPanel(
             condition = "input.ayuda_calidad_casen != 0 && input.ayuda_calidad_casen % 2 != 0",
             p("Se considera que las viviendas son de calidad global aceptable si la vivienda presenta una materialidad, saneamiento y tipo aceptable, o bien si la materialidad es recuperable siempre que el piso, techo, saneamiento y tipo sean aceptables."),
           ),
           
           checkboxGroupButtons(
             inputId = "vivienda_calidad_casen",
             #label = "Calidad global de la vivienda:",
             label=NULL,
             choices = c("Recuperable",
                         "Irrecuperable"),
             justified = TRUE
           ),
           
           # Malnutrición hijes ----
           h4("Malnutrición de menores",
              actionLink(
                inputId = "ayuda_malnutricion_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           
           conditionalPanel(
             condition = "input.ayuda_malnutricion_casen != 0 && input.ayuda_malnutricion_casen % 2 != 0",
             p("Uno de sus integrantes de 0 a 6 años está con sobrepeso u obesidad, o está en desnutrición o riesgo de desnutrición."),
           ),
           
           checkboxGroupButtons(
             inputId = "malnutricion_casen",
             #label = "Malnutrición de hijos/as",
             label=NULL,
             choices = c("Malnutrición de menores en el hogar"),
             justified = TRUE
           ),
           
           # Dificultad económica para alimentarse ----
           h4("Dificultad de acceso a alimentación sana",
              actionLink(
                inputId = "ayuda_comida_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           
           conditionalPanel(
             condition = "input.ayuda_comida_casen != 0 && input.ayuda_comida_casen % 2 != 0",
             p("Alguna persona en el hogar no ha pudido comer alimentos saludables y nutritivos por falta de dinero u otros recursos."),
           ),
           
           checkboxGroupButtons(
             inputId = "comida_sana_casen",
             #label = "Dificultad económica para acceder a alimentación sana:",
             label=NULL,
             choices = c("Dificultad para acceder a alimentación sana"),
             justified = TRUE
           ),
           
           
           # Insuficiencia de alimentos ----
           h4("Insuficiencia de alimentos",
              actionLink(
                inputId = "ayuda_comida_insuficiencia_casen",
                class = "botonayuda",
                label = NULL, icon = icon("question-circle")
              )),
           
           conditionalPanel(
             condition = "input.ayuda_comida_insuficiencia_casen != 0 && input.ayuda_comida_insuficiencia_casen % 2 != 0",
             p("Alguna persona del hogar comió menos de lo que pensaba que debía comer por falta de dinero u otros recursos."),
           ),
           
           checkboxGroupButtons(
             inputId = "comida_insuficiencia_casen",
             #label = "Dificultad económica para acceder a alimentación sana:",
             label=NULL,
             choices = c("Insuficiencia de alimentos por falta de recursos"),
             justified = TRUE
           ),
           
    ), #end column
  ),
  hr(),
  #— ----
  fluidRow(
    # SEGUNDA ----
    column(12,
           #h3("Datos"),
           # verbatimTextOutput("prueba_null"),
           #verbatimTextOutput("prueba_variable"),
           
           #verbatimTextOutput("output_tabla"),
           
           #br(),
           
           #Barras ----
           h3("Población por comuna"),
           p("Población de la región que cumple con los criterios seleccionados"),
           pickerInput("selector_comunas_barras",
                       label = "Seleccione las comunas que desea graficar:",
                       width = "100%",
                       multiple = TRUE,
                       choices = NULL),
           br(),
           plotOutput("output_grafico_casen",
                      height="300px") %>% shinycssloaders::withSpinner(),
           
           hr(),
           
           #Densidad ----
           h3("Distribución de ingresos"),
           p("Representación gráfica de la distribución de los ingresos individuales de acuerdo a los criterios seleccionados. La altura de las curvas indica mayor proporción de personas que perciben la cifra de ingresos indicada en el eje horizontal."),
           plotOutput("output_densidad_casen",
                      height="300px") %>% shinycssloaders::withSpinner(),
           br(),
           
           #Opciones para gráfico de densidad ----
           fluidRow(
             #Pobreza
             column(6,
                    materialSwitch(inputId = "selector_densidad_pobreza_casen", 
                                   label = "Desagregar por pobreza:", 
                                   right = FALSE, value = FALSE)
             ),
             #Género
             column(6,
                    materialSwitch(inputId = "selector_densidad_genero_casen", 
                                   label = "Desagregar por género:", 
                                   right = FALSE, value = FALSE)
             )
           ),
           
           hr(),
           
           # #Deciles ----
           h3("Deciles de ingreso"),
           p("Los deciles de ingreso dividen la distribución de los ingresos en 10 sectores, donde cada uno representa a un 10% de los hogares,
               e indican el rango de ingresos de cada grupo.
                 Esto significa que el decil 1 es el rango de ingresos del 10% de los hogares con menores ingresos,
                 mientras que el decil 10 es el rango de ingresos del 10% de los hogares más ricos del país."),
           
           plotOutput("output_grafico_deciles",
                      height="400px") %>% shinycssloaders::withSpinner(),
           
           
           hr(), 
           #Grupos socioeconómicos ----
           h3("Grupos socioeconómicos"),
           p("Los grupos socioeconómicos dividen la población de la región en grupos de acuerdo a los ingresos combinados del hogar y la cantidad de personas que viven en el hogar. El grupo más bajo es el E, que representa a los menores ingresos del país, mientras que el más alto es el AB, con los mayores ingresos del país."),
           pickerInput("selector_comunas_gse",
                       label = "Seleccione las comunas que desea graficar:",
                       width = "100%",
                       multiple = TRUE,
                       choices = NULL),
           plotOutput("output_grafico_gse",
                      height="400px") %>% shinycssloaders::withSpinner(),
           
    )#end column
  ), #end row
  
  fluidRow(
    column(12,
           hr(),
           #Mapa ----
           h3("Mapa regional"),
           p("Pase el cursor sobre las comunas para ver sus nombres y cifras."),
           
           radioGroupButtons(
             inputId = "selector_tipo_casen",
             label = "Seleccione el dato por graficar:",
             choices = c("Frecuencia",
                         "Porcentaje"),
             justified = TRUE
           ), 
           
 #          conditionalPanel(condition = "input.selector_tipo_casen == 'Porcentaje'",
                            #p("Los porcentajes se calculan a nivel regional; es decir, indican cómo se distribuye la población seleccionada en cada una de las comunas indicadas")
#           ),
           #esto no tiene sentido porque los casos se filtran, ergo siempre van a ser 100%
           # conditionalPanel(condition = "input.selector_tipo_casen == 'Porcentaje'",
           # radioGroupButtons(
           #   inputId = "selector_grupo_porcentaje",
           #   label = "Seleccione el grupo para calcular los porcentajes:",
           #   choices = c("Regional",
           #               "Comunal"),
           #   selected = "Regional",
           #   justified = TRUE
           # )
           # ),
           
           girafeOutput("output_mapa_casen",
                        height="900px") %>% shinycssloaders::withSpinner(),
           
           br(),
           
           # materialSwitch(inputId = "expansion_casen", 
           #                label = "Factor de expansión", 
           #                right = FALSE, value = TRUE)
           
           
    )
  ),

#footer ----
fluidRow(
  column(12, align = "center",
         
         hr(),
         p("Plataforma desarrollada por el equipo DATA UC, usando R Shiny"),
         
         HTML("<p>Diseño y desarrollo: 
                    <a href='http://bastian.olea.biz' 
                       style='color: #999999'>
                    Bastián Olea Herrera.</a></p>"),
         
         p("Facultad de Matemáticas"),
         p("Pontificia Universidad Católica de Chile"),
         tags$a(img(
           src = "logodatauc.png",
           width = 200, style = "padding: 10px"
         ),
         #href = "http://www.mat.uc.cl"
         hreg = "http://datascience.uc.cl"
         )
         
         
  )) #end footer
  
))
