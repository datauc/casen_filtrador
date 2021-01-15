library(dplyr)
library(tidyr)

#importar datos
casen <- readstata13::read.dta13("~/Casen/Casen 2017.dta") %>% as_tibble()

#filtrar sólo la RM
casen_rm <- casen %>%
  filter(region == "Región Metropolitana de Santiago")

#seleccionar variables
casen_1 <- casen_rm %>% 
  select(comuna,
         expc,                    #factor de expansión comunal
         sexo,                    #género
         esc,                     #años de escolaridad
         edad,                    #edad
         ytotcorh,                #Ingreso total del hogar corregido
         ytotcor,                 #Ingreso total corregido
         yoprcor,                 #Ingreso ocupación principal
         ypc,                     #Ingreso total per cápita del hogar corregido
         ytrabajocor,             #ingreso del trabajo
         ytrabajocorh,            #ingreso del trabajo del hogar
         ypchautcor,              #ingreso autónomo per cápita 
         y26_2c,                  #jubilación o pensión
         numper,                  #numero de personas en el hogar
         s4,                      #hijos vivos
         pco1,                    #jefe de hogar
         activ,                   #actividad
         hacinamiento,            #hacinamiento
         pobreza,                 #pobreza
         pobreza_multi_5d,        #pobreza multidimensional
         r1a,                     #nacionalidad
         r3,                      #pertenencia a pueblos originarios
         v12,                     #metros cuadrados de la casa
         indmat,                  #índice de materialidad de la vivienda
         r1a,
         pobreza,
         pobreza_multi_4d,
         ytrabajocorh,
         esc,
         edad,
         r3,
         activ,
         o17,
         oficio4,
         zona,
         hacinamiento,
         hh_d_servbas,
         calglobviv,
         hh_d_mal,
         r8b,
         r8e)

#aplicar factor de expansión
casen_datos_filtrador <- tidyr::uncount(casen_1, weights = expc)

# #calcular medidas de tendencia central
# casen_promedios <- casen_2 %>%
#   group_by(comuna) %>%
#   summarize(across(2:13, ~ mean(.x, na.rm = TRUE))) %>%
#   mutate(tipo = "promedio")
# 
# casen_medianas <- casen_2 %>%
#   group_by(comuna) %>%
#   summarize(across(2:13, ~ median(.x, na.rm = TRUE))) %>%
#   mutate(tipo = "mediana")

#unir datos
# casen_datos <- bind_rows(casen_promedios, casen_medianas) %>%
#   arrange(comuna, tipo)

#glimpse(casen_datos)

#limpiar
remove(casen, casen_rm, casen_1)

#exportar datos
saveRDS(casen_datos_filtrador, file = "casen_datos_filtrador.rds")
