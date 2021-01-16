library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(ggiraph)

options(scipen = 9999)

datos_casen <- readRDS("casen_datos_filtrador.rds")

#calcular puntos de corte de los deciles de ingreso
deciles <- datos_casen %>%
  select(ytrabajocorh) %>%
  summarize(
    "Decil 9" = quantile(ytrabajocorh, probs = 0.9, na.rm=TRUE),
    "Decil 8" = quantile(ytrabajocorh, probs = 0.8, na.rm=TRUE),
    "Decil 7" = quantile(ytrabajocorh, probs = 0.7, na.rm=TRUE),
    "Decil 6" = quantile(ytrabajocorh, probs = 0.6, na.rm=TRUE),
    "Decil 5" = quantile(ytrabajocorh, probs = 0.5, na.rm=TRUE),
    "Decil 4" = quantile(ytrabajocorh, probs = 0.4, na.rm=TRUE),
    "Decil 3" = quantile(ytrabajocorh, probs = 0.3, na.rm=TRUE),
    "Decil 2" = quantile(ytrabajocorh, probs = 0.2, na.rm=TRUE),
    "Decil 1" = quantile(ytrabajocorh, probs = 0.1, na.rm=TRUE)
  )
