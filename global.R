library(shiny)
library(shinythemes)
library(dplyr)
library(writexl)
library(highcharter)
library(presupuestochile)

# tablas ----

subtitulos_normalizada <- subtitulos %>% 
  left_join(programas %>% select(anio, id_capitulo, id_programa)) %>% 
  left_join(capitulos %>% select(anio, id_capitulo, id_partida)) %>% 
  left_join(partidas %>% select(anio, id_partida)) %>% 
  select(anio, id_partida, id_capitulo, id_programa, id_subtitulo, valor_asignado_subtitulo)

subtitulos_nombres <- subtitulos %>% 
  select(anio, nombre_subtitulo, id_subtitulo) %>% 
  mutate_if(is.factor, as.character) %>% 
  distinct()

programas_nombres <- programas %>% 
  select(anio, nombre_programa, id_programa) %>% 
  mutate_if(is.factor, as.character) %>% 
  distinct()

capitulos_nombres <- capitulos %>% 
  select(anio, nombre_capitulo, id_capitulo) %>% 
  mutate_if(is.factor, as.character) %>% 
  distinct() %>% 
  mutate(
    nombre_capitulo = case_when(
      nombre_capitulo == "SUBSECRETARÍA DE RELACIONES ECONÓMICAS INTERNACIONALES" ~ "Subsecretaría de Relaciones Económicas Internacionales",
      nombre_capitulo == "SUBSECRETARIA DE CIENCIA, TECNOLOGÍA, CONOCIMIENTO E INNOVACIÓN" ~ "Subsecretaria de Ciencia, Tecnología, Conocimiento e Innovación",
      nombre_capitulo == "SERVICIO ELECTORAL" ~ "Servicio Electoral",
      nombre_capitulo == "INSTITUTO NACIONAL DESARROLLO SUSTENTABLE PESCA ARTESANAL Y ACUICULTURA" ~ "Instituto Nacional Desarrollo Sustentable Pesca Artesanal y Acuicultura",
      nombre_capitulo == "DIRECCIÓN GENERAL DE PROMOCIÓN DE EXPORTACIONES" ~ "Dirección General de Promoción de Exportaciones",
      nombre_capitulo == "DIRECCIÓN GENERAL DE CONCESIONES DE OBRAS PÚBLICAS" ~ "Dirección General de Promoción de Exportaciones",
      nombre_capitulo == "DIRECCIÓN GENERAL DE CONCESIONES DE OBRAS PÚBLICAS" ~ "Dirección General de Concesiones de Obras Públicas",
      nombre_capitulo == "DIRECCIÓN DE BIBLIOTECAS, ARCHIVOS Y MUSEOS" ~ "Dirección de Bibliotecas, Archivos y Museos",
      nombre_capitulo == "CONSEJO NACIONAL DE LA CULTURA Y LAS ARTES" ~ "Consejo Nacional de la Cultura y las Artes",
      TRUE ~ nombre_capitulo
    )
  )

partidas_nombres <- partidas %>% 
  select(anio, nombre_partida, id_partida) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    nombre_partida = case_when(
      nombre_partida == "ADQUISICION DE ACTIVOS FINANCIEROS" ~ "ADQUISICIÓN DE ACTIVOS FINANCIEROS",
      nombre_partida == "INGRESOS DE OPERACION" ~ "INGRESOS DE OPERACIÓN",
      nombre_partida == "INICIATIVAS DE INVERSION" ~ "INICIATIVAS DE INVERSIÓN",
      nombre_partida == "PRESTAMOS" ~ "PRÉSTAMOS",
      TRUE ~ nombre_partida
    )
  ) %>% 
  distinct()

anio_int <- rev(sort(unique(partidas$anio)))
partidas_chr <- c("Selecciona para filtrar", sort(partidas_nombres$nombre_partida))
# capitulos_chr <- c("Selecciona para filtrar", sort(levels(capitulos_nombres$nombre_capitulo)))
# programas_chr <- c("Selecciona para filtrar", sort(levels(programas_nombres$nombre_programa)))
# subtitulos_chr <- c("Selecciona para filtrar", sort(levels(subtitulos_nombres$nombre_subtitulo)))
