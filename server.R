shinyServer(function(input, output) {
  capitulos_chr <- reactive({
    if (input$partida_ui != "Selecciona para filtrar") {
      d <- subtitulos_normalizada %>% 
        inner_join(
          partidas_nombres %>% 
            filter(anio == input$anio_ui, nombre_partida == input$partida_ui) %>% 
            select(anio, id_partida)
        ) %>% 
        distinct(id_capitulo) %>% 
        inner_join(capitulos_nombres) %>% 
        select(nombre_capitulo) %>% 
        pull()
    } else {
      d <- capitulos_nombres %>% 
        filter(anio == input$anio_ui) %>% 
        select(nombre_capitulo) %>% 
        pull()
    }
    
    return(
      c("Selecciona para filtrar", d)
    )
  })
  
  output$capitulos_chr <- renderUI({
    selectInput("capitulo_ui", "Capítulo:", capitulos_chr())
  })
  
  programas_chr <- reactive({
    if (input$capitulo_ui != "Selecciona para filtrar") {
      d <- subtitulos_normalizada %>% 
        inner_join(
          capitulos_nombres %>% 
            filter(anio == input$anio_ui, nombre_capitulo == input$capitulo_ui) %>% 
            select(anio, id_capitulo)
        ) %>% 
        distinct(id_programa) %>% 
        inner_join(programas_nombres) %>% 
        select(nombre_programa) %>% 
        pull()
    } else {
      d <- programas_nombres %>% 
        filter(anio == input$anio_ui) %>% 
        select(nombre_programa) %>% 
        pull()
    }
    
    return(
      c("Selecciona para filtrar", d)
    )
  })
  
  output$programas_chr <- renderUI({
    selectInput("programa_ui", "Programa:", programas_chr())
  })
  
  subtitulos_chr <- reactive({
    if (input$programa_ui != "Selecciona para filtrar") {
      d <- subtitulos_normalizada %>% 
        inner_join(
          programas_nombres %>% 
            filter(anio == input$anio_ui, nombre_programa == input$programa_ui) %>% 
            select(anio, id_programa)
        ) %>% 
        distinct(id_subtitulo) %>% 
        inner_join(subtitulos_nombres) %>% 
        select(nombre_subtitulo) %>% 
        pull()
    } else {
      d <- subtitulos_nombres %>% 
        filter(anio == input$anio_ui) %>% 
        select(nombre_subtitulo) %>% 
        pull()
    }
    
    return(
      c("Selecciona para filtrar", d)
    )
  })
  
  output$subtitulos_chr <- renderUI({
    selectInput("subtitulo_ui", "Subtítulo:", subtitulos_chr())
  })
  
  # datos  usuario ----
  
  datos_usuario <- reactive({
    d <- subtitulos_normalizada %>% 
      filter(anio == input$anio_ui)
    
    d2 <- subtitulos_normalizada %>% 
      filter(anio == as.numeric(input$anio_ui) - 1) %>% 
      rename(valor_asignado_subtitulo_2 = valor_asignado_subtitulo)
    
    d3 <- subtitulos_normalizada %>% 
      filter(anio == as.numeric(input$anio_ui) - 5) %>% 
      rename(valor_asignado_subtitulo_3 = valor_asignado_subtitulo)
    
    # partida 
    
    if (input$partida_ui != "Selecciona para filtrar") {
      partida_usuario <- partidas_nombres %>%
        filter(anio == input$anio_ui, nombre_partida == input$partida_ui)
      
      d <- d %>%
        inner_join(partida_usuario) %>% 
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(partida_usuario) %>% 
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(partida_usuario) %>% 
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    } else {
      d <- d %>%
        inner_join(partidas_nombres) %>% 
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(partidas_nombres) %>% 
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(partidas_nombres) %>% 
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    }
    
    # capitulo
    
    if (input$capitulo_ui != "Selecciona para filtrar") {
      capitulo_usuario <- capitulos_nombres %>%
        filter(anio == input$anio_ui, nombre_capitulo == input$capitulo_ui)

      d <- d %>%
        inner_join(capitulo_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(capitulo_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(capitulo_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    } else {
      d <- d %>%
        inner_join(capitulos_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(capitulos_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(capitulos_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    }
    
    # programa
    
    if (input$programa_ui != "Selecciona para filtrar") {
      programa_usuario <- programas_nombres %>%
        filter(anio == input$anio_ui, nombre_programa == input$programa_ui)
      
      d <- d %>%
        inner_join(programa_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(programa_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(programa_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    } else {
      d <- d %>%
        inner_join(programas_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(programas_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(programas_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    }
    
    # subtitulo
    
    if (input$subtitulo_ui != "Selecciona para filtrar") {
      subtitulo_usuario <- subtitulos_nombres %>%
        filter(anio == input$anio_ui, nombre_subtitulo == input$subtitulo_ui)
      
      d <- d %>%
        inner_join(subtitulo_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(subtitulo_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(subtitulo_usuario) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    } else {
      d <- d %>%
        inner_join(subtitulos_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d2 <- d2 %>%
        inner_join(subtitulos_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
      
      d3 <- d3 %>%
        inner_join(subtitulos_nombres) %>%
        select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_"))
    }
    
    # reunir datos ----
    d <- d %>% 
      select(anio, starts_with("id_"), starts_with("nombre_"), starts_with("valor_")) %>% 
      left_join(d2 %>% select(starts_with("nombre_"), starts_with("valor_"))) %>% 
      left_join(d3 %>% select(starts_with("nombre_"), starts_with("valor_")))
    
    # if (input$subtitulo_ui == "Selecciona para filtrar") {
    #   d <- d %>% 
    #     group_by(anio, nombre_partida, nombre_capitulo, nombre_programa, nombre_subtitulo) %>% 
    #     summarise(valor_asignado_subtitulo = sum(valor_asignado_subtitulo))
    # }
    
    if (input$programa_ui == "Selecciona para filtrar") {
      d <- d %>% 
        group_by(anio, nombre_partida, nombre_capitulo, nombre_programa) %>% 
        # summarise(valor_asignado_subtitulo = sum(valor_asignado_subtitulo))
        summarize_if(is.numeric, sum, na.rm = T)
    }
    
    if (input$capitulo_ui == "Selecciona para filtrar") {
      d <- d %>%
        group_by(anio, nombre_partida, nombre_capitulo) %>%
        # summarise(valor_asignado_subtitulo = sum(valor_asignado_subtitulo))
        summarize_if(is.numeric, sum)
    }

    if (input$partida_ui == "Selecciona para filtrar") {
      d <- d %>%
        group_by(anio, nombre_partida) %>%
        # summarise(valor_asignado_subtitulo = sum(valor_asignado_subtitulo)) %>% 
        summarize_if(is.numeric, sum)
    }
    
    names(d)[length(names(d))] <- glue::glue("presupuesto_asignado_{ as.numeric(input$anio_ui) - 5 }")
    names(d)[length(names(d)) - 1] <- glue::glue("presupuesto_asignado_{ as.numeric(input$anio_ui) - 1 }")
    names(d)[length(names(d)) - 2] <- glue::glue("presupuesto_asignado_{ as.numeric(input$anio_ui) }")
    
    names(d) <- stringr::str_to_title(names(d))
    # names(d) <- stringr::str_replace_all(names(d), "Anio", "Año")
    names(d) <- stringr::str_replace_all(names(d), "_", " ")
      
    return(d)
  })
  
  datos_usuario_hc <- reactive({
    d2 <- datos_usuario() %>% janitor::clean_names()
    names(d2)[length(names(d2)) - 2] <- "valor_asignado"
    d2 <- d2 %>% 
      select(anio:valor_asignado)
    
    if (input$partida_ui != "Selecciona para filtrar") {
      d2 <- d2 %>% ungroup() %>% select(anio, nombre_capitulo, valor_asignado)
    }
    
    names(d2) <- c("anio", "nombre", "valor_asignado")
    
    h <- hchart(d2, "treemap",
                hcaes(x = nombre, value = valor_asignado, color = log(valor_asignado))) %>%
      hc_title(text = glue::glue("Distribucion los datos filtrados por el usuario - Año { input$anio_ui }"))
    
    return(h)
  })
  
  output$datos_usuario_hc <- highcharter::renderHighchart(datos_usuario_hc())
    
  output$datos_usuario_dt <- DT::renderDataTable(datos_usuario())
  
  partida_descarga <- reactive({
    if (input$partida_ui == "Selecciona para filtrar") {
      "todas_las_partidas"
    } else {
      tolower(gsub(" ", " ", iconv(to = "ASCII//TRANSLIT", input$partida_ui)))
    }
  })
  
  capitulo_descarga <- reactive({
    if (input$capitulo_ui == "Selecciona para filtrar") {
      "todas_los_capitulos"
    } else {
      tolower(gsub(" ", " ", iconv(to = "ASCII//TRANSLIT", input$capitulo_ui)))
    }
  })
  
  programa_descarga <- reactive({
    if (input$programa_ui == "Selecciona para filtrar") {
      "todas_los_programas"
    } else {
      tolower(gsub(" ", " ", iconv(to = "ASCII//TRANSLIT", input$programa_ui)))
    }
  })
  
  subtitulo_descarga <- reactive({
    if (input$subtitulo_ui == "Selecciona para filtrar") {
      "todas_los_subtitulos"
    } else {
      tolower(gsub(" ", " ", iconv(to = "ASCII//TRANSLIT", input$subtitulo_ui)))
    }
  })
  
  output$datos_usuario_xlsx <- downloadHandler(
    filename =  function() {
      glue::glue("{ input$anio_ui }_{ partida_descarga() }_{ capitulo_descarga() }_{ programa_descarga() }_{ subtitulo_descarga() }.xlsx")
    },
    content = function(filename) { writexl::write_xlsx(datos_usuario(), filename) },
    contentType = "application/xlsx"
  )
})
