shinyUI(fluidPage(theme = shinytheme("flatly"),
    titlePanel("Visualización del Presupuesto de la Nación de Chile"),
    
    sidebarPanel(
        selectInput("anio_ui", "Año:", anio_int),
        selectInput("partida_ui", "Partida:", partidas_chr),
        uiOutput("capitulos_chr"),
        # selectInput("programa_ui", "Programa:", programas_chr),
        uiOutput("programas_chr"),
        # selectInput("subtitulo_ui", "Subtítulo:", subtitulos_chr)
        uiOutput("subtitulos_chr"),
        downloadButton("datos_usuario_xlsx", "Descarga consulta en XLSX")
    ),
    
    mainPanel(
        highcharter::highchartOutput("datos_usuario_hc"),
        DT::dataTableOutput("datos_usuario_dt")
    )
))
