box::use(
  shiny[tags, moduleServer, NS, tagList, observeEvent, fluidRow, uiOutput,
        renderUI, isolate],
  semantic.dashboard[box, valueBox],
  shiny.semantic[cards, card],
  plotly[plotlyOutput, renderPlotly]
)

box::use(
  app/logic/graficos[plot.perfil]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Perfil Historico", width = 7, color = "orange",
        plotlyOutput(ns("plot_historico"), height = "80vh")),
    box(title = "Alertas", width = 2, color = "orange",
        uiOutput(ns("alertas"))),
    box(title = "Perfil Nuevo", width = 7, color = "orange",
        plotlyOutput(ns("plot_nuevo"), height = "80vh"))
  )
}

#' @export
server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    output$alertas <- renderUI({
      distancias <- rv$distancias
      distancias_maximas <- rv$distancias_maximas
      alertas <- list()
      
      for (x in names(distancias)) {
        aux <- strsplit(x, ":")[[1]]
        ID <- aux[1]
        ID_CUSTOMER <- aux[2]
        #distancias_maximas[[ID_CUSTOMER]]
        
        if(distancias[[x]] > 0.6) {
          alertas[[x]] <- card(
            tags$div(
              class = "content",
              tags$div(class = "header", ID_CUSTOMER),
              tags$div(class = "meta", "Cliente")
            ),
            tags$div(
              class = "content",
              tags$div(class = "header", ID),
              tags$div(class = "meta", "Transacción")
            ),
            tags$div(
              class = "content",
              tags$div(class = "header", paste0(round(distancias[[x]] * 100, 2), "%")),
              tags$div(class = "meta", "Distancia")
            ),
            tags$div(
              class = "extra content",
              tags$div(class = "ui red button", "Ver perfil", style = "width: 100%",
                       onclick = paste0("Shiny.onInputChange('", session$ns("perfil"), "', ['", x, "', Date.now()])"),)
            )
          )
        }
      }
      
      return(cards(alertas, style = "height: 80vh;overflow: auto;"))
    })
    
    output$plot_historico <- renderPlotly({
      perfil <- input$perfil[1]
      if(is.null(perfil)) {
        return(NULL)
      }
      perfil <- strsplit(perfil, ":")[[1]][2]
      perfiles <- isolate(rv$perfiles)
      plot.perfil(perfiles, perfil, 9)
    })
    
    output$plot_nuevo <- renderPlotly({
      perfil <- input$perfil[1]
      if(is.null(perfil)) {
        return(NULL)
      }
      simbolicos <- isolate(rv$simbolicos)
      plot.perfil(simbolicos, perfil, 9)
    })
    
  })
}
