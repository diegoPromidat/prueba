box::use(
  shiny[tags, moduleServer, NS, tagList, observeEvent, isolate],
  semantic.dashboard[box],
  shiny.semantic[selectInput, updateSelectInput],
  plotly[plotlyOutput, renderPlotly]
)

box::use(
  app/logic/graficos[plot.perfil]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = NULL, width = 16, ribbon = F, color = "orange",
        selectInput(ns("perfil"), "Seleccione el perfil", choices = c(1))
    ),
    box(title = NULL, width = 16, ribbon = F, color = "orange",
        plotlyOutput(ns("plot_perfil"), height = "60vh")
    )
  )
  
}

#' @export
server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(rv$simbolicos, {
      updateSelectInput(session, "perfil", choices = attr(rv$simbolicos, "concept"))
    })
    
    output$plot_perfil <- renderPlotly({
      simbolicos <- isolate(rv$simbolicos)
      plot.perfil(simbolicos, input$perfil)
    })
  })
}
