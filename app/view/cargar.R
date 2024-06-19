box::use(
  shiny[tags, moduleServer, NS, tagList, observeEvent],
  semantic.dashboard[box],
  shiny.semantic[fileInput, selectInput, toggle, grid, grid_template, 
                 actionButton, card],
  utils[read.table],
  DT[dataTableOutput, renderDataTable, datatable],
  RSDA[classic.to.sym]
)

box::use(
  app/logic/carga_datos[limpieza],
  app/logic/distancias[calcular.distancias]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    box(title = "Opciones", width = 16, color = "orange",
        grid(
          grid_template(
            default = list(
              areas = rbind(c("archivo", "archivo"), c("cols", "rows"), c("sep", "dec"), c("cargar", "cargar")),
              rows_height = c("auto", "50px", "80px", "auto"),
              cols_width = c("50%", "50%"))
          ),
          archivo = fileInput(ns("archivo"), "Cargar archivo", buttonLabel = "Subir"),
          cols = toggle(ns("cols"), "Nombres de columna"),
          rows = toggle(ns("rows"), "Nombres de fila"),
          sep = selectInput(ns("sep"), "Separador de datos", c(";", ",", "."), width = "90%"),
          dec = selectInput(ns("dec"), "Separador decimal", c(",", "."), width = "90%"),
          cargar = actionButton(ns("cargar"), label = "Cargar", width = "100%")
        )
    ),
        
    box(dataTableOutput(ns("tabla")), title = NULL, width = 16)
  )
}

#' @export
server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$cargar, {
      archivo <- input$archivo
      cols <- input$cols
      rows <- input$rows
      sep <- input$sep
      dec <- input$dec
      
      tryCatch(
        expr = {
          if(rows) {
            datos <- read.table(archivo$datapath, cols, sep, dec = dec, row.names = 1,
                                stringsAsFactors = T)
          } else {
            datos <- read.table(archivo$datapath, cols, sep, dec = dec,
                                stringsAsFactors = T)
          }
          datos <- limpieza(datos, transaccion = T)
          simbolicos <- classic.to.sym(datos, concept = c("ID", "CUSTOMER_ID"))
          distancias <- lapply(1:nrow(simbolicos), function(i) {
            calcular.distancias(rv$perfiles, simbolicos[i, ])
          })
          names(distancias) <- attr(simbolicos, "concept")
          
          rv$datos <- datos
          rv$simbolicos <- simbolicos
          rv$distancias <- distancias
        },
        error = function(e){
          rv$datos <- NULL
          rv$simbolicos <- NULL
          rv$distancias <- NULL
        }
      )
    })
    
    output$tabla <- renderDataTable({
      datatable(rv$datos, options = list(dom = 'ftip', scrollX = TRUE))
    })
  })
}
