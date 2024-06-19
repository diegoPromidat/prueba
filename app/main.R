box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, 
        icon, reactiveValues],
  semantic.dashboard[dashboard_page, dashboard_header, dashboard_sidebar,
                     dashboard_body, sidebar_menu, menuItem, tabItems, tabItem,
                     box]
)

box::use(
  app/view/cargar,
  app/view/perfiles,
  app/view/perfilesNuevos,
  app/view/monitorear
)

box::use(
  app/logic/carga_datos[cargar_perfiles, cargar_distancias]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  dashboard_page(
    dashboard_header(title = "PROMiDAT", color = "orange", inverted = T),
    dashboard_sidebar(
      sidebar_menu(
        menuItem("Perfiles Historicos", tabName = "perfiles", icon = icon("users")),
        menuItem("Cargar Perfiles Nuevos", tabName = "cargar", icon = icon("database")),
        menuItem("Perfiles Nuevos", tabName = "perfilesNuevos", icon = icon("user-plus")),
        menuItem("Monitorear", tabName = "monitorear", icon = icon("desktop"))
      )
    ),
    dashboard_body(
      tabItems(
        tabItem(
          tabName = "perfiles",
          perfiles$ui(ns("perfiles"))
        ),
        tabItem(
          tabName = "cargar",
          cargar$ui(ns("cargar"))
        ),
        tabItem(
          tabName = "perfilesNuevos",
          perfilesNuevos$ui(ns("perfilesNuevos"))
        ),
        tabItem(
          tabName = "monitorear",
          monitorear$ui(ns("monitorear"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(datos = NULL, simbolicos = NULL, distancias = NULL,
                         perfiles = cargar_perfiles(),
                         distancias_maximas = cargar_distancias())
    
    cargar$server("cargar", rv)
    perfiles$server("perfiles", rv)
    perfilesNuevos$server("perfilesNuevos", rv)
    monitorear$server("monitorear", rv)
  })
}
