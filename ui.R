shinyUI(
  fluidPage(
    
    titlePanel("Moockly"),
    sidebarLayout( 
      sidebarPanel(
        uiOutput('weeksData'),
        uiOutput('studentsData'),
        uiOutput('sesionesData'),
    actionButton("submit", label = "Mostrar Ruta")
    ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Análisis",
                      visNetworkOutput("network")
                    ),
                    tabPanel("Estadísticas",
                      tableOutput("datos")
                    )
                    )
        )

      )
    )
)