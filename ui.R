shinyUI(
  fluidPage(
    titlePanel("Learner Paths"),

      visNetworkOutput("network"),
      hr(),
      fluidRow(
        column(4, offset = 1,
               uiOutput('sesionesData'),
               actionButton("submit", label = "Filtrar")
        ),
        column(4, offset = 1,
               tableOutput("datos")
        ),
      )
  )
)