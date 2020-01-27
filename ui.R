shinyUI(
  
  fluidPage(theme = shinytheme("cerulean"),
            navbarPage("Moockly",
                       tabPanel("Inicio",
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
                                                         h3(textOutput("nombreCurso")),
                                                         visNetworkOutput("network")
                                                ),
                                                tabPanel("Estadísticas",
                                                         tableOutput("datos")
                                                )
                                    )
                                  )
                                  
                                )
                                
                       ),
                       tabPanel("Acerca de",
                                h2("¿Qué es Moockly?"),
                                p("Moockly es una aplicación de visualización de información, está pensada para instructores de cursos en línea abiertos y masivos MOOC. Aquí podrás analizar a través de un grafo dirigido, las rutas por los diferentes contenidos que los estudiantes han tomado en cada semana del curso."),
                                
                                h5("Contribuciones en "), a("https://github.com/alejocruzrcc/moockly")),
                       tabPanel("Instrucciones")
                       
            )))

