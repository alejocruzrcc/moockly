shinyServer(function(input, output, session) {
  
  # Data Preparation --------------------------------------------------------
  
  #Load dataset
  nodos <- read.csv(file = "nodos.csv")
  aristas <- as.data.frame(read.csv2(file = "completo.csv"))
  
  #Nodes
  nodes <- as.data.frame(nodos)
  colnames(nodes) <- c("id", "label", "group")
  
  #id has to be the same like from and to columns in edges
  #nodes$id <- nodes$label
  
  #Edges  
  edges <- aristas
  colnames(edges) <- c("from", "to", "label", "sectionsource", "sectiontarget", "student", "session", "datetime", "week")
  edgesvacio <- data.frame()
  #Lee XML del curso para asignar nombres de modulos
  nombremod <- xmlParse(file = "course/chapter/df13b37d70964faf88cfb44b7a36ac55.xml")
  modulos <- c(sort(unique(as.character(edges$week))))
  dicmod <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE) 
  mh <- read_xml(paste("course/course/2019-II.xml", sep= "", collapse = NULL))
  #print(xml_attr(mh, "url_name"))
  nombrecurso <- xml_attr(mh, "display_name")
  modulosxml <- as.array(xml_attr(xml_children(mh), "url_name"))
  modulosxml <- modulosxml[!is.na(modulosxml)]
  modulosxml <- head(modulosxml, -2)
  modulosxml <- modulosxml[-1]
  
  for(i in modulosxml){
    modfile <- read_xml(paste("course/chapter/", i ,".xml",sep = "", collapse = NULL))
    modleido <- xml_attr(modfile, "display_name")
    dicmod = rbind(dicmod, data.frame(key = i, label = modleido))
  }
  
  moduloshash  <- hash(keys= dicmod$label, values= dicmod$key) #Un diccionario de codigo de modulos y su valor
  
  ## Input de selección de modulos
  output$weeksData = renderUI({
    labels <- array(dicmod[,2])
    selectInput('weeks', 'Módulo del curso a analizar: ', choices = c("--", labels), selected = "All")
  })
  
  ## Actualiza dataframe segúin el módulo seleccionado 
  df_subset <- eventReactive(input$weeks,{
    if(input$weeks=="--") {df_subset <- edgesvacio}
    else{
      semseleccionada <- input$weeks
      df_subset <- edges[edges$week == toString(moduloshash[[ semseleccionada ]]),]
      }
  })
  
  ## input de  seleccion de estudiantes según modulo seleccionado
  output$studentsData <- renderUI({
    estudiantes <- c("--", sort(as.character(unique(df_subset()$student))))
    selectInput('students', 'Estudiante:', choices = estudiantes)
  })
  
  ## Actualiza dataframe correspondiente al estudiante seleccionado
  df_subset1 <- reactive({
    if(is.null(input$students)){df_subset()} else {df_subset()[df_subset()$student %in% input$students,]}
  })
  
  ## input de seleccion de sesion según el estudiante seleccionado 
  dicses <- reactive({
    iniciales <- filter(df_subset1(), from == 'Signin')
    finales <- filter(df_subset1(), to == 'Signout')
    inicialesfinales <- paste(array(iniciales[,"datetime"]), array(finales[,"datetime"]), sep=" --> ")
    dicses = data.frame(key = array(iniciales[, "session"]), label = c(inicialesfinales))
    return(dicses)
  
  })
  
  output$sesionesData = renderUI({
    if(input$students != "--" && input$weeks != "--"){
      sesioneshash  <- hash(dicses()$label, dicses()$key)
      labels <- array(dicses()[,2])}
    else{
      labels <- c(sort(unique(as.character(df_subset1()$session))))
    }
      selectInput('sessions', 'Sesión:',  choices = c("--", labels) )
  })
  
  #Actualiza dataframe con la sesión seleccionada
  df_subset2 <- reactive({
    if(is.null(input$sessions)){
      df_subset1()} 
    else {
      sesioneshash  <- hash(dicses()$label, dicses()$key)
      df_subset1()[df_subset1()$session %in% sesioneshash[[input$sessions]],]
    }
  })
  
  output$datos <- renderTable({
    df_subset2()
  })
  
  #Create graph for Louvain
  graph <- graph_from_data_frame(edges, nodes, directed = FALSE)
  

  muestra <- eventReactive( input$submit, {
    nodesfrom <- unique(c(as.vector(df_subset2()$from), as.vector(df_subset2()$to))) 
    #nodesfrom <- as.vector(df_subset2()$from)
    #nodesto <- as.vector(df_subset2()$to)
    nodes <- nodes %>% filter(id %in% nodesfrom)
    #nodes <- nodes %>% filter(id %in% nodesto)
    groupsnodes <- unique(nodes[,"group"])
    visNetwork(nodes, df_subset2(), width = "100%") %>%
      visEdges(shadow = TRUE, arrows ="to", color = "#0085AF") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T)) %>%
      visClusteringByGroup(groups = groupsnodes, label = "Grupo: ") %>%
      addFontAwesome()
  })
  
  output$network <- renderVisNetwork({
    muestra()
  })
  nombre <- eventReactive( input$submit, {
    nombrecurso
  })
  output$nombreCurso <- renderText({
    nombre()
  })
  
  
})