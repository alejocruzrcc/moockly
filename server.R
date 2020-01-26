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
  #mh <- xmlTreeParse("course/course/2019-II.xml", getDTD = F)
  #rootmhdoc <- xmlRoot(mh)
  #nodesxml <- xmlChildren(rootmhdoc)
  #url <- xml_attr(nodesxml[c("chapter")], "url_name")
  #print(nodesxml[1])
  for(i in 1:length(modulos)){
    modfile <- read_xml(paste("course/chapter/",modulos[[i]],".xml",sep = "", collapse = NULL))
    modleido <- xml_attr(modfile, "display_name")
    dicmod = rbind(dicmod, data.frame(key = modulos[[i]], label = modleido))
  }
  
  moduloshash  <- hash(dicmod$label, dicmod$key) #Un diccionario de codigo de modulos y su valor

  ## Input de selección de modulos
  output$weeksData = renderUI({
    labels <- array(dicmod[,2])
    selectInput('weeks', 'Módulo del curso a analizar: ', choices = c("--", labels), selected = "All")
  })
  
  ## Actualiza dataframe segúin el módulo seleccionado 
  df_subset <- eventReactive(input$weeks,{
    if(input$weeks=="--") {df_subset <- edgesvacio}
    else{
      df_subset <- edges[edges$week == moduloshash[[input$weeks]],]}
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
    dicmod
  })
  
  #Create graph for Louvain
  graph <- graph_from_data_frame(edges, nodes, directed = FALSE)
  
  
  muestra <- eventReactive( input$submit, {
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
  
  
})