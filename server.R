shinyServer(function(input, output, session) {
  
  # Data Preparation --------------------------------------------------------
  
  #Load dataset x modulos
  nodos <- read.csv(file = "nodos.csv")
  aristas <- as.data.frame(read.csv2(file = "completo.csv"))
  
  
  #Nodes
  nodes <- as.data.frame(nodos)
  colnames(nodes) <- c("id", "label", "group")
  
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
      labels <- array(dicses()[,2])
      }
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
    nodesfromto <- unique(c(as.vector(df_subset2()$from), as.vector(df_subset2()$to))) # valores unicos de nodos en graf generado 
    nodes <- nodes %>% filter(id %in% nodesfromto) #Filra el archivo de nodos con los valores unicos encontrados
    groupsnodes <- unique(nodes[,"group"])
    visNetwork(nodes, df_subset2(), width = "100%") %>%
      visEdges(shadow = TRUE, arrows ="to", color = "#22252C") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T)) %>%
      visGroups(groupname = "Video", color = "#1FB58F", shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "Quiz", color = "#EAB126", shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "Singin", color = "#1B7B34", shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "Singout", color = "#F24C4E", shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "Other", color = "#D98041", shadow = list(enabled = TRUE)) %>%
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
  
  ### PARA TOTALES ###
  
  #Load dataset totales
  nodosT <- read.csv2(file = "nodosT.csv")
  aristasT <- as.data.frame(read.csv(file = "completoT.csv"))
  
  #Nodes
  nodesT <- as.data.frame(nodosT)
  colnames(nodesT) <- c("id", "label")
  
  #Edges  
  edgesT <- aristasT
  colnames(edgesT) <- c("from", "to", "label", "sectionsource", "sectiontarget", "student", "session", "datetime", "title")
  

  ## input de  seleccion de estudiantes según modulo seleccionado
  output$studentsDataTotales <- renderUI({
    estudiantesT <- c("--", sort(as.character(unique(edgesT$student))))
    selectInput('studentsT', 'Estudiante:', choices = estudiantesT)
  })
  
  ## Actualiza dataframe correspondiente al estudiante seleccionado
  df_subtotal <- reactive({
    if(is.null(input$students)){edgesvacio} else {edgesT[edgesT$student %in% input$studentsT,]}
  })
  

  ## input de seleccion de sesion según el estudiante seleccionado 
  dicsesT <- reactive({
    
    iniciales <- filter(df_subtotal(), from == 'Signin')
    finales <- filter(df_subtotal(), to == 'Signout')
    inicialesfinales <- paste(array(iniciales[,"datetime"]), array(finales[,"datetime"]), sep=" --> ")
    dicses = data.frame(key = array(iniciales[, "session"]), label = c(inicialesfinales))
    return(dicses)
    
  })
  
  output$sesionesDataTotales = renderUI({
    if(input$studentsT != "--"){
      sesioneshash  <- hash(dicsesT()$label, dicsesT()$key)
      labelsT <- array(dicsesT()[,2])}
    else{
      labelsT <- c(sort(unique(as.character(df_subtotal()$session))))
    }
    selectInput('sessionsT', 'Sesión:',  choices = c("--", labelsT) )
  })
  
  #Actualiza dataframe con la sesión seleccionada
  df_subtotal1 <- reactive({
    if(is.null(input$sessions)){
      df_subtotal()} 
    else {
      sesioneshash  <- hash(dicsesT()$label, dicsesT()$key)
      df_subtotal()[df_subtotal()$session %in% sesioneshash[[input$sessionsT]],]
    }
  })
  
  ##MUESTRA GRAFO DE TOTALES
  muestraT <- eventReactive( input$submitTotales, {
    nodesfromto <- unique(c(as.vector(df_subtotal1()$from), as.vector(df_subtotal1()$to))) # valores unicos de nodos en graf generado 
    nodesT <- nodesT %>% filter(id %in% nodesfromto) #Filra el archivo de nodos con los valores unicos encontrados
    #groupsnodes <- unique(nodes[,"group"])
    visNetwork(nodesT, df_subtotal1(), width = "100%") %>%
      visEdges(shadow = TRUE, arrows ="to", color = "#22252C", title = "Hola") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T)) %>%
      #visGroups(groupname = "Video", color = "#1FB58F", shadow = list(enabled = TRUE)) %>%
      #visGroups(groupname = "Quiz", color = "#EAB126", shadow = list(enabled = TRUE)) %>%
      #visGroups(groupname = "Singin", color = "#1B7B34", shadow = list(enabled = TRUE)) %>%
      #visGroups(groupname = "Singout", color = "#F24C4E", shadow = list(enabled = TRUE)) %>%
      #visGroups(groupname = "Other", color = "#D98041", shadow = list(enabled = TRUE)) %>%
      #visClusteringByGroup(groups = groupsnodes, label = "Grupo: ") %>%
      addFontAwesome()
  })
  
  output$networktotales <- renderVisNetwork({
    muestraT()
  })
  output$datostotales <- renderTable({
    df_subtotal1()
  })
})