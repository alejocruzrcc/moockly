shinyServer(function(input, output, session) {
  
  # Data Preparation --------------------------------------------------------
  
  #Load dataset
  nodos <- read.csv(file = "nodes.csv")
  aristas <- as.data.frame(read.csv(file = "completo.csv"))
  
  #Nodes
  nodes <- as.data.frame(nodos)
  colnames(nodes) <- c("id", "label")
  
  #id has to be the same like from and to columns in edges
  #nodes$id <- nodes$label
  
  #Edges  
  edges <- aristas
  colnames(edges) <- c("from", "to", "sectionsource", "sectiontarget", "session", "student", "week", "datetime")
    edgesvacio <- data.frame()
  
  #Lee XML del curso para asignar nombres de modulos
  nombremod <- xmlParse(file = "course/chapter/df13b37d70964faf88cfb44b7a36ac55.xml")
  modulos <- c(sort(unique(as.character(edges$week))))
  dictot <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE) 
  for(i in 1:length(modulos)){
    modfile <- read_xml(paste("course/chapter/",modulos[[i]],".xml",sep = "", collapse = NULL))
    modleido <- xml_attr(modfile, "display_name")
    dictot = rbind(dictot, data.frame(key = modulos[[i]], label = modleido))
  }
  
  moduloshash  <- hash(dictot$label, dictot$key) #Un directorio de codigo de modulos y su valor
  
  ## Input de selección de modulos
  output$weeksData = renderUI({
    labels <- array(dictot[,2])
    selectInput('weeks', 'Seleccione módulo del curso: ', choices = c("--", labels), selected = "All")
  })
  
  ## Actualiza dataframe segúin el módulo seleccionado 
  df_subset <- eventReactive(input$weeks,{
    if(input$weeks=="--") {df_subset <- edgesvacio}
    else{
      df_subset <- edges[edges$week == moduloshash[[input$weeks]],]}
  })
  
  ##Crea input estudiantes con estudiantes activos en el modulo seleccionado
  output$studentsData <- renderUI({
    estudiantes <- c("--", sort(as.character(unique(df_subset()$student))))
    selectInput('students', 'Seleccione el estudiante:', choices = estudiantes)
  })
  
  ## Actualiza dataframe correspondiente al estudiante seleccionado
  df_subset1 <- reactive({
    if(is.null(input$students)){df_subset()} else {df_subset()[df_subset()$student %in% input$students,]}
  })
  
  ## Crea input de sesiones del estudainte seleccionado 
  output$sesionesData = renderUI({
    sesiones <- c("--", sort(as.character(unique(df_subset1()$session))))
    selectInput('sessions', 'Seleccione la sesión:',  choices = sesiones )
  })
  
  #Actualiza dataframe con la sesión seleccionada
  df_subset2 <- reactive({
    if(is.null(input$sessions)){df_subset1()} else {df_subset1()[df_subset1()$session %in% input$sessions,]}
  })
  
  
  output$datos <-  renderTable({
    dictot
  })
  
  #Create graph for Louvain
  graph <- graph_from_data_frame(edges, nodes, directed = FALSE)
  
  #Louvain Comunity Detection
  cluster <- cluster_louvain(graph)
  cluster_df <- data.frame(as.list(membership(cluster)))
  cluster_df <- as.data.frame(t(cluster_df))
  cluster_df$label <- rownames(cluster_df)
  
  muestra <- eventReactive( input$submit, {
    
    visNetwork(nodes,  df_subset2()) %>%
      visEdges(shadow = TRUE, arrows ="to") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T))

  })
  
  output$network <- renderVisNetwork({
    muestra()
  })
  
  
})