shinyServer(function(input, output) {
  
  # Data Preparation --------------------------------------------------------
  
  #Load dataset
  nodos <- read.csv(file = "nodesOther.csv")
  aristas <- as.data.frame(read.csv2(file = "edges_e173.csv"))
  
  #Nodes
  nodes <- as.data.frame(nodos)
  colnames(nodes) <- c("id", "label")
  
  #id has to be the same like from and to columns in edges
  #nodes$id <- nodes$label
  
  #Edges  
  edges <- aristas
  colnames(edges) <- c("from", "to", "student", "session")
  
  #Crea dataframe de sesiones
  sdf <- eventReactive( input$submit, {
    edgesnuevo <- edges %>%
      filter(session == input$sessions)
  })
  
  #Create graph for Louvain
  graph <- graph_from_data_frame(edges, nodes, directed = FALSE)
  
  #Louvain Comunity Detection
  cluster <- cluster_louvain(graph)
  
  cluster_df <- data.frame(as.list(membership(cluster)))
  cluster_df <- as.data.frame(t(cluster_df))
  cluster_df$label <- rownames(cluster_df)
  
  output$network <- renderVisNetwork({
    visNetwork(nodes, sdf()) %>%
      visEdges(shadow = TRUE, arrows ="to") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = TRUE)
  })
  
  output$sesionesData = renderUI({
    sesiones <- c(unique(as.character(aristas$Session)))
    selectInput('sessions', 'Sesiones', sesiones)
  })
  
  output$datos <-  renderTable({
    sdf()
  })

  
})