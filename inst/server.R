# Increase the size of the memory allocated to the documents
options(shiny.maxRequestSize=30*1024^2)

#Function to build diagram
buildGraph <- function(nodes, edges) {
  nodesDef <- paste0(nodes$nodes,
                     " [style='filled', fillcolor='", nodes$fillcolor,
                     "', fontcolor='", nodes$fontcolor,
                     "']")
  edgesDef <- paste0(edges$from,"->", edges$to,
                     " [dir='", edges$dir,
                     "', color='", edges$color,
                     "', penwidth=", edges$coefficient,
                     ", fontsize='10']")
  graphDef <- paste("digraph MAP {graph [overlap=false, outputorder=edgesfirst]",
                    paste(nodesDef, collapse=" "),
                    paste(edgesDef, collapse=" "), "}",
                    collapse = " ")
  return(grViz(graphDef))
}

#Function to build force networks
buildForceNet <- function(g, nodes, input){
  V(g)$resources <- nodes[V(g)$name,"value"]
  V(g)$type <- as.character(nodes[V(g)$name,"type"])
  V(g)$category <- as.character(nodes[V(g)$name,"category"])
  V(g)$power <- bonpow(g, exponent = -2)
  V(g)$degree <- degree(g)
  V(g)$betweenness <- betweenness(g)
  V(g)$weight <- get.vertex.attribute(g, input$VAttribute)
  V(g)$weight <- V(g)$weight/max(V(g)$weight)*input$sizeCoeff
  
  E(g)$resources <- E(g)$weight
  E(g)$betweenness <- edge_betweenness(g)
  E(g)$weight <- get.edge.attribute(g, input$EAttribute)
  E(g)$weight <- E(g)$weight/max(E(g)$weight)*input$sizeCoeff/10
  
  edges <- get.data.frame(g, what="edges")
  vertices <- get.data.frame(g, what="vertices")
  vertices$name <- as.factor(vertices$name)
  edges$from <- factor(edges$from, levels=vertices$name)
  edges$to <- factor(edges$to, levels=vertices$name)
  edges$from <- as.numeric(edges$from)-1
  edges$to <- as.numeric(edges$to)-1
  
  net <- forceNetwork(Links=edges, Nodes=vertices, NodeID = "name",
                      Source="from", Target="to",
                      Value= "weight", linkWidth = JS("function(d) { return 10*(d.value); }"),
                      Nodesize = "weight", radiusCalculation = JS(" (d.nodesize)*10"),
                      Group = input$group, charge=-input$charge,
                      colourScale = JS("d3.scale.category10()"),
                      zoom=TRUE, opacity=0.8, opacityNoHover=1, legen=TRUE)
  return(net)
}

# Start shiny server
require(shinyRGL)
shinyServer(function(input, output, session) {
  
  ###################################
  ##Import the data and format them
  importList <- reactive({
    if (input$importType=="Excel"){
      importList <- list()
      importList[["goals"]] <- read.xlsx(file=input$xlsxPath[1,"datapath"], sheetName="goals", as.data.frame=TRUE, row.names=1,
                                         colClasses=c("character","character","character","character","character","character","integer","logical"),
                                         stringsAsFactors=FALSE)
      importList[["actions"]] <- read.xlsx(file=input$xlsxPath[1,"datapath"], sheetName="actions", as.data.frame=TRUE, row.names=1,
                                           colClasses=c("character","character","character","integer","integer","logical"),
                                           stringsAsFactors=FALSE)
      importList[["people"]] <- read.xlsx(file=input$xlsxPath[1,"datapath"], sheetName="people", as.data.frame=TRUE, row.names=1,
                                          colClasses=c("character","character","character","integer","character","logical"),
                                          stringsAsFactors=FALSE)
      importList[["g2g"]] <- read.xlsx(file=input$xlsxPath[1,"datapath"], sheetName="g2g", as.data.frame=TRUE, row.names=1)
      importList[["a2g"]] <- read.xlsx(file=input$xlsxPath[1,"datapath"], sheetName="a2g", as.data.frame=TRUE, row.names=1)
      importList[["p2a"]] <- read.xlsx(file=input$xlsxPath[1,"datapath"], sheetName="p2a", as.data.frame=TRUE, row.names=1)
    } else {
      require(googlesheets)
      require(dplyr)
      spreadsheet <- gs_title(input$googlePath)
      importList <- list()
      importList[["goals"]] <- as.data.frame(gs_read_csv(spreadsheet, ws = "goals"))
      importList[["actions"]] <- as.data.frame(gs_read_csv(spreadsheet, ws = "actions"))
      importList[["people"]] <- as.data.frame(gs_read_csv(spreadsheet, ws = "people"))
      importList[["g2g"]] <- as.data.frame(gs_read_csv(spreadsheet, ws = "g2g"))
      importList[["a2g"]] <- as.data.frame(gs_read_csv(spreadsheet, ws = "a2g"))
      importList[["p2a"]] <- as.data.frame(gs_read_csv(spreadsheet, ws = "p2a"))
    }
    importList
  })
  
  #Make objects out of the 6 imported tables and format them 
  goals <- reactive({ if(is.null(importList()$goals)) NA else {
    goals <- importList()$goals
    goals$category <- factor(goals$category, levels=c("Social","Environmental","Financial","Commercial","Operational","Developmental"))
    goals
    }
  })
  actions <- reactive({ if(is.null(importList()$actions)) NA else {
    actions <- importList()$actions
    actions$category <- factor(actions$category, levels=c("Social","Environmental","Financial","Commercial","Operational","Developmental"))
    actions
    }
  })
  people <- reactive({ if(is.null(importList()$people)) NA else {
    people <- importList()$people
    people$profile <- factor(people$profile, levels=c("Me","Superior","Subordinate","Colleague","Customer","Owner","Creditor","Supplier","Partner","Competitor","State","Other"))
    people
    }
  })
  g2g <- reactive({
    if(is.null(importList()$g2g)) NA else {
      g2g <- importList()$g2g
      for (i in 1:length(g2g)) g2g[,i] <- as.integer(g2g[,i])
      g2g
    }
  })
  a2g <- reactive({
    if(is.null(importList()$a2g)) NA else {
      a2g <- importList()$a2g
      for (i in 1:length(a2g)) a2g[,i] <- as.integer(a2g[,i])
      a2g
    }
  })
  p2a <- reactive({
    if(is.null(importList()$p2a)) NA else {
      p2a <- importList()$p2a
      for (i in 1:length(p2a)) p2a[,i] <- as.integer(p2a[,i])
      p2a
    }
  })
  
  ###################################
  #Gather data in reactive values
  values <- reactiveValues()
  observe({
    if(!is.null(input$xlsxPath) & !is.null(input$googlePath)) {
      values$goals <- goals()
      values$actions <- actions()
      values$people <- people()
      values$g2g <- g2g()
      values$a2g <- a2g()
      values$p2a <- p2a()
      values$nodes <- NA
      values$edgeList <- NA
    }
  })
  
  #Update the reactive values based on user input
  observe({ if(!is.null(input$goalsEdit)) values$goals <- suppressWarnings(hot_to_r(input$goalsEdit)) })
  observe({ if(!is.null(input$actionsEdit)) values$actions <- suppressWarnings(hot_to_r(input$actionsEdit)) })
  observe({ if(!is.null(input$peopleEdit)) values$people <- suppressWarnings(hot_to_r(input$peopleEdit)) })
  observe({ if(!is.null(input$g2gEdit)) {
    g2g <- suppressWarnings(hot_to_r(input$g2gEdit))
    #The next 2 lines are necessary bacause the handsontable use labels rather than IDs
    names(g2g) <- row.names(values$goals)
    row.names(g2g) <- row.names(values$goals)
    values$g2g <- g2g
    }
  })
  observe({ if(!is.null(input$a2gEdit)) {
    a2g <- suppressWarnings(hot_to_r(input$a2gEdit))
    #The next 2 lines are necessary bacause the handsontable use labels rather than IDs
    names(a2g) <- row.names(values$goals)
    row.names(a2g) <- row.names(values$actions)
    values$a2g <- a2g
    }
  })
  observe({ if(!is.null(input$p2aEdit)) {
    p2a <- suppressWarnings(hot_to_r(input$p2aEdit))
    #The next 2 lines are necessary bacause the handsontable use labels rather than IDs
    names(p2a) <- row.names(values$actions)
    row.names(p2a) <- row.names(values$people)
    values$p2a <- p2a
    }
  })
  
  ###################################
  #Functions to format data entries
  formatInteger <- function(x){
    hot_cols(x, colWidths = 100, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
              td.align = 'center';
             if (value >= 3) {
              td.style.color = 'white';
              td.style.background = 'green';
             } else if (value ==2 ) {
              td.style.color = 'black';
              td.style.background = 'LimeGreen';
             } else if (value ==1 ) {
              td.style.color = 'black';
              td.style.background = 'LawnGreen';
             } else if (value ==0 ) {
              td.style.color = 'white';
              td.style.background = 'white';
             } else if (value ==-1 ) {
              td.style.color = 'black';
              td.style.background = 'pink';
             } else if (value ==-2 ) {
              td.style.color = 'black';
              td.style.background = 'HotPink';
             } else if (value <=-3 ) {
              td.style.color = 'white';
              td.style.background = 'red';
             }
           }")
  }
  
  #Format and display tables for input
  output$goalsEdit <- renderRHandsontable({
    rhandsontable(values$goals, stretchH = "all") %>%
      hot_rows(rowHeights = 30) %>%
      hot_col("category", allowInvalid = FALSE, type=c("dropdown"))
    })
  output$actionsEdit <- renderRHandsontable({
    rhandsontable(values$actions, stretchH = "all") %>%
      hot_rows(rowHeights = 30) %>%
    hot_col("category", allowInvalid = FALSE, type=c("dropdown"))
    })
  output$peopleEdit <- renderRHandsontable({
    rhandsontable(values$people, stretchH = "all") %>%
      hot_rows(rowHeights = 30) %>%
      hot_col("profile", allowInvalid = FALSE, type=c("dropdown"))
    })
  output$g2gEdit <- renderRHandsontable({
    rhandsontable(values$g2g, rowHeaders=values$goals$indicator, colHeaders=values$goals$indicator) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, rowHeaderWidth = 150) %>%
      hot_rows(rowHeights = 25) %>%
      formatInteger()
    })
  output$a2gEdit <- renderRHandsontable({
    rhandsontable(values$a2g, rowHeaders=values$actions$action, colHeaders=values$goals$indicator) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, rowHeaderWidth = 150) %>%
      hot_rows(rowHeights = 25) %>%
      formatInteger()
    })
  output$p2aEdit <- renderRHandsontable({
    rhandsontable(values$p2a, rowHeaders=values$people$name, colHeaders=values$actions$action) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, rowHeaderWidth = 150) %>%
      hot_rows(rowHeights = 25) %>%
      formatInteger()
    })
  
  ###################################
  #Edition (add or remove columns and lines, to save or reload the documents)
  
  observeEvent(input$addGoal, {
    initRowIDs <- row.names(values$goals)
    goalID <- isolate(paste0("G",max(as.integer(gsub("G","",row.names(values$goals))))+1))
    newgRow <- c(NA, NA, NA, NA, NA, NA, TRUE)
    newg2gRow <- rep(0, length(values$g2g))
    values$goals <- rbind(values$goals, newgRow)
    row.names(values$goals) <- c(initRowIDs, goalID)
    values$g2g <- rbind(values$g2g, newg2gRow)
    row.names(values$g2g) <- c(initRowIDs, goalID)
    values$g2g[,goalID] <- rep(0, nrow(values$g2g))
    values$a2g[,goalID] <- rep(0, nrow(values$a2g))
  })
  observeEvent(input$addAction, {
    initRowIDs <- row.names(values$actions)
    actionID <- isolate(paste0("A",max(as.integer(gsub("A","",row.names(values$actions))))+1))
    newaRow <- c(NA, NA, NA, TRUE)
    newa2gRow <- rep(0, length(values$a2g))
    values$actions <- rbind(values$actions, newaRow)
    values$a2g <- rbind(values$a2g, newa2gRow)
    row.names(values$actions) <- c(initRowIDs, actionID)
    row.names(values$a2g) <- c(initRowIDs, actionID)
    values$p2a[,actionID] <- factor(levels=c("Decision","Contribution","Consultation","Information"))
  })
  observeEvent(input$addPeople, {
    initRowIDs <- row.names(values$people)
    peopleID <- isolate(paste0("P",max(as.integer(gsub("P","",row.names(values$people))))+1))
    newpRow <- c(NA, NA, NA, NA, TRUE)
    newp2aRow <- rep(0, length(values$p2a))
    values$people <- rbind(values$people, newpRow)
    values$p2a <- rbind(values$p2a, newp2aRow)
    row.names(values$people) <- c(initRowIDs, peopleID)
    row.names(values$p2a) <- c(initRowIDs, peopleID)
  })
  observeEvent(input$reorder, {
    orderGoals <- order(values$goals$rank)
    orderActions <- order(values$actions$rank)
    orderPeople <- order(values$people$group)
    values$goals <- values$goals[orderGoals,]
    values$actions <- values$actions[orderActions,]
    values$people <- values$people[orderPeople,]
    values$g2g <- values$g2g[orderGoals,orderGoals]
    values$a2g <- values$a2g[orderActions,orderGoals]
    values$p2a <- values$p2a[orderPeople,orderActions]
  })
  observeEvent(input$remove, {
    keepGoals <- values$goals$keep
    keepActions <- values$actions$keep
    keepPeople <- values$people$keep
    values$goals <- values$goals[keepGoals,]
    values$actions <- values$actions[keepActions,]
    values$people <- values$people[keepPeople,]
    values$g2g <- values$g2g[keepGoals,keepGoals]
    values$a2g <- values$a2g[keepActions,keepGoals]
    values$p2a <- values$p2a[keepPeople,keepActions]
  })
  output$save <- downloadHandler(
    filename <- function(){
      paste("base.xlsx")
    },
    content = function(file) {
      write.xlsx(values$goals, file=file, sheetName="goals", showNA = FALSE, append=TRUE)
      write.xlsx(values$actions, file=file, sheetName="actions", showNA = FALSE, append=TRUE)
      write.xlsx(values$people, file=file, sheetName="people", showNA = FALSE, append=TRUE)
      write.xlsx(values$g2g, file=file, sheetName="g2g", showNA = FALSE, append=TRUE)
      write.xlsx(values$a2g, file=file, sheetName="a2g", showNA = FALSE, append=TRUE)
      write.xlsx(values$p2a, file=file, sheetName="p2a", showNA = FALSE, append=TRUE)
      write.xlsx(values$nodes, file=file, sheetName="nodes", showNA = FALSE, append=TRUE)
      write.xlsx(values$edgeList, file=file, sheetName="edgeList", showNA = FALSE, append=TRUE)
    }
  )
  
  ###################################
  #Representation (build graphs and analyses)
  
  #Goals network (format the g2g table to generate a network with DiagrammeR)
  output$g2gGraph <- renderGrViz({
    #Prepare nodes
    nodes <- select(values$goals, indicator, category)
    names(nodes) <- c("nodes", "fillcolor")
    nodes$nodes <- gsub(" ", "_", nodes$nodes)
    nodes$fillcolor <- as.character(nodes$fillcolor)
    nodes$fontcolor <- nodes$fillcolor
    nodes$fillcolor <- gsub("Financial","firebrick4",nodes$fillcolor)
    nodes$fillcolor <- gsub("Social","navy",nodes$fillcolor)
    nodes$fillcolor <- gsub("Environmental","darkgreen",nodes$fillcolor)
    nodes$fillcolor <- gsub("Commercial","lightgoldenrod",nodes$fillcolor)
    nodes$fillcolor <- gsub("Operational","palegreen",nodes$fillcolor)
    nodes$fillcolor <- gsub("Developmental","lightblue",nodes$fillcolor)
    nodes$fontcolor <- gsub("Financial","white",nodes$fontcolor)
    nodes$fontcolor <- gsub("Social","white",nodes$fontcolor)
    nodes$fontcolor <- gsub("Environmental","white",nodes$fontcolor)
    nodes$fontcolor <- gsub("Commercial","black",nodes$fontcolor)
    nodes$fontcolor <- gsub("Operational","black",nodes$fontcolor)
    nodes$fontcolor <- gsub("Developmental","black",nodes$fontcolor)
    #Prepare edges
    edges <- values$g2g
    names(edges) <- nodes$nodes
    row.names(edges) <- nodes$nodes
    edges$from <- row.names(edges)
    edges <- gather(edges, to, label, -from)
    edges <- edges[edges$label!=0 & !is.na(edges$label),]
    edges$color <- "green"
    edges[edges$label<0,"color"] <- "red"
    edges$coefficient <- ceiling((edges$label)^2/2)
    #Build the graph
    g2gGraph <- buildGraph(nodes, edges)
    g2gGraph
  })
  
  #Build all the necessary information of the diagrams
  diagramBases <- reactive({
    #Prepare nodes
    nodesG <- select(values$goals, indicator, category)
    nodesA <- select(values$actions, action, category)
    nodesP <- select(values$people, name, profile)
    names(nodesG) <- c("node", "category")
    names(nodesA) <- c("node", "category")
    names(nodesP) <- c("node", "category")
    nodesG$node <- paste(row.names(nodesG), gsub(" ", "_", nodesG$node), sep="_")
    nodesA$node <- paste(row.names(nodesA), gsub(" ", "_", nodesA$node), sep="_")
    nodesP$node <- paste(row.names(nodesP), gsub(" ", "_", nodesP$node), sep="_")
    nodes <- rbind(nodesG, nodesA, nodesP)
    row.names(nodes) <- nodes$node
    nodes$type <- as.character(substr(nodes$node,1,1))
    nodes$type <- replace(nodes$type, nodes$type=="G", "Goal")
    nodes$type <- replace(nodes$type, nodes$type=="A", "Action")
    nodes$type <- replace(nodes$type, nodes$type=="P", "People")
    nodes$type <- as.factor(nodes$type)
    
    #Prepare matrices
    matG2G <- values$g2g
    names(matG2G) <- nodesG$node
    row.names(matG2G) <- nodesG$node
    matG2G <- abs(as.matrix(matG2G))
    
    matA2G <- values$a2g
    names(matA2G) <- nodesG$node
    row.names(matA2G) <- nodesA$node
    matA2G <- abs(as.matrix(matA2G))
    
    matP2A <- values$p2a
    names(matP2A) <- nodesA$node
    row.names(matP2A) <- nodesP$node
    matP2A <- abs(as.matrix(matP2A))
    
    #Recompute the weights: action magnitude times roles gives the resource needed
    magnAction <- select(values$actions, magnitude)
    magnAction$magnitude <- replace(magnAction$magnitude, magnAction$magnitude==0, 1)
    magnAction$magnitude <- replace(magnAction$magnitude, is.na(magnAction$magnitude), 1)
    weightedP2A <- as.data.frame(matP2A)
    for (i in 1:ncol(weightedP2A)) weightedP2A[,i] <- weightedP2A[,i]*magnAction[i,"magnitude"]
    resourcesActions <- data.frame(source=names(weightedP2A), resources=colSums(weightedP2A))
    #The  the resources are sent from actions to goals
    weightedA2G <- as.data.frame(matA2G/replace(rowSums(matA2G), rowSums(matA2G)==0 | rowSums(matA2G)==0,1)*resourcesActions$resources)
    resourcesGoals <- data.frame(source=names(weightedA2G), resources=colSums(weightedA2G))
    #And finally from goals to goals
    weightedG2G <- matG2G/replace(rowSums(matG2G),rowSums(matG2G)==0,1)
    a <- as.data.frame(weightedG2G*-1)
    for (i in 1:length(a)) a[i,i]=1
    b <- resourcesGoals
    b <- b[row.names(a),"resources"]
    a <- as.matrix(a)
    allocation <- solve(t(as.matrix(a)),b)
    weightedG2G <- as.data.frame(weightedG2G*allocation)
    
    #Prepare edgeLists from weighted matrices
    edgListG2G <- weightedG2G %>%
      mutate(source=row.names(weightedG2G)) %>%
      gather(target, value, -source)
    edgListG2G$type <- "g2g"
    edgListA2G <- weightedA2G %>%
      mutate(source=row.names(weightedA2G)) %>%
      gather(target, value, -source)
    edgListA2G$type <- "a2g"
    edgListP2A <- weightedP2A %>%
      mutate(source=row.names(weightedP2A)) %>%
      gather(target, value, -source)
    edgListP2A$type <- "p2a"
    edgeList <-rbind(edgListG2G, edgListA2G, edgListP2A)
    
    #Prepare the base for the A2G chord diagram
    dimnames <- c(nodesG$node, nodesA$node)
    chordA2G <- matrix(nrow=length(dimnames), ncol=length(dimnames), dimnames=list(dimnames, dimnames))
    chordA2G <- as.data.frame(replace(chordA2G, is.na(chordA2G),0))
    chordA2G[nodesA$node,nodesG$node] <- as.data.frame(weightedA2G)[nodesA$node,nodesG$node]
    chordA2G[nodesG$node,nodesA$node] <- as.data.frame(t(weightedA2G))[nodesG$node,nodesA$node]
    
    #Prepare the base for the P2A chord diagram
    dimnames <- c(nodesP$node, nodesA$node)
    chordP2A <- matrix(nrow=length(dimnames), ncol=length(dimnames), dimnames=list(dimnames, dimnames))
    chordP2A <- as.data.frame(replace(chordP2A, is.na(chordP2A),0))
    chordP2A[nodesP$node,nodesA$node] <- as.data.frame(weightedP2A)[nodesP$node,nodesA$node]
    chordP2A[nodesA$node,nodesP$node] <- as.data.frame(t(weightedP2A))[nodesA$node,nodesP$node]
    
    #Prepare the sankey diagram base
    baseSankey <- filter(edgeList, value>0)
    for (i in 1:nrow(baseSankey)){
      baseSankey[i,"source"] <- which(nodes$node==baseSankey[i,"source"])-1
      baseSankey[i,"target"] <- which(nodes$node==baseSankey[i,"target"])-1
    } 
    baseSankey[,"source"] <- as.numeric(baseSankey[,"source"])
    baseSankey[,"target"] <- as.numeric(baseSankey[,"target"])
    
    #Prepare the resources base for other networks
    srcWL <- aggregate(value~source, edgeList,sum)
    names(srcWL) <- c("node","value")
    tgtWL <- aggregate(value~target, edgeList,sum)
    names(tgtWL) <- c("node","value")
    resources <- aggregate(value~node, rbind(srcWL, tgtWL), mean)
    row.names(resources) <- resources$node
    nodes$value <- resources[row.names(nodes),"value"]
    
    base <- list()
    base$chordA2G <- chordA2G
    base$chordP2A <- chordP2A
    base$sankey <- baseSankey
    values$nodes <- nodes
    values$edgeList <- edgeList
    base
  })
  
  wholeNet <- reactive({
    edgeList <- values$edgeList
    wholeNet <- unique(rbind(select(edgeList, -type), data.frame(source=edgeList$target, target=edgeList$source, value=0)))
    wholeNet <- aggregate(value~., wholeNet, mean)
    wholeNet <- spread(wholeNet, target, value, fill=0)
    row.names(wholeNet) <- wholeNet$source
    wholeNet <- select(wholeNet, -source)
    graph_from_adjacency_matrix(as.matrix(wholeNet), mode="max", weighted = TRUE, diag = FALSE)
  })
  
  actionsNet <- reactive({
    edgeList <- values$edgeList
    actionNet <- filter(edgeList, type=="p2a")
    actionNet <- spread(actionNet, target, value, fill=0)
    row.names(actionNet) <- actionNet$source
    actionNet <- select(actionNet, -source, -type)
    graph_from_incidence_matrix(as.matrix(actionNet), weighted=TRUE)
  })
  
  peopleNet <- reactive({
    edgeList <- values$edgeList
    peopleNet <- filter(edgeList, type=="p2a")
    peopleNet <- spread(peopleNet, target, value, fill=0)
    row.names(peopleNet) <- peopleNet$source
    peopleNet <- select(peopleNet, -source, -type)
    peopleNet <- as.matrix(peopleNet) %*% t(as.matrix(peopleNet))
    peopleNet <- replace(peopleNet, peopleNet<quantile(peopleNet, input$thquant),0)
    graph_from_adjacency_matrix(peopleNet, weighted = TRUE, diag=FALSE, mode="undirected")
  })

  observeEvent(input$refresha2g, {
    output$a2gGraph <- renderchordNetwork({ 
      chordA2G <- diagramBases()$chordA2G
      chordNetwork(chordA2G, labels=names(chordA2G))
      })
    output$a2gChordNet <- renderUI({ chordNetworkOutput("a2gGraph", height = "800px") })
    })
    
  observeEvent(input$refreshp2a, {
    output$p2aGraph <- renderchordNetwork({
      chordP2A <- diagramBases()$chordP2A
      chordNetwork(chordP2A, labels=names(chordP2A))
    })
    output$p2aChordNet <- renderUI({ chordNetworkOutput("p2aGraph", height = "800px") })
  })
  
  observeEvent(input$refreshAnalyses, {
    output$sankeyGraph <- renderSankeyNetwork({
      sankey <- diagramBases()$sankey
      nodes <- values$nodes
      sankeyNetwork(Links = sankey, Nodes = nodes, Source = 'source', Target = 'target',
                    Value = 'value', NodeID = 'node', nodePadding = 50, fontSize = 14, nodeWidth = 50,
                    width = "100%", iterations = 100)
      })
    output$wholeNetGraph <- renderForceNetwork({
      buildForceNet(wholeNet(), values$nodes, input)
      })
    
    output$actionsNetGraph <- renderForceNetwork({
      buildForceNet(actionsNet(), values$nodes, input)
      })
    
    output$peopleNetGraph <- renderForceNetwork({
      buildForceNet(peopleNet(), values$nodes, input)
      })
    })
  
})