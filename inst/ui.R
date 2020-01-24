require(shinyRGL)
require(rhandsontable)
require(shinyTable)
require(DiagrammeR)
require(igraph)
require(xlsx)
require(tidyr)
require(dplyr)
require(networkD3)
require(igraph)
require(RColorBrewer)

pal <- brewer.pal(12,"Set3")

shinyUI(
  navbarPage("G@pp",
             
             ####
             tabPanel("Importation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("importType", label=h5("Source?"),
                                      choices=c("Excel", "Google"), selectize=TRUE,
                                      multiple=FALSE, selected="Excel")
                        ),
                        mainPanel(
                          conditionalPanel(
                            'input.importType === "Excel"',
                            fileInput("xlsxPath", "Specify the .xlsx document to upload", accept=c(".xlsx"), multiple=TRUE)
                          ),
                          conditionalPanel(
                            'input.importType === "Google"',
                            textInput("googlePath", label="Type the name of the spreadsheet to import")
                          )
                        )
                      )
             ),
             
             ####
             tabPanel("Vertices",
                      sidebarLayout(
                        sidebarPanel(actionButton("addGoal", "Add goal", width = "100%"),
                                     tags$hr(),
                                     actionButton("addAction", "Add action", width = "100%"),
                                     tags$hr(),
                                     actionButton("addPeople", "Add people", width = "100%"),
                                     tags$hr(),
                                     actionButton("reorder", "Reorder", width = "100%"),
                                     tags$hr(),
                                     actionButton("remove", "Remove", width = "100%"),
                                     tags$hr(),
                                     width=2
                                     ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Goals", rHandsontableOutput("goalsEdit", width = "100%", height = "800px")),
                            tabPanel("Actions", rHandsontableOutput("actionsEdit", width = "100%", height = "800px")),
                            tabPanel("People", rHandsontableOutput("peopleEdit", width = "100%", height = "800px"))
                            ),
                          width = 10
                          )
                        )
                      ),
             
             ####
             tabPanel("Edges",
                      tabsetPanel(
                        tabPanel("Goals to goals",
                                 rHandsontableOutput("g2gEdit", width = "100%"),
                                 tags$hr(),
                                 grVizOutput("g2gGraph", height = "1000px")),
                        tabPanel("Actions to goals",
                                 rHandsontableOutput("a2gEdit", width = "100%"),
                                 tags$hr(),
                                 actionButton("refresha2g", "(Re)draw the diagram"),
                                 uiOutput("a2gChordNet")),
                        tabPanel("People to actions",
                                 rHandsontableOutput("p2aEdit", width = "100%"),
                                 tags$hr(),
                                 actionButton("refreshp2a", "(Re)draw the diagram"),
                                 uiOutput("p2aChordNet"))
                      )
             ),
             
             ####
             tabPanel("Analyses",
                      
                      sidebarLayout(
                        sidebarPanel(width=3,
                          selectInput("VAttribute", label="Vertex size attribute", choices=c("resources","degree","betweenness","power"), selected = "resources"),
                          selectInput("EAttribute", label="Edge size attribute", choices=c("resources","betweenness"), selected = "resources"),
                          selectInput("group", label="Vertex color", choices=c("type", "category"), selected = "type"),
                          numericInput("sizeCoeff", label="Size of the elements", min=1, max=25, value=1),
                          numericInput("charge", label="Repulsion force", min=1, max=1000, value=500),
                          actionButton("refreshAnalyses", "Refresh the diagrams")
                        ),
                        mainPanel(width=9,
                          tabsetPanel(
                            tabPanel("Work Flow",
                                     sankeyNetworkOutput("sankeyGraph", width = "100%", height="800px"),
                                     forceNetworkOutput("wholeNetGraph", width = "100%", height="800px")
                            ),
                            tabPanel("Goals analysis",
                                     forceNetworkOutput("actionsNetGraph", width = "100%", height="800px")
                            ),
                            tabPanel("Actions analysis"
                                     
                            ),
                            tabPanel("Social network analysis",
                                     sliderInput("thquant", label="Percentage of links deleted", min=0, max=1, value=0.25, step=0.05),
                                     forceNetworkOutput("peopleNetGraph", width = "100%", height="800px")
                            )
                          )
                        )
                      )
             ),
             tabPanel("Save", downloadButton("save", "Save"))
             
             #End
  )
)