fluidPage(theme = "mystyle.css",
          tags$style("
           body {
           -moz-transform: scale(0.8, 0.8); /* Moz-browser */
           zoom: 0.8; /* Other non-webkit browsers */
           zoom: 80%; /* Webkit browsers */
           }"),
          
          ## Header ###
          header <- dashboardHeader(
            title = "i3 Pipeline ",
            titleWidth = 500
          ),
          
          ## Sidebar ##
          sidebar <- dashboardSidebar(
            width = 250,
            #   # useShinyjs(),
            #   # extendShinyjs(text = showModal),
            #
            sidebarMenu(
              menuItem("Instructions", tabName = "instructions", icon = icon("info-circle", lib = "font-awesome")),
              menuItem("Model", tabName = "model", icon = icon("cog", lib = "font-awesome")),
              menuItem("Glossary of Data Fields", tabName = "glossary", icon = icon("question-circle", lib = "font-awesome"))
            ),
   
            sidebarMenu(id = "foot", class = "sidefooter", includeMarkdown(paste(path, "/", "www/footer.md",  sep = ""))
            ) #End sidebarPanel for Footer
          ),#End dashboardSidebar
          
          
          body <- dashboardBody(
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
            ),
            tabItems(
              tabItem(tabName = "instructions",
                      box(width = 12,
                          includeMarkdown("www/instructions.md")
                      ),

                      box(width = 12,
                          HTML('<img src="HQ.png" height="200" width=100%>')
                      ),
                      fluidRow(
                        column(width = 6,
                          box(width = NULL,
                            HTML('<img src="mission.png" height="100" width=100%>'))
                          ),
                        column(width = 6,
                          box(width = NULL,
                            HTML('<img src="ESOP_image.png" height="100" width=100%>'))
                          )
                        ),
                      bsModal("instructionsModal", h3(" "), "", size = "large",
                              includeMarkdown("www/instructions.md")
                      )
                      ),
      
              tabItem(tabName = "model",
                      
                      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      
                      
                      fluidRow(
                        column(width = 6,
                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard", lib = "font-awesome"),"New Opportunity: Parameters"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     numericInput("contractMonths", label = "Contract Period Months",
                                                  value = 12, min = 1, max = 120, step = 1),
                                     currencyInput("ourValue", "Our Value", value = NULL, 
                                                   format = "NorthAmerican", width = NULL, align = "right"),
                                     numericInput("govWin", label = "GovWin Fit Score",
                                                  value = 0, min = 0, max = 100, step = 1)
                                     # NOTE: GovWin Fit Score uses NAICS codes, GovWin IQ Smart Tags, opportunity description/keywords, 
                                     # place of performance, and past history with an agency
                                   )
                               ),
                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard"),"New Opportunity: Parameters"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     selectInput("portfolio", label ="Portfolio", choices = c("CORP" = "corp", "CREWS"= "crews",
                                                                                              "ESI" = "esi", "Orlando" = "orlando"),
                                                 selected = "CORP"),
                                     selectInput("role", "Role", c("PRIME" = "prime","SUB"= "sub","TBD" = "tbd")),
                                     selectInput("cpeg", "CPEG", c("Create"="create","Grow" = "grow","Protect" = "protect", 
                                                                   "Expand" = "expand")
                                     )
                                   )
                               ),
                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard"),"New Opportunity: Parameters"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     selectInput("competitionType", label ="Competition Type", choices = c("F&O" = "fo", "SBSA"= "sbsa",
                                                                                                           "Sole Source" = "ss", "TBD" = "tbd")),
                                     selectInput("customerType", "Customer Type", c("NB" = "nb","RC"= "rc","TBD" = "tbd")),
                                     selectInput("contractType", "Contract Type", c("CPFF"="cpff","FFP" = "ffp","IDIQ" = "idiq", 
                                                                                    "SINGLE AWA" = "single","SOLE SOURCE" = "sole", 
                                                                                    "TO" = "to","T&M"= "tm", "8A"="8a"))
                                   )
                               ),
                               
                               box(width = NULL,
                                   title = tagList(shiny::icon("cloud-upload"),"Train a New Model: Load Data and Parameters"), status = "primary", solidHeader = TRUE,
                                   
                                   # This style tag is used to group the import text and checkmark together
                                   tags$head(tags$style("#container * { display: inline;vertical-align:top}")),
                                   
                                   splitLayout(
                                     # Select a CSV file to upload
                                     fileInput("import", "Upload CSV File",
                                               multiple = FALSE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),
                                     # Download a template CSV file for import format
                                     div(downloadButton("downloadTemplate", "Download Template"),style =list("padding: 25px;"))
                                   ),
                                       splitLayout(cellWidths = c("35%", "25%", "40%"),
                                               actionButton("load", width = "150px", label = "Load Parameters",  icon("ellipsis-h"),  class = "btn btn-primary btn-md"),
                                               div(id="container",imageOutput("loadCheckmark"), textOutput("presolveMSG")),
                                               div(id="container",imageOutput("solvedCheckmark"), textOutput("solvedMSG"))
                                   )
                               ),
                               
                               
                               tags$style(HTML('#reset{background-color:#DE5A52; border-color:#DE5A52}')),# Change color of reset button
                               tags$style(HTML('#reset:hover{background-color:#9D544F; border-color:#9D544F}')),
                               tags$style(type='text/css',"#download {width=100%; margin-top: 25px;}"),
                                                   
                               # This creates a box for the File Output and Reset buttons
                               box(width = NULL,
                                   title = tagList(shiny::icon("cloud-download")," File Output"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     textInput("outputFile", label = "Name your exported file:", value = "BD_Pipeline_Scored"),
                                     downloadButton("download", "Download"),
                                     div(actionButton("reset", width = "120px", label = "Reset Model", class = "btn-primary"), style =list("padding: 25px;"))
                                   )
                               ),
   
                        ), #End Column
                        column(width = 5,
                               box(width=NULL, 
                               title = "Status Report", status = "warning", solidHeader = TRUE,
                               splitLayout(
                                 valueBoxOutput("opportunityBox", width = NULL),
                                 valueBoxOutput("scoreBox", width = NULL),
                                 valueBoxOutput("winBox", width = NULL)
                               ))
                        ) # End Column
                        
                      ) #End Fluid Row
                      
              )
              #   
              #   tabItem(tabName = "projects", fluidRow( column(width = 5,
              #                                                  plotOutput("descriptive1")), column(width = 5,
              #                                                                                      plotOutput("descriptive2") )), 
              #           br(), ## JGD 20190219 added a space between the plots on the first and second rows.
              #           fluidRow( column(width = 5,
              #                            plotlyOutput("descriptive3")), column(width = 5,
              #                                                                  plotlyOutput("descriptive4")) ## JGD 20190219 changed from plotOutput to plotlyOutput.
              #           )),
              #   
              #   tabItem(tabName = "datatable",
              #           div(dataTableOutput("fullSolution"), style = list("font-size:90%"))
              #   ),
              #   
              #   tabItem(tabName = "pom_sponsor_costs",
              #           div(dataTableOutput("table2"), style = "font-size:100%"),
              #           div(dataTableOutput("table3"), style = "font-size:100%")
              #   ),
              #   tabItem(tabName = "sponsor_chart",
              #           plotOutput("plot1", height = "600px")
              #   ),
              #   tabItem(tabName = "mis_programs",
              #           plotOutput("plot2", height = "600px")
              #   ),
              #   tabItem(tabName = "project_counts",
              #           plotOutput("plot3", height = "600px")
              #   ),
              #   tabItem(tabName = "cost_value_plot",
              #           plotlyOutput("costValuePlot", height = "800px")#,
              #           # JGD 20190301 trying to create an output for cost_value_plot
              #           # box(width = NULL,
              #           #     title = tagList(shiny::icon("cloud-download")," File Output"), status = "primary", solidHeader = TRUE,
              #           #     splitLayout(
              #           #       textInput("outputFile2", label = "Name your exported file:", value = "Cost_Value_Plot"),
              #           #       downloadButton("download", "Download")
              #           #     )
              #           # )
              #           
              #   ),
              #   tabItem(tabName = "statistics",
              #           fluidRow(
              #             column(6, style =list("padding: 10px;"),
              #                    div(style = list("height: 40px;"),
              #                        verbatimTextOutput("iterations")))
              #           ),
              #           
              #           fluidRow(
              #             column(6, style =list("padding: 10px;"),
              #                    div(style = list("height: 40px;"),
              #                        verbatimTextOutput("summaryProjectCount")))
              #           ),
              #           fluidRow(
              #             column(6, style =list("padding: 10px;"),
              #                    div(style = list("height: 40px;"),
              #                        verbatimTextOutput("summaryProjectsAdded")))
              #           ),
              #           fluidRow(
              #             column(6, style =list("padding: 10px;"),
              #                    div(style = list("height: 40px;"),
              #                        verbatimTextOutput("summaryProjectsDropped")))
              #           ),
              #           fluidRow(
              #             column(6, style =list("padding: 10px;"),
              #                    div(style = list("height: 40px;"),
              #                        verbatimTextOutput("summaryProjectsMoved")))
              #           ),
              #           fluidRow(
              #             column(6, style =list("padding: 10px;"),
              #                    div(style = list("height: 40px;"),
              #                        verbatimTextOutput("objectiveValue")))
              #           )
              #   ),
              #   tabItem(tabName = "glossary",
              #           box(width = 12,
              #               includeMarkdown("www/glossary.md")
              #           )
              #   )
              
              
            ) # End tabItems
          ) # End dashboardBody
           
) # End Fluid Page

ui <- dashboardPage(header, sidebar, body, skin = "blue")