fluidPage(theme = "mystyle.css",
          tags$style("
           body {
           -moz-transform: scale(0.8, 0.8); /* Moz-browser */
           zoom: 0.8; /* Other non-webkit browsers */
           zoom: 80%; /* Webkit browsers */
           }"),
          
          #Git
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
              menuItem("Description of Statistics", tabName = "glossary", icon = icon("question-circle", lib = "font-awesome"))
            ),
            
            sidebarMenu(id = "foot", class = "sidefooter", includeMarkdown(paste(path, "/", "www/footer.md",  sep = ""))
            ) #End sidebarPanel for Footer
          ),#End dashboardSidebar
          
          # Begin Dashboard Body
          body <- dashboardBody(
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
            ),
            # Instructions Tab
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
                      # bsModal("instructionsModal", h3(" "), "", size = "large",
                      #         includeMarkdown("www/instructions.md")
                      # )
              ),# End Tab Item - Instructions
              # Begin Tab Item - Model
              tabItem(tabName = "model",
                      
                      # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                      fluidRow(
                        column(width = 6,
                               
                               box(width = NULL,
                                   title = tagList(shiny::icon("cloud-upload"),"Train a New Model: Load Data "), status = "primary", solidHeader = TRUE,
                                   fileInput("data_import", "Upload pipeline CSV file (DO THIS STEP FIRST!)"),
                                   tableOutput("out_data_import")
                                   ), # End box
                               
                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard", lib = "font-awesome"),"Train Model"), status = "primary", solidHeader = TRUE,
                                   # This style tag is used to group the import text and checkmark together
                                   tags$head(tags$style("#container * { display: inline;vertical-align:top}")),
                                   
                                   splitLayout(cellWidths = c("35%", "50%"),
                                               actionButton("train", width = "150px", label = "Train Model",  icon("ellipsis-h"),  class = "btn btn-primary btn-md"),
                                               div(id="container",imageOutput("trainedCheckmark"), textOutput("trainedMSG")))
                               ), # End Box
                               
                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard", lib = "font-awesome"),"New Opportunity: Parameters"), status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     numericInput("contractMonths", label = "Contract Period Months",
                                                  value = 12, min = 1, max = 120, step = 1),
                                     currencyInput("ourValue", "Our Value", value = 1, 
                                                   format = "NorthAmerican", width = NULL, align = "right"),
                                     numericInput("govWin", label = "GovWin Fit Score",
                                                  value = 0, min = 0, max = 100, step = 1)
                                     # NOTE: GovWin Fit Score uses NAICS codes, GovWin IQ Smart Tags, opportunity description/keywords, 
                                     # place of performance, and past history with an agency
                                   ),
                                   splitLayout(
                                     selectInput("portfolio", label ="Portfolio", choices = c("CORP" = "CORP", "CREWS"= "CREWS",
                                                                                              "ESI" = "ESI", "ORLANDO" = "ORLANDO",
                                                                                              "TADSS" = "TADSS", "SMS" = "SMS"),
                                                                                               selected = "CORP"),
                                     selectInput("role", "Role", c("PRIME" = "prime","SUB"= "sub","TBD" = "tbd")),
                                     selectInput("cpeg", "CPEG", c("Create"="Create","Grow" = "Grow","Protect" = "Protect", 
                                                                   "Expand" = "Expand")
                                     )
                                   ),
                                   splitLayout(
                                     selectInput("competitionType", label ="Competition Type", choices = c("F&O" = "F&O", "SBSA"= "SBSA",
                                                                                                           "Sole Source" = "ss", "TBD" = "TBD")),
                                     selectInput("customerType", "Customer Type", c("NB" = "NB","RC"= "RC","TBD" = "TBD")),
                                     selectInput("contractType", "Contract Type", c("CPFF"="CPFF","FFP" = "FFP","IDIQ" = "IDIQ", 
                                                                                    "SINGLE AWA" = "SINGLE AWA","SOLE SOURCE" = "SOLE SOURCE", 
                                                                                    "TASKORDER" = "TASKORDER","T&M"= "T&M", "8A"="8A","TBD"="TBD"))
                                   )
                               ), # End box

                               box(width = NULL,
                                   title = tagList(shiny::icon("keyboard", lib = "font-awesome"),"Modeling Functions"), status = "primary", solidHeader = TRUE,
                                   # This style tag is used to group the import text and checkmark together
                                   tags$head(tags$style("#container * { display: inline;vertical-align:top}")),
                                   
                                   splitLayout(cellWidths = c("35%", "25%", "40%"),
                                               actionButton("load", width = "150px", label = "Load Parameters",  icon("ellipsis-h"),  class = "btn btn-primary btn-md"),
                                               div(id="container",imageOutput("loadedCheckmark"), textOutput("loadedMSG")),
                                               div(id="container",imageOutput("solvedCheckmark"), textOutput("solvedMSG"))
                                   ),
                                   
                                   tags$style(HTML('#reset{background-color:#DE5A52; border-color:#DE5A52}')),# Change color of reset button
                                   tags$style(HTML('#reset:hover{background-color:#9D544F; border-color:#9D544F}')),
                                   tags$style(type='text/css',"#download {width=100%; margin-top: 25px;}"),
                                   
                                   div(actionButton("reset", width = "120px", label = "Reset Model", class = "btn-primary"), style =list("padding: 25px;"))
                               ), # End Box
                               
                               ), # End Column
                        column(width = 5,
                               box(width=NULL,
                                   title = "Status Report", status = "primary", solidHeader = TRUE,
                                   splitLayout(
                                     valueBoxOutput("opportunityBox", width = NULL),
                                     valueBoxOutput("reducedOpportunityBox", width = NULL)
                                   )
                               ),
                               
                               box(width=NULL,
                                   title = "Receiver Operating Characteristic (ROC) Curve", status = "primary", solidHeader = TRUE,
                               plotOutput("rocPlot")
                               ),
                               
                               box(width=NULL,
                                   title = "Model Report", status = "warning", solidHeader = TRUE,
                                   splitLayout(
                                     valueBoxOutput("scoreBox", width = NULL),
                                     infoBoxOutput("winBox", width = NULL)
                                   )
                               )

                        ) # End Column

                      ) # End Fluid Row
             ),# End Tab Item - Model
             
             tabItem(tabName = "glossary",
                     box(width = 12,
                         includeMarkdown("www/glossary.md")
                     ))
            
             ) # End Tab Items
          ) # End dashboard Body

) # End Fluid Page

ui <- dashboardPage(header, sidebar, body, skin = "blue")