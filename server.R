server <- function(input, output, session){
  
  # template <- read.csv(paste(path, "/", "i3_Pipeline", ".csv", sep = ""),
  #                       as.is = T, header = T)

  print("Does this thing work?")
  print("test")
  
  #This function is used to download the template CSV file
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("i3_Pipeline", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    })
  print("test")
  #This function is responsible for loading in the selected file
  inputData_temp <- reactive({
    infile <- input$import
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }

    read.csv(infile$datapath,
             as.is = T,
             header = T,
             na.strings=c("","NA")) # Replace blanks with na
  })

  # The event executes when the "Load Parameters" button is selected
  obs2 <- observeEvent(input$load,{
print("test")
    # p <- 5 # Number of years
    output$statusUpdate <- renderText("text-warning")

    # # Convert multiplier values to numeric
    K_Months <- as.numeric(input$contractMonths)
   
    inputData <- inputData_temp()
    inputData[is.na(inputData)] <- NA 
    #
    # y<- 21 # Must be updated each POM cycle to reflect first year of FYDP
    m <- nrow(inputData) # Number of BD Items
    # n <- m*p # Number of decision variables

    output$opportunities <- renderPrint({
      cat("Number of Opportunities (m):", m)
    })

    # Create an info box for the number of projects input to the model
    output$opportunityBox <- renderInfoBox({
      infoBox("Opportunities", paste0(m), icon = icon("building", lib = "font-awesome"),
              color = "blue")
    })

    # Create a value box for the number of Opportunities input to the model
    output$opportunityBox <- renderValueBox({
      valueBox(
        value = formatC(m, digits = 1, format = "d"),
        subtitle = "Opportunities",
        icon = icon("building", lib = "font-awesome"),
        color = "yellow")
    })
 


output$test <- renderPrint({
  "Test Test Test"
})

test
  
  # Remove these columns
  df <-  inputData[ , !names(inputData) %in%
                c("ID","NAME","GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release",
                  "Prime.Contractor","Must.Win","Status","Actual.RFP.Release","Award.Date",
                  "BD_Sales.Lead", "Contract.Vehicle","Contracts.POC","Estimated.Start.Date","Modified.By",
                  "Moneyball","NAICS", "Prime.POC","Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due",
                  "Proposals.POC","TA.completed","Submission.Date","Estimated.Completion.Date","NAME.1",
                  "Solicitation..","Solicitation.Date","Technical.POC")]
  
  # Add a new column Result column that is 1 for "WON" and 0 for all else.
  df$Result <- with(df, ifelse(Stage == "WON", 1, 0))
  
  # Replace NB - RFQ with NB
  df$Customer.Type[df$Customer.Type == "NB - RFQ"] <- "NB"
  df$Customer.Type[df$Customer.Type == "RC - RFQ"] <- "RC"
  
  # Remove records with Stages other than "WON", "LOST", or "CANCEL"
  df <- df[!(df$Stage=="INACTIVE"|df$Stage=="PROPSUB"|df$Stage=="CAPTURE"|df$Stage=="PROPPREP"|df$Stage=="QUAL"|df$Stage=="IDENT"),]
  # Remove Stage column
  df <- df[ , !names(df) %in% c("Stage")]
  
  # Convert columns from Characters to Currency, Percentages, or Numbers
  df$Total.Value <- as.numeric(gsub('[$,]', '', df$Total.Value))
  df$Our.Value <- as.numeric(gsub('[$,]', '', df$Our.Value))
  df$Weighted.Value <- as.numeric(gsub('[$,]', '', df$Weighted.Value))
  df$Win.Probability = as.integer(sub("%", "",df$Win.Probability))
  df$Contract.Period.Months <- as.numeric(df$Contract.Period.Months)
  
  # #Convert the target variable to a factor
  # df$Result = as.factor(df$Result)
  
  # Split data in training and test sets
  set.seed(509)
  split <- initial_split(df, prop = 0.8, strata = Result)
  train <- split %>% training()
  test <- split %>% testing()
  
  # # Model uses the variable from "fit.reduced" in glm_model script.
  # logit <- glm2(Result ~ Portfolio + Contract.Type + Customer.Type + Contract.Period.Months + CPEG, data = df, family=binomial())
  # 

    output$loadCheckmark <- renderImage({
      list(src = "./www/tick.png",
           contentType = "image/png",
           height = 20,
           alt = "tick")
    }, deleteFile = FALSE)
  #   # 
  #   # output$presolveMSG <- renderText({"Presolve complete."})
  #   # 
  #   # # print(lp2)
  #   # 
  #   # picture <- 1
  #   # 
  insertUI("#load","afterEnd", actionButton("solve", "Solve"))

    output$solvedCheckmark <- renderImage({
      list(src = "./www/wait.png",
           contentType = "image/png",
           height = 20,
           alt = "tick")
    }, deleteFile = FALSE)

    output$solvedMSG <- renderText({"Press 'Solve' and please wait...."})

    observeEvent(input$solve, {
  #   #   
  #   #   # output$presolve_model <- renderPrint(lp2)
      
      # Model uses the variable from "fit.reduced" in glm_model script.
      logit <- glm2(Result ~ Portfolio + Contract.Type + Customer.Type + Contract.Period.Months + CPEG, data = df, family=binomial())
      
      ### Make a new prediction ####
      #define new observation
      newdata = data.frame(Portfolio = "CREWS", Contract.Type = "TASKORDER", Customer.Type = "RC", Contract.Period.Months = K_Months, CPEG = "Expand")
      
      #use model to predict value of am
      predict.glm(model, newdata, type="response")
      
      #TEMP#
      score = 0.5
      win = "W"
      
  #   #   
  #   #   solveLP <- solve(lp2)
  #   #   
      output$solvedCheckmark <- renderImage({
        list(src = "./www/tick.png",
             contentType = "image/png",
             height = 20,
             alt = "tick")
      }, deleteFile = FALSE)

      output$solvedMSG <- renderText({"Reached solution."})

      input$load == 0
  #   #   
  #   #   objective <- format(get.objective(lp2), scientific = FALSE, big.mark=",")
  #   #   output$objectiveValue <- renderPrint({
  #   #     cat("Objective value of solution:", objective)
  #   #   })
  #   #   
  #   #   iterationCount <- format(get.total.iter(lp2), scientific = FALSE, big.mark=",")
  #   #   output$iterations <- renderPrint({
  #   #     cat("Number of iterations to reach solution:", iterationCount)
  #   #   })
  #   #   
  #   #   primal <- get.primal.solution(lp, orig = "TRUE") # Get the full primal solution
  #   #   primaltemp <- data.matrix(primal) # Put the primal soution into a data matrix 
  #   #   primalsolution <- tail(primaltemp,n) # Keep the last n entries of the primal solution
  #   #   projectCount <- sum(primalsolution)
  #   #   output$projectCount <- renderPrint({
  #   #     cat("Projects funded:", projectCount)
  #   #   })
  #   #   output$summaryProjectCount <- renderPrint({
  #   #     cat("Number of projects funded:", projectCount)
  #   #   })
  #   #   
  #   #   # Create a value box for the number of projects inputted to the model
  #   #   output$fundedBox <- renderValueBox({
  #   #     valueBox(
  #   #       value = formatC(projectCount, digits = 1, format = "d"),
  #   #       subtitle = "Funded",
  #   #       icon = icon("dollar", lib = "font-awesome"),
  #   #       color = "green"
  #   #     )
  #   #   })
  #   #   
  #   #   dim(primalsolution) <- c(p,m) # Convert the remaining solution to a (p x m) matrix
  #   #   exportSolution <- t(primalsolution) # Transpose the soltion to make (m x p) matrix
  #   #   
  #   #   #######################################################
  #   #   # Create a matrix of costs
  #   #   #######################################################
  #   #   costTable <- data.frame(as.numeric(tempcosts)) # Table costs
  #   #   costTable <- data.matrix(costTable) # Convert to matrix
  #   #   
  #   #   colnames(costTable) <- NULL # Remove column names
  #   #   
  #   #   # Calculate costs for solution
  #   #   costMatrix <- matrix(0,m,p)
  #   #   for (i in 1:m){
  #   #     for (j in 1:p){
  #   #       costMatrix[i,j]=costTable[i,1] * exportSolution[i,j]
  #   #     }
  #   #   }
  #   #   totalcostMatrix <-  t(costTable) %*% exportSolution # Total cost by year
  #   #   
  #   #   costVector <- rowSums(costMatrix)
  #   #   
  #   #   #######################################################
  #   #   # Create cost vs value Chart
  #   #   #######################################################
  #   #   
  #   #   # (1xn) matrix of decision variables values scores
  #   #   v1 <- vectorVariables
  #   #   # Convert v1 to a mxp matrix
  #   #   dim(v1) <- c(m,p)
  #   #   # Multiply element-wise the value matrix and solution matrix 
  #   #   v2 <- v1 * exportSolution
  #   #   # Sum the values by row
  #   #   v3 <- rowSums(v2)
  #   #   # Get the cost matrix
  #   #   c1 <- costMatrix
  #   #   # Sum the costs by row
  #   #   c2 <- rowSums(c1)
  #   #   # Create a cost value data frame
  #   #   costValueTable <- data.frame(projectNumber, projectTitle, v3, c2, pomSponsor)
  #   #   colnames(costValueTable) <- c("Project.Number", "Project.Title", "Value.Score", "Cost", "POM.Sponsor")
  #   #   
  #   #   # Remove projects (rows) that ar enot chosen in the solution
  #   #   costValueTable<-costValueTable[!(costValueTable$Cost==0),]
  #   #   
  #   #   # Create Plotly chart to compare Cost vs Value (w/stability) of solution set
  #   #   output$costValuePlot <- renderPlotly({
  #   #     plot_ly(data = costValueTable, x = ~Value.Score, y = ~Cost,
  #   #             text = ~paste("Number: ", Project.Number, "<br>Title: ", Project.Title, "<br>Sponsor: ", POM.Sponsor),
  #   #             marker = list(size = 10,
  #   #                           color = 'rgba(26, 83, 255, .8)',
  #   #                           line = list(color = 'rgba(0, 26, 102, .8)',
  #   #                                       width = 2))) %>%
  #   #       layout(title = 'Value Score vs. Cost of Selected Projects',
  #   #              yaxis = list(zeroline = FALSE),
  #   #              xaxis = list(zeroline = FALSE))
  #   #   })
  #   #   
  #   #   #######################################################
  #   #   # Format the solution for display in a Shiny dataTable
  #   #   #######################################################
  #   #   
  #   #   yearColumn <- matrix("-----",m,1)
  #   #   for (i in 1:m){
  #   #     for (j in 1:p){
  #   #       if (costMatrix[i,j]>0){ 
  #   #         yearColumn[i] = paste("FY",(y+j-1))
  #   #       }
  #   #     }
  #   #   }
  #   #   
  #   #   mustFund <- matrix("-----",m,1)
  #   #   for (i in 1:m){
  #   #     for (j in 1:p){
  #   #       if (must_fund[i,j]==1){ 
  #   #         mustFund[i] = paste("FY",(y+j-1))
  #   #       }
  #   #     }
  #   #   }
  #   #   
  #   #   
  #   #   NET <- matrix("-----",m,1)
  #   #   for (i in 1:m){
  #   #     netCount = 0
  #   #     for (j in 1:p){
  #   #       if (netCount == 0){
  #   #         if (noEarlier[i,j]==1){
  #   #           NET[i] = paste("FY",(y+j-1))
  #   #           netCount = 1
  #   #         }
  #   #       }
  #   #     }
  #   #   }
  #   #   
  #   #   oldProgram <- matrix("-----",m,1)
  #   #   for (i in 1:m){
  #   #     if (previousProgram[i]>0){ 
  #   #       oldProgram[i] = paste("FY",previousProgram[i])
  #   #     }
  #   #   }
  #   #   
  #   #   formatSolution <- data.frame(projectNumber, projectTitle, misProgram, pomSponsor, exportSolution, costVector)
  #   #   colnames(formatSolution) <- c("Project Number", "Project Title", "MIS Program", "Capability Sponsor", paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "Cost")
  #   #   # rownames(formatSolution) <- projectNumber ## JGD 20190221 commented out, added as first column of table
  #   #   
  #   #   # This is the full solution in a table format
  #   #   tableSolution <- data.frame(projectNumber, projectTitle, misProgram, location, pomSponsor, mustFund, NET, oldProgram, yearColumn, costVector)
  #   #   colnames(tableSolution) <- c("Project Number", "Project Title", "MIS Program", "Location", "Capability Sponsor", "Must Fund", "No Earlier", "Stability", "Current", "Cost")
  #   #   # rownames(tableSolution) <- projectNumber ## JGD 20190221 commented out, added as first column of table
  #   #   
  #   #   output$fullSolution <- renderDataTable({
  #   #     datatable(tableSolution, options = list(
  #   #       order = list(9, 'desc'),  ## 02/22/19 MMZ  changed from 9 to 10 
  #   #       pageLength = 20,
  #   #       initComplete = JS( #Change table header background color and font color
  #   #         "function(settings, json) {",
  #   #         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #   #         "}")
  #   #     ),
  #   #     rownames = FALSE, # JGD 20190221 added
  #   #     class = "compact",
  #   #     caption = "MILCON Projects"
  #   #     ) %>% formatCurrency(columns = 'Cost', digits = 0) %>% formatStyle(columns = 'Current', backgroundColor = "khaki")
  #   #   })
  #   #   
  #   #   #Calculate Summary Statistics
  #   #   projectsAdded <- 0
  #   #   for (i in 1:m){
  #   #     if ((tableSolution[i,8] == "-----") && (tableSolution[i,9] != "-----")){  ## 02/22/19 MMZ  edited 7->8 8->9 
  #   #       projectsAdded = projectsAdded + 1
  #   #     }
  #   #   }
  #   #   projectsDropped <- 0
  #   #   for (i in 1:m){
  #   #     if ((tableSolution[i,8] != "-----") && (tableSolution[i,9] == "-----")){ ## 02/22/19 MMZ  edited 7->8 8->9
  #   #       projectsDropped = projectsDropped + 1
  #   #     }
  #   #   }
  #   #   
  #   #   
  #   #   # Count the number of projects moved.
  #   #   # Must convert to numeric since the columns have different factors.
  #   #   projectsMoved <- 0
  #   #   for (i in 1:m){
  #   #     if (tableSolution[i,8] != "-----"){
  #   #       a <- as.character(tableSolution[[i,8]])
  #   #       b <- as.character(tableSolution[[i,9]])
  #   #       if(a != b){
  #   #         projectsMoved = projectsMoved + 1 
  #   #       }
  #   #     }
  #   #   }
  #   #   
  #   #   output$summaryProjectsAdded <- renderPrint({
  #   #     cat("Number of new projects added:",  projectsAdded)
  #   #   })
  #   #   
  #   #   output$summaryProjectsDropped <- renderPrint({
  #   #     cat("Number of", paste("POM",y-1), "projects dropped:",  projectsDropped)
  #   #   })
  #   #   
  #   #   output$summaryProjectsMoved <- renderPrint({
  #   #     cat("Number of", paste("POM",y-1), "projects moved:",  projectsMoved)
  #   #   })
  #   #   
  #   #   plotData1 <- data.frame(misProgram, pomSponsor, costVector, yearColumn)
  #   #   colnames(plotData1) <- c("MIS_Program","Capability_Sponsor", "Cost", "Current")
  #   #   
  #   #   geomplotData <- plotData1[plotData1[,4] != "-----",] # Remove rows with no costs
  #   #   
  #   #   
  #   #   output$plot1 <- renderPlot({  
  #   #     ggplot(geomplotData, aes(x = Current, y = Cost, fill = Capability_Sponsor))+ 
  #   #       ggtitle("Total MILCON Costs by Fiscal Year")+
  #   #       geom_bar(stat="identity", position = "stack")+scale_y_continuous(labels = dollar)
  #   #   })
  #   #   
  #   #   
  #   #   output$plot2 <- renderPlot({      
  #   #     ggplot(geomplotData, aes(x = Capability_Sponsor, y = Cost, fill = MIS_Program))+
  #   #       ggtitle("Total MILCON Costs (PA) by Capability Sponsor")+
  #   #       geom_bar(stat = "identity", position = "stack")+scale_y_continuous(labels = dollar)+coord_flip()
  #   #   })
  #   #   
  #   #   fundedCount <- nrow(geomplotData)
  #   #   countColumn <- matrix(1,fundedCount,1)
  #   #   geomplotData2 <- data.frame(geomplotData, countColumn) # add count column
  #   #   colnames(geomplotData2) <- c("MIS_Program","Capability_Sponsor", "Cost", "Current", "Count")
  #   #   
  #   #   
  #   #   
  #   #   geomplotData3 <- ddply(geomplotData2, c("Capability_Sponsor", "Current"), .drop = FALSE, summarise, Count = sum(Count))
  #   #   geomplotData4 <- geomplotData3[geomplotData3[,2] != "-----",] 
  #   #   
  #   #   output$plot3 <- renderPlot({ 
  #   #     ggplot(geomplotData4, aes(x = Capability_Sponsor, y = Count, fill = Current))+
  #   #       ggtitle("Funded MILCON Projects by Capability Sponsor and FY")+
  #   #       geom_bar(stat = "identity", position = "dodge")
  #   #   })
  #   #   
  #   #   cropSolution <- formatSolution[,5:10]# Place costs in Year columns  ##MMZ 02/21/19 edited 
  #   #   costbyYear <- matrix(0,m,p)
  #   #   for (i in 1:m){
  #   #     for (j in 1:p){
  #   #       if (cropSolution[i,j]==1){ 
  #   #         costbyYear[i,j] = cropSolution[i,p+1]
  #   #       }
  #   #     }
  #   #   }  
  #   #   
  #   #   costSumData <- data.frame(pomSponsor, costbyYear)
  #   #   colnames(costSumData) <- c("Capability_Sponsor",paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4))
  #   #   costSumData2 <- costSumData %>%
  #   #     group_by(Capability_Sponsor) %>%
  #   #     summarize_all(funs(sum))
  #   #   yearlyCosts <- costSumData2[,2:6]
  #   #   sponsors <- as.matrix(costSumData2[,1],6,1)
  #   #   pomSponsorSum <- rowSums(costSumData2[,2:6])
  #   #   costTable <- data.frame(yearlyCosts, pomSponsorSum)
  #   #   colnames(costTable) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
  #   #   rownames(costTable) <- sponsors
  #   #   
  #   #   output$table2 <- renderDataTable({
  #   #     datatable(costTable, options = list(
  #   #       order = list(6, 'desc')), 
  #   #       # class = "compact",
  #   #       caption = "MILCON Costs (PA) by Capability Sponsor"
  #   #     ) %>% formatCurrency(columns = c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4),'FYDP'), digits = 0)
  #   #   })
  #   #   
  #   #   sumYearlyCosts <- colSums(yearlyCosts)
  #   #   colnames(sumYearlyCosts)=NULL
  #   #   rownames(sumYearlyCosts)=NULL
  #   #   sumYearlyCosts2 <- data.frame(t(sumYearlyCosts))
  #   #   sumFYDP <- sum(pomSponsorSum)
  #   #   sumCostTable <- data.frame(sumYearlyCosts2, sumFYDP)
  #   #   colnames(sumCostTable) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
  #   #   rownames(sumCostTable) <- "PA Cost"
  #   #   
  #   #   # Calculate full costs for solution
  #   #   vectorSolution <- as.vector(t(exportSolution))
  #   #   fullCostMatrix_temp <- matrix(0,p,n)
  #   #   costLookup <- M5_LHS
  #   #   for (i in 1:p){
  #   #     for (j in 1:n){
  #   #       fullCostMatrix_temp[i,j] = vectorSolution[j] * costLookup[i,j]
  #   #     }
  #   #   }
  #   #   fullCostMatrix <- rowSums(fullCostMatrix_temp)
  #   #   sumFYDP2 <- sum(fullCostMatrix)
  #   #   sumFullCostMatrix <- data.frame(t(fullCostMatrix), sumFYDP2)
  #   #   colnames(sumFullCostMatrix) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
  #   #   rownames(sumFullCostMatrix) <- "Full Cost"
  #   #   
  #   #   sumBudget <- sum(budget)
  #   #   sumBudgetMatrix <- data.frame(t(budget), sumBudget)
  #   #   colnames(sumBudgetMatrix) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
  #   #   rownames(sumBudgetMatrix) <- "Available"
  #   #   
  #   #   slack <- (sumBudgetMatrix - sumFullCostMatrix)
  #   #   colnames(slack) <- c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4), "FYDP")
  #   #   rownames(slack) <- "Slack"
  #   #   
  #   #   tempList <- list(sumCostTable, sumFullCostMatrix, sumBudgetMatrix, slack)
  #   #   table3Matrix <- do.call(rbind,tempList)
  #   #   
  #   #   output$table3 <- renderDataTable({
  #   #     datatable(table3Matrix, 
  #   #               # class = "compact",
  #   #               caption = "MILCON Cost Summary"
  #   #     ) %>% formatCurrency(columns = c(paste("FY",y),paste("FY",y+1),paste("FY",y+2),paste("FY",y+3),paste("FY",y+4),'FYDP'), digits = 0)
  #   #   })
  #   #   
  #   #   # Reactive value for output dataset
  #   #   outputTable <- reactive(tableSolution)
  #   #   

  #         # # Create a value box for the number of projects inputted to the model
  output$scoreBox <- renderValueBox({
    valueBox(
      value = formatC(score, digits = 1, format = "d"),
      subtitle = "Probability of Win",
      icon = icon("building", lib = "font-awesome"),
      color = "blue"
    )}) 
  output$winBox <- renderValueBox({
      valueBox(
        value = formatC(win, digits = 1, format = "d"),
        subtitle = "Projection",
        icon = icon("building", lib = "font-awesome"),
        color = "yellow"
      )}) 
    
      # Downloadable csv of selected dataset ----
      output$download <- downloadHandler(
        # filename = function(){"thename.csv"},
        filename = function() {
          paste(input$outputFile, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(outputTable(), file, row.names = FALSE) # Changed to row.names = FALSE
        }
      )
  
    }) # end ObserveEvent solve
     
  }) # end observeEvent load
  
  # This command resets the App when the reset button is pressed.
  observeEvent(input$reset, {
    print("reseting")
    session$reload()
  })
  
  # # Stop the App from running after the browser is closed
  # session$onSessionEnded(function(){
  #   stopApp()
  # })
  
} # end Server

# shinyApp(ui = ui, server = server)