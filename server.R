server <- function(input,output, session){
  
  output$out_data_import<-renderTable({
    if(is.null(input$data_import)){return()}
    else {
      input$data_import
    }
  })
  
  data_file <- reactive({
    if(is.null(input$data_import)){return()}
    else{
      file_spec<-input$data_import
      aa<-read.table(file_Spec$datapath, header = TRUE)
    }
  })
  
  #This function is repsonsible for loading in the selected file
  inputData_temp <- reactive({
    infile <- input$data_import
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath,
             as.is = T,
             skip = 0,
             header = T,
             na.strings=c("","NA")) ## replace blanks with na
  })

  print("Does this thing work?")
  

  
  observeEvent(input$train,{
    
    print("test")
    
    # if (is.null(input$import)){
    #   showNotification('Please upload CSV File', type = 'error')
    # } else {
    #   
      
      withProgress(message = "Loading Parameters", value = 0, {
        
        # # Convert multiplier values to numeric
        # K_Months <- input$contractMonths
        # print(K_Months)
        # print("test2")
        
        inputData <<- inputData_temp()
        
        inputData[is.na(inputData)] <<- 0 ## Replace na with 0

        m <<- nrow(inputData) # Number of opportunities
        print(m)

        output$opportunities <- renderPrint({
          cat("Number of Opportunities (m):", m)
        }) 
        
        # Create an info box for the number of projects input to the model
        output$opportunityBox <- renderInfoBox({
          infoBox("Opportunities", paste0(m), icon = icon("list", lib = "font-awesome"),
                  color = "blue")
        })
        
        # Create a value box for the number of Opportunities input to the model
        output$opportunityBox <- renderValueBox({
          valueBox(
            value = formatC(m, digits = 1, format = "d"),
            subtitle = "Opportunities",
            icon = icon("list", lib = "font-awesome"),
            color = "light-blue")
        })
        
        # Remove these columns
        df <-  inputData[ , !names(inputData) %in%
                            c("ID","NAME","GovWin.ID","Capture.Lead","Include.in.Moneyball","Expected.RFP.Release",
                              "Prime.Contractor","Must.Win","Status","Actual.RFP.Release","Award.Date",
                              "BD_Sales.Lead", "Contract.Vehicle","Contracts.POC","Estimated.Start.Date","Modified.By",
                              "Moneyball","NAICS", "Prime.POC","Prime.POC.Email","RFI.or.IWP","Proposal.or.EWP.Due",
                              "Proposals.POC","TA.completed","Submission.Date","Estimated.Completion.Date","NAME.1",
                              "Solicitation..","Solicitation.Date","Technical.POC")]
        
        print(is.data.frame(df))

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

        n <- nrow(df) # Number of reduced opportunities
        print(n)
        
        # Split data in training and test sets
        set.seed(509)
        split <- initial_split(df, prop = 0.8, strata = Result)
        train <- split %>% training()
        test <- split %>% testing()
        
        # Model uses the variable from "fit.reduced" in glm_model script.
        fit.reduced <- glm2(Result ~ Portfolio + Our.Value + Contract.Type + Customer.Type + Contract.Period.Months, data = df, family=binomial())
        model<- fit.reduced
        
        output$rocPlot <- renderPlot({
        # Plot the Receiver Operating Characteristic (ROC) curve
        predicted_probs <- predict(model, type = "response")
        actual_outcomes <- df$Result
        roc_obj <- roc(actual_outcomes, predicted_probs)
        auc_value <- auc(roc_obj)
        
        roc_plot <- plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
        
        # # Display the plot
        # print(roc_plot)
        })
        # 
        # # Create a value box for the number of Opportunities input to the model
        output$reducedOpportunityBox <- renderValueBox({
          valueBox(
            value = formatC(n, digits = 1, format = "d"),
            subtitle = "Modeled Opportunities",
            icon = icon("list", lib = "font-awesome"),
            color = "blue")
        })


      }) # end of Progress bar
      # } # end of If Statement
      output$trainedCheckmark <- renderImage({
        list(src = "./www/tick.png",
             contentType = "image/png",
             height = 20,
             alt = "tick")
      }, deleteFile = FALSE)
      
      output$trainedMSG <- renderText({"Model trained with CSV data."})
      
      observeEvent(input$load,{
      
        output$loadedCheckmark <- renderImage({
          list(src = "./www/tick.png",
               contentType = "image/png",
               height = 20,
               alt = "tick")
        }, deleteFile = FALSE)  
        
      output$loadedMSG <- renderText({"Parameter load complete."})
      
      #define new observation
      newdata = data.frame(Portfolio = input$portfolio, Our.Value = input$ourValue, Contract.Type = input$contractType, Customer.Type = input$customerType, Contract.Period.Months = input$contractMonths, CPEG = input$cpeg)
      print(newdata)
      
      insertUI("#load","afterEnd", actionButton("solve", "Solve"))
      
      output$solvedCheckmark <- renderImage({
        list(src = "./www/wait.png",
             contentType = "image/png",
             height = 20,
             alt = "tick")
      }, deleteFile = FALSE)
      
      output$solvedMSG <- renderText({"Press 'Solve' and please wait...."})
      
      observeEvent(input$solve, {
        
        # newdata = test
        
        ### Make a new prediction ####
  
        # Turn off scientific notation
        options(scipen = 999)
        
        #use model to predict value of a win
        prediction <- predict.glm(model, newdata, type="response")        
 
        # Create a value box for predicted model value
        output$scoreBox <- renderValueBox({
          valueBox(
            value = format(prediction, digits = 3, format = "d"),
            subtitle = "Probability of Win",
            icon = icon("brain", lib = "font-awesome"),
            color = "blue"
          )}) 
        
        # outcome <- NULL
        if(prediction > 0.5){
          # Create a value box for the number of projects inputted to the model
          output$winBox <- renderInfoBox({
            infoBox("Predicted Outcome: Win", icon = icon("thumbs-up", lib = "glyphicon"),
                    color = "green", fill = TRUE
            )}) 
        } else{
          output$winBox <- renderInfoBox({
            infoBox("Predicted Outcome: Loss", icon = icon("thumbs-down", lib = "glyphicon"),
                    color = "red", fill = TRUE
            )}) 
        }
        

        
      }) # End Solve
    }) # End observeEvent load
  }) # End Train
  
  # This command resets the App when the reset button is pressed.
  observeEvent(input$reset, {
    print("reseting")
    session$reload()
  })
      
      # # # Stop the App from running after the browser is closed
      # session$onSessionEnded(function(){
      #   stopApp()
      # })
} # End Server
