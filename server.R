##########################################
##### Event Aggregation App - server.R ###
##########################################

require(compiler)
enableJIT(3)
library(shiny) 

# Code here gets executed at App launch time
newdata <- data.frame()
perils  <- read.csv("perils.csv", stringsAsFactors = FALSE)
rp_idx  <- c(1,2,5,10,20,40,50,100,200,250,400,500,1000,2000)
rp      <- c(10000,5000,2000,1000,500,250,200,100,50,40,25,20,10,5)

shinyServer(function(input, output) {
  
  # Read input file
  readFile <- Reactive({
    infilename <- input$inFileName$datapath
    # TODO: count the columns and throw an error message if the days are not included.
    # Ignore the Peril Region for the time being
    data <- read.table(infilename, sep = "\t", header = FALSE, skip = 1, col.names = c("EventID","Year","Day","Loss"))
    data
  })
  # Reactive function that returns status to proceed with reading the file
  inputReady <- function() {
    ready <- FALSE
    if (is.null(input$inFileName) == FALSE &
        is.null(input$InceptDate) == FALSE &
        is.null(input$ModelIDs)   == FALSE &
        is.null(input$Days)       == FALSE &
        is.null(input$LossLimit)  == FALSE )
      ready <- TRUE
    return(ready)
  }
  
  # Reads the file and does the event aggregation
  processFile <- function() {
    
    # Just in case if they enter a negative Loss Limit...
    LossLimit <- as.numeric(abs(input$LossLimit))
    if (is.na(LossLimit))
      LossLimit = Inf
    
    # Compute number of days in the year
    IncepDate = format(input$InceptDate,"%j")
    IncepDate = as.numeric(IncepDate)
    
    #print("Reading input file...")
    infilename <- input$inFileName$datapath
    data       <- read.table(infilename, sep = "\t", header = FALSE, skip = 1, col.names = c("EventID","Year","Day","Loss"))
    
    # Add a Model ID column
    data$ModelID <- substr(data$EventID,1,2)
    #models       <- unique(data$ModelID)
    modelsSelect <- perils$ID[perils$DESCRIPTION %in% input$ModelIDs]
    #modelsSelect <- unlist(strsplit(input$ModelIDs,split = ","))
    
    # Change the event dates based on InceptionDate
    data$NewDay <- data$Day
    data$NewDay[data$NewDay < IncepDate] <-
      data$NewDay[data$NewDay < IncepDate] + 365
    
    # Sort new dataframe on NewDay
    data <- data[order(data$Year, data$NewDay, decreasing = FALSE),]
    
    # Calculate the new losses with aggregate hourly clauses
    data$NewLoss <- 0
    
    # Start with all the Events with Perils NOT subject to aggregation
    newdata         <- data[!data$ModelID %in% modelsSelect,]
    newdata$NewLoss <- newdata$Loss
    
    withProgress(message = 'Aggregating events', value = 0, {
      
      ntotal          <- unique(data$Year)
      delta_threshold <- 250
      threshold       <- 1
      
      for (ii in unique(data$Year)) {
        
        thisYear <- data[data$Year == ii & data$ModelID %in% modelsSelect,]  # select what to aggregate
        numEvents <- nrow(thisYear)
        
        if ( numEvents > 1) {
          
          # Pick first event in year as starting point...
          idx     <- 1
          day     <- thisYear$NewDay[1]
          loss    <- thisYear$Loss[1]
          # thisYear$NewLoss[1] <- 0
          
          for (jj in 2:numEvents) {
            delta <- thisYear$NewDay[jj] - day
            
            if (delta <= input$Days) {
              #
              # Consecutive Events can be aggregated
              # Check on size of loss
              #
              newloss <- loss + thisYear$Loss[jj]
              if (newloss <= LossLimit) {
                # Aggregated loss is within the Limit
                thisYear$NewLoss[idx] <- newloss
                loss                  <- newloss
                thisYear$NewLoss[jj]  <- 0
              } else {
                # Aggregated loss exceeds the Limit, don't aggregate
                # and reset the day and loss
                thisYear$NewLoss[idx] <- loss
                idx                   <- jj
                day                   <- thisYear$NewDay[jj]
                loss                  <- thisYear$Loss[jj]
              }
            } else {
              #
              # Consecutive Events can not be agggregated
              # Store previous loss and reset starting date
              #
              thisYear$NewLoss[idx] <- loss # store new loss for old pivot
              idx  <- jj
              day  <- thisYear$NewDay[jj]
              loss <- thisYear$Loss[jj]
              thisYear$NewLoss[jj] <- thisYear$Loss[jj] # store new loss for new event
            }
          }  # End of ii Loop 
        } else {
          # Year with only 1 Event...
          thisYear$NewLoss <- thisYear$Loss
        }
        
        newdata <- rbind(newdata,thisYear)
        
        # Perhaps we should increment every 100 years or so
        if( ii >= threshold ) {
          incProgress(delta_threshold/ntotal, detail = paste('Year ',ii))
          #setProgress(ii/ntotal, detail = paste('Year...',ii))
          threshold <- threshold + delta_threshold
        }
      }
      
    }) # End of withProgress()
    
    # Sort back on original [Event Year] and [Event Day]
    newdata <- newdata[order(newdata$Year, newdata$Day, decreasing = FALSE),]

    #newdata <- newdata[newdata$NewLoss > 0,]
    return(newdata)
    
  }
  
  models <- reactive({
    data         <- read.table(input$inFileName$datapath, sep = "\t", header = FALSE, skip = 1, col.names = c("EventID","Year","Day","Loss"))
    data$ModelID <- substr(data$EventID,1,2)
    models       <- unique(data$ModelID)
    models
  })
  
  output$checkGroup <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    }
    choices <- perils$DESCRIPTION[perils$ID %in% models()]
    checkboxGroupInput(inputId = "ModelIDs",
                       label = "Perils",
                       choices = choices,
                       selected = choices)
  }) 
  
  output$InceptDate <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    } 
    dateInput(
      inputId = "InceptDate",
      label = "Inception Date",
      value = "2016-01-01",
      format = "dd-mm-yyyy",
      startview = "month"
    )
  })
  
  output$Days <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    } 
    numericInput(
      inputId = "Days",
      label = "Number of Days",
      value = "5",
      min = 1,
      max = 365,
      step = 1
    )
  })
  
  output$LossLimit <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    } 
    numericInput(
      inputId = "LossLimit",
      label = "Program Exit Point",
      value = 1e9,
      min = 0,
      max = Inf
    )
  })
  
  output$LossLimitNote <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    }
    helpText("Note: Limit should be in same Currency and Units as export file")
  })
  
  output$submitButton <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    }
    #submitButton("Apply Event Definition")
    actionButton(inputId = "go",label = "Apply Event Definition", icon = icon("default"))
  })
  
  output$download <- renderUI({
    if (is.null(input$inFileName$datapath) ||
        !nzchar(input$inFileName$datapath)) {
      return(invisible())
    }
    if (is.null(newdata())) return(invisible())
    downloadButton(outputId = "downloadData", label = "Download")
  })
  
  # Store the aggregated events in newdata()
  newdata <- eventReactive(input$go, {
    if( inputReady() )
      processFile()
  })
  
  output$tableOutput <- renderDataTable({
      aep_old <- sort( tapply(X = newdata()$Loss,INDEX = newdata()$Year,FUN = sum), decreasing=TRUE )
      oep_old <- sort( tapply(X = newdata()$Loss,INDEX = newdata()$Year,FUN = max), decreasing=TRUE )
      aep_new <- sort( tapply(X = newdata()$NewLoss,INDEX = newdata()$Year,FUN = sum), decreasing=TRUE )
      oep_new <- sort( tapply(X = newdata()$NewLoss,INDEX = newdata()$Year,FUN = max), decreasing=TRUE )
      
      aep_old   <- aep_old[ rp_idx ]
      oep_old   <- oep_old[ rp_idx ]
      aep_new   <- aep_new[ rp_idx ]
      oep_new   <- oep_new[ rp_idx ]
      oep_delta <- (oep_new/oep_old-1)*100
      
      aep_old   <- format( aep_old,   digits=0, trim=FALSE, scientific = FALSE, big.mark ="," )
      aep_new   <- format( aep_new,   digits=0, trim=FALSE, scientific = FALSE, big.mark ="," )
      oep_old   <- format( oep_old,   digits=0, trim=FALSE, scientific = FALSE, big.mark ="," )
      oep_new   <- format( oep_new,   digits=0, trim=FALSE, scientific = FALSE, big.mark ="," )
      oep_delta <- format( oep_delta, digits=1, trim=FALSE, scientific = FALSE )
      table     <- data.frame( rp, aep_new, aep_old, oep_new, oep_old, oep_delta )
      colnames(table) <- c("Return Period","AEP","AEP (Orig)","OEP","OEP (Orig)", "OEP diff (%)")
      table
    }
    ,options=list(paging=FALSE,searching = FALSE,searchable = FALSE)
  )
  
  output$downloadData <- downloadHandler(filename <- function() {
    modelsSelect <- perils$ID[perils$DESCRIPTION %in% input$ModelIDs]
    paste(sub("[.][^.]*$", "", input$inFileName$name, perl=TRUE), "_D", input$Days, "_L", input$LossLimit, "_M", paste(modelsSelect,collapse="-"), ".txt", sep = "")
  },
  content <- function(file) {
      write.table(newdata()[newdata()$NewLoss > 0,c("EventID","NewLoss")], file=file, sep="\t", append=FALSE, eol="\r\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  })
  
})