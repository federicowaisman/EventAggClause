####################################
### Event Aggregation App - ui.R ###
####################################

library(shiny) 

shinyUI(
  fluidPage(
    titlePanel("Event Aggregation Clause"),
    
    sidebarLayout(

      sidebarPanel(
        
        fileInput(
          inputId = "inFileName",
          label = "Select ARPS Export Text File",
          multiple = FALSE,
          accept = NULL
        ),
        uiOutput(outputId = "InceptDate"),
        uiOutput(outputId = "Days"),
        uiOutput(outputId = "LossLimit"),
        uiOutput(outputId = "LossLimitNote"),
        uiOutput(outputId = "checkGroup"),
        uiOutput(outputId = "submitButton")
      ),
      
      mainPanel(
        dataTableOutput("tableOutput"),
        uiOutput(outputId = "download") 
      )
    )  
  )
)