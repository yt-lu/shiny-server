library(shiny)
library(DT)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    * {
                    font-family: Palatino,garamond,serif;
                    font-weight: 500;
                    line-height: 1.2;
                    #color: #000000;
                    }
                    "))
    ),  
  
  # App title 
  titlePanel(title="STA126 Test 4 Data Sets (Spring 2020)"),
  
  # Sidebar layout 
  sidebarLayout(
    
    # Sidebar objects
    sidebarPanel(
      
      numericInput(inputId = "id",
                   "Enter the last four of your MU ID", 
                   value=NULL),  
      # Object
      #actionButton(inputId = "newdata", "Get a Data Set"),
      
      # Sidebar width can not exceed 12, default 4.
      width = 4
    ), # end of sidebar panel
    
    # Main panel----
    mainPanel(
      
      htmlOutput("textblock")

    ) #end of mainPanel
    
  ) #end of sidebarlayout
  
) #end of fluidpage
