library(shiny)
library(DT)
ui <- fluidPage(
  
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
    
    tabsetPanel(
      tabPanel("Question 1", 
               fluidRow(column(12, htmlOutput("textblocknum"))),
               fluidRow(column(12, DT::dataTableOutput("numTable"))
               )),
      tabPanel("Question 2",
               fluidRow(column(12, htmlOutput("textblockcat"))),
               fluidRow(column(12, DT::dataTableOutput("catTable"))
               )
    ))
    # Display textblock 1 ----
    #verbatimTextOutput("textblock1"),
    
    
    # Display sample data table ----
    # tableOutput("table1")
    
    
    
    ) #end of mainPanel
  
  ) #end of sidebarlayout
  
) #end of fluidpage

server <- function(input, output){
  
  # Get new data set upon clicking on the activeButton
  # by getting a new random seed
  # id <- eventReactive(input$newdata, {
  #   sample(1:10^6,1)
  # })
  
  catData <- reactive({
    
    if (is.na(input$id)){
      userdata <- data.frame(Individual=NA, Outcome=NA)
      return(userdata)
    }else{
    # Plant the random number seed
    set.seed(input$id) 
    
    n = sample(30:40, 1)
    
    A_one <- 1:n
    group_one <- rep(c('Red'), times = 47)
    group_two <- rep(c('Blue'), times = 53)
    population <- c(group_one, group_two)
    A_two <- sample(population, n)
    
    

    userdata <- data.frame(A_one, A_two)
    col_headings <- c('Individual', 'Outcome')
    colnames(userdata) <- col_headings
    
    
    #row.names(userdata) <- c('a','b','c','d','e','f','g')
    #userdata <<- cbind(names = row.names(userdata), userdata)
    #rownames(userdata) <- c('a','b','c','d','e','f','g')
    
    return(userdata)
    }
  }) # end reactive
  
  numData <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      userdata <- data.frame(Individual=NA, Scores=NA, Notes=NA)
      return(userdata)
    }else{
    set.seed(input$id) 
    
    n = sample(25:30, 1)
    
    A_one <- 1:n
    A_two <- sample(rnorm(1000, 75, 5), n)
    A_three <- A_one %% 2
    
    userdata <- data.frame(A_one, A_two, A_three)
    col_headings <- c('Individual', 'Scores', 'Notes')
    colnames(userdata) <- col_headings
    
    
    #row.names(userdata) <- c('a','b','c','d','e','f','g')
    #userdata <<- cbind(names = row.names(userdata), userdata)
    #rownames(userdata) <- c('a','b','c','d','e','f','g')
    
    return(userdata)
    }
    
  }) # end reactive
  

  # Output: Textblock 1 ----
  output$textblocknum <- renderText({
    paste("Data set id:",
          "<font color=\"#FF0000\"><b>", 
            input$id,
          "</b></font>",
          "<ul>",
            "<li>Each row represents area readings from a pair of photos taken by a gap of 10 minutes.</li>",
            "<li>Seven pairs of photos are taken over time.</li>",
            "<li>All numbers in the table are sizes of the spill measured from photos evidences.</li>",
          "</ul>")
    })
  
  output$textblockcat <- renderText({
    paste("Data set id:",
          "<font color=\"#FF0000\"><b>", 
          input$id,
          "</b></font>",
          "<ul>",
          "<li>Each row represents area readings from a pair of photos taken by a gap of 10 minutes.</li>",
          "<li>Seven pairs of photos are taken over time.</li>",
          "<li>All numbers in the table are sizes of the spill measured from photos evidences.</li>",
          "</ul>")
  })
  
  
  # Output: Sample data table ----
  output$catTable <- DT::renderDataTable({
    datatable(catData(), rownames = FALSE, options = list(
      pageLength = 50,
      dom = "t",
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:1))))%>%
      formatStyle("Outcome", backgroundColor=styleEqual(c("Red","Blue"),c('orange','gray')))
  },striped = TRUE,colnames = TRUE)
  
  # Output: Sample data table ----
  output$numTable <- DT::renderDataTable({
    datatable(numData(), rownames = FALSE, options = list(
      pageLength = 50,
      dom = "t",
      columnDefs = list(list(className ='dt-center', targets = 0:1), 
                        list(visible=FALSE, targets=2))))%>%
      formatStyle("Notes", backgroundColor=styleEqual(c(1,0),c('orange','gray')), target = "row")%>%
      formatRound(columns=c('Scores'), digits=2)
  },striped = TRUE,colnames = TRUE)
  
} #end server

shinyApp(ui = ui, server = server)