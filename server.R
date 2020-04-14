library(shiny)
library(DT)


server <- function(input, output){
  
  # Get new data set upon clicking on the activeButton
  # by getting a new random seed
  # id <- eventReactive(input$newdata, {
  #   sample(1:10^6,1)
  # })
  
  cl <- reactive({
    if (is.na(input$id)){
      return(NA)
    }else{
      # Plant the random number seed
      set.seed(input$id)
      cl <- sample(88:96, 1)
      return(cl)
    }
  })
  
  cat <- reactive({
    
    if (is.na(input$id)){
      return(NULL)
    }else{
      # Plant the random number seed
      set.seed(input$id) 
      
      cl <- sample(88:96, 1)
      n <- sample(30:40, 1)
      
      group_one <- rep(c('Red'), times = 47)
      group_two <- rep(c('Blue'), times = 53)
      population <- c(group_one, group_two)
      a <- sample(population, n)
      A <- as.data.frame(table(a))
      names(A) <- c('color', 'freq')
      x <- with(A[which(A$color == 'Red'),], freq)
      return(c(prop.test(x, n, conf.level = cl/100), x, n))
    }
  }) # end reactive
  
  mu <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id) 
      mu <- round(runif(1, 70, 80),2)
    }})
    
  num <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id) 
      mu <- round(runif(1, 70, 80),2)
      n <- sample(25:30, 1)
      
      A_one <- 1:n
      A_two <- sample(rnorm(1000, 75, 5), n)
      A_three <- A_one %% 2
      
      userdata <- data.frame(A_one, A_two, A_three)
      col_headings <- c('Individual', 'Scores', 'Notes')
      colnames(userdata) <- col_headings

      return(c(t.test(A_two, mu = mu), sd(A_two)))
    }
    
  }) # end reactive
  
  
  # Output: Textblock 1 ----
  output$textblock <- renderText({
    HTML(
    paste("<font size = 4pt>Data set id:",
          "<font color=\"#FF0000\"><b>", 
          input$id,
          "</b></font>",
          "<ul>",
           "<li>Question 1:",
               "<ul>",
                   "<li>Sample mean: ", 
                        "<font color=\"#FF0000\"><b>",
                         sprintf('%.3f', num()$estimate),
                        "</b></font>",
                 "</li>",
                  "<li>Sample standard deviation: ", 
                          "<font color=\"#FF0000\"><b>",
                        sprintf('%.3f', num()[10]),
                         "</b></font>",
                  "</li>",
                  # "<li>",
                  #       sprintf('%.2f confidence interval:', attr(num()$conf.int, "conf.level")),
                  #       "<font color=\"#FF0000\"><b>",
                  #       sprintf('(%.3f, %.3f)', num()$conf.int[1], num()$conf.int[2]),
                  #       "</b></font>",
                  # "</li>",
                  "<li>Null value:", mu(), "</li>",
                  "<li>DF:<font color=\"#FF0000\"><b>", sprintf('%.0f', num()$parameter), "</b></font></li>",
                  "<li>Test statistic:<font color=\"#FF0000\"><b>", sprintf('%.3f', num()$statistic), "</b></font></li>",
                  "<li>Two-sided P-value:<font color=\"#FF0000\"><b>", sprintf('%.3f', num()$p.value), "</b></font></li>",
                  "</ul>",
          "</li>",
          "<li>Question 2:",
          "<ul>",
          "<li>Sample proportion: ", 
          "<font color=\"#FF0000\"><b>",
          sprintf('%.0f / %.0f = %.3f', cat()[10], cat()[11], cat()$estimate),
          "</b></font>",
          "</li>",
          "<li>z_&alpha;/2 = ", qnorm((1-cl()/100)/2, lower.tail = FALSE), "</li>",
          "<li>",
          sprintf('%.2f confidence interval:', attr(cat()$conf.int, "conf.level")),
          "<font color=\"#FF0000\"><b>",
          sprintf('(%.3f, %.3f)', cat()$conf.int[1], cat()$conf.int[2]),
          "</b></font>",
          "</li>",
          "</ul>",
          "</li>",
          "</ul></font>"))
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