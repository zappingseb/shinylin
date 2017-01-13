#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
if(!require("lme4")){
  install.packages("lme4")
  library(lme4)
}
if(!require("mcr")){
  install.packages("mcr")
  library(mcr)
}

library(stringr)
rhandson_plugin = function(input,output){
  DF <- data.frame(Value = 1:10, Status = TRUE, Name = LETTERS[1:10],
                   Date = seq(from = Sys.Date(), by = "days", length.out = 10),
                   stringsAsFactors = FALSE)
  
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
  })
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path("./data", sprintf("%s.rds", "input_table")))
  })
  
}

# Define server logic required to draw a histogram
my_df = function(bins){
  #' Function to Create a data table that is exactly correllated by 1:2
  #' @param bins (integer): Telling how many entries the data table shall have
  #' @return out (data.frame): A dataframe with exactly two columns x and y
  x = c(1:(1*bins))
  y = seq(2,2*bins,2)
  return(data.frame(x,y))
}

shinyServer(function(input, output) {
  
  rhandson_plugin(input,output)
  
  # Example for responsive UI
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = 1,
           "pressure" = 2,
           "cars" = 3)
  })
  
  output$nrows <- reactive({
    nrow(datasetInput())
  })
  # - - END OF EXAMPLE
  
  response <- reactive(my_df(input$bins))
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$myplot <- renderPlot({

    # Create a green line of the data frame
    # created with my_df and the number of bins
    # set in the input
    input_file <- input$file1
    
    if(is.null(input_file)){
      ggplot(data=response(),aes(x=x,y=y))+
        geom_line(color="green")
    }else{
      data <- read.csv(input_file$datapath,header=input$header)
      if(all(colnames(data)==c("x","y"))){
        ggplot(data=data,aes(x=x,y=y))+
          geom_line(color="green")
      }
    }

    
  })
  
  output$table <- renderTable(
    response()
  )
  
  # Rende a output table including the generation of a linear model
  output$linreg <- renderTable({
    input_df <- response()
    model = lm(y ~ x,input_df)
    out<- data.frame(model$coefficients)
    rownames(out)<-c("intercept","slope")
    out[,2]<-out[,1]
    out[,1]<-rownames(out)
    colnames(out)<-c("","value")
    out
  })
  
  # Check how Excel Input works
  # This function can be used to copy paste an Excel table into
  # an R data table
  dataframe <- reactive(input$bigText)
  
  output$bigTextOut <- renderTable({
    if(dataframe()!=""){
      x <- read.table(textConnection(dataframe()),header=T,sep = "\t")
    }else{
      x<-data.frame("")
    }

  })
  
})
