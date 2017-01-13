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
  DF <- data.frame(Value = 1:3)
  # Read in all the reactive input from the server
  values <- reactiveValues()
  
  ## Observe the DF (dataframe) value
  # from the server
  observe({
    if (!is.null(input$hot)) {
      # Get it as the input
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        # Else get it from the DF variable
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  # Fill the RHandsonTable with the 
  # Data from the dataframe
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all")
  })
  
  ## Save 
  # On click on the input Save button a "input_table.rds" file is written
  # into the /data table
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

my_linear_plot = function(input,output){
  #' Function to render a linear plot of value pairs due to the
  #' bins input string or a datafile
  # Read in the
  response <- reactive(my_df(input$bins))
  
  output$myplot <- renderPlot({
    
    # Create a green line of the data frame
    # created with my_df and the number of bins
    # set in the input
    
    # In case there is an input file use this inputfile
    input_file <- input$file1
    
    if(is.null(input_file)){
      ggplot(data=response(),aes(x=x,y=y))+
        geom_line(color="green")
    }else{
      
      # Read the inputfile and plot the ggplot item
      data <- read.csv(input_file$datapath,
                       header=input$header_file1)
      if(all(colnames(data)==c("x","y"))){
        ggplot(data=data,aes(x=x,y=y))+
          geom_line(color="green")
      }
    }
    
    
  })
  
  # output the table of the readin in the "table" item of the ui
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
}

shinyServer(function(input, output) {
  
  # Run all functions needed for the Rhandson Table
  rhandson_plugin(input,output)
  
  # Run a function to create a linear ggplot and table
  my_linear_plot(input,output)
  
  # Standard Example for a responsive Histogram
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = '#529f00', border = 'white')
    
  })
  
  
  
  # Check how Excel Input works
  # This function can be used to copy paste an Excel table into
  # an R data table
  input_data_reactive <- reactive(input$bigText)
  
  output$bigTextOut <- renderRHandsontable({
    
    if(input$row_bigText){
      row_names_bigText = 1
    }else{
      row_names_bigText = NULL
    }
    
    if (!is.null(input_data_reactive()) && input_data_reactive()!=""){
      DF <- read.table(
        textConnection(input_data_reactive()),
        sep="\t", header=input$header_bigText, row.names = row_names_bigText)
      rhandsontable(DF, stretchH = "all")
    }
  })
  
})
