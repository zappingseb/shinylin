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

file_input = function(input,output){
  #Read in File data
  reactive_file1_header <- reactive(input$header_file1)
  reactive_file2_header <- reactive(input$header_file2)
  reactive_file1_row <- reactive(input$row_file1)
  reactive_file2_row <- reactive(input$row_file2)
  
  output$file1_table <- render_by_file(input,
                                       reactive_file1_header,
                                       reactive_file1_row)
  
  output$file2_table <- render_by_file(input,
                                       reactive_file2_header,
                                       reactive_file2_row,
                                       filenum=2)
}

text_paste_input = function(input,output){
  # Check how Excel Input works
  # This function can be used to copy paste an Excel table into
  # an R data table
  input_data_reactive_bigText <- reactive(input$bigText)
  
  input_header_bigText <- reactive(input$header_bigText)
  input_row_bigText <-reactive(input$row_bigText)
  
  input_data_reactive_bigText2 <- reactive(input$bigText2)
  input_header_bigText2 <- reactive(input$header_bigText2)
  input_row_bigText2 <-reactive(input$row_bigText2)
  
  output$bigTextOut <- render_by_textbox(
    input_data_reactive_bigText,input_header_bigText,input_row_bigText
  )
  output$bigTextOut2 <- render_by_textbox(
    input_data_reactive_bigText2,input_header_bigText2,input_row_bigText2
  )
}

manually_input = function(input,output){
  DF <- data.frame(Value = 1:3)
  DF2 <- DF
  # Read in all the reactive input from the server
  values <- reactiveValues()
  
  ## Observe the DF (dataframe) value
  # from the server table input$hot
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
  
  observe({
    if (!is.null(input$hot2)) {
      # Get it as the input
      DF2 = hot_to_r(input$hot2)
    } else {
      if (is.null(values[["DF2"]]))
        # Else get it from the DF variable
        DF2 <- DF2
      else
        DF2 <- values[["DF2"]]
    }
    values[["DF2"]] <- DF2
  })
  
  # Fill the RHandsonTable with the 
  # Data from the dataframe
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, stretchH = "all")
  })
  
  output$hot2 <- renderRHandsontable({
    DF_input <- values[["DF2"]]
    if (!is.null(DF_input))
      rhandsontable(DF_input, stretchH = "all")
  })
  
  ## Save 
  # On click on the input Save button a "input_table.rds" file is written
  # into the /data table
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path("./data", sprintf("%s.rds", "input_table")))
  })
  
  observeEvent(input$save2, {
    finalDF <- isolate(values[["DF2"]])
    saveRDS(finalDF, file=file.path("./data", sprintf("%s.rds", "input_table2")))
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

row_tester = function(row_info){
  if(row_info){
    return(1)
  }else{
    return(NULL)
  }
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
      
      row_nums <- row_tester(input$row_file1)
      
      # Read the inputfile and plot the ggplot item
      data <- read.csv(input_file$datapath,
                       header=input$header_file1,
                       row.names=row_nums 
                       )
      
      
      
      
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

render_by_file = function(input,
                          header_input,
                          row_input,filenum=1){

      renderRHandsontable({
        
        if(filenum==1){
          input_file <- input$file1
        }else{
          input_file <- input$file2
        }
        
        
        # Return an empty table of no file was provided
        if(is.null(input_file$datapath)){
          data <- data.frame(c("test"))
        # Else read the file
        }else{
          data <- read.csv(input_file$datapath,
                           header=header_input(),
                           row.names = row_tester(row_input()))
        }
        
        # Return an empty table of no file was provided
        if(!is.null(data)){
          rhandsontable(data, stretchH = "all")
        }else{
          rhandsontable(data.frame(error=c("no data")), stretchH = "all")
        }
        
      })

}

render_by_textbox = function(input_data_reactive,
                             header_info,
                             row_info){
  renderRHandsontable({
    
    if(row_info()){
      row_names_bigText = 1
    }else{
      row_names_bigText = NULL
    }
    
    if (!is.null(input_data_reactive()) && input_data_reactive()!=""){
      DF <- read.table(
        textConnection(input_data_reactive()),
        sep="\t", header=header_info(), row.names = row_names_bigText)
      rhandsontable(DF, stretchH = "all")
    }
  })
}



shinyServer(function(input, output) {
  
  # Run all functions needed for the Rhandson Table
  # Manually input management
  manually_input(input,output)
  
  # Run a function to create a linear ggplot and table
  my_linear_plot(input,output)
  
  # Handle file inputs in the file_input function
  file_input(input,output)
  
  # Handle everything in context with the copy paste
  # handlers
  text_paste_input(input,output)
  
  # Standard Example for a responsive Histogram
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = '#529f00', border = 'white')
    
  })
  
  
  
  
  
  output$downloadData <- downloadHandler(filename="test_data1.csv",content=function(file){
    file.copy("./data/01_test_data.csv",file)},contentType="text/csv")
  output$downloadData_two <- downloadHandler(filename="test_data2.csv",content=function(file){
    file.copy("./data/02_test_data.csv",file)},contentType="text/csv")

})
