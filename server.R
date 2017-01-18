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
library(data.table)
if(!require("lme4")){
  #install.packages("lme4")
  library(lme4)
}
if(!require("mcr")){
  #install.packages("mcr")
  library(mcr)
}
if(!require("grid")){
  #install.packages("grid")
  library(grid)
}
if(!require("gridExtra")){
  #install.packages("gridExtra")
  library(gridExtra)
}
if(!require("stringr")){
  #install.packages("stringr")
  library(stringr)
}

file_input = function(input,output){
  #Read in File data  checkboxes into reactive variables
  reactive_file1_header <- reactive(input$header_file1)
  reactive_file2_header <- reactive(input$header_file2)
  reactive_file1_row <- reactive(input$row_file1)
  reactive_file2_row <- reactive(input$row_file2)
  
  # Fill the file data tabels with the values from the
  # files
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
  DF <- data.frame(Value = 1.0:3.0)
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
      rhandsontable(DF, stretchH = "all",digits=8)%>% 
      hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
  })
  
  output$hot2 <- renderRHandsontable({
    DF_input <- values[["DF2"]]
    if (!is.null(DF_input))
      rhandsontable(DF_input, stretchH = "all",digits=8)%>% 
      hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
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
  #' Checks if the checkbox row_info is true
  #' or false
  #' 
  #' @return 1 if TRUE and NULL if FALSE
  if(row_info){
    return(1)
  }else{
    return(NULL)
  }
}

analysis_generate = function(values,
                          fieldnames,
                          analysis_fields,
                          output){
  # generate an empty dataset
  the_data<-NULL

  # Use the renderText function to render the error_message UI 
  # part. In case of any error it will be brought to the
  # error message window
  output$error_message <- renderText({
    
    withProgress(message = 'Creating Plots', value = 0,{
      if(!is.null(values[["DF"]]) && !is.null(values[["DF2"]])){
        
        
        error_counter <- 0
        
        if(dim(values[["DF"]])[2] == dim(values[["DF2"]])[2]){
          
          all_plots <- list()
          all_tables <- list()
          all_titels <- c()
          
          for(column_number in 1:dim(values[["DF"]])[2]){
            
            incProgress(1/dim(values[["DF"]])[2], detail = paste("Performing dataset", column_number))
            
            if(length(values[["DF"]][,column_number])==length(values[["DF2"]][,column_number]) &&
               length(values[["DF"]][,column_number])>2 &&
               length(data_delete_na(values[["DF"]][,column_number]))==length(data_delete_na(values[["DF2"]][,column_number]))
            ){
              
              # Get the plot title
              if(colnames(values[["DF"]])[column_number]==colnames(values[["DF2"]])[column_number] && 
                 colnames(values[["DF2"]])[column_number]!="Value"){
                title_of_plot <- colnames(values[["DF2"]])[column_number]
              }else{
                title_of_plot <- ""
              }
              # Append it to the row with all titles
              all_titels <- c(all_titels, title_of_plot)
              
              # CLEAN THE DATA FROM NAs
              the_data <- data_construct(values,column_number)
              
              # Generate the linear regression for the data and append the 
              # pretty output to the list of all outputs
              method <- analysis_fields[["method_for_regression"]]()
              if(method=="Passing-Bablok regression" && (dim(the_data)[1]<10)){
                render_confidence_intervals <- F
                
              }else{
                render_confidence_intervals <- analysis_fields[["confidence_intervals"]]()
                
              }
              
              regression_result <- regression_construct_table(the_data = the_data,
                                                              method_to_choose = method,
                                                              calculate_ci = render_confidence_intervals)
              
              all_tables[[column_number]] <- regression_pretty_output(regression_result[[1]])
              
              
              # Append the regression line data by using the
              # model generated by the mcreg function
              the_data <- data_add_regression(the_data,
                                              regression_result[[2]])
              
              # Append a plot to the list of all plots with the linear
              # regression included
              all_plots[[column_number]] <- plot_linreg(the_data,
                                                        fieldnames,
                                                        title_of_plot,
                                                        render_confidence_intervals)
              
            }else if(error_counter>0){
              return("Please make sure you have the same number of experiments for each dataset")
              plot_linreg(data.frame(),output)
              
            }else{
              error_counter=error_counter+1
              plot_linreg(data.frame(),output)
              return("Please make sure you have the same number of experiments for each dataset
                     and the number of experiments is greater 2")
            }
            
            }
          
          
          
          
          data_out <- regression_combine_pretty(all_tables,all_titels)
          colnames(data_out)<- c('Linear regression Intercept',
                                 'Linear regression Slope',
                                 'Deming regression Intercept',
                                 'Deming regression Slope',
                                 'Passing-Bablok regression Intercept',
                                 'Passing-Bablik regression slope',
                                 'Pearsson R2')         
          saveRDS(data_out,
                  file=file.path("./data", sprintf("%s.rds", "output_table")))
          output$linreg <- renderTable({data_out},
                                       include.rownames=T,
                                       digits=4 )
          
          tryCatch({output$myplot <- renderPlot({
            grid.arrange(grobs = all_plots,ncol=3)
          })},error=function(e){
            output$myplot <- renderPlot({plot_empty()})
          })
          
          ""
          
        }else if(error_counter>0){
          print("ERRR")
          "Please make sure you have the same number of experiments for each dataset"
          
          
        }else{
          plot_linreg(data.frame(c()),output)
          "Please provide two tables with the same number of columns (datasets)"
        }#wrong size dataset
        }#is null dataset
    })
    
    
  })

  
  
}
data_delete_na = function(the_data){
  return(the_data[!is.na(the_data)])
}

data_construct = function(values,column_number){
  #' Delte NAs from the input reactive values input
  #' in a specific column and return
  #' it as a dataframe
  
  the_data <- data.frame(x=values[["DF"]][,column_number],
                         y=values[["DF2"]][,column_number])
  colnames(the_data) <- c("x","y")
  return(the_data[!is.na(the_data[,1]),])
}

data_add_regression = function(the_data,
                               mcreg_model){
  #' Append regression data to measurement data
  #' 
  #' The mcreg_model is taken to calculate the regression
  #' line going through the data and the confidence
  #' interval. Both can be done with the calcResponse
  #' function. The result is appended to the data by
  #' labeling the "valuetype" column with "regression", "UCI"
  #' and "LCI".
  #' @param the_data (data.frame) A Datframe with the columns
  #' x, y carrying the measurement data
  #' @param mcreg_model (MCResult) A result of an mcr regression
  #' calculated by the mcreg function
  #' @return the_data (data.frame) A data frame carrying
  #' the measurement data, regression data and
  #' confidence interval data. 
  #' @seealso calcResponse, mcreg
  # Calculate the distance between max and min of x-axis
  
  # Append the new column valuetype to the
  # data and label it with "measure" for the input values
  
  the_data["valuetype"] <- "measure"
  
  range_x <- max(the_data$x) - min(the_data$x)
  
  # Add 10% of the range to the max and min value
  x_min <- min(the_data$x)-range_x*0.1
  x_max <- max(the_data$x)+range_x*0.1

  # Create the values of the x axis by a sequence of
  # 100 values
  x_reg <- seq(x_min,x_max,(x_max-x_min)/10)

  
  bias <- data.frame(calcResponse(mcreg_model,alpha=0.02,x.levels = x_reg))

  # Calculate y <- intercept + slope * x
  y_reg <- bias$Y
  
  # Append this data to the input data
  regression_data <- data.frame(x=x_reg,y=y_reg,valuetype="regression")
  
  upper_confidence <- data.frame(x=bias$X,y=bias$Y.UCI,valuetype="UCI")
  lower_confidence <- data.frame(x=bias$X,y=bias$Y.LCI,valuetype="LCI")

  the_data <- rbind(the_data,regression_data,upper_confidence,lower_confidence)
  
  return(the_data)
}

plot_empty = function(){
  #' Generate an empty ggplot
  #' 
  #' Function to create a totally white plot in case
  #' not the right data was provided
  return(ggplot(data.frame(x=0,y=0),aes(x=x,y=y))+
           geom_point(color="white")+
           theme_bw()+
           theme(axis.text = element_blank(),
                 legend.background = element_rect(fill = "transparent",color=NULL),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.line.x = element_line(color="white"),
           axis.line.y = element_line(color="white"),
           axis.title = element_blank(),
           plot.background = element_blank(),
           panel.border= element_blank(),
           axis.ticks = element_blank()
           ))+
    guides(fill=FALSE)
}

plot_linreg = function(the_data,
                       method_names,
                       title_of_plot="",
                       confidence_intervals = F){
  #' Plotting a linear regression plot in 1:1 ratio format
  #' 
  #' This function plots round circles with measured values and a grey
  #' dashed line in between for the linear regression.
  #' 
  #' @param the_data (data.frame) A dataframe with the columns
  #' x, y and valuetype where valuetype can be "measure", "regression"
  #' "UCI" or "LCI". 
  #' @param method_names (list) A list of two reactive elements represending
  #' input boxes giving the names of the x-axis [[1]] and y-axis [[2]]
  #' @param title_of_plot (string) A string giving the for the plot if it is
  #' provided
  #' @param confidence_intervals (bool) If the confidence_interval is set to TRUE
  #' a grey area from the data columns "UCI" and "LCI" will be plotted around
  #' the linear regression line.
  #' @return a grob object carrying the plot
  return({
    # Create a green line of the data frame
    # created with my_df and the number of bins
    # set in the input
    if(!is.null(the_data) && dim(the_data)[1]>=1){
      
      # If the confidence-intervals value is true then
      # A confidence interval data set is created
      #
      # Therefore A table with 3 columns x,y.UCI,y.LCI is
      # constructed and plotted as a grey are
      if(confidence_intervals){
        data_merged <-  merge(subset(the_data,valuetype=="UCI",select=c(1,2)),
                        subset(the_data,valuetype=="LCI",select=c(1,2)),
                        by=1,suffixes=c(".UCI",".LCI"))

        plot_conf <- geom_ribbon(data=data_merged,
                                 aes(x=x, ymax=y.UCI, ymin=y.LCI), fill="#D3D3D3", alpha=0.65)
        data_for_ratio <- the_data
      }else{
        # Else an empty element is used
        plot_conf <- element_blank()
        # The data to calculate the aspect ratio of the plot
        # is reduced to the regression data
        data_for_ratio <- subset(the_data,valuetype=="regression")
      }
      
      
      ggplot()+
        # If wanted plot the confidence intervals
        plot_conf+
        # Plot the data as circles
        geom_point(data=subset(the_data,valuetype=="measure"),aes(x=x,y=y),
                   color="#529f00",shape=21,
                   size=7)+
        # Plot the regression as a dotted line
        geom_line(data=subset(the_data,valuetype=="regression"),
                  mapping=aes(x=x,y=y),
                  size=0.7,
                  color="#3f3f3f",
                  linetype="longdash")+
        # Define the look of the plots
        theme(panel.grid = element_blank(),
              panel.background = element_rect(fill="transparent"),
              panel.border = element_rect(size=1,color="#d3d3d3",fill="transparent"),
              axis.ticks = element_line(size=1,color="#d3d3d3"),
              axis.title = element_text(size=12),
              axis.text = element_text(size=12))+
        # Make the plots being 1:1 ration by giving them (length(x-axis))/length(y-axis) as
        # the ratio
        coord_fixed(ratio=
                      (max(data_for_ratio$x)-min(data_for_ratio$x))/(max(data_for_ratio$y)-min(data_for_ratio$y))
                    )+
        # Define the namex of X / Y / TITLE
        labs(x=method_names[["method1"]](),
             y=method_names[["method2"]](),
             title=paste(title_of_plot))
        
    }else{
      plot_empty()
    }
    
  })
}
regression_map_methods = function(method_string){
  #' Map the long method string to the
  #' short names delivered by mcr package
  #' 
  #' @seealso mcreg

  if(method_string=="Linear Regression"){
    return("LinReg")
  }else if(method_string=="Deming Regression"){
    return("Deming")
  }else if(method_string=="Passing-Bablok regression"){
    return("PaBa")
  }else{
    return("LinReg")
  }
}

regression_construct_table =function(the_data,
                                     names_of_regressions=c("LinReg",
                                                            "Deming",
                                                            "PaBa"),
                                     method_to_choose = "Linear Regression",
                                     calculate_ci = F){
  #' Calculate the linear regression for a table with x and y values
  #' 
  #' Perform this analysis for three different regression methods
  #' and concatinate the 3 tables columnwise
  #' @param the_data (data.frame) A dataframe with two columns named
  #' x and y
  #' @param names_of_regressions (character vector) A vector with the names of the
  #' regression methods of mcreg that shall be used. The standard input is preferred
  #' @return output_table(data.frame) A data frame with two rows and 12 columns
  #' for 3x (EST,SE,LCI,UCI)
  #' @seealso regression_calc, regression_pretty_output
  
  method_to_choose = regression_map_methods(method_to_choose)
  counter <- 0
  for(method in names_of_regressions){
    
    # Read the output of the mcr regression analysis for the method
    if(method_to_choose==method){
      output_of_regression <- regression_calc(the_data,
                                              method,
                                              calculate_ci)
      output_model <- output_of_regression[[2]]
    }else{
      output_of_regression <- regression_calc(the_data,
                                              method,
                                              F)
      
    }
    linreg_table <- data.frame(output_of_regression[[1]])
    
    # Calculate the pearson correllation for the data
    linreg_table[,"pearson"] <- c(cor(the_data$x,the_data$y)^2,0)
    
    # if this is the first run take the table
    if(counter==0){
      output_table <- linreg_table
    # Append the table to the big one
    }else{
      output_table <- cbind(output_table,linreg_table)
    }
    counter=counter+1                               
  }
  
  return(list(output_table,output_model))
}

regression_pretty_output = function(regression_table,
                                    names_of_regressions=c("LinReg","Deming","PaBa")){
  #' Function to select specific columns and introduce naming
  #' 
  #' Select just certain columns from a table and name them by the method used. Mainly
  #' the slope and intercept are taken from the data and the pearsson correllation
  #' @param regression table (data.frame) A dataframe with two rows and 5x length(names_of_regressions)
  #' columns
  #' @return int_table (data.frame) A dataframe with 3x length(names_of_regressions) columns and just one row
  
  int_table <- data.frame()
  for(i in 1:length(names_of_regressions)){
    
    # Read out the inercept line (1)
    int_table [1,
      paste(names_of_regressions[i],"Intercept",sep=" ")
    ]<-regression_table[1,i*5-4]
    
    # Read out the slope line (2)
    int_table [1,paste(names_of_regressions[i],"Slope",sep=" ")] <- regression_table[2,i*5-4]
    #Read out the Pearsson Correllation line
    if(i==length(names_of_regressions)){
      int_table [1,"R2"] <- regression_table[1,i*5]
    }
    
  }
  
  return(int_table)
  
}

regression_combine_pretty = function(list_of_tables,
                                     all_titles){
  #' Combining pretty result tables
  #' 
  #' The function takes a list of tables constructed by regression_pretty_output
  #' and returns one large table with each of the pretty tables in a line
  #' row.names can be given by the all_titles input
  #' @param list_of_tables (list) A list of dataframes that are appended row-wise
  #' @param all_titles (character vector) A list of names that will be used
  #' as the rownames of the output
  #' @return output_table (data.frame) A dataframe containing all input tables
  #' and having the rownames of all_titles
  #' @seealso analysis_generate
  
  for(i in 1:length(list_of_tables)){
    if(i==1){
      output_table <- data.frame(list_of_tables[[1]])
    }else{
      output_table <- rbind(output_table,data.frame(list_of_tables[[i]]))
    }
  }
  rownames(output_table) <- all_titles
  return(output_table)
}

regression_calc = function(input_dataframe,
                           method="LinReg",
                           calculate_ci){
  #' Calculate a linear model for a dataframe and return the coefficients
  #' 
  #' @param input_dataframe (dataframe) A dataframe with the columns x and y
  #' where the linear model y~x is calculated with the mcreg function of
  #' the mcr toolbox
  #' @param calculate_ci (bool) Define if bootstrap shall be used to calculate
  #' the confidence intervals
  #' @seealso mcreg
  #' @return out (list) with two elements
  #' 1) A Matrix containing the MCResult.getCoefficients result
  #' of the Regression calculation
  #' 2) the MCreg result itself
  out <- list()
  if(length(input_dataframe)>0){
    
    if( (dim(input_dataframe)[1]<10) | !calculate_ci){
      my_result <- mcreg(input_dataframe$x,
                         input_dataframe$y,
                         method.reg = method,
                         method.ci="analytical",nsamples=1000,na.rm=T)
    }else{
      my_result <- mcreg(input_dataframe$x,
                         input_dataframe$y,
                         method.reg = method,
                         method.ci="bootstrap",na.rm=T)
    }
    out[[1]] <- MCResult.getCoefficients(my_result)
    out[[2]] <- my_result
#       out<-tryCatch(MCResult.getCoefficients(mcreg(input_dataframe$x,
#                                          input_dataframe$y,
#                                          method.reg = method,
#                                          method.ci="analytical",nsamples=25,na.rm=T)),
#                     error=function(e){
#                                            print("Using LM")
#                                            x<-input_dataframe$x
#                                            y<-input_dataframe$y
#                                            model<- lm(formula = y~x)
#                                            out <- data.frame(summary(model)$coefficients[,1])
#                                            rownames(out)<-c("intercept","slope")
#                                            out[,2]<-out[,1]
#                                            out[,1]<-rownames(out)
#                                            colnames(out)<-c("","value")
#                                            return(out)
#                                          })

      return(out)
  }

}

render_by_file = function(input,
                          header_input,
                          row_input,filenum=1){

  #' Generate rhandsontable from files
  #' 
  #' The function checks out the input file defined
  #' by the filenum and responds with a rhandsontable
  #' that contains the values of the input file
  #' 
      renderRHandsontable({
        
        if(filenum==1){
          input_file <- input$file1
        }else{
          input_file <- input$file2
        }
        
        
        # Return an empty table of no file was provided
        if(is.null(input_file$datapath)){
          data <- NULL
        # Else read the file
        }else{
          data <- read.csv(input_file$datapath,
                           header=header_input(),
                           row.names = row_tester(row_input()))
        }
        
        # Return an empty table of no file was provided
        if(!is.null(data)){
          rhandsontable(data, stretchH = "all",digits=8,useTypes=T, readOnly = TRUE)%>% 
            hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
        }else{
          #rhandsontable(c(), stretchH = "all")
        }
        
      })

}

render_by_textbox = function(input_data_reactive,
                             header_info,
                             row_info){
  #' Generate a table by parsing a TextBox
  #' 
  #' the function takes reactive text box input
  #' and two check field inputs and responds
  #' with a rhandsontable based on these inputs
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
      rhandsontable(DF, stretchH = "all",digits=8)%>% 
        hot_cols(renderer=htmlwidgets::JS("safeHtmlRenderer"))
    }
  })
}

input_finder = function(input,output,values){
  #' A function to check out which tables to analyze
  #' 
  #' This function checks out the input select fields
  #' and by those selects the rhandsontable that should
  #' actively be used for the linear regression analysis
  
  # Get The value of the input DropDown Field
  # for the first input
  dataset1 <- reactive(input$dataset)
  dataset2 <- reactive(input$dataset2)
  
  # Fill the values table with the data
  # from the right data.frame based on
  # the dropdown filed
  observe({
    if(dataset1() == "Copy Paste"){
      # In case of choosing copy paste take
      # the copy paste data frame
      if(!is.null(input$bigTextOut)){
        if(length(input$bigTextOut$data)>0){
          values[["DF"]] <- tryCatch(hot_to_r(input$bigTextOut),error=function(e){c()})
        }
      }
    }else if(dataset1() == "Manually Typing"){
      # In case manually typing was chosen use
      # the "hot" data.frame
      if(!is.null(input$hot)){
        if(length(input$hot$data)>0){
          values[["DF"]] <- tryCatch(hot_to_r(input$hot),error=function(e){c()})
        }
      }
    }else if(dataset1() == "CSV Upload"){
      # In case of the File Upload use the
      # CSV upload table
      if(!is.null(input$file1_table)){
        if(length(input$file1_table$data)>0){
          values[["DF"]] <- tryCatch(hot_to_r(input$file1_table),error=c())
        }
      }
    }
  })
  
  observe({
    if(dataset2() == "Copy Paste"){
      # In case of choosing copy paste take
      # the copy paste data frame
      if(!is.null(input$bigTextOut2)){
        if(length(input$bigTextOut2$data)>0){
          values[["DF2"]] <- tryCatch(hot_to_r(input$bigTextOut2),error=c())
        }
      }
    }else if(dataset2() == "Manually Typing"){
      # In case manually typing was chosen use
      # the "hot" data.frame
      if(!is.null(input$hot2)){
        if(length(input$hot2$data)>0){
          values[["DF2"]] <- tryCatch(hot_to_r(input$hot2),error=c())
        }
      }
    }else if(dataset2() == "CSV Upload"){
      # In case of the File Upload use the
      # CSV upload table
      if(!is.null(input$file2_table)){
        if(length(input$file2_table$data)>0){
          values[["DF2"]] <- tryCatch(hot_to_r(input$file2_table),error=c())
        }
      }
    }
  })
}

shinyServer(function(input, output) {

  # Run all functions needed for the Rhandson Table
  # Manually input management
  manually_input(input,output)
  
  # Run a function to create a linear ggplot and table
 
  
  # Handle file inputs in the file_input function
  file_input(input,output)
  
  # Handle everything in context with the copy paste
  # handlers
  text_paste_input(input,output)
  
  # Check out which tables are active and generate this
  # table as values_to_treat.
  values_to_treat <- reactiveValues()
  input_finder(input,output,values_to_treat)
  
  
  
  # Get the names of the methods
  fieldnames <-list()
  fieldnames[["method1"]] <- reactive(input$dataset1_name)
  fieldnames[["method2"]] <- reactive(input$dataset2_name)
  
  # Get the settings for the analysis
  analysis_fields <-list()
  analysis_fields[["method_for_regression"]] <- reactive(input$method_for_regression)
  analysis_fields[["confidence_intervals"]] <- reactive(input$confidence_intervals)
  
  
  analysis_generate(values_to_treat,
                 fieldnames,
                 analysis_fields,
                 output)
  
  
  output$downloadData <- downloadHandler(filename="test_data1_ITA.csv",content=function(file){
    file.copy("./data/ITA_data.csv",file)},contentType="text/csv")
  output$downloadData_two <- downloadHandler(filename="test_data2_LCMS.csv",content=function(file){
    file.copy("./data/LCMSMS_data.csv",file)},contentType="text/csv")
  outputOptions(output, "error_message", suspendWhenHidden = FALSE)
  
  # Make it possible to download the output_rds file as a CSV
  output$download_analysis <- downloadHandler(
    filename="analysis_output.csv",content=function(file){
      write.csv(readRDS("./data/output_table.rds"),file)},contentType="text/csv")

})
