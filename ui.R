#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Provide a Text Area where tables can be copy pasted  
  textAreaInput(inputId="bigText", "label"="mylab"),
  tableOutput("bigTextOut"),
  
  # Sidebar with a slider input for number of bins 
    fluidRow(
       column(4,h2("basic sestup",align="center"),
              br(),
              div(img(src="worldmap.png",width=200,align="center"),align="center"),
              br(),
              sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
              
              
    ),
    
    # Show a plot of the generated distribution
    column(6,
       plotOutput("distPlot")
    ),
    column(4,
      plotOutput("myplot")
    ),
    column(6,
           h2("Here we can put some basic description"),
           p("Auf jeden Fall muss hier etwas stehen über ",
           a("Sebastian's Website",href="http://mail-wolf.de"),
          "sonst ist das ja alles Quatsch."),
          p("und einen zweiten Abschnitt brauchen wir, nur wegen cool und
            weil so viel Text rein muss, dass es zweizeilig wird."),
          tableOutput("linreg"),
          selectInput("dataset", "Dataset", c("diamonds", "rhandson", "file", "cars")),
          
          # Create an R handson Reader based on the tutorial on:
          # https://github.com/jrowen/rhandsontable
          conditionalPanel(
            condition = "input.dataset == 'rhandson'",
            fluidRow(column(4,
                            helpText("Shiny app based on an example given in the rhandsontable package.", 
                                     "Right-click on the table to delete/insert rows.", 
                                     "Double-click on a cell to edit"),
                            
                            wellPanel(
                              h3("Table options"),
                              radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
                            ),
                            br(), 
                            
                            wellPanel(
                              h3("Save"), 
                              actionButton("save", "Save table")
                            )),
                      column(8,rHandsontableOutput("hot"))
                      )
            ),
          
          # Create an input file reader
          conditionalPanel(
            condition = "input.dataset == 'file'",
            fluidRow(
              column(8,
                     fileInput('file1', 'Choose CSV File',
                                accept=c('text/csv', 
                                          'text/comma-separated-values,text/plain', 
                                          '.csv')
                               )),
              column(2,checkboxInput("header", "Header", TRUE)))
            )
    ),
    column(2,
           tableOutput('table')
           )
    
    
  )
))
