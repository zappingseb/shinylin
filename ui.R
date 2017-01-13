#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

if(!require("shiny")){
  install.packages("shiny")
  library(shiny)
}
if(!require("rhandsontable")){
  install.packages("rhandsontable")
  library(rhandsontable)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  
  
  # Application title
  div(class="navbar",
      fluidRow(
        column(10,titlePanel("Comparing Measurement Methods")),
        column(2,br(),"sebastian",tags$b("wolf"),br(),div(id="line")))
    
      ),
  

  
  # Sidebar with the import functions and the old slider from
  # the example App
    fluidRow(
       column(4,
              class="leftspace",
              h2("Uploading Datasets to analyze",align="center"),
              br(),
              p("Please use this section to upload data. Therefore you have three
                possibilities including ",tags$b("Excel Copy&Paste,"),tags$b("CSV Upload")," and ",
                tags$b("Manually typing")," into an empty table"),
              br(),
              
              # Slider of the Example App
              sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30),
              
# --------- InputBox for the 1st Dataset ----------
             p(h3("First dataset")), 
             div(class = "inputbox",
                  
                  selectInput("dataset",
                              "Choose a method to upload your data set:",
                              c("","Copy Paste", "CSV Upload", "Manual Entry")),
                  
                  # Create an R handson Reader based on the tutorial on:
                  # https://github.com/jrowen/rhandsontable
                  conditionalPanel(
                    condition = "input.dataset == 'Manual Entry'",wellPanel(
                    fluidRow(column(3,
                                      actionButton("save", "Save table")
                                    ),
                             column(8,rHandsontableOutput("hot"))
                    ))
                  ),
                  
                  # Create an input file reader
                  conditionalPanel(
                    condition = "input.dataset == 'CSV Upload'",
                    wellPanel(
                      fluidRow(
                        column(8,
                               fileInput('file1', 'Choose CSV File',
                                         accept=c('text/csv', 
                                                  'text/comma-separated-values,text/plain', 
                                                  '.csv')
                               )),
                        column(2,checkboxInput("header_file1", "Header", TRUE),
                        checkboxInput("row_file1","Row Names",F))
                        )#fluidRow
                    )
                  ),
                  conditionalPanel(
                    condition = "input.dataset == 'Copy Paste'",
                    # Provide a Text Area where tables can be copy pasted  
                    wellPanel(
                      fluidRow(
                        column(8,textAreaInput(inputId="bigText",
                                               "label"="Paste your Excel data into this field:")),
                        column(2,checkboxInput("header_bigText", "Header", TRUE),
                               checkboxInput("row_bigText","Row Names",F))
                      ),
                      
                      rHandsontableOutput("bigTextOut")
                    )
                  )
          ),# div
# --------- InputBox for the 2nd Dataset ----------
        p(h3("Second dataset")),
        div(class="inputbox",
            selectInput("dataset2",
                        "Choose a method to upload your data set:",c("","Copy Paste", "CSV Upload", "rhandson")),
            # Create an input file reader
            conditionalPanel(
              condition = "input.dataset2 == 'CSV Upload'",
              wellPanel(
                fluidRow(
                  column(8,
                         fileInput('file2', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')
                         )),
                  column(2,checkboxInput("header2", "Header", TRUE)))
              )
            ),
            # Copy Paste from Textarea to RHandsonTable
            conditionalPanel(
              condition = "input.dataset2 == 'Copy Paste'",
              # Provide a Text Area where tables can be copy pasted  
              wellPanel(
                fluidRow(
                  column(8,textAreaInput(inputId="bigText2",
                                         "label"="Paste your Excel data into this field:")),
                  column(2,checkboxInput(inputId="header_bigText2", "Header", TRUE),
                         checkboxInput("row_bigText","Row Names",F))
                ),
                
                rHandsontableOutput("bigTextOut2")
              )
            )
            
            )#div
               
    ),# column
# --------- PlayGround Stuff ----------
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
          tableOutput("linreg")
    ),
    column(2,
           tableOutput('table')
           )
    
    
  )#fluidRow
)#fluidPage
)#shinyUI
