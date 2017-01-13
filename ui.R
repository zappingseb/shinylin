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
                  
#------------ Navigation Bar --------
  div(class="navbar",
      fluidRow(
        column(5,titlePanel("Comparing Measurement Methods")),
        column(5,h2(actionButton("helpbutton", "?"))),
        column(2,br(),HTML("<a href='http://mail-wolf.de'>sebastian<b>wolf</b></a>"),br(),div(id="line")))
    
      ),#div
#------------- Help Panel ----------
    # A Help panel being activated on Click of the HELP Button
    conditionalPanel(condition = "input.helpbutton % 2 != 0",
                     class="helpbar",
                     fluidRow(
                       column(4,
                              h4("Uploading Datasets to analyze"),
                              br(),
                              p("Please use this section to upload data. Therefore you have three
                  possibilities including ",tags$b("Microsoft Excel® Copy & Paste,"),tags$b("CSV Upload")," and ",
                                tags$b("Manually typing")," into a default table"),
                              p("Data shall be provided in tables. Therefore a new line shall be added for each
                  point of a dataset within the same method. These lines will be compared between
                  methods. Columns shall be added if multiple datasets will be compared for linearity.
                  Multiple columns are",tags$b("not"),"possible for the ",tags$b("Manually typing"),
                                "data input."),
                              p("A TestData.CSV file can be downloaded. Please use this File
                  with the header checkbox 'checked'"),
                              fluidRow(
                                column(6,downloadButton('downloadData','1st test data')),
                                column(6,downloadButton('downloadData_two','2nd test data'))
                              )
                              
                     )#column
            )#fluidRow
                       
      ),#conditional panel
  
#----------- LEFT SIDE -------------
  # Sidebar with the import functions and the old slider from
  # the example App
    fluidRow(
       column(4,
              class="leftspace",
              h2("Uploading Datasets to analyze",align="left"),
              
              
              
# --------- InputBox for the 1st Dataset ----------
             p(h3("First method datasets")), 
             div(class = "inputbox",
                 textInput(inputId="dataset1_name",label="Name the Method",value = "Method 1"),
                  selectInput("dataset",
                              "Choose a method to upload your data set:",
                              c("","Copy Paste", "CSV Upload", "Manual Entry")),
                  
                  # Create an R handson Reader based on the tutorial on:
                  # https://github.com/jrowen/rhandsontable
                  conditionalPanel(
                    condition = "input.dataset == 'Manual Entry'",wellPanel(
                      p("Help can be found",a(href="https://github.com/jrowen/rhandsontable","here")),
                    fluidRow(column(3,
                                      actionButton("save", "Save table")
                                    ),
                             column(8,rHandsontableOutput("hot",height = "200px"))
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
                        ),#fluidRow
                        fluidRow(
                          column(11,rHandsontableOutput("file1_table"))
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
        p(h3("Second method datasets")),
        div(class = "inputbox",
            textInput(inputId="dataset2_name",label="Name the Method",value = "Method 2"),
            selectInput("dataset2",
                        "Choose a method to upload your data set:",
                        c("","Copy Paste", "CSV Upload", "Manual Entry")),
            
            # Create an R handson Reader based on the tutorial on:
            # https://github.com/jrowen/rhandsontable
            conditionalPanel(
              condition = "input.dataset2 == 'Manual Entry'",
              wellPanel(
                p("Help can be found",a(href="https://github.com/jrowen/rhandsontable","here")),
                fluidRow(column(3,
                                actionButton("save2", "Save table")
                ),
                column(8,rHandsontableOutput("hot2"))
                ))
            ),
            
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
                  column(2,checkboxInput("header_file2", "Header", TRUE),
                         checkboxInput("row_file2","Row Names",F))
                ),#fluidRow
                fluidRow(
                  column(11,rHandsontableOutput("file2_table"))
                )#fluidRow
              )
            ),
            conditionalPanel(
              condition = "input.dataset2 == 'Copy Paste'",
              # Provide a Text Area where tables can be copy pasted  
              wellPanel(
                fluidRow(
                  column(8,textAreaInput(inputId="bigText2",
                                         "label"="Paste your Excel data into this field:")),
                  column(2,checkboxInput("header_bigText2", "Header", TRUE),
                         checkboxInput("row_bigText2","Row Names",F))
                ),
                
                rHandsontableOutput("bigTextOut2")
              )
            )
        )# div
               
    ),# column
#----------- RIGHT SIDE -------------
# --------- PlayGround Stuff ----------
    # Show a plot of the generated distribution
    column(6,
           h2("Analysis",align="center"),br(),
           # Slider of the Example App
           sliderInput("bins",
                       "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30),
           p("In this section the results of the linearity analysis will be displayed. Please
             feel free to use the checkboxes to display or hide certain analysis parts"),
           tags$hr(),
           
       plotOutput("distPlot"),
       plotOutput("myplot"),
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
