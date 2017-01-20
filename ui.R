#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

if(!require("shiny")){
  #install.packages("shiny")
  library(shiny)
}
if(!require("rhandsontable")){
  #install.packages("rhandsontable")
  library(rhandsontable)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
                  
#------------ Navigation Bar --------
  div(class="navbar",
      fluidRow(
        column(1,h2(actionButton("helpbutton", "Help"))),
        column(9,titlePanel("Comparing Measurement Methods")),
        column(2,br(),HTML("<a href='http://mail-wolf.de'>sebastian<b>wolf</b></a>"),br(),div(id="line")))
    
      ),#div
  conditionalPanel(condition="output.error_message!=''",
      div(class="error_window"
          ,p(tags$b("Error:")),
          textOutput("error_message")
          )#div
      ),#conditionalPanel
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
                                tags$b("Manually typing")," into a default table."),
                              p("Data shall be provided in tables. Therefore a new line shall be added for each
                  point of a dataset within the same method. These lines will be compared between
                  methods. Columns shall be added if multiple datasets will be compared for linearity.
                  Multiple columns are",tags$b("not"),"possible for the ",tags$b("Manually typing"),
                                "data input."),
                              p("Two testdata CSV files can be downloaded. These are generated
                   with data from this",a(href="http://pubs.acs.org/doi/abs/10.1021/pr5011179","paper."),
                  "These compare  immunoturbidimetric analysis (ITA) on a Cobas Integra 800 with an
                   Liquid Chromatography - Mass spectrometry method (LC-MS/MS). Please use these files
                  with the header checkbox 'checked' in each of the two sections below."),
                              fluidRow(
                                column(6,downloadButton('downloadData','1st test data')),
                                column(6,downloadButton('downloadData_two','2nd test data'))
                              )
                              
                     ),#column
                     column(7,
                           h4("Analysis"),
                           br(),
                           p("On top of this section you will find a bar that allows you to have certain
                             settings for the analysis."),
                           p(tags$b("Regression Method:"),"In this dropdown field you can choose the regression
                             method outcome that shall be used for plotting the results. Three different
                             regression methods are offered here. For further information on these regression
                             methods we would recommend you the",
                             a("Wikipedia article",href="https://en.wikipedia.org/wiki/Linear_regression"),"."),
                           p(tags$b("Confidence Intervals:"),"Regression sometimes cannot fit you data
                             perfectly as it will not be perfectly linearly correllated. Therefore the error
                             of the regression can be displayed as a 95 % confidence interval with this checkbox.
                             Due to the computational intensity this could take upto 15 seconds"),
                           p("For using the slope and the intercept of each regression analysis we provide you
                             with the values in a table format. The table displayed on the buttom of the analysis
                             will be exported as a CSV file by clicking the",tags$b("Download Analysis table button."))
                           
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
                              c("","Copy Paste", "CSV Upload", "Manually Typing")),
                  
                  # Create an R handson Reader based on the tutorial on:
                  # https://github.com/jrowen/rhandsontable
                  conditionalPanel(
                    condition = "input.dataset == 'Manually Typing'",wellPanel(
                      fluidRow(column(4,
                                      actionButton("save", "Save table")
                                    ),
                             column(7,rHandsontableOutput("hot",height = "200px"))
                    )),
                    p("Help can be found",a(href="https://github.com/jrowen/rhandsontable","here"))
                    
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
                        c("","Copy Paste", "CSV Upload", "Manually Typing")),
            
            # Create an R handson Reader based on the tutorial on:
            # https://github.com/jrowen/rhandsontable
            conditionalPanel(
              condition = "input.dataset2 == 'Manually Typing'",
              wellPanel(
               
                fluidRow(column(4,
                                actionButton("save2", "Save table")
                ),
                column(7,rHandsontableOutput("hot2"))
                )),
                p("Help can be found",a(href="https://github.com/jrowen/rhandsontable","here"))
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
    column(7,
           h2("Analysis",align="center"),
           p(h3(HTML("&nbsp;"))),
           # Create an input field for the visualization which allows the user to set certain properties
           # and maybe leave certain things out
           div(class="inputbox",
               fluidRow(
                 column(4,selectInput("method_for_regression",label="Choose a Regression method to be plotted",c("Linear Regression",
                                                                                                      "Deming Regression",
                                                                                                      "Passing-Bablok regression"))),
                 column(2,checkboxInput("confidence_intervals","Show Confidence intervals",T)),
                 downloadButton('download_analysis', 'Download Analysis table')
               )#fluidRow
              ),#div
           br(),
           

           plotOutput("myplot"),

           tableOutput("linreg")
    )
    
    
  )#fluidRow
)#fluidPage
)#shinyUI
