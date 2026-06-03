options(repos = c(CRAN = "https://cloud.r-project.org"))

library(shiny)

ui <- navbarPage(
  title = "Correlation Coefficient and Scatter Plot",
  tabPanel("Correlation Coefficient",
           fluidPage(
             fluidRow(
               column(4,
                      titlePanel("Randomized x and y Values"),
                      actionButton("randomize", "Randomize x and y"),
                      actionButton("clear", "Clear All"),
                      textInput("x1", "Enter x values (comma separated):", ""),
                      textInput("y1", "Enter y values (comma separated):", ""),
                      selectInput("operation_x", "Choose Operation for x2:",
                                  choices = c("Addition (+)", "Subtraction (-)",
                                              "Multiplication (*)", "Division (/)",
                                              "Exchange x and y")),
                      conditionalPanel(
                        condition = "input.operation_x != 'Exchange x and y'",
                        numericInput("constant_x", "Enter a constant for x2:", value = 1)
                      ),
                      conditionalPanel(
                        condition = "input.operation_x != 'Exchange x and y'",
                        selectInput("operation_y", "Choose Operation for y2:",
                                    choices = c("Addition (+)", "Subtraction (-)",
                                                "Multiplication (*)", "Division (/)",
                                                "Exchange x and y"))
                      ),
                      conditionalPanel(
                        condition = "input.operation_x != 'Exchange x and y'",
                        numericInput("constant_y", "Enter a constant for y2:", value = 1)
                      ),
                      numericInput("new_x", "Add new point: x value", value = 0),
                      numericInput("new_y", "Add new point: y value", value = 0),
                      actionButton("add_point", "Add New Point")
               ),
               column(8, 
                      h3("Scatter Plot: Group 1 and Group 2"),
                      plotOutput("scatterPlot3"),
                      textOutput("correlation1")
               )
             )
           )
        ),
  tabPanel("About",
           fluidPage(
             h3("About"),
             p("This app is designed to visualize how 
             Pearson's correlation coefficient is affected 
             by variables when performing operations, 
               allowing one to better understand the definition of 
               Pearson's correlation coefficient."),
             br(),
             p("Developed by: Ivy Gu & Jayden Zhou"),
             p("Instructor: Dr. Weijia Jia")
           )
  )
)
