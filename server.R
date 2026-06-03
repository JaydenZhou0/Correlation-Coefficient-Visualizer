options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("tidyverse")

library(shiny)
library(tidyverse)
library(ggplot2)

correlation_coefficient <- function(x, y) {
  r <- (sum((x - mean(x)) * (y - mean(y)))) / 
    (sqrt(sum((x - mean(x))^2)) * sqrt(sum((y - mean(y))^2)))
  return(r)
}

dataF <- function(x, y) {
  return(data.frame(x = x, y = y))
}

combinedDataF <- function(x1, y1, x2, y2) {
  df1 <- data.frame(x = x1, y = y1, group = "Group 1")
  df2 <- data.frame(x = x2, y = y2, group = "Group 2")
  return(rbind(df1, df2))
}

server <- function(input, output, session) {
  
  data_vals <- reactiveValues(x = numeric(), y = numeric())
  
  observeEvent(input$randomize, {
    x_random <- round(rnorm(10, 0, 5), 2)
    y_random <- round(rnorm(10, 0, 5), 2)
    
    data_vals$x <- x_random
    data_vals$y <- y_random
    
    updateTextInput(session, "x1", value = paste(x_random, collapse = ","))
    updateTextInput(session, "y1", value = paste(y_random, collapse = ","))
  })
  
  observeEvent(input$clear, {
    data_vals$x <- numeric()
    data_vals$y <- numeric()
    
    updateTextInput(session, "x1", value = "")
    updateTextInput(session, "y1", value = "")
    updateSelectInput(session, "operation_x", selected = "Addition (+)")
    updateSelectInput(session, "operation_y", selected = "Addition (+)")
    updateTextInput(session, "constant_x", value = "")
    updateTextInput(session, "constant_y", value = "")
  })
  
  observeEvent(input$add_point, {
    data_vals$x <- c(data_vals$x, input$new_x)
    data_vals$y <- c(data_vals$y, input$new_y)
  })
  
  output$correlation1 <- renderText({
    x1 <- data_vals$x
    y1 <- data_vals$y
    
    if (length(x1) == length(y1) && length(x1) > 1) {
      r1 <- correlation_coefficient(x1, y1)
      paste("The correlation coefficient is: ", round(r1, 3))
    } else {
      "The lengths of x and y do not match or are insufficient!"
    }
  })
  
  output$scatterPlot3 <- renderPlot({
    x1 <- data_vals$x
    y1 <- data_vals$y
    
    operation_x <- input$operation_x
    constant_x <- input$constant_x
    operation_y <- input$operation_y
    constant_y <- input$constant_y
    
    if (length(x1) == 0 || length(y1) == 0) {
      ggplot() +
        theme_bw() +
        labs(x = "x", y = "y", title = "Scatter Plot: Group 1 vs Group 2") +
        xlim(0, 10) + ylim(0, 10) + 
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      if (operation_x == "Exchange x and y") {
        x2 <- y1
        y2 <- x1 
      } else {
        x2 <- switch(operation_x,
                     "Addition (+)" = x1 + constant_x,
                     "Subtraction (-)" = x1 - constant_x,
                     "Multiplication (*)" = x1 * constant_x,
                     "Division (/)" = x1 / constant_x)
        
        y2 <- switch(operation_y,
                     "Addition (+)" = y1 + constant_y,
                     "Subtraction (-)" = y1 - constant_y,
                     "Multiplication (*)" = y1 * constant_y,
                     "Division (/)" = y1 / constant_y,
                     "Exchange x and y" = x1)
      }
      
      r1 <- correlation_coefficient(x1, y1)
      r2 <- correlation_coefficient(x2, y2)
      data <- combinedDataF(x1, y1, x2, y2)

      ggplot(data = data, aes(x = x, y = y)) +
        geom_point(aes(color = group), alpha = 0.5) + 
        geom_smooth(data = subset(data, group == "Group 1"), method = "lm", se = FALSE, color = "blue") +  
        geom_smooth(data = subset(data, group == "Group 2"), method = "lm", se = FALSE, color = "orange") + 
        annotate("text", x = mean(x1), y = mean(y1) - 0.5, label = sprintf("r1 = %0.3f", r1), color = "blue", hjust = -0.1) +
        annotate("text", x = mean(x2), y = mean(y2) - 0.5, label = sprintf("r2 = %0.3f", r2), color = "orange", hjust = -0.1) +
        scale_color_manual(values = c("Group 1" = "darkblue", "Group 2" = "red")) +
        theme_bw() +
        labs(x = "x", y = "y", title = "Scatter Plot: Group 1 vs Group 2")
    }
  })
}