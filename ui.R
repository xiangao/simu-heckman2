library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Heckman model compared with OLS"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    sliderInput("n", "Sample size (exponentiated):",
                min=4, max=9, value=4, step=1),
    sliderInput("rho", "Correlation between u and v",
                min=-.9, max=.9, value=0, step=.3),
    sliderInput("sel.cri", "selection proportion (low,  high):",
                min=0, max=1, value=0, step=1)
      ),
      mainPanel(plotOutput("plot1")
                )
      ))
