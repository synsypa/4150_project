# App Implementation - UI
# Superimpose Normal Dist on Histograms
# Construct confidence intervals
# Last Modified: 11/21/15 by kjiang

# Initialization
library(shiny)

# Define UI to draw Histogram
shinyUI(fluidPage(
  
# Application Title
  titlePanel("4150 Group HW 3"),

# Choosing stock to show
  sidebarLayout(
    sidebarPanel(
      selectInput("stk", "Choose Stock (by symbol):", 
                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom")),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1, max = 100, value = 50, step = 1, ticks = FALSE),
      
      sliderInput("cLevel",
                  "Confidence Level:",
                  min = 0, max = 1, value = .95, step = .01)
      
  ),

# Display Histogram and Confidence Interval
  mainPanel(
    plotOutput("histPlot"),
    textOutput("confInt")
  )
)
))