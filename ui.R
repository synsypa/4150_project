# App Implementation - UI
# 4150 Project
# Last Modified: 12/13/15 by kjiang

# Initialization
library(shiny)

# Navigation Bar
shinyUI(navbarPage("Energy Stock Analysis",
                   tabPanel("Single Stock"),
                   tabPanel("Two Stock")
))

# Define UI for Single Stock analysis
shinyUI(
  fluidPage(
    navbarPage("Energy Stock Analysis",
      
              # Single Stock Base Analysis
                tabPanel("Histograms and CIs",
                  titlePanel("Basic Statistics"),
                  # Inputs for Single Stock Analysis
                  sidebarLayout(
                    sidebarPanel(
                      # Select Stock to View
                      selectInput("stk", "Choose Stock:", 
                                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom")),
                      
                      # Select bins for histogram      
                      sliderInput("bins",
                                  "Number of bins:",
                                  min = 1, max = 100, value = 50, step = 1, ticks = FALSE),
                      
                      # Select Confidence Level
                      sliderInput("cLevel",
                                  "Confidence Level:",
                                  min = 0, max = 1, value = .95, step = .01)
                    ),

                    # Display Histogram and Confidence Interval
                    mainPanel(
                      plotOutput("histPlot", width = "100%"),
                      textOutput("confMean"),
                      textOutput("confSD")
                    )
                    )
                ),
              
              # Single Stock Regression
              navbarMenu("Single Stock Analysis",
                tabPanel("Linear Regressions",
                  titlePanel("Single-stock Regressions on Time"),
                  sidebarLayout(
                    sidebarPanel(
                      # Select Stock to View
                      selectInput("stk2", "Choose Stock:", 
                                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom"))
                    ),
                    
                    # Display Regression Output          
                    mainPanel(
                      plotOutput("s1_regPlot"),
                      textOutput("s1_regRes"),
                      plotOutput("s1_res")
                    )
                  )
                )
              ),
              
              # Multi Stock Analysis
              navbarMenu("Two Stock Analysis",                     
                         tabPanel("t-testing",
                                  titlePanel("Two-stock Returns Comparison"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      # Select Stocks to Compare (C vs D)
                                      selectInput("stkC", "Choose Stock:", 
                                                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                                                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                                                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                                                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                                                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom")),
                                      
                                      selectInput("stkD", "Choose Stock:", 
                                                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                                                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                                                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                                                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                                                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom")),
                                      
                                      # Select Confidence Level
                                      sliderInput("cLevel2",
                                                  "Confidence Level:",
                                                  min = 0, max = 1, value = .95, step = .01)
                                    ),
                                    mainPanel(
                                      textOutput("s2_ttest")
                                    )
                                  )
                         ),
                         tabPanel("Linear Regressions",
                                  titlePanel("Two-stock Regressions"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      # Select Stocks to Compare (A vs B)
                                      selectInput("stkA", "Choose Stock:", 
                                                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                                                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                                                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                                                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                                                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom")),
                                      
                                      selectInput("stkB", "Choose Stock:", 
                                                  c("Peabody Energy Corp"="btu", "Chevron Corp"="cvx",
                                                    "Consolidated Edison, Inc"="ed", "EOG Resources Inc"="eog",
                                                    "Linn Energy LLC"="line", "Marathon Oil Corp"="mro",
                                                    "Plug Power Inc."="plug", "Renewable Energy Group, Inc"="regi",
                                                    "SolarCity Corp"="scty", "Exxon Mobil Corp"="xom"))
                                    ),
                                    
                                    # Display Regression Output          
                                    mainPanel(
                                      plotOutput("s2_regPlot"),
                                      textOutput("s2_regRes"),
                                      plotOutput("s2_res")
                                    )
                                  )
                         )
              )
    )
  )
)