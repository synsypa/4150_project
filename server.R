# App Implementation - SERVER
# Superimpose Normal Dist on Histograms
# Construct confidence intervals
# Last Modified: 11/21/15 by kjiang

# Initialization
library(shiny)

# Load Stock Data
setwd("./data")
files <- list.files("./", pattern = ".csv")
priceFiles <- do.call("cbind", lapply(files, read.csv))

  # Create dateframe of close prices sorted increasing by date
  closePrices <- priceFiles[, names(priceFiles) == 'Close']
  closePrices$date <- priceFiles[, c(1)]
  names(closePrices) <- c(strtrim(files, nchar(files)-4), "date")
  closePrices <- closePrices[rev(order(as.Date(closePrices$date, format="%d-%m-%y"))),]

# Draw a Histogram
shinyServer(function(input, output) {
  
  # Generate the Histogram
  output$histPlot = renderPlot({
    stock = tolower(input$stk)
    lPrice = diff(log(closePrices[[stock]]))
    bins = seq(min(lPrice), max(lPrice), length.out=input$bins + 1)
    
    hist(lPrice, breaks = bins, prob = TRUE,
         main = paste("Diff of Log Returns for ", stock), xlab = "Diff Log Return")
    curve(dnorm(x, mean = mean(lPrice), sd = sd(lPrice)), add = TRUE)
  })
  
  # Calculate Confidence Interval
  output$confInt = renderText({
    stock = tolower(input$stk)
    lPrice = diff(log(closePrices[[stock]]))
    mean = mean(lPrice)
    sd = sd(lPrice)
    n = length(lPrice) 
    error = qt(input$cLevel, df=n-1)*sd/sqrt(n)
    left = round(mean - error, digits = 5)
    right = round(mean + error, digits = 5)
    
    paste("The ", input$cLevel, "Confidence Interval is [", left, right, "]")
  })
})