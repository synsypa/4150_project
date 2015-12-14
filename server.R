# App Implementation - SERVER
# 4150 Project
# Construct confidence intervals
# Last Modified: 12/13/15 by kjiang

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

  # Create dataframe of Log-Returns for each stock
  btu = diff(log(closePrices$btu))
  cvx = diff(log(closePrices$cvx))
  ed = diff(log(closePrices$ed))
  eog = diff(log(closePrices$eog))
  line = diff(log(closePrices$line))
  mro = diff(log(closePrices$mro))
  plug = diff(log(closePrices$plug))
  regi = diff(log(closePrices$regi))
  scty = diff(log(closePrices$scty))
  xom = diff(log(closePrices$xom))
  
  log_return  = data.frame(btu,cvx,ed,eog,line,mro,plug,regi,scty,xom)
  
  # Reference Dataframe for scatterplots
  means = colMeans(log_return)
  sds = apply(log_return, 2, sd)
  symbols = c("btu","cvx","ed","eog","line","mro","plug","regi","scty","xom")
  
  scatter_ref = data.frame(symbols, means, sds)
  
shinyServer(function(input, output) {
  ## SINGLE STOCK ANALYSES
  # Generate Scatterplot of Means and SDs
  output$scatterPlot = renderPlot({
    plot(scatter_ref$means, scatter_ref$sds, type = "p", xlim = c(-.005, .005), ylim = c(0,.08),
         main = "Log-returns over 2014", 
         xlab = "Mean Log-Returns", ylab = "Standard Deviation of Log-Returns",
         col=ifelse(scatter_ref$symbols == input$stk, "red", "blue") , pch = 19, cex = 1)
    text(means, sds, labels = symbols, cex = 1, pos = c(1,2,4,4,1,2,2,2,2,1),
         col=ifelse(scatter_ref$symbols == input$stk, "red", "blue"))
  })
  
  # Generate Scatterplot of Means and SDs (for 1 Stock Regression Page)
  output$scatterPlot2 = renderPlot({
    plot(scatter_ref$means, scatter_ref$sds, type = "p", xlim = c(-.005, .005), ylim = c(0,.08),
         main = "Log-returns over 2014", 
         xlab = "Mean Log-Returns", ylab = "Standard Deviation of Log-Returns",
         col=ifelse(scatter_ref$symbols == input$stk2, "red", "blue") , pch = 19, cex = 1)
    text(means, sds, labels = symbols, cex = 1, pos = c(1,2,4,4,1,2,2,2,2,1),
         col=ifelse(scatter_ref$symbols == input$stk2, "red", "blue"))
  })
  
  # Generate the Histogram
  output$histPlot = renderPlot({
    stock = tolower(input$stk)
    bins = seq(min(log_return[[stock]]), max(log_return[[stock]]), length.out=input$bins + 1)
    
    hist(log_return[[stock]], breaks = bins, prob = TRUE,
         main = paste("Diff of Log Returns for ", stock), xlab = "Diff Log Return")
    curve(dnorm(x, mean = mean(log_return[[stock]]), sd = sd(log_return[[stock]])), add = TRUE)
  })
  
  # Calculate Confidence Interval of Mean
  output$confMean = renderText({
    stock = tolower(input$stk)
    mean = mean(log_return[[stock]])
    sd = sd(log_return[[stock]])
    n = length(log_return[[stock]]) 
    error = qt(input$cLevel+((1-input$cLevel)/2), df=n-1)*sd/sqrt(n)
    left = round(mean - error, digits = 5)
    right = round(mean + error, digits = 5)
    
    paste("The ", input$cLevel, "Confidence Interval of the Mean is [", left,",", right, "]")
  })
  
  # Calculate Confidence Interval of SD
  output$confSD <- renderText({ 
      stock = tolower(input$stk)
      mean = mean(log_return[[stock]])
      sd = sd(log_return[[stock]])
      n = length(log_return[[stock]]) 
      error = sqrt(((n-1)*sd^2)/qchisq(input$cLevel+((1-input$cLevel)/2), df=n-1 ))
      left = round(sd - error, digits = 5)
      right = round(sd + error, digits = 5)
      
      paste("The ", input$cLevel, "Confidence interval of the Standard Deviation is [", left,",", right,"]")
    })
  
  # Produce Regression Plot for Stock vs Time
  output$s1_regPlot <- renderPlot({
    stock = tolower(input$stk2)
    lr = log_return[[stock]]
    n = length(lr) 
    time <- seq(from=1,to = n, by = 1)
    model <- lm(lr ~ time)
    plot(time,lr, main = "Log Returns w/ fitted regression",
         xlab="Trading days since beginning 2014",ylab = input$stk2)
    abline(model)
    
    output$s1_res <- renderPlot({
      plot(resid(model),ylim =c(min(resid(model)), max(resid(model))),
           main = "Graph of residuals",xlab = "Trading days since beginning 2014",ylab = "Residuals" ) 
    }) 
  })
  
  # Produce Regression Results for Stock vs Time
  output$s1_regRes <- renderText({ 
    stock = tolower(input$stk2)
    lr = log_return[[stock]]
    n <- length(lr)
    time <- seq(from=1,to = n, by = 1)
    model <- lm(lr ~ time)
    plot(time,lr,xlab="Trading days since beginning 2014",ylab = input$stk2)
    abline(model)
    paste("Slope of regression line = ",round(model$coefficients[2],digits = 5),"
          ","| Intercept = ",round(model$coefficients[1],digits =5), "| R-squared = ",
          round(summary(model)$r.squared,digits = 5))
    })
  
  ## MULTISTOCK ANALYSES
  # Generate Scatterplot of Means and SDs (for 2 Stock t-test Page)
  output$scatterPlot3 = renderPlot({
    plot(scatter_ref$means, scatter_ref$sds, type = "p", xlim = c(-.005, .005), ylim = c(0,.08),
         main = "Log-returns over 2014", 
         xlab = "Mean Log-Returns", ylab = "Standard Deviation of Log-Returns",
         col=ifelse(scatter_ref$symbols == input$stkC | scatter_ref$symbols == input$stkD , "red", "blue"),
         pch = 19, cex = 1)
    text(means, sds, labels = symbols, cex = 1, pos = c(1,2,4,4,1,2,2,2,2,1),
         col=ifelse(scatter_ref$symbols == input$stkC | scatter_ref$symbols == input$stkD , "red", "blue"))
  })
  
  # Produce t-test results
  output$s2_ttest <- renderText({
    if (input$stkC != input$stkD){
      stock1 = tolower(input$stkC)
      stock2 = tolower(input$stkD)
      lr1 = log_return[[stock1]]
      lr2 = log_return[[stock2]]
      test = t.test(lr1,lr2,paired=TRUE)
      if (test$p.value > (1 - input$cLevel2)) {
        paste("Under a paired t-test, we cannot reject the hypothesis that the mean log-returns of ",
              toupper(stock1), " and ", toupper(stock2), " are equal. ( p-value = ",
              round(test$p.value, digits = 5), ") at Confidence Level = ", input$cLevel2)
      }
      else {
        paste("Under a paired t-test, we REJECT the hypothesis that the mean log-returns of ",
              toupper(stock1), " and ", toupper(stock2), " are equal ( p-value = ",
              round(test$p.value, digits = 5), ") at Confidence Level = ", input$cLevel2)
      }
      
    }
    else(paste("You have selected the same stock, please select 2 different stocks"))      
  })
  
  # Generate Scatterplot of Means and SDs (for 2 Stock Regression Page)
  output$scatterPlot4 = renderPlot({
    plot(scatter_ref$means, scatter_ref$sds, type = "p", xlim = c(-.005, .005), ylim = c(0,.08),
         main = "Log-returns over 2014", 
         xlab = "Mean Log-Returns", ylab = "Standard Deviation of Log-Returns",
         col=ifelse(scatter_ref$symbols == input$stkA | scatter_ref$symbols == input$stkB, "red", "blue"),
         pch = 19, cex = 1)
    text(means, sds, labels = symbols, cex = 1, pos = c(1,2,4,4,1,2,2,2,2,1),
         col=ifelse(scatter_ref$symbols == input$stkA | scatter_ref$symbols == input$stkB, "red", "blue"))
  })
  
  # Produce Regression Plot for Stock vs Stock
  output$s2_regPlot <- renderPlot({
    stock1 = tolower(input$stkA)
    stock2 = tolower(input$stkB)
    lr1 = log_return[[stock1]]
    lr2 = log_return[[stock2]]
    n = length(lr1)
    model = lm(lr1 ~ lr2)
    plot(lr1,lr2,main = "Log Returns w/ fitted regression",xlab = stock2 ,ylab = stock1)
    abline(model)
    output$s2_res <- renderPlot({
      plot(resid(model),ylim =c(min(resid(model)), max(resid(model))) ,
           main = "Graph of residuals",xlab = "",ylab = "Residuals" ) 
    }) 
  })
  
  # Produce Regression Results for Stock vs Stock
  output$s2_regRes <- renderText({ 
    stock1 = tolower(input$stkA)
    stock2 = tolower(input$stkB)
    lr1 = log_return[[stock1]]
    lr2 = log_return[[stock2]]
    n = length(lr1)
    model <- lm(lr1 ~ lr2)
    paste("Slope of regression line = ",round(model$coefficients[2],digits = 5),"
            ","| Intercept = ",round(model$coefficients[1],digits =5),"| R-Squared = ",
            round(summary(model)$r.squared,digits = 5))
    })
  

  
})