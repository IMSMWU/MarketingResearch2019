library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Standardizing a Normal Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu",
                  "Add:",
                  min = -5,
                  max = 5,
                  value = 0),
      sliderInput("sd",
                  "Divide by:",
                  min = 1, max = 5, value = 0),
      actionButton("play", "Play again"),
      checkboxInput("reveal", "Reveal Solution"),
      h4("Hint:"),
      withMathJax(helpText("$$x \\sim N(\\mu, \\sigma^2) \\\\ \\Rightarrow {x - \\mu \\over \\sigma} \\sim N(0,1)$$")),
      conditionalPanel("input.reveal",
                       uiOutput('moments'))
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  nn <- as.numeric
  mu1 <- round(runif(1, -5, 5), 0)
  sd1 <- round(runif(1, 1, 5), 0)
  output$distPlot <- renderPlot({

    moms <- c(mu1, sd1)
    
    plt <- ggplot(data.frame(x = 0))+
      xlim(c(-15, 15)) +
      stat_function(fun = dnorm, args = list((nn(moms[1]) + nn(input$mu)), (nn(moms[2])/nn(input$sd))), geom = "line", aes(color = "Your transformation")) +
      stat_function(fun = dnorm, args = list(0,1), geom = "line", aes(color = "Standard Normal") ) + 
      theme_bw() +
      theme(legend.title = element_blank(), 
            plot.title = element_text(color="darkgreen", size=20),
            plot.subtitle = element_text(color="darkgreen", size=20))
    
    if((nn(moms[2])/nn(input$sd)) == 1 & (nn(moms[1]) + nn(input$mu)) == 0){
      plt <- plt + ggtitle("You got it!", subtitle = "Your distribution is Standard Normal")
    }
    suppressMessages(suppressWarnings(
    print(plt)
    ))
  }) # plot
  output$moments <- renderUI({
    txt <- paste("$$\\mu = ", nn(mu1), ",\ \\sigma^2 = ",nn(sd1)^2, "$$")
    withMathJax(helpText(txt))
  })
  
  moments <- observeEvent(input$play, {
     
      mu <- round(runif(1, -5, 5), 0)
      sd <- round(runif(1, 1, 5), 0)
      output$distPlot <- renderPlot({
        moms <- c(mu, sd)
       
        plt <- ggplot(data.frame(x = 0))+
          xlim(c(-15, 15)) +
          stat_function(fun = dnorm, args = list((nn(moms[1]) + nn(input$mu)), (nn(moms[2])/nn(input$sd))), geom = "line", aes(color = "Your transformation")) +
          stat_function(fun = dnorm, args = list(0,1), geom = "line", aes(color = "Standard Normal") ) + 
          theme_bw() +
          theme(legend.title = element_blank(), 
                plot.title = element_text(color="darkgreen", size=20),
                plot.subtitle = element_text(color="darkgreen", size=20))
         
        if((nn(moms[2])/nn(input$sd)) == 1 & (nn(moms[1]) + nn(input$mu)) == 0){
        plt <- plt + ggtitle("You got it!", subtitle = "The distribution is Standard Normal")
        }
        suppressMessages(suppressWarnings(
          print(plt)
        ))
      }) # plot
      
      output$moments <- renderUI({
        txt <- paste("$$\\mu = ", nn(mu), ",\ \\sigma^2 = ",nn(sd)^2, "$$")
        withMathJax(helpText(txt))
      })
  }) # observe
  

}

shinyApp(ui = ui, server = server)


