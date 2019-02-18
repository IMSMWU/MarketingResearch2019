library(shiny)
library(ggplot2)
ui <- fluidPage(
   titlePanel("Standardizing a Normal Distribution"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("mu",
                     "Mean:",
                     min = -50,
                     max = 50,
                     value = 0),
         sliderInput("sd",
                     "Standard Deviation",
                     min = 0, max = 10, value = 1),
         uiOutput("formula")
      ),
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   
  output$formula <- renderUI({
    if(input$sd == 1){
      txt <- paste("$$", input$mu, "+", input$sd, "\\times x, \\quad x \\sim N(0,1) \\\\ \\Rightarrow N(",input$mu,",",input$sd,")","$$")
      
    }else{
    txt <- paste("$$", input$mu, "+", input$sd, "\\times x, \\quad x \\sim N(0,1) \\\\ \\Rightarrow N(",input$mu,",",input$sd,"^2)","$$")
    }
    withMathJax(helpText(txt))
  })
  
  output$distPlot <- renderPlot({
    ggplot(data.frame(x = 0))+
      xlim(c(input$mu - 4 * input$sd, input$mu + 4 * input$sd)) +
      stat_function(fun = dnorm, args = list(input$mu, input$sd), geom = "line", color = "blue") +
      theme_bw()
    })
}

shinyApp(ui = ui, server = server)

