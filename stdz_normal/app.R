#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Standardizing a Normal Distribution"),
   
   # Sidebar with a slider input for number of bins 
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
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
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

# Run the application 
shinyApp(ui = ui, server = server)

