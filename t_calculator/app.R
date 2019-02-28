library(shiny)

ui <- fluidPage(
   titlePanel("Confidence Interval Calculator"),
   
   sidebarLayout(
      sidebarPanel(
         numericInput("mean",
                     "Sample mean:",
                     value = 0),
         numericInput("sd",
                      "Sample Standard Deviation",
                      value = 1),
         numericInput("n", 
                      "Sample size:",
                      value = 50),
         sliderInput('CI', "Confidence Interval (%):", min = 1, max = 99, step = 1, value = 95)
      ),
      
      mainPanel(
         textOutput("result")
      )
   )
)


server <- function(input, output) {
   
   output$result <- renderText({
     mu <- input$mean
     sd <- input$sd
     n <- input$n
     quant <- input$CI
     
     se <- sd/sqrt(n)
     
      sprintf("$$
              \\bar{x} = %.2f \\\\
              SE_{\\bar{x}} = %.2f \\\\
              t = %.2f \\\\
              CI = \\bar{x]} \\pm t \\times SE_{\\bar{x}} = [%.2f, %.2f] \\\\
              ", mu, )
   })
}


shinyApp(ui = ui, server = server)

