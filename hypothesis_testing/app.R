library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Hypothesis testing"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Sample Selection", style = "color:darkgreen"),
      sliderInput("ndraws",
                  "Sample Size",
                  min = 1,
                  max = 1000,
                  value = 500),
     
      h3("Null Hypothesis (H0)", style = "color:blue"),
      sliderInput('h0', "Average listening time of WU students is equal to",
                  min = 0, max = 2, step = 0.01, value = 0.6),
      sliderInput("ci",
                  "Confidence Interval (in %)",
                  min = 0, max = 100, step = 1, value = 95),
      checkboxInput('var_known', "Population Variance known?"),
      checkboxInput('show_pop', "Show Population?"),
      conditionalPanel("input.show_pop",
                       h3("Population Parameters", style = "color:black"),
                       sliderInput('shp', "Shape", min = .1, 15, value = 7, step = 0.1),
                       sliderInput('rte', "Rate", min = .1, 15, value = 10, step = 0.1),
                       textOutput("mu_gamma")
      )
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("confPlot")
    )
  )
)

server <- function(input, output) {
  
  sample <- reactive({
    set_population <- input$show_pop
    draws <- input$ndraws
    if(set_population){
      set.seed(1)
      shape <- input$shp
      rate <- input$rte
      out <- rgamma(draws, shape, rate)
    }else{
      set.seed(1)
      out <- rgamma(draws, 7, 10)
    }
  })
  
  output$distPlot <- renderPlot({
    bins <- 50
    mean <- input$shp/input$rte
    plt_dat <- data.frame(x = sample())
    smean <- mean(plt_dat$x)
    plt <- ggplot(plt_dat, aes(x = x), color = 'darkgreen') +
      geom_histogram(bins = bins, mapping = aes(y = ..density..), fill = 'darkgreen') +
      labs(y = 'Density', x = 'Value', subtitle = sprintf("Sample Mean: %.3f", smean)) +
      geom_vline(xintercept = input$h0, color = "blue") + 
      ggtitle("Music Listening Times of WU Students (avg. hours/day)") +
      theme_bw()
    show_population <- input$show_pop
    if(show_population){
      plt <- plt + stat_function(fun = dgamma, args = list(input$shp, input$rte), geom = "line", color = "black") +
        geom_vline(xintercept = mean, color = "black")
    }

    #suppressMessages(suppressWarnings(
    print(plt)
    #))
  })
  
  output$confPlot <- renderPlot({
    pop_var_known <- input$var_known
    dat <- as.vector(sample())
    if(pop_var_known){
      gsd <- as.numeric(sqrt(input$shp / (input$rte)^2))
      se_mean <- as.numeric(gsd / sqrt(input$ndraws))
      mu <- as.numeric(input$h0)
      p1 <- (1 - (input$ci/100))/2
      p2 <- 1-p1
      min <- mu - 4 * se_mean
      max <- mu + 4 * se_mean
      smean <- as.numeric(mean(dat))
      print(smean)
      plt <- ggplot(data.frame(x = smean)) + 
        geom_vline(xintercept = smean, color = 'black') +
        xlim(c(mu-8*se_mean, mu + 8*se_mean)) + 
        stat_function(fun = dnorm, args = list(mu, se_mean), geom = "line", color = "blue") +
        stat_function(fun = dnorm, args = list(mu, se_mean), xlim = c(min, qnorm(p1, mean = mu, sd = se_mean)), geom = "area",
                      color = 'blue', fill = 'blue', alpha = 0.6) +
        stat_function(fun = dnorm, args = list(mu, se_mean), xlim = c(max, qnorm(p2, mean = mu, sd = se_mean)), geom = "area",
                      color = 'blue', fill = 'blue', alpha = 0.6) +
        
        ggtitle("Implied Confidence Interval of the H0") +
        theme_bw()
      
    }
    #suppressMessages(suppressWarnings(
    print(plt)
    #))
  })
  
  
  output$mu_gamma <- renderText({
    mean <- input$shp/input$rte
    sprintf("Population Mean: %.3f", mean)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

