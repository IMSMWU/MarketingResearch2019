library(shiny)

# 1. Set population density + params
# 2. Set number of draws
# 3. Set sample size
# 4. Plot distribution of single sample
# 5. Plot distribution of sample mean
# 5.1 Optional: plot theoretical distribution 
# 5.1.1 Optional: plot confidence intervals
#################
# Instructions: #
#################

# To add a new distribution:
## a) Add dist. to user input
## b) Add conditional panel with dist. parameters
## c) Calculate population moments
## d) Define the random sample
## e) Add output for population mean (has to be added to the conditional panel)
library(ggplot2)

ui <- fluidPage(
  
  # Application title
  titlePanel("Central Limit Theorem"),
  
  sidebarLayout(
    sidebarPanel(
      # a) User input
      selectInput('dist', "Population Distribution", choices = 
                    c(Normal = "Normal", Gamma = "Gamma", Uniform = "Uniform")),
      h3("Population Parameters", style="color:black"),
      # b) Parameters conditional on choice of distribution
      conditionalPanel("input.dist == 'Normal'",
                       p("e.g. Net profit of a marketing campaign"),
                       sliderInput('mu', "Mean", min = -10, 10, 0, step = .1),
                       sliderInput('sd', "SD", min = .1, 5, 1, step = .1),
                       textOutput("mu_norm")),
      conditionalPanel("input.dist == 'Gamma'",
                       p("e.g. Music listening time"),
                       sliderInput('shp', "Shape", min = .1, 10, 2, step = 0.1),
                       sliderInput('rte', "Rate", min = .1, 10, 5, step = 0.1),
                       textOutput("mu_gamma")),
      conditionalPanel("input.dist ==  'Uniform'",
                       p("Equally likely events in a certain range"),
                       sliderInput('area', "Interval", min = -10, 10, c(-5, 5), step = 0.1),
                       textOutput("mu_uniform")),
      # Global sample characteristics
      h3("Sample Selection", style="color:darkgreen"),
      sliderInput('ndraws', 'Sample Size', min = 2, 1000, 500),
      # Number of samples for visualization of CLT
      h3("CLT", style="color:darkblue"), 
      sliderInput('nsamps', 'Number of Samples', min = 2, 5000, 1000),
      checkboxInput('overlay', "Re-sample and draw Normal Distribution overlay?"),
      conditionalPanel('input.overlay',
                       checkboxInput('ci', "Show Confidence Intervals?"),
      conditionalPanel('input.ci',
                       sliderInput('alpha', "Confidence Level (in %)", min = 0, max = 100, step = 1, value = 95)))
    )
    ,
    # Plots
    mainPanel(
      h1("Values of Single Sample", style="color:darkgreen"),
      plotOutput("distPlot"),
      h1("Means of Many Samples", style ="color:darkblue"),
      plotOutput("distMean")
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  bins <- 50
  
  # c) Get distribution moments conditional on choice of distribution
  moments <- reactive({
    gmu <- input$shp / input$rte
    gsd <- sqrt(input$shp / (input$rte)^2)
    umu <- sum(input$area)/2
    usd <- sqrt(1/12 * (diff(input$area))^2)
    mom <- switch(input$dist,
                  Normal = list('mu' = input$mu, 'sd' = input$sd),
                  Gamma = list('mu' = gmu, 'sd' = gsd),
                  Uniform = list('mu' = umu, 'sd' = usd)
    )
    return(mom)
  })

  # d) Get function for random draws of many samples
  whichdist_samps <- reactive({
    choice <- switch(input$dist,
                     Normal = rnorm(input$ndraws * input$nsamps, input$mu, input$sd),
                     Gamma = rgamma(input$ndraws * input$nsamps, input$shp, input$rte),
                     Uniform = runif(input$ndraws * input$nsamps, input$area[1], input$area[2])
    )
  })
  
  # Plot histogram of characteristic sample
  output$distPlot <- renderPlot({
    plt_dat <- data.frame(x = whichdist_samps()[1:input$ndraws])
    smean <- mean(plt_dat$x)
    plt <- ggplot(plt_dat, aes(x = x), color = 'darkgreen') +
      geom_histogram(bins = bins,  fill = 'darkgreen') +
      labs(y = 'Count', x = 'Value', subtitle = sprintf("Sample Mean: %.3f", smean)) +
      ggtitle("Sample Counts of Values") +
      theme_bw()
    
    suppressMessages(suppressWarnings(print(plt)))
  })
  
  # Plot histogram of sample means
  output$distMean <- renderPlot({
    samples <- matrix(whichdist_samps(), input$nsamps, input$ndraws)
    iota <- matrix(1, input$ndraws, 1)
    means <- as.vector(samples %*% iota) / input$ndraws
    mmeans <- mean(means)
    plt_dat <- data.frame(x = means)
    plt <- ggplot(plt_dat, aes(x = x), color = 'darkblue') +
      geom_histogram(mapping = aes(y = ..density..),bins = bins, fill = 'darkblue') +
      labs(y = "Density", x = "Means", subtitle = sprintf("Mean of Sample Means: %.3f", mmeans)) +
      ggtitle("Sampling Distribution of the Mean") +
      theme_bw()
    draw_overlay <- input$overlay
    # Overlay theoretical Normal
    if(draw_overlay){
      params <- moments()
      N <- input$ndraws
      mu <- as.numeric(params[1])
      sd <- as.numeric(params[2])
      se_mean <- as.numeric(sd / as.numeric(sqrt(N)))
      plt <- plt + 
        stat_function(fun = dnorm, args = list(mu, se_mean), geom = "line", color = "red")
      
      draw_ci <- input$ci
      # Overlay CI (needs normal)
      if(draw_ci){
        min <- mu - 4 * se_mean
        max <- mu + 4 * se_mean
        p1 <- (1 - (input$alpha/100))/2
        p2 <- 1-p1
        plt <- plt +
          stat_function(fun = dnorm, args = list(mu, se_mean), xlim = c(min, qnorm(p1, mean = mu, sd = se_mean)), geom = "area",
                        color = 'red', fill = 'red', alpha = 0.6) +
          stat_function(fun = dnorm, args = list(mu, se_mean), xlim = c(max, qnorm(p2, mean = mu, sd = se_mean)), geom = "area",
                        color = 'red', fill = 'red', alpha = 0.6) 
      }
    }
    suppressMessages(suppressWarnings(print(plt)))
  })
  
  # e) Get Population mean for given distribution
  output$mu_norm <- renderText({
    paste("Population Mean: ", input$mu)
  })
  
  output$mu_gamma <- renderText({
    mean <- input$shp/input$rte
    sprintf("Population Mean: %.3f", mean)
  })
  
  output$mu_uniform <- renderText({
    mean <- sum(input$area)/2
    sprintf("Population Mean: %.2f", mean)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

