library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Hypothesis testing"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Sample", style = "color:darkgreen"),
      sliderInput("ndraws",
                  "Sample Size",
                  min = 2,
                  max = 1000,
                  value = 500),
     
      h3("Null Hypothesis (H0)", style = "color:blue"),
      sliderInput('h0', "Average listening time of WU students is equal to",
                  min = 0.6, max = 0.8, step = 0.01, value = 0.7),
      sliderInput("ci",
                  "Confidence Interval (in %)",
                  min = 0, max = 100, step = 1, value = 95),
      withMathJax(),
      uiOutput("alpha"),
      checkboxInput('var_known', "Population Variance known?"),
      conditionalPanel("input.var_known",
                       withMathJax(helpText(("$$Z_{CAL} = {\\bar x - H0 \\over \\sigma / \\sqrt{n}}$$"))),
                       uiOutput("z_stat")),
      conditionalPanel("!input.var_known", 
                       withMathJax(helpText(("$$t_{CAL} = {\\bar x - H0 \\over s / \\sqrt{n}}$$"))),
                       uiOutput("t_stat")
                       ),
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
    ssd <- sd(plt_dat$x)
    plt <- ggplot(plt_dat, aes(x = x), color = 'darkgreen') +
      geom_histogram(bins = bins, mapping = aes(y = ..density..), fill = 'darkgreen') +
      labs(y = 'Density', x = 'Value', subtitle = sprintf("Sample Mean: %.3f; Sample SD: %.3f", smean, ssd)) +
      geom_vline(xintercept = input$h0, color = "blue") + 
      ggtitle("Music Listening Times of WU Students (avg. hours/day)") +
      theme_bw()
    show_population <- input$show_pop
    if(show_population){
      plt <- plt + stat_function(fun = dgamma, args = list(input$shp, input$rte), geom = "line", color = "black") +
        geom_vline(xintercept = mean, color = "black")
    }

    suppressMessages(suppressWarnings(
    print(plt)
    ))
  })
  
  output$confPlot <- renderPlot({
    dat <- as.vector(sample())
    gsd <- as.numeric(sqrt(input$shp / (input$rte)^2))
    se_mean <- as.numeric(gsd / sqrt(input$ndraws))
    se_mean_unk <- as.numeric(sd(dat) / sqrt(input$ndraws))
    mu <- as.numeric(input$h0)
    p1 <- (1 - (input$ci/100))/2
    p2 <- 1-p1
    min <- -4 * se_mean
    min_u <- -8
    max <-  4 * se_mean
    max_u <- 8
    if(input$ndraws == 2){
      min_u <- -15
      max_u <- abs(min_u)
    }
    smean <- as.numeric(mean(dat))
    df <- as.numeric(input$ndraws - 1)
    plt <- ggplot(data.frame(x = smean)) +
      ggtitle("Implied Confidence Interval of the H0") +
      theme_bw() +
      labs(x = "Difference to H0")
    var_known <- input$var_known
    if(var_known){
      plt <- plt +
        xlim(c(min_u, max_u)) +
        labs(subtitle = "Difference to H0 - standard normal distribution") +
        geom_vline(xintercept = (smean - mu)/se_mean, color = 'darkgreen') +
        stat_function(fun = dnorm, args = list(0, 1), geom = "line", color = "blue") +
        stat_function(fun = dnorm, args = list(0, 1), xlim = c(min_u, qnorm(p1, mean = 0, sd = 1)), geom = "area",
                      color = 'blue', fill = 'blue', alpha = 0.6) +
        stat_function(fun = dnorm, args = list(0, 1), xlim = c(max_u, qnorm(p2, mean = 0, sd = 1)), geom = "area",
                      color = 'blue', fill = 'blue', alpha = 0.6) 
    }else{
      plt <- plt +
        xlim(c(min_u, max_u)) +      
        labs(subtitle = "Difference to H0 - t-distribution") +
        geom_vline(xintercept = (smean - mu)/se_mean_unk, color = 'darkgreen') +
        stat_function(fun = dt, args = list(df), geom = "line", color = "blue") +
        stat_function(fun = dt, args = list(df), geom = "area", xlim = c(min_u, qt(p1, df = df)), 
                      color = "blue", fill = 'blue', alpha = 0.6) +
        stat_function(fun = dt, args = list(df), geom = "area", xlim = c(max_u, qt(p2, df = df)), 
                      color = "blue", fill = 'blue', alpha = 0.6)
    }
    show_population <- input$show_pop
    if(show_population){
     plt <- plt +
       geom_vline(xintercept = input$shp/input$rte - mu, color = 'black')
    }
    suppressMessages(suppressWarnings(
    print(plt)
    ))
  })
  
  output$alpha <- renderUI({
    txt <- paste0("$$\\text{Significance Level (}\\alpha\\text{): }", ((1 - (input$ci/100))), "$$")
    withMathJax(helpText(txt))
  })
  
  output$t_stat <- renderUI({
    dat <- as.vector(sample())
    xbar <- mean(dat)
    s <- sd(dat)
    n <- length(dat)
    se <- s / sqrt(n)
    tstat <- (xbar - input$h0) / se 
    p1 <- (1 - (input$ci/100))/2
    p2 <- 1-p1
    df <- as.numeric(input$ndraws - 1)
    critl <- qt(p1, df)
    crith <- qt(p2, df)
    txt <- sprintf("$$t_{CAL} = {%.3f - %.3f \\over %.3f / \\sqrt{%d}}  =  %.3f \\\\ \\text{Critical Values:} %.3f,\\ %.3f$$", xbar, input$h0, s, n, tstat, critl, crith)
    withMathJax(helpText(txt))
  })
  
  output$z_stat <- renderUI({
    dat <- as.vector(sample())
    xbar <- mean(dat)
    n <- length(dat)
    sig <- sqrt(input$shp/input$rte^2)
    sigx <- sig/sqrt(n)
    zstat <- (xbar - input$h0) /  sigx
    p1 <- (1 - (input$ci/100))/2
    p2 <- 1-p1
    critl <- qnorm(p1)
    crith <- qnorm(p2)
    txt <- sprintf("$$Z_{CAL} = {%.3f - %.3f \\over %.3f / \\sqrt{%d}}  =  %.3f \\\\ \\text{Critical Values:} %.3f,\\ %.3f$$", xbar, input$h0, sig, n, zstat, critl, crith)
    withMathJax(helpText(txt))
  })
  
  output$mu_gamma <- renderText({
    mean <- input$shp/input$rte
    sig <- sqrt(input$shp/input$rte^2)
    sprintf("Population Mean: %.3f; Population SD: %.3f", mean, sig)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

