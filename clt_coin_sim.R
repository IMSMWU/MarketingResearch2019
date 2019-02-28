library(ggplot2)
set.seed(1)
population <- c("heads", "tails")
sample_size <- 100 # Needs to be "large"
samples <- 100000 # Needs to be "LARGE"

mean_heads <- vector(mode = "integer", length = samples)

for(i in 1:samples){
  sample <- sample(population, size = sample_size, replace = TRUE)
  heads_in_samp <- sum(sample == 'heads')
  mean_heads[i] <- heads_in_samp / sample_size
}

mean(num_heads)
sd(num_heads)


(plt <- ggplot(data.frame(x = num_heads), aes(x=x, y = ..density..)) +
    geom_histogram(fill = 'blue', color = 'white', bins= 100) +
  theme_bw())

se <- sqrt( 0.25) / sqrt(sample_size)

ggplot(data.frame(x=0)) +
  lims(x = c(.3, .7)) + 
  stat_function(fun = dnorm, args = list(mean = mean(num_heads), sd = se), geom = 'line', color = 'red')

