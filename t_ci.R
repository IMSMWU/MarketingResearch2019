ggplot(data.frame(x=0)) +
  lims(x = c(-4,4)) +
  stat_function(fun = dt, args = list(50), geom = 'line') +
  stat_function(fun = dt, args = list(50), geom = 'area', xlim = c(-4, qt(0.025, 50))) +
  stat_function(fun = dt, args = list(50), geom = 'area', xlim = c(4, qt(0.975, 50))) +
  theme_bw() +
  labs(title = "95% Confidence interval of t-distribution", subtitle="with 50 degrees of freedom")
