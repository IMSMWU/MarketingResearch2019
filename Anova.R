# Data Generation
set.seed(321)
hours_population_1 <- rnorm(25000, 15, 5)
set.seed(125671)
sample_1 <- sample(1:25000, size = 100, replace = FALSE)
sample_1_hours <- hours_population_1[sample_1]
sample_1_df <- data.frame(hours = round(sample_1_hours, 
                                        0), group = "A")
sample_1_df$index <- 1:100
set.seed(321)
hours_population_2 <- rnorm(25000, 25, 6)
set.seed(125672)
sample_2 <- sample(1:25000, size = 100, replace = FALSE)
sample_2_hours <- hours_population_2[sample_2]
sample_2_df <- data.frame(hours = round(sample_2_hours, 
                                        0), group = "B")
sample_2_df$index <- 1:100
set.seed(321)
hours_population_3 <- rnorm(25000, 35, 6)
set.seed(125678)
sample_3 <- sample(1:25000, size = 100, replace = FALSE)
sample_3_hours <- hours_population_3[sample_3]
sample_3_df <- data.frame(hours = round(sample_3_hours, 
                                        0), group = "C")
sample_3_df$index <- 1:100
hours_abc <- rbind(sample_1_df, sample_2_df, sample_3_df)



# Overview
library(tidyverse)
library(DT)
spread(hours_abc, group, hours) %>% datatable

# Plot of data for each group
ggplot(hours_abc, aes(y = hours, x = index, color = group)) +
  geom_point() +
  facet_grid(~group) +
  labs(x = "", y='listening time (hours)') +
  theme_bw() +
  theme(legend.position = 'none', axis.ticks.x = element_blank(), axis.text.x=element_blank())


# Plot on slide 9
grandmean <- mean(hours_abc$hours)

ind <- ggplot(hours_abc, aes(x=index,y=hours,color=group)) + 
  geom_point(size=1) + facet_grid(~group, scales = "free_x") + 
  scale_x_continuous(breaks = c(1, seq(10, 100, 10),100)) + 
  geom_hline(aes(yintercept = grandmean,color=group)) +
  labs(x = "",y = "Listening Time", colour="Group",size=11, fill="") +
  geom_segment(aes(x=index,y=grandmean, xend=index, yend=hours),size=.5) + 
  theme(axis.title = element_text(size = 12),
        axis.text  = element_text(size=12),
        strip.text.x = element_text(size = 12),
        legend.position="none")+ theme_bw()+
  theme(legend.position = 'none', axis.ticks.x = element_blank(), axis.text.x=element_blank())


groupmean <- tapply(hours_abc$hours, hours_abc$group, mean)

groupmean <- sort(rep(groupmean, 100))
  
grp <- ggplot(hours_abc, aes(x=index,y=hours,color=group)) + 
  geom_point(size=1) + facet_grid(~group, scales = "free_x") + 
  scale_x_continuous(breaks = c(1, seq(10, 100, 10), 100)) + geom_hline(aes(yintercept = grandmean,color=group)) +
  geom_hline(aes(yintercept = groupmean,color=group)) +
  labs(x = "",y = "Listening Time", colour="group",size=11, fill="") +
  geom_segment(aes(x=index,y=grandmean, xend=index, yend=groupmean),size=.3, arrow = arrow(length = unit(0.03, "npc"))) + 
  theme_bw()+
  theme(legend.position = 'none', axis.ticks.x = element_blank(), axis.text.x=element_blank())


nogrp <- ggplot(hours_abc, aes(x = index, y = hours)) +
  geom_point() +
  labs(x = "",y = "Listening Time", colour="group",size=11, fill="") +
  geom_hline(aes(yintercept = grandmean)) +
  theme_bw()+
  theme(legend.position = 'none', axis.ticks.x = element_blank(), axis.text.x=element_blank())


gridExtra::grid.arrange(ind, grp, nogrp)
# End: Plot on slide 9


# Residual SS
ggplot(hours_abc, aes(x=index,y=hours,color=group)) + 
  geom_point(size=1) + facet_grid(~group, scales = "free_x") + 
  scale_x_continuous(breaks = c(1,seq(10,100,10))) + geom_hline(data = data.frame(m = groupmean, group = c(rep("A", 100), rep("B", 100), rep("C", 100))), aes(yintercept = m,color=group)) +
  labs(x = "",y = "Listening Time", colour="group",size=11, fill="") +
  geom_segment(aes(x=index,y=groupmean, xend=index, yend=hours),size=.5) + 
  theme(legend.position = 'none', axis.ticks.x = element_blank(), axis.text.x=element_blank())
+ theme_bw()

# Anova in R
music_mod <- aov(hours ~ group, data = hours_abc)
summary(music_mod)

# Manual calculation  # DEMO
# Model
sq_groups <-  (tapply(hours_abc$hours, hours_abc$group, 
                      mean) - mean(hours_abc$hours))^2
sq_groups
SSM <- sum(100 * sq_groups) # each group has 100 observations
SSM

dfm <- length(unique(hours_abc$group)) - 1
dfm

MSM <- SSM/dfm
MSM

# Residual sum of squares
SSR <- sum((hours_abc$hours - rep(tapply(hours_abc$hours, 
                                     hours_abc$group, mean), each = 100))^2)
SSR

dfr <- nrow(hours_abc) - length(unique(hours_abc$group))
dfr

MSR <- SSR/dfr
MSR

# F-statistic
F_stat <- MSM/MSR
F_stat
critical_value <- qf(0.95, df1 = dfm, df2 = dfr)
critical_value

F_stat > critical_value # if TRUE reject H0

list(Df = c(dfm, dfr), Sum_Sq = c(SSM, SSR), Mean_Sq = c(MSM, MSR), F_value =  F_stat)
summary(music_mod)

# p-value calculation just rounds to 0...
1-pf(F_stat, dfm, dfr) # 1 - since p* is cummulative probability up to a point
