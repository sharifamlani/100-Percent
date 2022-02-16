#Sharif Amlani
#R 4.1.1
#Winter 2021

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(bbr)
library(ggplot2)
library(sjPlot)
library(ggrepel)

######################## Upload Data ##################

#Upload Data
Years <- seq(1970, 2020, by = 1)

NBA.Final <- NULL
for(i in Years){
NBA.Loop <- get_season(i)

NBA.Final <- rbind(NBA.Final, NBA.Loop)
}

NBA.1 <- NBA.Final
######################## Examine Data ##################
head(NBA.1)

######################## Data Management ##############
NBA.1$fg_pct_100 <- NBA.1$fg_pct*100

######################## Correlation ##################
cor(NBA.1$fga, NBA.1$fg, use = "complete")
cor(NBA.1$fga, NBA.1$pts, use = "complete")
cor(NBA.1$fga, NBA.1$fg_pct, use = "complete")

######################## Plot ##################
#Plot 1
P1<- ggplot2::ggplot(NBA.1, aes(x = fga, y = pts)) +
  geom_point(color = "black", alpha = 0.2) +
  stat_smooth(method = "lm", color = "orange") +
  theme_minimal() +
  labs(x = "Field Goals Attempted Per Season",
       y = "Points Scored Per Season",
       title = "You Miss 100 Percent of the Shots You Don't Take",
       subtitle = paste("Field Goals Attempted vs. Points Scored\nTotal Correlation = ", round(cor(NBA.1$fga, NBA.1$pts, use = "complete"), 2), sep = "")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom") +
  geom_text_repel(data = subset(NBA.1, fga > 2100 & pts > 2600), aes(label = player), alpha = 1, size = 3,
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50'); P1 


#Plot2
Results <- lm(pts ~ fga * fg_pct_100, data = NBA.1); summary(Results)
plot_model(Results, type = "pred", terms = c("fga", "fg_pct_100")) 

Output.1 <- data.frame(get_model_data(Results, type = "pred", terms = c("fga", "fg_pct_100")))

P2<- ggplot() +
  geom_point(data = NBA.1, aes(x = fga, y = pts), color = "black", alpha = 0.1) + # must include argument label "data"
  stat_smooth(data = Output.1, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = group)) +
  geom_ribbon(data = Output.1, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  theme_minimal() +
  labs(x = "Field Goals Attempted Per Season",
       y = "Points Scored Per Season",
       title = "Points Scored as a Function of Field Goals Attempted and Shooting Percentage",
       caption = "Estimates Generated Using Linear Model.",
       fill = "Scoring Percentage",
       color = "Scoring Percentage") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        legend.position = "bottom") +
  geom_text_repel(data = subset(NBA.1, fg_pct_100 > 50 & fga > 1900), aes(x = fga, y = pts, label = player),
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50'); P2



setwd("C:/Users/Shari/OneDrive/University of California, Davis/Research Projects/Basketball/Analysis/100 Percent Analysis/Figures")

ggsave(P1, 
       file = "Figure 1 - 100 Percent.png",
       width = 7, height = 6,  dpi = 300)

ggsave(P2, 
       file = "Figure 2 - 100 Percent.png",
       width = 7, height = 6,  dpi = 300)
