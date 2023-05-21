rm(list = ls())
getwd()
setwd("~/Desktop/251")###

library(tidyverse)
library(haven)
library(naniar)


olemiss <- read.csv("olemiss2021.csv")

ggplot(data = olemiss, aes(x = EPA , y = yards_gained))+
  geom_point()


umPalette <- c("#CE1126", "#006BA6" , "#14213D")

olemiss %>%
  mutate(play = case_when(pass == 1 ~ "Pass", 
                          rush == 1 ~ "Rush",
                          punt == 1 ~ "Kick",
                          fg_inds == 1 ~ "Kick",
                          TRUE ~ "Drop")) %>%
  filter(play != "Drop") %>% 
  ggplot(aes(x = yards_to_goal, y = EPA))+
  geom_point(aes(color = as.factor(play), size = 2, alpha = .75))+
  geom_hline(yintercept = 0)+
  labs(title = "Ole Miss 2021 Offense",
       y = "Expected Points Added",
       x = "Yards to Goal")+
  scale_color_manual(name = "Play Type", values = umPalette)+
  facet_wrap(~ down)+
  guides(alpha = FALSE, size = FALSE)


