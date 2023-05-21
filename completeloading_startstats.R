### These first three lines should remain the same in each script you create. 
### Be sure to make sure your working directory is set  the correct folder before starting
rm(list = ls())
getwd()
setwd("~/Box Sync/POL 251 teach/Learning R")

### Each time we reopen rstudio we will load the packages we will be using 
library(tidyverse)
library(haven)
library(naniar)

### here I am loading my dataset, selecting the variables I will use,
### renaming those variables, and removing people who didnt answer the questions 
anes <- read_dta("2020ANES_raw.dta") %>% 
  select(V201231x ,V201151, V201152, V202580 , V201368, V202504, V202325, V201234 , V202167 , V202165 ) %>% 
  rename(PID7 = V201231x , Biden_ft = V201151 , Trump_ft  = V201152 , 
         healthcare = V202580 , official_consq = V201368 , American_id = V202504 , 
         mil_tax = V202325 , for_people = V201234 , cong_ft = V202167 , sc_ft =V202165  )  %>% 
  replace_with_na(replace = list(PID7=c(-9,-8) , Biden_ft = c( -4, -9, 998) , Trump_ft = c( -4, -9, 998) ,
                                 healthcare = c(-1 , -5 , -6 , -7) , official_consq = c(-9, -8) ,
                                 American_id = c(-1 , -5 , -6 , -7 , -9), mil_tax = c(-1 , -5 , -6 , -7 , -9),
                                 for_people = c(-9, -8) , cong_ft = c( -4, -5, -6 , -7, -9) ,
                                 sc_ft = c( -4, -5, -6 , -7, -9)))

### The attach command tells most commands that come after it that I will be using the 
### that I will be using the anes dataset 
### some specific functions require us to reidentify our dataset such as the count function below
attach(anes)


mean(Biden_ft[PID7 < 4], na.rm = TRUE)
mean(Biden_ft[PID7 > 4], na.rm = TRUE)

t.test(Biden_ft[PID7< 4], Biden_ft[PID7 > 4])

unique(for_people)
t.test(American_id[for_people == 1], American_id[for_people == 2])


mean(mil_tax)
unique(mil_tax)
unique(for_people)

t.test(mil_tax[for_people == 1], mil_tax[for_people == 2])

unique(official_consq)
unique(for_people)

### These functions can tell us some useful descriptive stats about our variables
count(anes,  PID7)  
summary(PID7)
mean(PID7, na.rm = TRUE)
sd(PID7, na.rm = TRUE)

### this function creates a frequency table telling us the percent of people 
### in each category. 

### I did not know how to do this, but like most things in r (& other programming)
### if you google what your problem is or what you would like to do you are very 
### likely to find a solution
library(scales)
pervar <- "%"
anes %>%                              
  group_by(PID7) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(percent(freq))

anes %>%
  group_by(PID7) %>%
  summarise(mean_var = mean(Trump_ft, na.rm = TRUE))
  
anes %>%
  group_by(PID7) %>%
  summarise_at(c("Biden_ft" , "Trump_ft"), mean , na.rm = TRUE)

ggplot() +
  geom_point(aes(x = PID7, y = Trump_ft)) +
geom_smooth(aes(x = PID7, y = Trump_ft) , method = lm , se = FALSE)
  
cor(American_id, Biden_ft, use = "pairwise.complete.obs")


ggplot() + 
  geom_histogram(aes(x = PID7))
  

glm(Biden_ft ~ Trump_ft , data = anes)

ggplot(data = anes , aes(x = Trump_ft , y = Biden_ft))+
  geom_point()+
  geom_smooth(method = lm)
  
  
  
#n is the number of observation
unique(Bid)


