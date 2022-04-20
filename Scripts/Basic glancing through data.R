#install.packages("pacman")
library(pacman)
p_load(rio, tidyverse, here, epikit, janitor,apyramid,gtsummary,skimr)

library(gtsummary)

df1 <- import(here("Data", "age_sex_data.csv"))


## for a quick overview

glimpse(df1)

summary <- df1 %>% 
  tbl_summary()
summary

skim(df1)

#find incomplete cases

incomplete <- df1 %>%  
  filter(!complete.cases())

df1 <-  df1 %>%  
  clean_names() %>%                                                               #### remove all capital letters and spaces
  rename(age = age) %>%                                               ### rename any columns that need renamin NV = OV  
  mutate(age_cat = age_categories(age, breakers = c(10,15,20,25,30))) %>%
  mutate(age_cat1 = recode(age_cat,
                           "10-14" = "teen",
                           "15-19" = "teen",
                           "20-24" = "twenties",
                           "25-29" = "twenties",
                           "30+" = "thirties" )) %>% 
 mutate(type = paste (gender , age_cat1, sep = " ")) %>% 
  mutate(state = recode(state, 
                        "kerala" = "kerala",
                        "Kerala" = "kerala",
                        "kl" = "kerala",
                        "KL"= "kerala",
                        "krla" = "kerala",
                        "Tamil Nadu"= "tamil nadu",
                       "tamizh nadu" = "tamil nadu",
                        "TN" = "tamil nadu")) %>% 
  
         
         

df1 %>% 
  tbl_summary
           
                             
 