#install.packages("pacman")
library(pacman)
p_load(rio, tidyverse, here, epikit, janitor,apyramid,gtsummary,skimr,forcats)

library(gtsummary)

df1 <- import(here("Data", "age_sex_data.csv"))


## for a quick overview

glimpse(df1)

summary <- df1 %>% 
  tbl_summary()

summary

skim(df1)

#find incomplete cases
which(is.na(df1$score_1))
which(is.na(df1$score_2))
df1[!complete.cases(df1), ]

## Analysis Chain
df1 <-df1 %>% 
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
  mutate(state = recode(state,                                          ## Correct spelling errors
                        "kerala" = "kerala",
                        "Kerala" = "kerala",
                        "kl" = "kerala",
                        "KL"= "kerala",
                        "krla" = "kerala",
                        "Tamil Nadu"= "tamil nadu",
                       "tamizh nadu" = "tamil nadu",
                        "TN" = "tamil nadu")) %>% 
  mutate(ses = recode(ses,                                              ## Correct spelling errors
                        "High" = "high")) %>% 
  mutate(ses = fct_relevel(ses, "low", "medium", "high")) %>% 
  mutate(score_2 = na_if(score_2, "A")) %>%                            ## Changing all the A to NA
  mutate(score_2 = na_if(score_2 , "")) %>%                            ## Changing blank spaces to NA
  mutate(score_1 = na_if(score_1 , "")) %>% 
  #which(is.na(df1$score_2)) %>% 
  #df1[!complete.cases(df1), ] #run seperately in console ,out of pipe chain %>% 
  mutate(score_1 = as.integer(score_1)) %>%                             ## Convert to integer
  mutate(score_2 = as.integer(score_2)) %>% 
  mutate(score_1 = ifelse(is.na(score_1),
                            median(score_1, na.rm = T),                ## replace missing with median
                            score_1)) %>% 
  drop_na(score_2) %>%                                                ## remove missing values
  mutate(final_score = score_1 + score_2) %>%                         ## compute new variables
  mutate(final_score_cat =cut(final_score, breaks=c(-Inf, 11, 14, Inf), labels=c("low","middle","high"))) # recode new_var
  

## Inf to 11 - Including 11 -> Low
## 12 to 14 - including 14 -> middle
## 15 and above -> high

summary <- df1 %>% 
  gtsummary::tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                          all_categorical() ~ "{p}% ({n} / {N})"))
summary
         

                             
## Visualizing the individual tables with gt table
## Visulizing plots with ggplot

