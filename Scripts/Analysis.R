#Loading libraries and [a]
library(pacman)
library(gtsummary)
library(rio)
library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(flextable)
library(rstatix)
library(epitools)

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 6,
  background.color = "#EFEFEF")



#importing dataset
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

###################################################################################################################
## Univariate Analysis Chain
df1 <-df1 %>% 
  clean_names() %>%                                                               #### remove all capital letters and spaces
  remove_empty() %>% 
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
  mutate(final_score_cat =cut(final_score, breaks=c(-Inf, 11, 14, Inf), labels=c("low","middle","high"))) %>%# recode new_var
  mutate(final_score_dichot = cut(final_score, breaks= c(-Inf, 13, Inf), labels = c("low", "high")))

summary <- df1 %>% 
  gtsummary::tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                          all_categorical() ~ "{p}% ({n} / {N})"))
summary
         
################################################################################
#Bivariate Analysis

# Testing Hypothesis between two categorical variables
# 2 x 2 table 
score_vs_gender <- df1 %>% 
  tabyl(final_score_dichot, gender) %>% 
  adorn_totals(where = "row") %>% 
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") 

# Odds Ratio (Where appropriate)
score_vs_gender_tbl <- table(df1$final_score_dichot, df1$gender)
or <- oddsratio.wald(score_vs_gender_tbl, rev = c("both"))
rr <- riskratio.wald(score_vs_gender_tbl, rev = c("both"))






# Convert to flextable
score_vs_gender_flex  <- score_vs_gender %>% 
  flextable::flextable() %>%                     
  flextable::autofit() %>% 
  set_caption(caption = "Table : Score vs Gender") %>% 
  set_header_labels(final_score_dichot = "Score") %>% 
  add_footer_lines(paste("p value: ", round( or$p.value[6], 4))) %>%           
  add_footer_lines(paste("OR: ", 
                         
                         round((or$measure[2]), 4),
                         
                         " ( ",
                         
                         round((or$measure[4]), 4),
                         
                         "-" , 
                         
                         round((or$measure[6]), 4),
                         
                         ")"
                         
  )) %>% 
  add_footer_lines(paste("RR: ", 
                         
                         round((rr$measure[2]), 4),
                         
                         " ( ",
                         
                         round((rr$measure[4]), 4),
                         
                         "-" , 
                         
                         round((rr$measure[6]), 4),
                         
                         ")"
                         
  ))


score_vs_gender_flex

# Testing hypothesis between 2 means - 

df1 %>% 
  #group_by(gender) %>% 
  mean(final_score)

diff_means <- df1 %>% 
  group_by(gender) %>% 
  summarise(mean = mean(final_score)) 


t_test <- df1 %>% 
  t.test(final_score ~ gender, data = . , alternative = "two.sided")






###########################################################################################################################\
# Framework for cleaning (Janitor)

#df_clean <- import(here("Data", "dirty_data.xlsx")) %>%    
  #row_to_names(1,remove_row =TRUE,  remove_rows_above = TRUE) %>%                    #Assign any row to be the header row
  #clean_names() %>%                                                                  #convert to snake case
  #remove_empty() %>%                                                                 #remove any empty rows or columns
  #remove_constant() %>%                                                              #
  #mutate(hire_date = as.numeric(hire_date)) %>% 
  #mutate(hire_date = excel_numeric_to_date(hire_date))







