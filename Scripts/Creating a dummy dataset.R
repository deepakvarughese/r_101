library(here)
library(dplyr)
library(rio)

## Task : Create a dummy dataset with some demographic variables and some study variables ====
## Demographic Variable 
## age , gender, SES, 
## Study Variables 
###Score 1, Score 2 , Score 3
### factor 1, factor 2 , factor 3

## Creating a dummy age variable ====
age <- round(rnorm(50, mean = 20 , sd = 4),0)     ### generate a variable age with 50 observations, having a mean value of 50 and an sd of 2
                                                  ### the variable should be rounded off to 0 digits
#hist(age) - generating a rough distribution
# range(age) - checking for extreme values



## Creating a dummy gender variable =====
gender <- sample(letters[1:2], 50, replace = TRUE , prob = c(0.55, 0.45))  ### generate a factor variable with 2 letters a and b  with 50                                                                               #### observations and probablty of a being 55% and b being 45%

df1 <- data.frame(age , gender)                                             ### creating a dataframe with age and gender

df1 <- df1 %>% mutate(Gender = recode(gender,"a" = "male","b" = "female"))  ### using dplyr to recode "a" as male and "b" as female
df1 <- subset(df1, select = -c(gender))                                     ## removing the  gender column with "a" and "b"
gender <- df1$Gender                                                        ## creating a variable called "gender" based on the gender                                                                                  ## variable in the dataframe 


## Writing dataframe 1 as a .csv file
export(df1, here("data", "age_sex_data.csv"))                                # using the rio and here packages we ask R to save df1 in the                                                                                #data folder

## ## Writing dataframe 1 as a .xlsx file
export(df1, here("data", "age_sex_data.xlsx"))     

 ################## End of dataset needed for age gender pyramid######################################


