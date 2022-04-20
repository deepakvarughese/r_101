library(pacman)
p_load(rio, here,skimr, janitor, tidyverse, rstatix,gtsummary)
df1 <- import(here("Data","age_sex_data.csv"))
columns(df1)
skim(df1)    # using skim function from the skimr package to get an overview 
glimpse(df1)  # looking at all the data types



df1 <- df1 %>% 
  clean_names() %>%                          # remove all caps and spaces and create a uniform column name
  rename(age_of_patient = age) %>%           # demonstrating how to change a column name by changing "age" to "age of patient"
  rename(age = age_of_patient)

# renaming it back to age

names(df1)                   # display the column names if needed 


df1$index <- 1:nrow(df1)  # Create a column for index number
df1$age[11] = NA          # Manually changing value in 11th row to NA for learning purpose
sum(is.na(df1$age))       # Identify number of missing values in the column
which(is.na(df1$age))     # Identify which value is missing in the column. 
df1[ , 'column1'] <- NA   # Adding an empty column for learning purposes
df1 <- df1 %>% discard(~all(is.na(.) | . ==""))  # Remove all missing columns
df1[nrow(df1)+1,] <- NA    # Add empty rows in R   
df1 <- na.omit(df1)        # Remove empty rows in R



# For all quantitative variables

# Check for missing values
sum(is.na(df1$age))
# Sort Ascending to see for any abnormal values
df1[order(df1$age), ]
# Sort Descending to see for any abnormal values
df1[order(-df1$age), ]


range(df1$age)
mean(df1$age, na.rm = TRUE)
median(df1$age, na.rm = TRUE)
sd(df1$age,na.rm = TRUE )
IQR(df1$age,na.rm = TRUE )
hist(df1$age, na.rm = TRUE)

qqnorm(df1$age, main=' QQ Plot for Age')
qqline(df1$age)
shapiro.test(df1$age)         # If p is less than 0.05 data is not normal. If p is more than 0.05 data is normal. 

## For all categorical variables
unique(df1$gender)

## introducing some errors for learning
df1$gender[20] = "FEMALE"
df1$gender[25] = "MaLE"
df1$gender[14] = "fmalLle"


#correcting spelling errors / recoding variables
df1$gender[df1$gender == "fmalLle" | df1$gender == "female" ] <- "female"
df1$gender[df1$gender == "MaLE" ] <- "male"
str_to_lower(df1$gender)
table(df1$gender)
df1$gender <-  as.factor(df1$gender)


summary(df1)
get_summary_stats(df1) 

df1 %>%
  group_by(gender) %>%
  rstatix::get_summary_stats(age)
  
df1 %>% 
  select(age,Gender) %>% 
  tbl_summary(by=Gender) %>%
  add_p()


