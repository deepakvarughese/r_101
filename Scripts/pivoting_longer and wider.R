library(tidyverse)
library(janitor)
library(rio)
library(here)

df <- import(here("Data", "dirty_data_1.xlsx")) %>% 
  clean_names()



#pivoting longer as variable names also contain data
df_long <- pivot_longer(df, 
                    cols = course_1_2009 : course_2_2010, 
                    names_to = "course_year",
                    values_to = "value")

#seperating course amd year into seperate variables. ##note that we split into 3 because of the presence of 3 underscores. 
df_long1 <- separate(df_long, course_year, c("course","course_number", "year")) %>% 
  select(-course)


#total_number_of people _who attended_courses_by_year
participants_by_year <-df_long1 %>% 
  filter(value == "Yes") %>% 
  group_by(year) %>% 
  summarize(count = n())

#total_number_of_people_who_attended_courses_by_course
participants_by_course <-df_long1 %>% 
  filter(value == "Yes") %>% 
  group_by(course_number) %>% 
  summarize(count = n())

       