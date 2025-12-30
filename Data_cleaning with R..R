#Cleaning messy CSV data with R using Tidyverse,stringr,lubridate & Janitor

#Install packages (tidyverse,Janitor)
install.packages("tidyverse")
exists("Tidyverse")
library("tidyverse")
library("stringr")
library("lubridate")

install.packages("janitor")
library("janitor")

#Testing the package if is working.
library(janitor)

data.frame("First Name" = c("John", "Mary")) %>%
  clean_names()

#Using packages tidyverse, lubridate, stringr & janitor to clean CSV messy data.
messy_data <- data.frame(
  ID = c(1, 2, 2, 3, 4, 5),
  Name = c(" john doe ", "MARY ANN", "MARY ANN", "peter  ", NA, "lucy"),
  Gender = c("M", "F", "F", "male", "Female", ""),
  Age = c("25", "30", "30", "twenty", "", "28"),
  Join_Date = c("12-01-2024", "2024/02/15", "2024/02/15", "15 Mar 2024", "", "01-04-2024"),
  Salary = c("50000", "60000", "60000", "70,000", NA, "55000")
)
messy_data
library("tidyverse")
library("lubridate")
library("stringr")
library("janitor")

#Clean columns names
data<-messy_data %>% clean_names()

#Remove duplicate rows.
data<-data %>% distinct()

#Clean text columns
data$name<-data$name %>% str_trim() %>% str_to_title()

#Cleaning Gender column
data$gender <- str_to_lower(data$gender)

data$gender <- case_when(
  data$gender %in% c("m", "male") ~ "Male",
  data$gender %in% c("f", "female") ~ "Female",
  TRUE ~ NA_character_
)

#converting age column to numeric.
data$age<-as.numeric(data$age)
data$age[is.na(data$age)] <- median(data$age, na.rm = TRUE)

#Fixing date formats.
data$join_date <- parse_date_time(
  data$join_date,
  orders = c("dmy", "ymd", "d b Y")
)

#Clean salary.
data$salary <- str_remove_all(data$salary, ",")
data$salary <- as.numeric(data$salary)
data$salary[is.na(data$salary)] <- median(data$salary, na.rm = TRUE)
glimpse(data)
summary(data)

#Adding salary category column to the data.
data <- data %>%
  mutate(
    salary_category = ifelse(salary < 60000, "Low", "High")
  )
write.csv(data, "clean_data.csv", row.names = FALSE)



