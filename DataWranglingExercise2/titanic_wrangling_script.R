# File-Name:      titanic_wrangling_script.R
# Date:           2017-10-17
# Author:         Monisha Gopal
# Purpose:        Explore some options for dealing with missing values 
#                 in the titanic dataset. Original problem found here:
#                 https://www.kaggle.com/c/titanic
#
# Data Used:      data/titanic_original.csv
# Packages Used:  dplyr


## Library/Source -------------------------------
library('dplyr')



## Load data ------------------------------------
titanic_data <- read.csv(file.path('data', 'titanic_original.csv'))



## Clean data -----------------------------------

# Replace missing values in 'embarked' column with 'S' because 'S' is the mode
titanic_data$embarked[is.na(titanic_data$embarked)] <- "S"

# Replace missing values in the 'age' column with the mean age
mean_age <- mean(titanic_data$age, na.rm = TRUE)
titanic_data$age[is.na(titanic_data$age)] <- mean_age

# Replace missing values in the 'boat' column with a dummy value
titanic_data$boat[is.na(titanic_data$boat)] <- "None"

# Create an indicator variable for missing cabin number
titanic_data <- mutate(titanic_data, has_cabin_number = ifelse(is.na(cabin), 1, 0))



## Save data ------------------------------------
write.csv(titanic_data, 'titanic_clean.csv')
