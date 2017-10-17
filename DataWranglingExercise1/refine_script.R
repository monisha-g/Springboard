# File-Name:      refine_script.R
# Date:           2017-10-14
# Author:         Monisha Gopalakrishnan
# Purpose:        Take the refine_original dataset and clean it up to make
#                 it easier for analysis. Inspired by this blog post:
#                 http://d3-media.blogspot.nl/2013/11/how-to-refine-your-data.html
#
# Data Used:      data/refine_original.csv
# Packages Used:  dplyr, RecordLinkage, stringr, plyr


## Library/Source -------------------------------
library('plyr')
library('dplyr')
library('RecordLinkage')
library('stringr')



## Load data ------------------------------------
refine_data <- read.csv(file.path('data', 'refine_original.csv'))


## Clean data -----------------------------------

# Standardize company names
company_names <- c("philips", "azko", "van houten", "unilever")
refine_data <- mutate(refine_data, company = 
                                    standardize_string(company, company_names))

# Separate product code and product number
code_and_number <- str_split_fixed(refine_data$`Product code / number`, '-', 2)
refine_data$product_code <- code_and_number[,1]
refine_data$product_number <- code_and_number[,2]

# Add product categories
refine_data <- mutate(refine_data, product_category = revalue(product_code, 
                                                       c("p" = "Smartphone",
                                                         "v" = "TV",
                                                         "x" = "Laptop",
                                                         "q" = "Tablet")))

# Add full address for geocoding
refine_data <- mutate(refine_data, full_address = paste(address, city, country,
                                                               sep=","))

# Create dummy variables for company
refine_data <- mutate(refine_data, company_philips = ifelse(company == "philips", 1, 0),
                                   company_azko = ifelse(company == "azko", 1, 0),
                                   company_van_houten = ifelse(company == "van_houten", 1, 0),
                                   company_unilever = ifelse(company == "unilever", 1, 0))

# Create dummy variables for product category
refine_data <- mutate(refine_data, product_smartphone = ifelse(product_category == "Smartphone", 1, 0),
                                   product_tv = ifelse(product_category == "TV", 1, 0),
                                   product_laptop = ifelse(product_category == "Laptop", 1, 0),
                                   product_tablet = ifelse(product_category == "Tablet", 1, 0))

## Save data ------------------------------------
write.csv(refine_data, file.path('data', 'refine_clean.csv'))



