# File-Name:      CHIS.R     
# Date:           2017-10-25          
# Author:         Monisha Gopal
# Purpose:        Practice data exploration using CHIS dataset (frequency/density plots,
#                 mosaic plot)
#
# Data Used:      2009 California Health Interview Study (CHIS) adult-responses
#                 (link: http://healthpolicy.ucla.edu/chis/data/public-use-data-file/Pages/2009.aspx)
# Packages Used:  dplyr, ggplot2, reshape2, haven, ggthemes

## Library/Sources ------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(haven)
library(ggthemes)



## Load data ------------------------------------
adult_raw <- read_sas("adult.sas7bdat")   #Change path to local path

# Only want following 10 variables to make exploration easier:
# RBMI: BMI Category description
# BMI_P: BMI value
# RACEHPR2: race
# SRSEX: sex
# SRAGE_P: age
# MARIT2: Marital status
# AB1: General Health Condition
# ASTCUR: Current Asthma Status
# AB51: Type I or Type II Diabetes
# POVLL: Poverty level
adult <- adult_raw %>%
         select(RBMI, BMI_P, RACEHPR2, SRSEX, SRAGE_P, MARIT2, AB1, ASTCUR,
                AB51, POVLL)



## Initial data exploration ---------------------
str(adult)
head(adult)
summary(adult)

# Age histogram - note binwidth = ~2.23
ggplot(adult, aes(x = SRAGE_P)) + 
  geom_histogram()

# BMI histogram
ggplot(adult, aes(x = BMI_P)) + 
  geom_histogram()

# Age colored by BMI, default binwidth
ggplot(adult, aes(x = SRAGE_P, col = factor(RBMI), fill = factor(RBMI))) + 
  geom_histogram(binwidth = 1) 



## Data cleaning --------------------------------

# Remove individual aboves 84 (unusually high number of 85 year olds)
adult <- adult[adult$SRAGE_P <= 84, ]

# Remove individuals with a BMI below 16 and above or equal to 52 
# (remove positive tail)
adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

# Relabel the race variable:
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = 
                           c("Latino", "Pacific Islander", 
                             "American Indian/Alaskan Native", "Asian", 
                             "African American", "White",
                             "Other Single/Multiple Race"))

# Relabel the BMI categories variable:
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", 
                                            "Normal-weight", 
                                            "Over-weight", 
                                            "Obese"))



## Data exploration: Frequency/Density plots ----
# The color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

# 4 frequency histograms of BMI frequency by age
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  fix_strips + 
  BMI_fill +
  facet_grid(RBMI ~ .) +
  theme_classic()

# Plot 1 - Count histogram of BMI by age
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill

# Plot 2 - Density histogram of BMI by age
# Note -  we get the density within each BMI category,
#         not within each age group
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..density..)) +
  BMI_fill

# Plot 3 - Faceted count histogram of BMI by age
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill + 
  facet_grid(RBMI ~ .)

# Plot 4 - Faceted density histogram
# Note - same issue as plot 2
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..density..)) +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Plot 5 - Density histogram with position = "fill"
# Note - calculates density across category not bin
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..density..), position = "fill") +
  BMI_fill

# Plot 6 - The accurate histogram - density across bin
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1, aes(y = ..count../sum(..count..)), position = "fill") +
  BMI_fill


## Mosaic plot of data ---------------------

mosaicGG <- function(data, X, FILL) {
  # Generalized function to create a mosaic plot comparing
  # two variables.
  #
  # Args:
  #   data - the original data frame (data.frame)
  #   X - the x variable's name in original data frame (character)
  #   FILL - the y variable's name in the original data frame (character)
  #
  # Returns:
  #   Mosaic plot for two variables in the data frame. The fill is 
  #   determined by residuals to show over-/under-representation.
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # Plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

# Mosaic plot 1 - BMI described by age
mosaicGG(adult, "SRAGE_P", "RBMI")

# Mosaic plot 2 - Poverty described by age
mosaicGG(adult, "SRAGE_P", "POVLL")
