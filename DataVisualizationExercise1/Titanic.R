# File-Name:      Titanic.R
# Date:           2017-10-18
# Author:         Monisha Gopalakrishnan
# 
# Data Used:      titanic data set provided by DataCamp Data Visualization ggplot2 tutorial   
# Packages Used:  ggplot2



## Explore data ---------------------------------
str(titanic)

# Output of str(titanic):
# 'data.frame':	714 obs. of  4 variables:
# $ Survived: int  0 1 1 1 0 0 0 1 1 1 ...
# $ Pclass  : int  3 1 3 1 3 1 3 3 2 3 ...
# $ Sex     : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 1 1 1 ...
# $ Age     : num  22 38 26 35 35 54 2 27 14 4 ...

head(titanic)

# Output of head(titanic):
# Survived Pclass    Sex Age
# 1        0      3   male  22
# 2        1      1 female  38
# 3        1      3 female  26
# 4        1      1 female  35
# 5        0      3   male  35
# 6        0      1   male  54


## Create plots ---------------------------------
# 1 - Plot the distribution of sexes within the different passenger classes
# in the ship
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# 2 - Take Plot 1 and create two plots side-by-side based on survival
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ Survived)

# Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 3 - Plot the number of passengers as a function of passenger class. 
# Account for passenger sex. Create two side-by-side plots based on 
# survival
ggplot(titanic, aes(x = Pclass, y = Age, col = Sex)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) +
  facet_grid(. ~ Survived)

