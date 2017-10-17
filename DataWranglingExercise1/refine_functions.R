# File-Name:      refine_function.R
# Date:           2017-10-14
# Author:         Monisha Gopalakrishnan
#
# Data Used:      data/refine_original.csv
# Packages Used:  dplyr, RecordLinkage



## Library/Source -------------------------------
library(dplyr)
library(RecordLinkage)



## Function(s) ----------------------------------
standardize_string <- function(keywords, options) {
  # Standardizes the spelling of a string.
  #
  # Args: 
  #   keywords - Vector of original strings 
  #   options - Vector of strings containing standardized words
  #
  # Returns:
  #   The string from the options vector that is most similar to each keyword
  
  # Error handling
  if (class(keywords) != "character" || class(options) != "character") {
    stop("Both keyword and options must be character data types.")
  }
  
  keywords <- sapply(keywords, tolower)
  for (index in 1:length(keywords)) {
    for (str in options) {
      if (jarowinkler(keywords[index], str) > 0.7) {
        # A higher jarowinkler score means higher similarity
        keywords[index] <- str
      }
    }
  }
  return(keywords)
}


