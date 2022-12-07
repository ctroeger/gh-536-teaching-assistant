################################
## Introduction to R ##
################################

# This class is taught entirely in R.
# There are a number of online tutorials
# and resources available to us to learn
# how to better use this language. 

# This file is a VERY simple example of
# some of the functions and data processing
# that are expected in the first homework
# assignment. 

# Line 18: Data creation and manipulation
# Line 88: Importing data
# Line 102: Packages
# Line 130: Descriptive statistics
# Line 179: Transformations

##----------------------------------------------------------------
## Data creation and manipulation
  # R has an "environment". Essentially this means
  # that R can load multiple objects (data frames)
  # at the same time. You can assign a name to 
  # a data frame by the "<-" or "=" sign:
  
  my_data <- data.frame(response = 1,
                        predictor = 0)
  
  # This is the same as
  my_data = data.frame(response = 1,
                       predictor = 0)
  
  # What do the data look like? A useful function is
  # head()
  head(my_data)
  
  # It is a two column, one row dataframe with a column
  # "response" and a column "predictor"
  
  # dim() [short for dimensions] shows rows x columns
  dim(my_data)
  
  # We can add to our data in a couple ways.
  # First, suppose we know the specific values we want
  # We can remake the dataframe
  my_data <- data.frame(response = c(1, 2, 3, 4, 5),
                        predictor = c(5, 4, 3, 2, 1))
  # Note the "c()" this is a function that tells R
  # that what follows is a vector (multiple ordered values).
  # c() expects the vector to be comma separated for different values
  # When modifying a dataframe in this way, the columns
  # must have the same lengths:
  my_data <- data.frame(response = c(1, 2, 3, 4, 5, 6),
                        predictor = c(5, 4, 3, 2, 1))
  
  # Another way to add observations is using the function
  # rbind(). This is short for "rowbind" and adds
  # rows to the bottom of your dataframe
  my_data <- rbind(my_data,
                   data.frame(response = c(6, 7),
                              predictor = c(0, -1)))
  # Note that the first object, "my_data" and the second,
  # a new dataframe without a name, must have the same
  # columns. This will not work:
  my_data <- rbind(my_data,
                   data.frame(v = 1,
                              t = 2))
  
  # You can add columns by defining a new column
  # in that dataframe. R identifies this with the
  # icon "$". Inversely, the new column (vector) 
  # must have the same number of rows
  my_data$confounder <- c(5, 5, 5, 4, 4, 2, 1)
  
  # The last main way to add to our data is by merge()
  # Merge takes two dataframes and combines them
  # by matching on a specific variable name. 
  additional_data <- data.frame(z = c(NA, -1, -2, -3, -4, -5, -6),
                                response = c(2, 1, 5, 3, 4, 6, 7))
  
  my_data <- merge(my_data, additional_data, by = "response")
  
  my_data

##--------------------------------------------------------------  
## Importing data
  # Importing data is most easily accomplished by 
  # reading in a comma separated values file (CSV).
  # Here is an example (you must change the filepath to your data)
  mda <- read.csv("C:/Users/ctroeger/OneDrive - UW/PhD/GH Teaching Assistant/Data/MDA_Coverage.csv")
  head(mda)
  dim(mda)
  
  # Sometimes it is annoying to convert between different
  # file types simply to import data. Most commonly, I have
  # encountered the need to read in Excel (.xlsx) files.
  # Base R doesn't have the ability to do that. 
  # We need a 'package' with functions to accomplish this. 
  
## Packages ------------------------------------------------------------
  # R is open-source, public, and freely available. One of the
  # perks is that anyone can create a package. A package is 
  # a collection of functions and data. Packages greatly expand
  # what you are able to do with R. We will use a variety of
  # packages in this course and in other statistical analyses.
  # Most packages can easily be installed from the R central
  # database (CRAN; https://cran.r-project.org/)
  
## Importing Excel files
  # Continuing our example, let's install a package to help
  # us import .xlsx files directly.
  install.packages("openxlsx")

  # Now, we have that package available to us in our R sessions.
  # However, we must ask R to find and activate those functions.
  library(openxlsx)
  
  # Let's see how the function works
  help("read.xlsx")
  
  # I have commented this line of code "out" with a hashtag (#)
  # because I don't want it to run (because I don't actually have
  # any data saved here.) but this is how we would use that function
  
   #excel_data <- read.xlsx("path_to_fake_data.xlsx", sheet = "sheet1")

##------------------------------------------------------------------------
# Descriptive statistics
  # Often, we will want to learn more about our data before running
  # any analyses on them. summary() is a good way to quickly see
  # our data 
  summary(my_data)
  
  # Perhaps, we will want to see distributions. Histograms are a good option
  hist(my_data$response)
  
  # Or maybe correlations
  cor(my_data$response, my_data$confounder)
  cor.test(my_data$response, my_data$confounder)
  
  # How about a t-test?
  t.test(my_data$response, my_data$confounder)
  
  # Measures of central tendency are common ways to learn about the data
  mean(my_data$response) # mean
  median(my_data$response) # median
  sd(my_data$response)   # standard deviation
  quantile(my_data$response) # quartiles
  
## What if we want to see these values across all variables?
  # The easiest answer is to do "summary(my_data)" as we 
  # saw above, but perhaps you want to save the results?
  
## Loops are a good way to do this.
  data_means <- c() # I am setting an empty vector before the loop to capture outputs
  # Loops are created using for(){}
  # you put whatever function inside the {} marks
  # In this example, I want to find the mean of each of the four columns
  # of the my_data dataframe. 
  for(i in 1:4){ # "i" is often used for loops but it can be anything. 
                 # The values 1, 2, 3, 4 will be assigned to "i" sequentially.
                 # the ":" says for each integer between values, so 1:4 is 1,2,3,4
    my_column <- my_data[, i] # columns in a dataframe are indexed (identified)
                              # by []. [row, column]
    mean_tmp <- mean(my_column) # finds the mean of that column
    data_means <- c(data_means, mean_tmp) # "appends" the observed mean to other means
  }
  
  # We now have a vector of the means for each column. 
  data_means
  
## That was an inefficient way to get our answers but it is intended
  # to demonstrate loops. There are much easier ways to accomplish that
  # task as you become more confident using R. 
  
##------------------------------------------------------------------------
# Data transformations
  
## Commonly, we will need to transform data. Below are a few examples.
  
  # Changing data by a single function is easy. A single function could be
  # addition, division, standardization, etc.
  my_data$confounder + 3
  my_data$shifted_predictor <- my_data$predictor / 2
  my_data$standardized_predictor <- my_data$predictor / sd(my_data$predictor)
  
  # Perhaps we want to create a new variable based on the value of a different one.
  # the function ifelse() is a shortcut. It says, if condition is met, do this,
  # otherwise do that.
  ifelse(my_data$response > 3, "Greater than 3", "Three or fewer")
  my_data$binary_response <- ifelse(my_data$response > 3, 1, 0)
  my_data  
  
  