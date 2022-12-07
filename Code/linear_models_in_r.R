################################################
## Demo of basic R data and statistical models ##
################################################

## Libraries
library("ggplot2")      # Plotting
library("lme4")         # Mixed-models
library("data.table")   # Data manipulation
library("reshape2")     # Data manipulation

## Data
dt <- read.csv("C:/Users/ChrisTroeger/Downloads/MDA_Coverage.csv")
head(dt)

## For this example, I am going to re-structure the data so they are "long"
dt <- melt(dt, id.vars = c("OBJECTID","ADMIN0","ADMIN2","ADMIN2ID"))

##------------------------------------------------
# Faraway data
  library(faraway)

## Diabetes data
# We will use these data to demonstrate a few common functions
# in R for data manipulation, pre-analytic investigation, and
# for simple linear and generalized linear models. 
  data(diabetes)
# These data are part of the faraway package. They are from a 
# study of 403 African Americans in Virginia USA to understand
# the prevalence of diabetes, obesity, and cardiovascular risk
# factors. You can learn more by reading the description that 
# opens when you run this command, including variable names and
# a variable key. 
  help(diabetes)
  
## Variable information
# For this example, I am going to focus on "chol" (total cholesterol)
# as the response (Y) variable and use some other columns like age,
# gender, bp.1s (first systolic blood pressure) to predict chol.
  my_variables <- c("chol","age","gender","bp.1s", "waist")
  
# Let's get a sense of these variables
  summary(diabetes[, my_variables])
  
# What do you notice?
  
# Other ways we can review data are histograms
  hist(diabetes$chol)

# I prefer using ggplot, so let's make a nice graph that way too
  ggplot(diabetes, aes(x = chol)) + 
    geom_histogram(fill = "gray", col = "black")
  
# We might be interested in the linear relationship between variables
# base R
  plot(diabetes$age, diabetes$chol) # first variable is x-axis

  ggplot(diabetes, aes(x = age, y = chol)) +
    geom_point()
  
# ggplot objects can be assigned to a variable to hold them:
  my_scatter <- ggplot(diabetes, aes(x = age, y = chol)) +
                  geom_point()
# and we can add to/modify from that
  my_scatter <- my_scatter + theme_bw()
  
  my_scatter + geom_point(aes(col = gender))

# ggplot has a nice feature where you can add a linear trend line:
  my_scatter + stat_smooth(method = "lm", se = F)  
  
# Okay, let's make a really nice looking ggplot with many options
# that you can learn more about on your own
  ggplot(diabetes, aes(x = age, y = chol, col = gender)) +
    geom_point(alpha = 0.5, size = 2) +
    stat_smooth(method = "lm", se = F) +
    theme_bw() +
    xlab("Age") +
    ylab("Total cholesterol") +
    scale_color_manual("Gender", values = c("purple","goldenrod")) +
    ggtitle("Total cholesterol and age, stratified by gender")

## To summarize: in this dataset, total cholesterol is positively
# associated with age among all subjects. However, while there is
# a positive association for females, there is no association for males.
  
##---------------------------------------------------------------------------
## Linear model
# Linear models fit a statistical relationship between y ~ x
# the function lm() will run that model. Let's fit the relationship
# between chol and age
  mod1 <- lm(chol ~ age, data = diabetes)
  summary(mod1) # shows model fit info  
# Do you feel comfortable interpreting this model?

# We saw above that age and cholesterol might be affected by gender.
# We can add that into our model like this:
  mod2 <- lm(chol ~ age + gender, data = diabetes)
  summary(mod2)  

# Wait though, that is not what we plotted above. This model
# has a single slope but different intercepts by gender. 
# How can we replicate the plot in a statistical model?
  mod3 <- lm(chol ~ age*gender, data = diabetes)
  summary(mod3)
  
## Predictions:
  diabetes$pred1 <- predict(mod1, newdata = diabetes)
  
# I'm going to also create a dummy dataframe for the values I want predicted
  pred_df <- expand.grid(age = 20:max(diabetes$age),
                         gender = c("male","female")) # what does expand.grid() do?
  
  pred_df$pred1 <- predict(mod1, newdata = pred_df)
  pred_df$pred2 <- predict(mod2, newdata = pred_df)
  pred_df$pred3 <- predict(mod3, newdata = pred_df)
  
# Hey, remember that we have a "my_scatter" showing the data? Let's utilize it!
  my_scatter + geom_line(data = pred_df, aes(y = pred1), col = "#88CCEE", lwd = 1.25)

  my_scatter + geom_line(data = pred_df, aes(y = pred2, col = gender), lwd = 1.25) +
    scale_color_manual("Gender", values = c("purple","goldenrod"))
  
  my_scatter + geom_line(data = pred_df, aes(y = pred3, col = gender), lwd = 1.25) + 
    scale_color_manual("Gender", values = c("purple","goldenrod"))
  
## What if we want to make a new variable?
# Let's create BMI. We know BMI is not a very reliable or 
# accurate indicator (among other problems), but it is a
# good example of something we might want to make from our
# data through transformation.
# recall BMI is kg/m^2
# The problem is that we have imperial, not metric measurements
  diabetes$kg <- diabetes$weight / 2.2
  diabetes$meters <- diabetes$height * 0.0254
  
  diabetes$bmi <- diabetes$kg / (diabetes$meters^2)
  
## Let's try to decide on a "best" model (that has some subjectivity)
  mod4 <- lm(chol ~ age*gender + bmi, data = diabetes)
  summary(mod4)  

  AIC(mod1, mod2, mod3, mod4)
  
##-----------------------------------------------------------
## Generalized linear models

# Binomial model
#  Total cholesterol > 240 is sometimes considered high.
  diabetes$high_chol <- ifelse(diabetes$chol > 240, 1, 0)
  my2x2 <- table(diabetes$high_chol, diabetes$gender)
  my2x2  

# Our outcome is now binary and is not longer appropriate to run
# a simple linear model (a more detailed description of why can
# be found in a variety of places). Let's see how to run a
# GENERALIZED LINEAR MODEL
  gmod1 <- glm(high_chol ~ gender, family = "binomial", data = diabetes)
  summary(gmod1)
  coef(gmod1)
  
# We need to exponentiate to get the odds ratio from that model
  odds1 <- exp(coef(gmod1))
  odds1  
  
# Does that match our crude odds (AD/BC)?
  (my2x2[1,1] * my2x2[2,2]) / (my2x2[1,2] * my2x2[2,1])
# Yes!
  
# Predict it
  diabetes$pred_bin <- predict(gmod1, newdata = diabetes)
# Wait, those numbers are negative?!
# Recall that this model is a logistic regression model.
# Exponentiating the predictions isn't right, we need to
# take the inverse logit function
# There are functions in other packages, but for
# practice, let's create our own function
  inv_logit <- function(x){
    y <- exp(x) / (1 + exp(x))
    return(y)
  }
  
  diabetes$pred_bin <- inv_logit(diabetes$pred_bin)
  hist(diabetes$pred_bin)
  
# Our model is really bad!

## Poisson model
# NOTE: This is not biologically correct, it is simply for demonstration!!
# Poisson (count models) assume that values can be 0 or positive integers, but
# cannot be negative.
  diabetes$time <- floor(diabetes$time.ppn) # This rounds values to lower integer

  hist(diabetes$time)  
  
# Let's make a model with this parameterization
  pmod1 <- glm(time ~ gender + age, family = "poisson", data = diabetes)
  summary(pmod1)  
  coef(pmod1)
  
# Like an odds model, the coefficients should be exponentiated
# to get a risk ratio
  rr1 <- exp(coef(pmod1))
  rr1  

# Let's compare with the observed values
  diabetes$pred_pois <- predict(pmod1, newdata = diabetes)
# exponentiate that
  diabetes$pred_pois <- exp(diabetes$pred_pois)

  ggplot(diabetes, aes(x = time)) + 
    theme_bw() + 
    geom_histogram(aes(fill = "Observed"), alpha = 0.4) +
    geom_histogram(aes(x = pred_pois, fill = "Predicted"), alpha = 0.4)
  
# Our model isn't very good here either!