##########################################
## Simple demonstration of generalized linear
## mixed models
##########################################
library(data.table)
library(lme4)
library(nlme)
library(ggplot2)

## Use "sleepstudy" to illustrate
# ?sleepstudy : # Reaction times in a sleep deprivation study

dt <- data.table(sleepstudy)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
dt

summary(dt)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(dt, aes(x = Days, y = Reaction)) + geom_point() + theme_bw() +
  scale_x_continuous(breaks = 0:9)

## ----------------------------------------------------------------------------------------------------------------------------------------------
# This should look familiar, it is how we specify a linear regression
# where Reaction is the outcome and Days the predictor
my_lm <- lm(Reaction ~ Days, data = dt)
summary(my_lm) # Reaction time increases by ~10.5 milliseconds per day

# The predict function produces regression fit estimates for each
# row in the original dataset
dt$linear_pred <- predict(my_lm)

ggplot(dt, aes(x = Days, y = Reaction)) + geom_point() + theme_bw() +
  scale_x_continuous(breaks = 0:9) + 
  geom_line(aes(y = linear_pred), col = "purple") 


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# the random intercept reflects variation by individual
# These pieces of code are simply to plot the 
# subjects by increasing order of mean reaction time
dt[, subject_order := factor(Subject)]
dt[, subject_order := reorder(subject_order, Reaction, mean)]

ggplot(dt, aes(x = subject_order, y = Reaction)) + geom_boxplot() + theme_bw() +
  xlab("Subject Number")


## ---------------------------------------------------------------------------------------------------------------------------------------
## # We can look at the histogram (remember that random effects assume a normal distribution
## # of coefficients of mean mu and sd sigma^2)
## 
## reaction_means <- dt[, lapply(.SD, function(x) mean(x)),
##                      .SDcols = "Reaction",
##                      by = "Subject"]
## 
## ggplot(reaction_means, aes(x = Reaction)) +
##   geom_histogram(binwidth = 50, col = "black", fill = "gray") +
##   theme_bw() + scale_y_continuous("Subjects", breaks = 0:12)


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Let's replot, considering individuals
ggplot(dt, aes(x = Days, y = Reaction)) + geom_point(alpha = 0.5) + theme_bw() +
  scale_x_continuous(breaks = 0:9) + 
  stat_smooth(aes(group = Subject), method = "lm", # stat_smooth is a quick way to plot linear fit
                       se = F, col = "black")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Add a panel
ggplot(dt, aes(x = Days, y = Reaction)) + geom_point() + theme_bw() +
  scale_x_continuous(breaks = 0:9) + 
  geom_line(aes(y = linear_pred), col = "purple") +
  facet_wrap(~Subject) # This will create a panel for Subjects


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# lmer functions the same as lm. The difference is that we 
# *must* specify a random effect, the (1 | X) part of the code
my_lmer <- lmer(Reaction ~ Days + (1 | Subject), data = dt)
#my_lmer <- lmer(Reaction ~ Days, data = dt) # Not run, will produce an error
summary(my_lmer)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict from a "merMod" (the object created by lmer) works
# exactly the same as for predict.lm
dt$ran_int_pred <- predict(my_lmer)

ggplot(dt, aes(x = Days, y = Reaction)) + geom_point() + theme_bw() +
  scale_x_continuous(breaks = 0:9) + facet_wrap(~Subject) + 
  geom_line(aes(y = linear_pred), col = "purple") + 
  geom_line(aes(y = ran_int_pred), col = "blue")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
my_subjects <- c(309, 335, 337, 330)
ggplot(dt[Subject %in% my_subjects], aes(x = Days, y = Reaction)) + 
  geom_point(aes(col = Subject), size = 3, alpha = 0.5) + theme_bw() +
  scale_x_continuous(breaks = 0:9) +
  geom_line(aes(y = linear_pred), col = "black") + 
  geom_line(aes(y = ran_int_pred, col = Subject))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
fix_ef <- fixef(my_lmer) # Fixed-effects (grand Intercept and Days)
ran_ef <- ranef(my_lmer) # Is a list object
# Let's convert that into a data table instead of a list
ran_ef <- as.data.table(ran_ef) # Also works with as.data.frame()

# This has column names that are non-intuitive to me
# condval is the conditional value (difference from overall intercept)
# condsd is the conditional standard deviation (why is it the same for all Subjects?)
setnames(ran_ef, c("grp","condval"),
         c("Subject","random_intercept"))

# Plot the values of the random intercepts (should be centered at 0)
ggplot(ran_ef, aes(x = random_intercept)) + 
  geom_histogram(binwidth = 20, fill = "gray", col = "black") + 
  theme_bw()

# Not exactly, but the mean should be 0
mean(ran_ef$random_intercept)



## ----------------------------------------------------------------------------------------------------------------------------------------------------------
ran_ef$fixed_intercept <- fix_ef[1]
ran_ef[, subject_intercept := fixed_intercept + random_intercept]


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Syntax is slightly different, specify random effects outside of the formula, still use a |
my_nlme <- lme(Reaction ~ Days, random = ~ 1 | Subject, data = dt)
summary(my_nlme)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Adding a random slope is done by specifying a variable on the
# left side of the | shape. This means the model will produce
# an estimated slope for each Subject
my_rslope <- lmer(Reaction ~ Days + (Days | Subject), data = dt)
summary(my_rslope)

# And in nlme package:
# my_lme_slope <- lme(Reaction ~ Days, random = ~ Days | Subject, data = dt)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
dt$ran_ef_pred <- predict(my_rslope)

ggplot(dt, aes(x = Days, y = Reaction)) + geom_point() + theme_bw() +
  scale_x_continuous(breaks = 0:9) + facet_wrap(~Subject) + 
  geom_line(aes(y = linear_pred), col = "purple") + 
  geom_line(aes(y = ran_int_pred), col = "blue") +
  geom_line(aes(y = ran_ef_pred), col = "red")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(dt[Subject %in% my_subjects], aes(x = Days, y = Reaction)) + 
  geom_point(aes(col = Subject), size = 3, alpha = 0.5) + theme_bw() +
  scale_x_continuous(breaks = 0:9) +
  geom_line(aes(y = linear_pred), col = "black") + 
  geom_line(aes(y = ran_ef_pred, col = Subject))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------
# These are functions to produce estimates of model fit
# You should be familiar with AIC and BIC, but essentially
# they are penalized goodness of fit statistics. Lower
# values indicate better fits. 

AIC(my_lm, my_lmer, my_rslope)
BIC(my_lm, my_lmer, my_rslope)

