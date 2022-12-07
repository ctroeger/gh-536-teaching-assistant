## ---- warning = F, message = F------------------------------------------------------------------------------------------------------------------------------
## Libraries
  library("ggplot2")      # Plotting
  library("lme4")         # Mixed-models
  library("data.table")   # Data manipulation
  library("reshape2")     # Data manipulation
  library("knitr")        # Markdown helper


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
## Data
# I am reading in the data again later, so I am saving
# a backup
  dt <- read.csv("C:/Users/ChrisTroeger/Downloads/MDA_Coverage.csv")
  back_up_dt <- dt



## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# What are the columns? How are they structured?
  head(dt)
  str(dt)

# Descriptive statistics
# Often, we will want to learn more about our data before running
# any analyses on them. summary() is a good way to quickly see
# our data 
  summary(dt)
  
  # Perhaps, we will want to see distributions. Histograms are a good option
  hist(dt$COV_99)
  
  # Or maybe correlations
  cor(dt$COV_99, dt$COV_03) # Why does this return an NA?
  cor.test(dt$COV_99, dt$COV_03)
  
  # How about a t-test?
  t.test(dt$COV_99, dt$COV_03)


## ---- warning = F, message = F------------------------------------------------------------------------------------------------------------------------------
# Base R
  plot(dt$COV_99, dt$COV_03)

# ggplot2 (my preferred plotting)
  ggplot(dt, aes(x = COV_99, y = COV_03)) + # Uses 'dt' and defines plotting characteristics (aesthetics)
    geom_point()                            # Tells ggplot to make a scatter
  


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  mod1 <- lm(COV_03 ~ COV_99, data = dt)
  summary(mod1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Is value > 90%?
  dt$high_cov <- ifelse(dt$COV_03 > 90, 1, 0)
  table(dt$high_cov)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  mod2 <- glm(high_cov ~ COV_99, data = dt, family = "binomial")
  summary(mod2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  odds <- exp(coef(mod2))
  odds


## ---- warning = F, message = F------------------------------------------------------------------------------------------------------------------------------
# Make prediction
  dt$pred1 <- predict(mod1, newdata = dt)

# Plot it (won't work so well if there is more than 1 predictor... why?)
  ggplot(dt, aes(x = COV_99, y = COV_03)) + geom_point() + geom_line(aes(y = pred1))
  


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict our binomial model
  dt$pred2 <- predict(mod2, newdata = dt)
  head(dt) # our values are negative!
  
# That's because a binomial model logit-transforms the outcome (high_cov) so
# so we must back-transform it (inverse logit).
# There are packages with that function but let's make it ourselves for practice.
  inv_logit <- function(x){
    y <- exp(x) / (1 + exp(x))
    return(y)
  }
  
  dt$pred2_linear <- inv_logit(dt$pred2)
  head(dt)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# re-assign the data again so I don't have the prediction columns
  dt <- back_up_dt

## For this example, I am going to re-structure the data so they are "long"
  dtl <- melt(dt, id.vars = c("OBJECTID","ADMIN0","ADMIN2","ADMIN2ID"))
  
## Create a "year" variable
  dtl$year <- substr(dtl$variable, 5, 6) # keep the 5th and 6th characters
  dtl$year <- ifelse(dtl$year == "99", 1999, as.numeric(dtl$year) + 2000)
  head(dtl)


## ---- warning = F, message = F------------------------------------------------------------------------------------------------------------------------------
  ggplot(dtl, aes(x = year, y = value)) + geom_point()


## ---- warning = F, message = F------------------------------------------------------------------------------------------------------------------------------
  ggplot(dtl[dtl$ADMIN2 %in% c("Banfora","Batie","Leo","Manga","Pama","Toma"), ], 
         aes(x = year, y = value, col = ADMIN2)) + geom_point() + 
    theme_bw() + 
    stat_smooth(method = "lm", se = F) # fits a linear model, suppresses standard errors


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  nrow(dtl)
  nrow(dtl[is.na(dtl$value),])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Find the ADMIN2 areas with missing values
  missing <- dtl[is.na(dtl$value),]

# Some areas have multiple missing values. For this exercise, I want unique areas.
  missing <- missing[, c("ADMIN2", "ADMIN2ID")]
  missing <- unique(missing)
  
# Create a dummy indicator that this area has missing values
  missing$has_missing <- 1
  
# Now, use the merge() function to merge back with our data
  dtl <- merge(dtl, 
               missing, 
               by = "ADMIN2", # This is the variable that needs to exist in both datasets to merge with.
               all.x = T)     # If I don't add this, I will lose locations that aren't in 'missing', the 
                              # opposite of what I want!
  
  dtl$has_missing <- ifelse(is.na(dtl$has_missing), 0, 1)
  
  dtl_no_missing <- dtl[dtl$has_missing == 0,] # new dataset without missing areas


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  linmod <- lm(value ~ year, data = dtl)
# Same model but without areas with missing data  
  linmod_no_missing <- lm(value ~ year, data = dtl_no_missing)
  
  rintmod <- lmer(value ~ year + (1 | ADMIN2), data = dtl)
  rslpmod <- lmer(value ~ year + (1 + year | ADMIN2), data = dtl) # This is a poor model!


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  dtl$predlin <- predict(linmod, newdata = dtl)
  dtl$predrint <- predict(rintmod, newdata = dtl)
  dtl$predrslp <- predict(rslpmod, newdata = dtl)


## ---- warning = F, message = F------------------------------------------------------------------------------------------------------------------------------
  mm_pred <- data.frame(year = unique(dtl$year),
                        ADMIN2 = "New") # If you tell it an existing ADMIN2, you will get the prediction
                                        # including the random effects
  mm_pred$predlin <- predict(linmod, newdata = mm_pred)
  mm_pred$predlin_no_missing <- predict(linmod_no_missing, newdata = mm_pred)

# new.levels are random effect groups that don't exist in the original dataset. These
# groups will be predicted with the overall intercept and slope
  mm_pred$predrint <- predict(rintmod, newdata = mm_pred, allow.new.levels = T)
  mm_pred$predrslp <- predict(rslpmod, newdata = mm_pred, allow.new.levels = T)
  
# Complicated ggplot to see results
  ggplot(mm_pred, aes(x = year)) + 
    geom_point(data = dtl, aes(x = year, y = value)) +
    geom_line(aes(y = predlin, col = "Linear")) + 
    geom_line(aes(y = predlin_no_missing, col = "Linear without missing")) +
    geom_line(aes(y = predrint, col = "Random Intercept")) +
    geom_line(aes(y = predrslp, col = "Random Slope")) +
    theme_bw()
  


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
  coef(linmod)
  fixef(rintmod)
  fixef(rslpmod)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# We can get that from the summaries
# kable() is a nice function to produce tables
# kable() will print a dataframe so I am 
# artificially creating one by rbind() the 
# coefficients from the models. 
  kable(rbind(summary(linmod)$coefficients[2,1:3],
              summary(rintmod)$coefficients[2,],
              summary(rslpmod)$coefficients[2,]),
        caption = "Coefficients for year")
  
# Or we can get that from the variance-covariance matrix
# A variance covariance matrix has the variance within
# coefficients in a model in the "diagonal". So we use
# the function diag() in R. However, that is the variance
# (thus the name) but the standard error of the estimate
# is more often reported, so we take the square root of that
# sqrt(). Lastly, I want to report the standard error of 
# the beta on year, so I ask for the second observation
# (the first observation would be the standard error of
# the intercept term). 
# vcov() prints the variance-covariance matrix
  vcov(linmod)
  
  lin_se <- sqrt(diag(vcov(linmod)))[2]
  print(paste0("The standard error on year for the linear model is: ",
               round(lin_se, 2)))



## -----------------------------------------------------------------------------------------------------------------------------------------------------------
AIC(linmod, rintmod, rslpmod)
BIC(linmod, rintmod, rslpmod)

