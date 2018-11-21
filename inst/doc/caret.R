## ----echo = FALSE--------------------------------------------------------
set.seed(1)

## ----eval = FALSE--------------------------------------------------------
#  install.packages("caret")

## ----eval = FALSE--------------------------------------------------------
#  vignette("caret", package = "caret")

## ----eval = FALSE--------------------------------------------------------
#  library("doMC")
#  registerDoMC(cores = 8)

## ----eval = FALSE--------------------------------------------------------
#  library("doParallel")
#  cl = makeCluster(8)
#  registerDoParallel(cl)

## ----eval = FALSE--------------------------------------------------------
#  ?train
#  ?varImp

## ----eval = FALSE--------------------------------------------------------
#  ?ipred::treebag

## ----eval = FALSE--------------------------------------------------------
#  ?plot.train
#  ?plot.varImp.train

## ----fig.keep="none", message = FALSE------------------------------------
library("caret")
library("pls")
data(diamonds, package = "ggplot2")
i = sample(nrow(diamonds), 1000) # some subset to help plotting
diamonds = diamonds[i,]
m = train(price~., method = "pls", data = diamonds, 
    tuneLength = 10)
# a plot of model object gives us the resampling 
# information across tuning parameters
plot(m)
## using the varImp function with plot we get
## variable importance scores
plot(varImp(m))
# a plot of the final model shows predicted against
# observed values
plot(m$finalModel)
## a plot of residuals against fitted values
plot(fitted.values(m),resid(m))

## ------------------------------------------------------------------------
names(m)

## ----eval = FALSE--------------------------------------------------------
#  str(m)

