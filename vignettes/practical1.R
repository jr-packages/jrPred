## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, results = "hide", fig.keep = "none")

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  install.packages("drat")

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  drat::addRepo("jr-packages")
#  install.packages("jrPred")

## ---- echo = TRUE, message = FALSE---------------------------------------
library("jrPredictive")

## ---- echo = TRUE, message = FALSE---------------------------------------
library("caret")

## ---- echo = TRUE--------------------------------------------------------
data(FuelEconomy, package = "AppliedPredictiveModeling")

## ------------------------------------------------------------------------
m1 = train(FE ~ EngDispl, method = "lm", data = cars2010)

## ------------------------------------------------------------------------
predict(m1, newdata = data.frame(EngDispl = 7))

## ------------------------------------------------------------------------
sqrt(mean(resid(m1)^2))
# or 
RMSE(fitted.values(m1), cars2010$FE)

## ------------------------------------------------------------------------
m2 = train(FE ~ poly(EngDispl, 2, raw = TRUE), data = cars2010,
    method = "lm")

## ------------------------------------------------------------------------
sqrt(mean(resid(m2)^2)) - sqrt(mean(resid(m1)^2))
# Yes

## ------------------------------------------------------------------------
m3 = train(FE ~ EngDispl + NumCyl, data = cars2010, method = "lm")

## ------------------------------------------------------------------------
sqrt(mean(resid(m3)^2))

## ---- echo  TRUE, fig.keep="none"----------------------------------------
plot(cars2010$EngDispl, cars2010$FE)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  abline(m1$finalModel, col = 2)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  x_values = seq(1,8.4,0.1)
#  new_pred_values = predict(m2, newdata = data.frame(EngDispl = x_values)
#  lines(x = x_values, y = new_pred_values, col = 3)

## ------------------------------------------------------------------------
# Yes, line looks to curve with the data now we have added a quadratic term

## ---- echo = TRUE--------------------------------------------------------
## points = TRUE to also show the points
plot3d(m3, cars2010$EngDispl, cars2010$NumCyl, cars2010$FE,
    points = FALSE)

## ---- echo = TRUE--------------------------------------------------------
threejs::scatterplot3js(cars2010$EngDispl, cars2010$NumCyl,
    cars2010$FE, size = 0.5)

