## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, results = "hide", fig.keep = "none")

## ------------------------------------------------------------------------
data(OJ, package = "ISLR")

## ---- message = FALSE----------------------------------------------------
library("caret")
library("jrPred")

## ---- eval = FALSE-------------------------------------------------------
#  par(mfrow = c(4, 5), mar = c(4, 0.5, 0.5, 0.5))
#  plot(Purchase ~ ., data = OJ)

## ------------------------------------------------------------------------
m1 = train(Purchase ~ PriceCH + PriceMM,
    data = OJ, method = "glm")

## ------------------------------------------------------------------------
getTrainPerf(m1)

## ------------------------------------------------------------------------
# with no model we essentially predict according to
# proportion of observations in data

# work out proportions
probs = table(OJ$Purchase)/nrow(OJ)
# sample using proportions
preds = sample(levels(OJ$Purchase), prob = probs)
# work out correct proportion
mean(preds != OJ$Purchase)

## ------------------------------------------------------------------------
predict(m1, newdata = data.frame(PriceCH = 2.3, PriceMM = 2.4))

## ---- fig.cap = "Examining the decision boundary for orange juice brand purchases by price.", echo = TRUE, fig.keep="all"----
boundary_plot(m1,OJ$PriceCH, OJ$PriceMM, OJ$Purchase,
              xlab="Price CH", ylab="Price MM")

## ------------------------------------------------------------------------
# We now have a curved decision boundary.
# There are two regions of where we would predict MM, bottom left, and a tiny one up in the top right.

## ---- warning = FALSE, echo = TRUE---------------------------------------
mLM = train(Purchase ~ ., data = OJ, method = "glm")

## ---- echo = TRUE--------------------------------------------------------
remove = findLinearCombos(model.matrix(Purchase ~ ., data = OJ))

## ---- echo = TRUE--------------------------------------------------------
(badvar = colnames(OJ)[remove$remove])

## ---- echo = TRUE--------------------------------------------------------
OJsub = OJ[, -remove$remove]

## ------------------------------------------------------------------------
mLM = train(Purchase~., data = OJsub, method = "glm")
getTrainPerf(mLM)

## ------------------------------------------------------------------------
## could use confusionMatrix
(cmLM = confusionMatrix(predict(mLM,OJsub),OJsub$Purchase))

## ------------------------------------------------------------------------
# or
sensitivity(predict(mLM,OJsub),OJsub$Purchase)
specificity(predict(mLM,OJsub),OJsub$Purchase)

## ------------------------------------------------------------------------
#The model is fairly good at picking up both positive events, person buys CH, and negative events, MM.

## ---- message = FALSE----------------------------------------------------
mKNN = train(Purchase~., data = OJsub, method = "knn")

## ------------------------------------------------------------------------
cmKNN = confusionMatrix(predict(mKNN,OJsub),OJsub$Purchase)
(info = data.frame(Model = c("logistic","knn"),
           Accuracy = c(cmLM$overall["Accuracy"],
               cmKNN$overall["Accuracy"]),
           Sensitivity = c(cmLM$byClass["Sensitivity"],
               cmKNN$byClass["Sensitivity"]),
           Specificity = c(cmLM$byClass["Specificity"],
               cmKNN$byClass["Specificity"])))

## ------------------------------------------------------------------------
# Accuracy increases at first with knn before then getting worse after a peak value of 9.
(mKNN2 = train(Purchase~., data = OJsub, method = "knn",
    tuneGrid = data.frame(k = 1:30)))

## ---- message=FALSE, warning = FALSE-------------------------------------
library("jrPred")
data(FuelEconomy, package = "AppliedPredictiveModeling")
regKNN = train(FE~., data = cars2010, method = "knn")
regLM = train(FE~., data = cars2010, method = "lm")
getTrainPerf(regKNN)
getTrainPerf(regLM)

## ------------------------------------------------------------------------
# The KNN regression model is not as good as the linear model, only just

## ---- echo = TRUE--------------------------------------------------------
data(FuelEconomy, package = "AppliedPredictiveModeling")

## ------------------------------------------------------------------------
mKNN = train(FE ~ ., method = "knn", data = cars2010)

## ------------------------------------------------------------------------
# set the train control object
tc10fold = trainControl(method = "cv", number = 10)
# fit the model using this train control object
mKNN10 = train(FE~., method = "knn", data = cars2010,
    trControl = tc10fold)
getTrainPerf(mKNN10)

## ------------------------------------------------------------------------
mKNNcv10 = train(FE~., method = "knn", data = cars2010,
     trControl = tc10fold, tuneGrid = data.frame(k= 2:20))

## ------------------------------------------------------------------------
mKNNcv10$bestTune

## ------------------------------------------------------------------------
tc5fold = trainControl(method = "cv", number = 5)
tc15fold = trainControl(method = "cv", number = 15)

## ------------------------------------------------------------------------
mKNNcv5 = train(FE~., data = cars2010, method = "knn",
    trControl = tc5fold, tuneGrid = data.frame(k = 2:20))

mKNNcv15 = train(FE~., data = cars2010, method = "knn",
    trControl = tc15fold, tuneGrid = data.frame(k = 2:20))
mKNNcv5$bestTune
mKNNcv15$bestTune

## ---- echo = TRUE--------------------------------------------------------
data(Glass, package = "mlbench")

## ------------------------------------------------------------------------
tc = trainControl(method = "cv", number = 10)
model = train(Type ~ ., data = Glass, trControl = tc, method = "knn")
getTrainPerf(model)

