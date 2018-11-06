## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, results = "hide", fig.keep = "none")

## ---- echo = FALSE, message = FALSE--------------------------------------
library("caret")
data(FuelEconomy, package = "jrPred")
set.seed(27)

## ---- echo = TRUE--------------------------------------------------------
data("FuelEconomy", package = "jrPred")

## ------------------------------------------------------------------------
mLM = train(FE~EngDispl+NumCyl+NumGears, method = "lm", data = cars2010)

## ------------------------------------------------------------------------
res = resid(mLM)
(trainRMSE = sqrt(mean(res*res)))

## ---- echo = TRUE, results='hold'----------------------------------------
## pick an index for samples
trainIndex = createDataPartition(cars2010$FE, p = 0.5, list = FALSE)
## set up validation set approach
tcVS =  trainControl(method = "cv", 
                     number = 1, 
                     index = list(Fold1 = trainIndex))

mLMVS = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
              data = cars2010, trControl = tcVS)
getTrainPerf(mLMVS)

## ------------------------------------------------------------------------
# set up train control objects
tcKFOLD = trainControl(method = "cv", number = 10)
# run model
mLMKFOLD = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tcKFOLD)

## ------------------------------------------------------------------------
getTrainPerf(mLMVS)
getTrainPerf(mLMKFOLD)
# 10-fold is lower than validation set, we mentioned it tended to
# over estimate test error

## ------------------------------------------------------------------------
mLMVS$times$everything
mLMKFOLD$times$everything

## ------------------------------------------------------------------------
# a number of trainControl objects 
tc2 = trainControl(method = "cv", number = 2)
tc5 = trainControl(method = "cv", number = 5)
tc10 = trainControl(method = "cv", number = 10)
tc15 = trainControl(method = "cv", number = 15)
tc20 = trainControl(method = "cv", number = 20)
# train the model using each
mLM2 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc2)
mLM5 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc5)
mLM10 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc10)
mLM15 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc15)
mLM20 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc20)
# use a data frame to store all of the relevant information
(info = data.frame("Folds" = c(2,5,10,15,20),
    "Time" = c(mLM2$times$everything[1],
        mLM5$times$everything[1],
        mLM10$times$everything[1],
        mLM15$times$everything[1],
        mLM20$times$everything[1]),
    "Estimate" = c(mLM2$results$RMSE,
                   mLM5$results$RMSE,
                   mLM10$results$RMSE,
                   mLM15$results$RMSE,
                   mLM20$results$RMSE)))

## ------------------------------------------------------------------------
# as there are more folds it takes longer to compute,
# not an issue with such a small model but something
# to consider on more complicated models.
# Estimates are going down as the number of folds increases.
# This is because for each held out fold we are using a greater
# proportion of the data in training so expect to get a better
# model.

