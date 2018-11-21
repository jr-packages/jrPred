## ----eval=FALSE----------------------------------------------------------
#  library("caret")
#  data(BostonHousing, package = "mlbench")
#  ## an rfeControl object is the analog to trainControl in the
#  ## recursive feature elimination algorithm
#  rc = rfeControl(method = "cv")
#  rfe(medv~., data = BostonHousing, sizes = 4:6, rfeControl = rc,
#      trControl = trainControl(method = "cv"), method = "lm")

