mxnetResult.LogLoss <- function() {
  df <- data.frame(
    test_LogLoss = preds_train.test.logloss
    ,train_LogLoss = preds_train.train.logloss
    ,num.round = NUM.ROUND
    ,array.batch.size = ARRAY.BATCH.SIZE
    ,learning.rate = LEARNING.RATE
    ,momentum = MOMENTUM
  )
  
  return(df)
}