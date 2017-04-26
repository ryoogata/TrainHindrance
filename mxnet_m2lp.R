require(caret)
require(mxnet)
require(dplyr)
require(MLmetrics)
require(nnet) #just used for class.ind


source("script/R/fun/mxnetResult.R")
result.mlp.df <- readRDS("result/result.m2lp.df.data")

# Customized mlogloss eval_metric
# mx.metric.mlogloss <- mx.metric.custom("mlogloss", function(label, pred){
#   label_mat <- data.frame(i = 1:length(label),
#                           j = label + 1,
#                           x = rep(1, length(label)))
#   label_mat <- sparseMatrix(i = label_mat$i,
#                             j = label_mat$j,
#                             x = label_mat$x)
#   label_mat <- as.matrix(label_mat)
#   return(MultiLogLoss(label_mat, t(pred)))
# })

mlogloss = function(y, p, min_eta=1e-15,max_eta = 1.0){
  class_loss = c(dim(p)[2]);
  loss = 0;
  p = mLogLoss.normalize(p,min_eta, max_eta);
  for(ix in 1:dim(y)[2]) {
    p[,ix] = ifelse(p[,ix]>1,1,p[,ix]);
    class_loss[ix] = sum(y[,ix]*log(p[,ix]));
    loss = loss + class_loss[ix];
  }
  #return loss
  return (list("loss"=-1*loss/dim(p)[1],"class_loss"=class_loss));
}

mx.metric.mlogloss <- mx.metric.custom("mlogloss", function(label, pred){
  p = t(pred);
  m = mlogloss(class.ind(label),p);
  gc();
  return(m$loss);
})

mLogLoss.normalize = function(p, min_eta=1e-15, max_eta = 1.0){
  #min_eta
  for(ix in 1:dim(p)[2]) {
    p[,ix] = ifelse(p[,ix]<=min_eta,min_eta,p[,ix]);
    p[,ix] = ifelse(p[,ix]>=max_eta,max_eta,p[,ix]);
  }
  #normalize
  for(ix in 1:dim(p)[1]) {
    p[ix,] = p[ix,] / sum(p[ix,]);
  }
  return(p);
}


#
# 前処理
#
source("script/R/fun/Data-pre-processing.R")

my_preProcess <- c("range")

data_preProcess <- "none"

if ( data_preProcess == "none") {
  TRAIN <- all.train
  TRAIN.TRAIN <- train.train
  TRAIN.TEST <- train.test
  TEST <- test
} else if ( data_preProcess == "nzv") {
  TRAIN <- all.nzv.train
  TRAIN.TRAIN <- train.nzv.train
  TRAIN.TEST <- train.nzv.test
  TEST <- test
} else if ( data_preProcess == "dummy") {
  TRAIN <- train.dummy
  TRAIN.TRAIN <- train.dummy.train
  TRAIN.TEST <- train.dummy.test
  TEST <- test.dummy
} else if ( data_preProcess == "dummy.nzv.highlyCorDescr") {
  TRAIN <- train.dummy.nzv.highlyCorDescr
  TRAIN.TRAIN <- train.dummy.nzv.highlyCorDescr.train
  TRAIN.TEST <- train.dummy.nzv.highlyCorDescr.test
  TEST <- test.dummy.nzv.highlyCorDescr
}

# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(response, datetime)))


# response を数値化
# 0: good, 1: human, 2: mechanical, 3: weather
TRAIN.TRAIN$response <- as.character(TRAIN.TRAIN$response)
TRAIN.TRAIN$response[TRAIN.TRAIN$response == "good"] <- 0
TRAIN.TRAIN$response[TRAIN.TRAIN$response == "human"] <- 1
TRAIN.TRAIN$response[TRAIN.TRAIN$response == "mechanical"] <- 2
TRAIN.TRAIN$response[TRAIN.TRAIN$response == "weather"] <- 3
TRAIN.TRAIN$response <- as.numeric(TRAIN.TRAIN$response)

TRAIN.TEST$response <- as.character(TRAIN.TEST$response)
TRAIN.TEST$response[TRAIN.TEST$response == "good"] <- 0
TRAIN.TEST$response[TRAIN.TEST$response == "human"] <- 1
TRAIN.TEST$response[TRAIN.TEST$response == "mechanical"] <- 2
TRAIN.TEST$response[TRAIN.TEST$response == "weather"] <- 3
TRAIN.TEST$response <- as.numeric(TRAIN.TEST$response)

TRAIN.TRAIN.PRED <- preProcess(TRAIN.TRAIN, method = my_preProcess) %>%
  predict(., TRAIN.TRAIN) %>%
  data.matrix(.) %>%
  t(.)

TRAIN.TEST.PRED <- preProcess(TRAIN.TEST, method = my_preProcess) %>%
  predict(., TRAIN.TEST) %>%
  data.matrix(.) %>%
  t(.)

#
TEST.PRED <- preProcess(TEST, method = my_preProcess) %>%
  predict(., TEST) %>%
  data.matrix(.) %>%
  t(.)


# Multi Layer Perceptron
data <- mx.symbol.Variable("data")

fc1 <- mx.symbol.FullyConnected(data, name = "fc1", num_hidden = 512)
act1 <- mx.symbol.Activation(fc1, act_type = "relu")
act1 <- mx.symbol.BatchNorm(act1, eps = 1e-06)
dp1 <- mx.symbol.Dropout(act1, p = 0.5)

fc2 <- mx.symbol.FullyConnected(dp1, num_hidden = 512)
act2 <- mx.symbol.Activation(fc2, act_type = "relu")
act2 <- mx.symbol.BatchNorm(act2, eps = 1e-06)
dp2 <- mx.symbol.Dropout(act2, p = 0.5)

fc3 <- mx.symbol.FullyConnected(dp2, num_hidden = 4)
softmax <- mx.symbol.SoftmaxOutput(fc3)

devices <- mx.cpu()
mx.set.seed(0)

# num.round の最適値を探す
for(i in 1:100){
  NUM.ROUND <- i
  ARRAY.BATCH.SIZE <- 100
  LEARNING.RATE <- 0.01
  MOMENTUM <- 0.9
  
  model <- mx.model.FeedForward.create(
    softmax
    ,X = TRAIN.TRAIN.PRED[explanation_variable,]
    ,y = TRAIN.TRAIN.PRED["response",]
    ,ctx = devices
    ,num.round = NUM.ROUND
    ,array.batch.size = ARRAY.BATCH.SIZE
    ,learning.rate = LEARNING.RATE
    ,momentum = MOMENTUM
    ,eval.metric = mx.metric.mlogloss
    #,eval.metric = mx.metric.accuracy
    ,initializer = mx.init.uniform(0.07)
    ,epoch.end.callback = mx.callback.log.train.metric(100)
  )
  
  
  # 学習用データで検証
  preds_train.train <- predict(model, TRAIN.TRAIN.PRED[explanation_variable,], ctx = devices) %>%
    t(.) %>%
    as.data.frame(.)
  
  names(preds_train.train) <- c("good", "human", "mechanical", "weather")
  
  y_pred  <- preds_train.train %>%
    as.matrix(.)
  
  preds_train.train.logloss <- MLmetrics::MultiLogLoss(y_true = TRAIN.TRAIN.PRED["response",], y_pred = y_pred)
  
  
  # テスト用データで検証
  preds_train.test <- predict(model, TRAIN.TEST.PRED[explanation_variable,], ctx = devices) %>%
    t(.) %>%
    as.data.frame(.)
  
  names(preds_train.test) <- c("good", "human", "mechanical", "weather")
  
  y_pred  <- preds_train.test %>%
    as.matrix(.)
  
  preds_train.test.logloss <- MLmetrics::MultiLogLoss(y_true = TRAIN.TEST.PRED["response",], y_pred = y_pred)
  
  
  # 結果の保存
  result.m2lp.df <- rbind(result.m2lp.df, mxnetResult.LogLoss())
  saveRDS(result.m2lp.df, "result/result.m2lp.df.data")
}


# グラフ描画
g <- ggplot(NULL) + ylab("ROC")
g <- g + geom_line(data = result.m2lp.df, aes(x = num.round, y = test_ROC_1, colour = "test"))
g <- g + geom_line(data = result.m2lp.df, aes(x = num.round, y = train_ROC_1, colour = "train"))
print(g)


# 求めた num.round の最適値を用いてモデルを作成
ARRAY.BATCH.SIZE <- 100
LEARNING.RATE <- 0.01
MOMENTUM <- 0.9

model <- mx.model.FeedForward.create(
  softmax
  ,X = TRAIN.TRAIN.PRED[explanation_variable,]
  ,y = TRAIN.TRAIN.PRED["response",]
  ,ctx = devices
  ,num.round = 63
  ,array.batch.size = ARRAY.BATCH.SIZE
  ,learning.rate = LEARNING.RATE
  ,momentum = MOMENTUM
  ,eval.metric = mx.metric.accuracy
  ,initializer = mx.init.uniform(0.07)
  ,epoch.end.callback = mx.callback.log.train.metric(100)
)

#
# 予測データにモデルの当てはめ
#
pred_test <- predict(model, TEST.PRED, ctx = devices) %>%
  t(.)

out <- data.frame(TEST$id, pred_test[,2])
PREPROCESS <- paste(my_preProcess, collapse = "_")


# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_.m2lp.csv", sep = "")
  
  if ( !file.exists(SUBMIT_FILENAME) ) {
    write.table(out, #出力データ
                SUBMIT_FILENAME, #出力先
                quote = FALSE, #文字列を「"」で囲む有無
                col.names = FALSE, #変数名(列名)の有無
                row.names = FALSE, #行番号の有無
                sep = "," #区切り文字の指定
    )
    break
  }
}
