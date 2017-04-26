require(caret)
require(caretEnsemble)
require(pROC)
require(doParallel)

require(rpart)
require(partykit)
require(glmnet)


source("script/R/fun/tools.R")
result.rpart.df <- readRDS("result/result.ensemble.df.data")

#
# 前処理
#
source("script/R/fun/Data-pre-processing.R")

my_preProcess <- c("center", "scale")

data_preProcess <- "none"

TRAIN <- all.train
TRAIN.TRAIN <- train.train
TRAIN.TEST <- train.test
TEST <- test

#
# 欠損値処理
#

# 欠損値の確認
sapply(train, function(x) sum(is.na(x)))

#
# rpart & glmnet
#

# seeds の決定
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 500)
seeds[[51]] <- sample.int(1000, 1)

my_control <- trainControl(
  method = "cv"
  ,number = 10
  ,summaryFunction = mnLogLoss
  ,classProbs = TRUE
  ,verbose = TRUE
  ,savePredictions = "final"
  ,index = createResample(TRAIN.TRAIN$response, 10)
  ,seeds = seeds
)

doParallel <- trainControl(
  method = "cv"
  ,number = 10
  ,summaryFunction = mnLogLoss
  ,classProbs = TRUE
  ,allowParallel=TRUE
  ,verboseIter=TRUE
  ,savePredictions = "final"
  ,index = createResample(TRAIN.TRAIN$response, 10)
  ,seeds = seeds
)

# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(response, datetime)))

minbucket_variable <- 100
maxdepth_variable <- 14 

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

# rpart, glmnet それぞれのモデルを作成

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  ,trControl = doParallel
  ,tuneList = list(
    rpart = caretModelSpec(
      method = "rpart"
      ,metric = "logLoss"
      ,tuneGrid = expand.grid(
        cp = 0
      )
      ,control = rpart.control(
        maxdepth = maxdepth_variable
        ,minbucket = minbucket_variable
        ,method = "class"
      )
    ),
    glmnet = caretModelSpec(
      method = "glmnet"
      ,metric = "logLoss"
      ,family = "multinomial"
      ,tuneGrid = expand.grid(
        alpha = 0.01
        ,lambda = 0.01
      )
    )
  )
)

stopCluster(cl)
registerDoSEQ()

# アンサンブルモデル作成用データ作成

# rpart 

if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.rpart <- predict(
    model_list[[1]]$finalModel
    ,subset(TRAIN, select = -c(response))
  )
} else {
  # preProcess を指定している場合
  pred_test.rpart <- preProcess(
    subset(TRAIN, select = -c(response))
    ,method = my_preProcess
  ) %>%
    predict(., subset(TRAIN, select = -c(response))) %>%
    predict(model_list[[1]]$finalModel, .)
}

pred_test.rpart <- as.data.frame(pred_test.rpart)
names(pred_test.rpart) <- paste0(names(pred_test.rpart), ".rpart")

# glmnet 

if (is.null(model_list[[2]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.glmnet <- predict(
    model_list[[2]]$finalModel
    ,newx = Matrix(as.matrix(subset(TRAIN, select = -c(response, datetime))), sparse = TRUE )
    ,s = model_list[[2]]$bestTune$lambda
    ,type = "response"
  )
} else {
  # preProcess を指定している場合
  pred_test.glmnet <- preProcess(
    subset(TRAIN, select = -c(response, datetime))
    ,method = my_preProcess
  ) %>%
    predict(., subset(TRAIN, select = -c(response, datetime))) %>%
    as.matrix(.) %>%
    Matrix(., sparse = TRUE ) %>%
    predict(
      model_list[[2]]$finalModel
      ,newx = .
      ,s = model_list[[2]]$bestTune$lambda
      ,type = "response"
    )
}

pred_test.glmnet <- as.data.frame(pred_test.glmnet)
names(pred_test.glmnet) <- c("good.glmnet", "human.glmnet", "mechanical.glmnet", "weather.glmnet")


# アンサンブル用モデルの作成 ====

train.ensemble <- cbind(TRAIN[,c("datetime", "response")], pred_test.rpart, pred_test.glmnet)

# 訓練データと検証データに分割する
inTrain.ensemble <- caret::createDataPartition(train.ensemble$response, p = .8, list = FALSE)
train.train.ensemble <- train.ensemble[inTrain.ensemble,]
train.test.ensemble <- train.ensemble[-inTrain.ensemble,]

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_ensemble <- caretList(
  x = train.train.ensemble[,c("good.rpart", "human.rpart", "mechanical.rpart", "weather.rpart", "good.glmnet"
                        , "human.glmnet", "mechanical.glmnet", "weather.glmnet")]
  ,y = train.train.ensemble$response
  ,trControl = doParallel
  ,tuneList = list(
    glmnet = caretModelSpec(
      method = "glmnet"
      ,metric = "logLoss"
      ,family = "multinomial"
      ,tuneGrid = expand.grid(
        alpha = 0.01
        ,lambda = 0.01
        # alpha = 1:5 * 0.1
        # ,lambda = 10^{1:5 * -1}
        # alpha = 1:5 * 0.01
        # ,lambda = 1:5 * 0.01
        # alpha = c(0.006, 0.007, 0.008, 0.009, 0.01, 0.011, 0.012, 0.013, 0.015, 0.016)
        # ,lambda = c(0.006, 0.007, 0.008, 0.009, 0.01, 0.011, 0.012, 0.013, 0.015, 0.016)
        # alpha = c(0.006, 0.007, 0.008, 0.009, 0.01)
        # ,lambda = c(0.006, 0.007, 0.008, 0.009, 0.01)
        #alpha = c(0.006, 0.005, 0.004, 0.003, 0.002)
        #,lambda = c(0.006, 0.005, 0.004, 0.003, 0.002)
      )
    )
  )
)

stopCluster(cl)
registerDoSEQ()

#
# モデル比較
#
allProb <- caret::extractProb(
  list(model_ensemble[[1]])
  ,testX = subset(train.test.ensemble, select = -c(response,datetime))
  ,testY = unlist(subset(train.test.ensemble, select = c(response)))
)

# dataType 列に Test と入っているもののみを抜き出す
testProb <- subset(allProb, dataType == "Test")
tp <- subset(testProb, object == "Object1")

# 精度確認
y_pred  <- dplyr::select(tp, c(good, human, mechanical, weather)) %>%
  as.matrix(.)

MLmetrics::MultiLogLoss(y_true = tp$obs, y_pred = y_pred)

# 結果の保存
result.ensemble.df <- rbind(result.ensemble.df, summaryResult.LogLoss(model_ensemble[[1]]))
saveRDS(result.ensemble.df, "result/result.ensemble.df.data")


# 評価用データの作成 ( 前処理 ) ====

# rpart 

if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred.rpart <- predict(model_list[[1]]$finalModel, TEST, type = "prob")
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  pred.rpart <- preProcess(TEST, method = my_preProcess) %>%
    predict(., TEST) %>%
    predict(model_list[[1]]$finalModel, ., type = "prob")
  
  pred_test <- pred_test
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}

pred.rpart <- as.data.frame(pred.rpart)
names(pred.rpart) <- paste0(names(pred.rpart), ".rpart")

# glmnet

if (is.null(model_list[[2]]$preProcess)){
  # preProcess を指定していない場合
  pred.glmnet <- predict(
    model_list[[2]]$finalModel
    ,newx = Matrix(as.matrix(subset(TEST, select = -c(datetime))), sparse = TRUE )
    ,s = model_list[[2]]$bestTune$lambda
    ,type = "response"
  )
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  pred.glmnet <- preProcess(
    TEST
    ,method = my_preProcess
  ) %>%
    predict(., subset(TEST, select = -c(datetime))) %>%
    as.matrix(.) %>%
    Matrix(., sparse = TRUE ) %>%
    predict(
      model_list[[2]]$finalModel
      ,newx = .
      ,s = model_list[[2]]$bestTune$lambda
      ,type = "response"
    )
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}

pred.glmnet <- as.data.frame(pred.glmnet)
names(pred.glmnet) <- c("good.glmnet", "human.glmnet", "mechanical.glmnet", "weather.glmnet")

TEST.ENSEMBLE <- cbind(datetime = TEST[,c("datetime")], pred.rpart, pred.glmnet)


# アンサンブルモデルを利用した評価用データ作成

if (is.null(model_ensemble[[1]]$preProcess)){
  # preProcess を指定していない場合
  ensemble.glmnet <- predict(
    model_ensemble[[1]]$finalModel
    ,newx = Matrix(as.matrix(subset(TEST.ENSEMBLE, select = -c(datetime))), sparse = TRUE )
    ,s = model_ensemble[[1]]$bestTune$lambda
    ,type = "response"
  )
} else {
  # preProcess を指定している場合
  ensemble.glmnet <- preProcess(
    subset(TRAIN, select = -c(response, datetime))
    ,method = my_preProcess
  ) %>%
    predict(., subset(TEST.ENSEMBLE, select = -c(datetime))) %>%
    as.matrix(.) %>%
    Matrix(., sparse = TRUE ) %>%
    predict(
      model_ensemble[[1]]$finalModel
      ,newx = .
      ,s = model_ensemble[[1]]$bestTune$lambda
      ,type = "response"
    )
}

#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST.ENSEMBLE$datetime, ensemble.glmnet)

sapply(out, function(x) sum(is.na(x)))

# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_ensemble.rpart.glmnet.csv", sep = "")
  
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
