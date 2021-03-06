require(caret)
require(caretEnsemble)
require(pROC)
require(doParallel)

require(rpart)
require(partykit)
require(rattle)

source("script/R/fun/tools.R")
result.rpart2.df <- readRDS("result/result.rpart2.df.data")

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
# rpart
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
cp_variable <- 0

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  #,trControl = my_control
  ,trControl = doParallel
  #,preProcess = my_preProcess
  ,tuneList = list(
    rpart = caretModelSpec(
      method = "rpart2"
      ,metric = "logLoss"
      ,tuneGrid = expand.grid(
        maxdepth = seq(1, 20)
      )
      ,control = rpart.control(
                                cp = cp_variable
                                ,minbucket = minbucket_variable
                                ,method = "class"
      )
    )
  )
)

stopCluster(cl)
registerDoSEQ()

model_list[[1]]$times
# $everything
# ユーザ   システム       経過  
# 5.844      1.743     42.882 

model_list[[1]]
model_list[[1]]$finalModel
rattle::fancyRpartPlot(model_list[[1]]$finalModel)
model_list[[1]]$finalModel$variable.importance
varImp(model_list[[1]], scale = FALSE, useModel = FALSE)
varImp(model_list[[1]], scale = FALSE)
plot(varImp(model_list[[1]], scale = FALSE))

ggplot(model_list[[1]]) 


#
# テストデータにモデルを当てはめる ( Prob )
#
allProb <- caret::extractProb(
                              list(model_list[[1]])
                              ,testX = subset(TRAIN.TEST, select = -c(response))
                              ,testY = unlist(subset(TRAIN.TEST, select = c(response)))
                             )

# dataType 列に Test と入っているもののみを抜き出す
testProb <- subset(allProb, dataType == "Test")
tp <- subset(testProb, object == "Object1")

# 精度確認
y_pred  <- dplyr::select(tp, c(good, human, mechanical, weather)) %>%
  as.matrix(.)

MLmetrics::MultiLogLoss(y_true = tp$obs, y_pred = y_pred)

# 結果の保存
result.rpart2.df <- rbind(result.rpart.df, summaryResult.LogLoss(model_list[[1]]))
saveRDS(result.rpart2.df, "result/result.rpart2.df.data")

# predict() を利用した検算 
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.verification <- predict(
    model_list[[1]]$finalModel
    ,subset(TRAIN.TEST, select = -c(response))
  )
} else {
  # preProcess を指定している場合
  pred_test.verification <- preProcess(
    subset(TRAIN.TEST, select = -c(response))
    ,method = my_preProcess
  ) %>%
    predict(., subset(TRAIN.TEST, select = -c(response))) %>%
    predict(model_list[[1]]$finalModel, .)
}

MLmetrics::MultiLogLoss(y_true = TRAIN.TEST$response, y_pred = pred_test.verification)


#
# 予測データにモデルの当てはめ
#
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test <- predict(model_list[[1]]$finalModel, TEST, type = "prob")
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  pred_test <- preProcess(TEST, method = my_preProcess) %>%
    predict(., TEST) %>%
    predict(model_list[[1]]$finalModel, ., type = "prob")
  
  pred_test <- pred_test
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}


#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST$datetime, pred_test)

sapply(out, function(x) sum(is.na(x)))


# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  
  if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
    # 実行環境が Mac の場合
    SUBMIT_FILENAME <- paste("/Users/ryo/Desktop/deepanalytics/submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_rpart.csv", sep = "")
  } else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
    # 実行環境が Windows の場合
    SUBMIT_FILENAME <- paste("/Users/r-ogata/Desktop/deepanalytics/submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_rpart.csv", sep = "")
  } else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
    # 実行環境が Ubuntu の場合
    SUBMIT_FILENAME <- paste("submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_rpart.csv", sep = "")
  }
  
  
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
