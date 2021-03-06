require(caret)
require(caretEnsemble)
require(doParallel)

require(xgboost)
require(Matrix)

source("script/R/fun/tools.R")
result.xgbLinear.df <- readRDS("result/result.xgbLinear.df.data")

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
# xgbLinear
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
  #,seeds = seeds
)

# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(response, datetime)))

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  ,trControl = doParallel
  #,preProcess = my_preProcess
  ,tuneList = list(
    xgboost = caretModelSpec(
      method = "xgbLinear"
      ,metric = "logLoss" 
      ,label = TRAIN.TRAIN$response
      # ,objective = "multi:softmax"
      # ,num_class = 4
      ,tuneGrid = expand.grid(
        nrounds = c(50)
        ,lambda = c(.3)
        ,alpha =  c(1)
        ,eta = c(.1)
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
                              ,testX = subset(TRAIN.TEST, select = -c(response, datetime))
                              ,testY = unlist(subset(TRAIN.TEST, select = c(response)))
                             )

# dataType 列に Test と入っているもののみを抜き出す
testProb <- subset(allProb, dataType == "Test")
tp <- subset(testProb, object == "Object1")


###精度確認
y_pred  <- dplyr::select(tp, c(good, human, mechanical, weather)) %>%
  as.matrix(.)

MLmetrics::MultiLogLoss(y_true = tp$obs, y_pred = y_pred)


# 結果の保存
result.xgbLinear.df <- rbind(result.xgbLinear.df, summaryResult(model_list[[1]]))
saveRDS(result.xgbLinear.df, "result/result.xgbLinear.df.data")

# predict() を利用した検算 
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.verification <- predict(
    model_list[[1]]$finalModel
    #,unlist(subset(TRAIN.TEST, select = -c(response,datetime)))
    ,as.matrix(subset(TRAIN.TEST, select = -c(response,datetime)))
    ,type = "response"
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
  
  pred_test <- pred_test[,"yes"]
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}


#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST$id, pred_test)
names(out) <- c("id","pred")

# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_xgbLinear.csv", sep = "")
  
  if ( !file.exists(SUBMIT_FILENAME) ) {
    write.table(out, #出力データ
                SUBMIT_FILENAME, #出力先
                quote = FALSE, #文字列を「"」で囲む有無
                col.names = TRUE, #変数名(列名)の有無
                row.names = FALSE, #行番号の有無
                sep = "," #区切り文字の指定
    )
    break
  }
}
