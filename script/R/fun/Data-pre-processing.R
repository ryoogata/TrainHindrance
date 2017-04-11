# Packages ----------------------------------------------------------------

require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

source("script/R/fun/functions.R")
source("variables.R")

# データの読み込み --------------------------------------------------------

# 学習用データ
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  train <- read.csv("/Users/ryo/Desktop/deepanalytics/train.edited.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  train <- read.csv("/Users/r-ogata/Desktop/deepanalytics/train.edited.csv")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  train <- read.csv("data/train.edited.csv")
}

# 評価用データ
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  test <- read.csv("/Users/ryo/Desktop/deepanalytics/test/test.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  test <- read.csv("/Users/r-ogata/Desktop/deepanalytics/test/test.csv")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  test <- read.csv("test/test.csv")
}


train <- na.omit(train)

# train/test のタグ付け
train$data <- "train"
# test$data <- "test"

# train と test をマージ
#all <- bind_rows(train, test)
all <- train

# 目的変数の factor 化
all[which(all$response == 0 & all$data == "train"),"response"] <- "good"
all[which(all$response == 1 & all$data == "train"),"response"] <- "human"
all[which(all$response == 2 & all$data == "train"),"response"] <- "mechanical"
all[which(all$response == 3 & all$data == "train"),"response"] <- "weather"

#
# Dummy 変数なし
#

# all から train/test のデータを抽出
all.train <- all[which(all$data == "train"),]
all.train$response <- as.factor(all.train$response)
#all.test <- all[which(all$data == "test"),]

# 不要な列: data を
all.train <- subset(all.train, select = -c(data))
#all.test <- subset(all.test, select = -c(data))

# 再現性のため乱数シードを固定
set.seed(10)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
inTrain <- caret::createDataPartition(all.train$response, p = .8, list = FALSE)
train.train <- all.train[inTrain,]
train.test <- all.train[-inTrain,]

dim(train.train)
# [1] 304253     17

train.train$response %>% table
# .
# 0      1      2      3 
# 292147   5891   3488   2727 

dim(train.test)
# [1] 76061    17

train.test$response %>% table
# .
# 0     1     2     3 
# 73036  1472   872   681 

