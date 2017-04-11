# Packages ----------------------------------------------------------------

require(dplyr)

# 指数表示の回避
options(scipen=10)

source("script/R/fun/functions.R")
source("variables.R")

# Datasets ----------------------------------------------------------------

#
# データの読み込み
#

# 観測日時: datetime
# 局ID: centralid
# 市町村区コード: municipality
# データ種別コード: datatype
# 品質コード: quality
# 緯度: latitude
# 経度: longitude
# 種別: kind
# 風向16方位: 16direction
# 風向36方位: 36direction
# 風向360度:  360degrees
# 測定値(最大瞬間風速観測時の時刻): exact_datetime
# 最大瞬間風速: mwgs (maximum wind gust speed)
# 風速: windspeed
# 湿度: humidity
# 降水量: precipitation
# 住所: address
# 標高: elevation
# 建物高: buildinghigh
# 気温: temperature
# 風向風速: windinfo


# モデル学習用支障データ ====
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  train.orig <- read.csv("~/Desktop/deepanalytics/train.csv"
              ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  train.orig <- read.csv("/Users/r-ogata/Desktop/deepanalytics/train.csv"
              ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  train.orig <- read.csv("./data/train.csv"
              ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
}
# dim(train.orig)
# [1] 78768     6
# sapply(train.orig, function(x) sum(is.na(x)))
# 
# length(train.orig[,"tyuou"])           # 78768
# length(train.orig[,"keihintohoku"])    # 78768
# length(train.orig[,"keiyou"])          # 78768
# length(train.orig[,"uchibou"])         # 78768
# length(train.orig[,"saikyoukawagoe"])  # 78768
# 
# train.tyuou <- train[which(train.orig$tyuou != 0),"tyuou"]
# train.keihintohoku <- train[which(train.orig$keihintohoku != 0),"keihintohoku"]
# train.keiyou <- train[which(train.orig$keiyou != 0),"keiyou"]
# train.uchibou <- train[which(train.orig$uchibou != 0),"uchibou"]
# train.saikyoukawagoe <- train[which(train.orig$saikyoukawagoe != 0),"saikyoukawagoe"]
# 
# length(train.tyuou)           # 3884
# length(train.keihintohoku)    # 4104
# length(train.keiyou)          # 1972
# length(train.uchibou)         # 2035
# length(train.saikyoukawagoe)  # 3357
# 
# # table(train.tyuou)
# # train.tyuou
# # 1    2    3 
# # 2099 1450  335 
# hist(train.tyuou)
# 
# # table(train.keihintohoku)
# # train.keihintohoku
# # 1    2    3 
# # 2631 1211  262
# hist(train.keihintohoku)
# 
# # table(train.keiyou)
# # train.keiyou
# # 1   2   3 
# # 497 651 824 
# hist(train.keiyou)
# 
# # table(train.uchibou)
# # train.uchibou
# # 1    2    3 
# # 604  274 1427 
# hist(train.uchibou)
# 
# # table(train.saikyoukawagoe)
# # train.saikyoukawagoe
# # 1    2    3 
# # 1715  915  727 
# hist(train.saikyoukawagoe)
# 
# # train$datetime <- as.POSIXlt(train$datetime)

# 気温データ ====

makeTemperature <- function(vector){
  makeDF("temperature", vector) %>%
  dplyr::mutate_at(vars(temperature), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
           ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
           ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean))
}


# 学習用路線毎の気温データの作成 
temperature.tyuou <- makeTemperature(centralid.tyuou)
temperature.keihintohoku <- makeTemperature(centralid.keihintohoku)
temperature.keiyou <- makeTemperature(centralid.keiyou)
temperature.uchibou <- makeTemperature(centralid.uchibou)
temperature.saikyoukawagoe <- makeTemperature(centralid.saikyoukawagoe)


# 降水量データ ====

# 
makePrecipitation <- function(vector){
  makeDF("precipitation", vector) %>%
  dplyr::mutate_at(vars(precipitation), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max)
           ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min)
           ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean))
}

# 学習用路線毎の降水量データの作成 
precipitation.tyuou <- makePrecipitation(centralid.tyuou)
precipitation.keihintohoku <- makePrecipitation(centralid.keihintohoku)
precipitation.keiyou <- makePrecipitation(centralid.keiyou)
precipitation.uchibou <- makePrecipitation(centralid.uchibou)
precipitation.saikyoukawagoe <- makePrecipitation(centralid.saikyoukawagoe)

 
# 湿度データ ====

# 
makeHumidity <- function(vector){
  makeDF("humidity", vector) %>%
  dplyr::mutate_at(vars(humidity), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max)
           ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min)
           ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean))
}

# 学習用路線毎の湿度データの作成 
humidity.tyuou <- makeHumidity(centralid.tyuou)
humidity.keihintohoku <- makeHumidity(centralid.keihintohoku)
humidity.keiyou <- makeHumidity(centralid.keiyou)
humidity.uchibou <- makeHumidity(centralid.uchibou)
humidity.saikyoukawagoe <- makeHumidity(centralid.saikyoukawagoe)


# 風速データ ====

# 
makeWindspeed <- function(vector){
  makeDF("windspeed", vector) %>%
  dplyr::mutate_at(vars(windspeed), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max)
           ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min)
           ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean))
}

# 学習用路線毎の風速データの作成 
windspeed.tyuou <- makeWindspeed(centralid.tyuou)
windspeed.keihintohoku <- makeWindspeed(centralid.keihintohoku)
windspeed.keiyou <- makeWindspeed(centralid.keiyou)
windspeed.uchibou <- makeWindspeed(centralid.uchibou)
windspeed.saikyoukawagoe <- makeWindspeed(centralid.saikyoukawagoe)


# 最大瞬間風速データ(最大瞬間風速) ====

#
makeMwgs <- function(vector){
  makeDF("mwgs", vector) %>%
  dplyr::mutate_at(vars(mwgs), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max)
           ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min)
           ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean))
}

# 学習用路線毎のデータの作成 
mwgs.tyuou <- makeMwgs(centralid.tyuou)
mwgs.keihintohoku <- makeMwgs(centralid.keihintohoku)
mwgs.keiyou <- makeMwgs(centralid.keiyou)
mwgs.uchibou <- makeMwgs(centralid.uchibou)
mwgs.saikyoukawagoe <- makeMwgs(centralid.saikyoukawagoe)


# データ結合 ====

# 中央線
train.tyuou <- dplyr::select(train.orig, c(datetime, tyuou)) %>%
  dplyr::full_join(., temperature.tyuou, by = "datetime") %>%
  dplyr::full_join(., precipitation.tyuou, by = "datetime") %>%
  dplyr::full_join(., humidity.tyuou, by = "datetime") %>%
  dplyr::full_join(., mwgs.tyuou, by = "datetime") %>%
  dplyr::full_join(., windspeed.tyuou, by = "datetime")

names(train.tyuou)[2] <- "response"

# 京浜東北線
train.keihintohoku <- dplyr::select(train.orig, c(datetime, keihintohoku)) %>%
  dplyr::full_join(., temperature.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., precipitation.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., humidity.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., mwgs.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., windspeed.keihintohoku, by = "datetime")

names(train.keihintohoku)[2] <- "response"

# 京葉線
train.keiyou <- dplyr::select(train.orig, c(datetime, keiyou)) %>%
  dplyr::full_join(., temperature.keiyou, by = "datetime") %>%
  dplyr::full_join(., precipitation.keiyou, by = "datetime") %>%
  dplyr::full_join(., humidity.keiyou, by = "datetime") %>%
  dplyr::full_join(., mwgs.keiyou, by = "datetime") %>%
  dplyr::full_join(., windspeed.keiyou, by = "datetime")

names(train.keiyou)[2] <- "response"

# 宇都宮線
train.uchibou  <- dplyr::select(train.orig, c(datetime, uchibou )) %>%
  dplyr::full_join(., temperature.uchibou , by = "datetime") %>%
  dplyr::full_join(., precipitation.uchibou , by = "datetime") %>%
  dplyr::full_join(., humidity.uchibou , by = "datetime") %>%
  dplyr::full_join(., mwgs.uchibou , by = "datetime") %>%
  dplyr::full_join(., windspeed.uchibou , by = "datetime")

names(train.uchibou)[2] <- "response"

# 埼京川越線
train.saikyoukawagoe  <- dplyr::select(train.orig, c(datetime, saikyoukawagoe )) %>%
  dplyr::full_join(., temperature.saikyoukawagoe , by = "datetime") %>%
  dplyr::full_join(., precipitation.saikyoukawagoe , by = "datetime") %>%
  dplyr::full_join(., humidity.saikyoukawagoe , by = "datetime") %>%
  dplyr::full_join(., mwgs.saikyoukawagoe , by = "datetime") %>%
  dplyr::full_join(., windspeed.saikyoukawagoe , by = "datetime")

names(train.saikyoukawagoe)[2] <- "response"

train <-  rbind(train.tyuou, train.keihintohoku) %>%
  rbind(., train.keiyou) %>%
  rbind(., train.uchibou) %>%
  rbind(., train.saikyoukawagoe)


if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/train.edited.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/train.edited.csv")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  SAVE_DIR = paste0("data/train.edited.csv")
}

write.table(train , file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)