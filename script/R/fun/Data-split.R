# Packages ----------------------------------------------------------------

require(dplyr)

# 指数表示の回避
options(scipen=10)

source("script/R/fun/functions.R")
source("variables.R")

# Datasets ----------------------------------------------------------------

# 気温データ ====

if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  temperature <- read.csv("~/Desktop/deepanalytics/temperature.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  temperature <- read.csv("/Users/r-ogata/Desktop/deepanalytics/temperature.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  temperature <- read.csv("./data/temperature.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
}
# dim(temperature)
# [1] 17093352        6
#
names(temperature) <- c("datetime", "centralid", "municipality", "datatype", "quality", "temperature")
#
# centralid 毎にファイルに保存
splitsave(temperature)
# sapply(temperature, function(x) sum(is.na(x)))


# 降水量データ ====
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  precipitation <- read.csv("~/Desktop/deepanalytics/precipitation.csv"
                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  precipitation <- read.csv("/Users/r-ogata/Desktop/deepanalytics/precipitation.csv"
                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  precipitation <- read.csv("./data/precipitation.csv"
                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
}
# dim(precipitation)
# [1] 13726958        6
#
names(precipitation) <- c("datetime", "centralid", "municipality", "datatype", "quality", "precipitation")
#
# centralid 毎にファイルに保存
splitsave(precipitation)
# sapply(precipitation, function(x) sum(is.na(x)))

 
# 湿度データ ====
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  humidity <- read.csv("~/Desktop/deepanalytics/humidity.csv"
                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  humidity <- read.csv("/Users/r-ogata/Desktop/deepanalytics/humidity.csv"
                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  humidity <- read.csv("./data/humidity.csv"
                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
}
 
names(humidity) <- c("datetime", "centralid", "municipality", "datatype", "quality", "humidity")
#
# centralid 毎にファイルに保存
splitsave(humidity)
# sapply(humidity, function(x) sum(is.na(x)))


# 風速データ ====
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  windspeed <- read.csv("~/Desktop/deepanalytics/wind.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  windspeed <- read.csv("/Users/r-ogata/Desktop/deepanalytics/wind.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  windspeed <- read.csv("./data/wind.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
}
# dim(windspeed)
# [1] 15532645        6
names(windspeed) <- c("datetime", "centralid", "municipality", "datatype", "quality", "windspeed")
#
# centralid 毎にファイルに保存
splitsave(windspeed)
#
# sapply(windspeed, function(x) sum(is.na(x)))


# 最大瞬間風速データ(最大瞬間風速) ====
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  mwgs <- read.csv("~/Desktop/deepanalytics/wind_max.csv"
               ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  mwgs <- read.csv("/Users/r-ogata/Desktop/deepanalytics/wind_max.csv"
               ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  mwgs <- read.csv("./data/wind_max.csv"
               ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
}
# dim(mwgs)
# [1] 15612256       10
names(mwgs) <- c("datetime", "centralid", "municipality", "datatype", "quality",
                   "mwgs", "degrees360", "exact_datetime", "direction16", "direction36")
# centralid 毎にファイルに保存
splitsave(mwgs)
#
# sapply(mwgs, function(x) sum(is.na(x)))