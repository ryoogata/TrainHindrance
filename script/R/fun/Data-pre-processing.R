# Packages ----------------------------------------------------------------

require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

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
train <- read.csv("./data/train.csv"
                ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(train)
# [1] 78768     6
sapply(train, function(x) sum(is.na(x)))


# 詳細区分支障データ ====
detail <- read.csv("./data/detail.csv"
                ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(detail)
# [1] 78768     6
sapply(detail, function(x) sum(is.na(x)))

names(detail) <- c("datetime","tyuou.detail","keihintohoku.detail","keiyou.detail","uchibou.detail","saikyoukawagoe.detail")



# 支障区分ID参照用ファイル ====
reference <- read.csv("./data/reference.tsv"
                ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8", sep = "\t")
# dim(reference)
# [1] 55  4
sapply(reference, function(x) sum(is.na(x)))


# 応募用サンプルファイル ====
sample_submit <- read.csv("./data/sample_submit.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(sample_submit)
# [1] 393839      5
sapply(sample_submit, function(x) sum(is.na(x)))


# 気象観測地点情報ファイル ====
observation_point <- read.csv("./data/observation_point.tsv"
                ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8", sep = "\t")
# dim(observation_point)
# [1] 219  12

names(observation_point) <- c("id", "centralid", "municipality", "address", "latitude", "longitude", "elevation",
                              "buildinghigh", "temperature", "humidity", "precipitation", "windinfo")
sapply(observation_point, function(x) sum(is.na(x)))


# 気温データ ====
temperature <- read.csv("./data/temperature.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(temperature)
# [1] 17093352        6

names(temperature) <- c("datetime", "centralid", "municipality", "datatype", "quality", "temperature")
temperature$temperature <- as.numeric(temperature$temperature)

sapply(temperature, function(x) sum(is.na(x)))



# 降水量データ ====
precipitation <- read.csv("./data/precipitation.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(precipitation)
# [1] 13726958        6

names(precipitation) <- c("datetime", "centralid", "municipality", "datatype", "quality", "precipitation")
sapply(precipitation, function(x) sum(is.na(x)))


# 湿度データ ====
humidity <- read.csv("./data/humidity.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(humidity)
# [1] 17093370        6

names(humidity) <- c("datetime", "centralid", "municipality", "datatype", "quality", "humidity")
humidity$humidity <- as.numeric(humidity$humidity)

sapply(humidity, function(x) sum(is.na(x)))



# 雷データ ====
thunder <- read.csv("./data/thunder.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(thunder)
# [1] 125787      4
names(thunder) <- c("datetime", "latitude", "longitude", "kind")
sapply(thunder, function(x) sum(is.na(x)))


# 風速データ ====
wind <- read.csv("./data/wind.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(wind)
# [1] 15532645        6
names(wind) <- c("datetime", "centralid", "municipality", "datatype", "quality", "windspeed")
sapply(wind, function(x) sum(is.na(x)))


# 風向データ ====
wind_dir <- read.csv("./data/wind_dir.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(wind_dir)
# [1] 15611390        8
names(wind_dir) <- c("datetime", "centralid", "municipality", "datatype", "quality", "360degrees", "16direction", "36direction")
sapply(wind_dir, function(x) sum(is.na(x)))


# 最大瞬間風速データ ====
wind_max <- read.csv("./data/wind_max.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(wind_max)
# [1] 15612256       10
names(wind_max) <- c("datetime", "centralid", "municipality", "datatype", "quality",
                     "mwgs", "360degrees", "exact_datetime", "16direction", "36direction")
sapply(wind_max, function(x) sum(is.na(x)))

all <- dplyr::inner_join(train, detail, by = c("datetime"))
