# Packages ----------------------------------------------------------------

require(caret)
require(dplyr)

# 指数表示の回避
options(scipen=10)

# function ----------------------------------------------------------------

# 列:centralid 毎に新しい data.frame を作成する
split <- function(dataframe){
  id <- unique(dataframe$centralid)
  for(i in id){
    df.name <- deparse(substitute(dataframe)) 
    name <- paste0(df.name, ".", i)
    assign(x = name, value = dataframe[which(dataframe$centralid == i),], envir = .GlobalEnv)
  }
}

splitsave <- function(dataframe){
  id <- unique(dataframe$centralid)
  for(i in id){
    df.name <- deparse(substitute(dataframe)) 
    name <- paste0(df.name, ".", i)
    assign(x = name, value = dataframe[which(dataframe$centralid == i),])
    
    if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Mac の場合
      SAVE_DIR = paste0("~/Desktop/deepanalytics/", deparse(substitute(dataframe)), "/", name, ".csv")
    } else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Windows の場合
      SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/", deparse(substitute(dataframe)), "/", name, ".csv")
    }
    write.table(eval(parse(text=name)) , file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)
  }
}

# 列:datetime に 10 分加算した data.frame を返す
add10minute <- function(dataframe){
  dataframe$datetime <- as.character(as.POSIXlt(dataframe$datetime) + 600)
  return(dataframe)
}

# 気象データ区分(温度, 湿度, etc ) と路線の ID を入力して、data.frame を作成する
makeDF <- function(weatherdata, centralid){
  # 
  for(i in centralid){
    if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Mac の場合
      FILE = paste0("/Users/ryo/Desktop/deepanalytics/", weatherdata, "/", weatherdata, ".", i, ".csv")
    } else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Windows の場合
      FILE = paste0("/Users/r-ogata/Desktop/deepanalytics/", weatherdata, "/", weatherdata, ".", i, ".csv")
    } 
    
    df.name <- paste0(weatherdata, "." , i)
    assign(x = df.name, value = read.csv(FILE, header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8"))
  }
  n <- length(centralid)
  tmp.name <- paste0(weatherdata, ".", centralid[1])
  tmp <- eval(parse(text = tmp.name))
  for(i in 2:n){
    tmp.name <- paste0(weatherdata, ".", centralid[i])
    tmp <- rbind(tmp, eval(parse(text = tmp.name)))
  }
  return(tmp)
}

# 前後の値の平均を求める
dataCompletion <- function(dataframe, colname, rownumber){
  if ( is.na(dataframe[rownumber - 1, colname]) == FALSE || is.na(dataframe[rownumber + 1, colname]) == FALSE ){
    result <- mean(c(dataframe[rownumber - 1, colname], dataframe[rownumber + 1, colname]), na.rm = TRUE)
  } else {
    result <- NA
  }
  return(result)
}


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

# variable ====
centralid.tyuou <- c(33130167, 2030130001, 33739426, 33140505)
centralid.keihintohoku <- c(33114285, 33110164, 33130167, 33149672, 33144696, 2030130002, 33139659)
centralid.keiyou <- c(33124109, 2030120003, 33130174, 2030130002, 33130167)
centralid.uchibou <- c(33120159, 33129553, 34124264, 34120500, 34124620, 33129468)
centralid.saikyoukawagoe <- c(33114285, 33119768, 33114029)

centralid.sotobou <- c(33129448, 33129576, 33129135)
centralid.yamanote <- c(2030130001, 33139659, 2030130002, 33130167)
centralid.utsunomiya <- c(33094345, 33099131, 33099252, 33094030, 33094340, 33089175, 33114247, 33114285, 33110164, 33130167)
centralid.shounanshinjuku <- c(33144016, 33149220, 33139659, 2030130001, 33110164, 33140537, 33114285, 33119753, 33109203)
centralid.takasaki <- c(33114285, 33119753, 33109203)


# モデル学習用支障データ ====
# train.orig <- read.csv("./data/train.csv"
 train.orig <- read.csv("~/Desktop/deepanalytics/train.csv"
# train.orig <- read.csv("/Users/r-ogata/Desktop/deepanalytics/train.csv"
                ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(train.orig)
# [1] 78768     6
sapply(train.orig, function(x) sum(is.na(x)))

length(train.orig[,"tyuou"])           # 78768
length(train.orig[,"keihintohoku"])    # 78768
length(train.orig[,"keiyou"])          # 78768
length(train.orig[,"uchibou"])         # 78768
length(train.orig[,"saikyoukawagoe"])  # 78768

train.tyuou <- train[which(train.orig$tyuou != 0),"tyuou"]
train.keihintohoku <- train[which(train.orig$keihintohoku != 0),"keihintohoku"]
train.keiyou <- train[which(train.orig$keiyou != 0),"keiyou"]
train.uchibou <- train[which(train.orig$uchibou != 0),"uchibou"]
train.saikyoukawagoe <- train[which(train.orig$saikyoukawagoe != 0),"saikyoukawagoe"]

length(train.tyuou)           # 3884
length(train.keihintohoku)    # 4104
length(train.keiyou)          # 1972
length(train.uchibou)         # 2035
length(train.saikyoukawagoe)  # 3357

# table(train.tyuou)
# train.tyuou
# 1    2    3 
# 2099 1450  335 
hist(train.tyuou)

# table(train.keihintohoku)
# train.keihintohoku
# 1    2    3 
# 2631 1211  262
hist(train.keihintohoku)

# table(train.keiyou)
# train.keiyou
# 1   2   3 
# 497 651 824 
hist(train.keiyou)

# table(train.uchibou)
# train.uchibou
# 1    2    3 
# 604  274 1427 
hist(train.uchibou)

# table(train.saikyoukawagoe)
# train.saikyoukawagoe
# 1    2    3 
# 1715  915  727 
hist(train.saikyoukawagoe)

# train$datetime <- as.POSIXlt(train$datetime)

train.tyuou <- dplyr::select(train, c(datetime, tyuou)) %>%
  dplyr::full_join(., precipitation.tyuou, by = "datetime")


# 気温データ ====
# temperature <- read.csv("./data/temperature.csv"
# temperature <- read.csv("/Users/r-ogata/Desktop/deepanalytics/temperature.csv"
# temperature <- read.csv("~/Desktop/deepanalytics/temperature.csv"
#                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(temperature)
# [1] 17093352        6
#
# names(temperature) <- c("datetime", "centralid", "municipality", "datatype", "quality", "temperature")
#
# centralid 毎にファイルに保存
# splitsave(temperature)
# sapply(temperature, function(x) sum(is.na(x)))

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
# precipitation <- read.csv("/Users/r-ogata/Desktop/deepanalytics/precipitation.csv"
# precipitation <- read.csv("./data/precipitation.csv"
# precipitation <- read.csv("~/Desktop/deepanalytics/precipitation.csv"
#                 ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(precipitation)
# [1] 13726958        6
#
# names(precipitation) <- c("datetime", "centralid", "municipality", "datatype", "quality", "precipitation")
#
# centralid 毎にファイルに保存
# splitsave(precipitation)
# sapply(precipitation, function(x) sum(is.na(x)))

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
# humidity <- read.csv("./data/humidity.csv"
# humidity <- read.csv("~/Desktop/deepanalytics/humidity.csv"
# humidity <- read.csv("/Users/r-ogata/Desktop/deepanalytics/humidity.csv"
#                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# 
# names(humidity) <- c("datetime", "centralid", "municipality", "datatype", "quality", "humidity")
#
# centralid 毎にファイルに保存
# splitsave(humidity)
# sapply(humidity, function(x) sum(is.na(x)))

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


# 雷データ ====
#thunder <- read.csv("./data/thunder.csv"
#thunder <- read.csv("/Users/r-ogata/Desktop/deepanalytics/thunder.csv"
thunder <- read.csv("~/Desktop/deepanalytics/thunder.csv"
                ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(thunder)
# [1] 125787      4
names(thunder) <- c("datetime", "latitude", "longitude", "kind")
sapply(thunder, function(x) sum(is.na(x)))

thunder$datetime <- as.POSIXlt(thunder$datetime)
thunder$align <- xts::align.time(thunder$datetime - 10*60, 10*60)

# http://qiita.com/tktz/items/733c37b1d6102ae52120


write.table(thunder, file = "~/Desktop/deepanalytics/thundermap.csv", sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)


# 風速データ ====
# windspeed <- read.csv("./data/wind.csv"
# windspeed <- read.csv("~/Desktop/deepanalytics/wind.csv"
# windspeed <- read.csv("/Users/r-ogata/Desktop/deepanalytics/wind.csv"
#               ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(windspeed)
# [1] 15532645        6
# names(windspeed) <- c("datetime", "centralid", "municipality", "datatype", "quality", "windspeed")
#
# centralid 毎にファイルに保存
# splitsave(windspeed)
#
# sapply(windspeed, function(x) sum(is.na(x)))

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


# 風向データ(風向16方位) ====
# wind_direction16 <- read.csv("./data/wind_dir.csv"
# wind_direction16 <- read.csv("~/Desktop/deepanalytics/wind_dir.csv"
# wind_direction16 <- read.csv("/Users/r-ogata/Desktop/deepanalytics/wind_dir.csv"
#               ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(wind_direction16)
# [1] 15611390        8
# names(wind_direction16) <- c("datetime", "centralid", "municipality", "datatype", "quality", "degrees360", "direction16", "direction36")
# sapply(wind_wind_dir, function(x) sum(is.na(x)))

# centralid 毎にファイルに保存
# splitsave(wind_direction16)

# 
makeWindDirection16 <- function(vector){
  makeDF("wind_direction16", vector) %>%
  dplyr::mutate_at(vars(direction16), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,direction16 = dplyr::select(., c(direction16))
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, direction16))
}

# 学習用路線毎の降水量データの作成 
wind_direction16.tyuou <- makeWindDirection16(centralid.tyuou)
wind_direction16.keihintohoku <- makeWindDirection16(centralid.keihintohoku)
wind_direction16.keiyou <- makeWindDirection16(centralid.keiyou)
wind_direction16.uchibou <- makeWindDirection16(centralid.uchibou)
wind_direction16.saikyoukawagoe <- makeWindDirection16(centralid.saikyoukawagoe)


# 最大瞬間風速データ(最大瞬間風速) ====
# mwgs <- read.csv("./data/wind_max.csv"
# mwgs <- read.csv("~/Desktop/deepanalytics/wind_max.csv"
# mwgs <- read.csv("/Users/r-ogata/Desktop/deepanalytics/wind_max.csv"
#              ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
# dim(mwgs)
# [1] 15612256       10
# names(mwgs) <- c("datetime", "centralid", "municipality", "datatype", "quality",
#                    "mwgs", "degrees360", "exact_datetime", "direction16", "direction36")
# centralid 毎にファイルに保存
# splitsave(mwgs)
#
# sapply(mwgs, function(x) sum(is.na(x)))

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

# 不要なオブジェクトの削除
rm(list = ls(pattern = "mwgs|precipitation|temperature|windspeed|humidity|train."))


if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/train.edited.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/train.edited.csv")
} 

write.table(train , file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)