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

# 日付データ ====

# install.packages("Nippon", repos = "cran.ism.ac.jp")

datetime <- read.csv("datetime.csv"
                     ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")

datetime$datetime <- as.POSIXlt(datetime$datetime)

datetime$month <- datetime$datetime$mon + 1 # 月
datetime$day <- datetime$datetime$mday # 日
datetime$hour <- datetime$datetime$hour # 時間
datetime$wday <- datetime$datetime$wday # 曜日
datetime$holiday <- Nippon::is.jholiday(as.Date(datetime$datetime)) # 祝日
datetime[which(datetime$holiday == TRUE),"holiday"] <- 1 # 祝日: 1

datetime$datetime <- as.character(datetime$datetime)


# 気温データ ====

makeTemperature <- function(vector){
  makeDF("temperature", vector) %>%
  dplyr::mutate_at(vars(temperature), as.numeric) %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
           ,datetime = dplyr::select(., c(datetime))
           ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max)
           ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min)
           ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
           ,temperature.median = apply(dplyr::select(., -c(datetime)), 1, median)
           ,temperature.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean, temperature.median, temperature.sd))
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
           ,precipitation.median = apply(dplyr::select(., -c(datetime)), 1, median)
           ,precipitation.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean, precipitation.median, precipitation.sd))
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
           ,humidity.median = apply(dplyr::select(., -c(datetime)), 1, median)
           ,humidity.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean, humidity.median, humidity.sd))
}

# 学習用路線毎の湿度データの作成 
humidity.tyuou <- makeHumidity(centralid.tyuou)
humidity.keihintohoku <- makeHumidity(centralid.keihintohoku)
humidity.keiyou <- makeHumidity(centralid.keiyou)
humidity.uchibou <- makeHumidity(centralid.uchibou)
humidity.saikyoukawagoe <- makeHumidity(centralid.saikyoukawagoe)



# 雷データ ( CG：対地雷 ) ====

makeThunderCG <- function(vector){
  tmp <- makeDF("thunder.CG", vector) %>% 
    na.omit(.) %>%
    dplyr::mutate_at(vars(datetime), as.POSIXct) %>%
    dplyr::mutate(aling = xts::align.time(.$datetime - 10*60, 10*60)) %>%
    dplyr::mutate_at(vars(aling), as.character)
    
  tmp <-  table(tmp$aling) %>%
    as.data.frame(.) %>%
    dplyr::mutate_at(vars(Var1), as.character) %>%
    dplyr::rename(datetime=Var1) %>%
    dplyr::left_join(datetime, . ,by="datetime")
    
  tmp[is.na(tmp$Freq), "Freq"] <- 0
  names(tmp)[7] <- "Freq.CG"
  
  tmp <- add10minute(tmp)
  
  return(tmp[,c("datetime", "Freq.CG")])
}

# 学習用路線毎の雷データ ( CG：対地雷 )の作成 
thunder.CG.tyuou <- makeThunderCG(centralid.tyuou)
thunder.CG.keihintohoku <- makeThunderCG(centralid.keihintohoku)
thunder.CG.keiyou <- makeThunderCG(centralid.keiyou)
thunder.CG.uchibou <- makeThunderCG(centralid.uchibou)
thunder.CG.saikyoukawagoe <- makeThunderCG(centralid.saikyoukawagoe)


# 雷データ ( IC：雲間雷 ) ====

makeThunderIC <- function(vector){
  tmp <- makeDF("thunder.IC", vector) %>% 
    na.omit(.) %>%
    dplyr::mutate_at(vars(datetime), as.POSIXct) %>%
    dplyr::mutate(aling = xts::align.time(.$datetime - 10*60, 10*60)) %>%
    dplyr::mutate_at(vars(aling), as.character)
  
  tmp <-  table(tmp$aling) %>%
    as.data.frame(.) %>%
    dplyr::mutate_at(vars(Var1), as.character) %>%
    dplyr::rename(datetime=Var1) %>%
    dplyr::left_join(datetime, . ,by="datetime")
  
  tmp[is.na(tmp$Freq), "Freq"] <- 0
  names(tmp)[7] <- "Freq.IC"
  
  tmp <- add10minute(tmp)
  
  return(tmp[,c("datetime", "Freq.IC")])
}


# 学習用路線毎の雷データ ( IC：雲間雷 ) の作成 
thunder.IC.tyuou <- makeThunderIC(centralid.tyuou)
thunder.IC.keihintohoku <- makeThunderIC(centralid.keihintohoku)
thunder.IC.keiyou <- makeThunderIC(centralid.keiyou)
thunder.IC.uchibou <- makeThunderIC(centralid.uchibou)
thunder.IC.saikyoukawagoe <- makeThunderIC(centralid.saikyoukawagoe)

 
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
           ,windspeed.median = apply(dplyr::select(., -c(datetime)), 1, median)
           ,windspeed.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean, windspeed.median, windspeed.sd))
}

# 学習用路線毎の風速データの作成 
windspeed.tyuou <- makeWindspeed(centralid.tyuou)
windspeed.keihintohoku <- makeWindspeed(centralid.keihintohoku)
windspeed.keiyou <- makeWindspeed(centralid.keiyou)
windspeed.uchibou <- makeWindspeed(centralid.uchibou)
windspeed.saikyoukawagoe <- makeWindspeed(centralid.saikyoukawagoe)


# 風向データ ====

makeWindDir <- function(vector){
  makeDF("wind_dir", vector) %>%
    dplyr::mutate_at(vars(direction16), as.numeric) %>%
    reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
    add10minute(.) %>%
    data.frame(.
               ,datetime = dplyr::select(., c(datetime))
               ,wind_dir.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
               ,wind_dir.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
               ,stringsAsFactors = FALSE
    ) %>%
    dplyr::select(.,c(datetime, wind_dir.mean, wind_dir.sd))
}

# 学習用路線毎の風速データの作成 
wind_dir.tyuou <- makeWindDir(centralid.tyuou)
wind_dir.keihintohoku <- makeWindDir(centralid.keihintohoku)
wind_dir.keiyou <- makeWindDir(centralid.keiyou)
wind_dir.uchibou <- makeWindDir(centralid.uchibou)
wind_dir.saikyoukawagoe <- makeWindDir(centralid.saikyoukawagoe)


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
           ,mwgs.median = apply(dplyr::select(., -c(datetime)), 1, median)
           ,mwgs.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
           ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean, mwgs.median, mwgs.sd))
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
  dplyr::full_join(., windspeed.tyuou, by = "datetime") %>%
  dplyr::full_join(., wind_dir.tyuou, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.tyuou, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.tyuou, by = "datetime")

names(train.tyuou)[2] <- "response"

# 京浜東北線
train.keihintohoku <- dplyr::select(train.orig, c(datetime, keihintohoku)) %>%
  dplyr::full_join(., temperature.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., precipitation.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., humidity.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., mwgs.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., windspeed.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., wind_dir.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.keihintohoku, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.keihintohoku, by = "datetime")

names(train.keihintohoku)[2] <- "response"

# 京葉線
train.keiyou <- dplyr::select(train.orig, c(datetime, keiyou)) %>%
  dplyr::full_join(., temperature.keiyou, by = "datetime") %>%
  dplyr::full_join(., precipitation.keiyou, by = "datetime") %>%
  dplyr::full_join(., humidity.keiyou, by = "datetime") %>%
  dplyr::full_join(., mwgs.keiyou, by = "datetime") %>%
  dplyr::full_join(., windspeed.keiyou, by = "datetime") %>%
  dplyr::full_join(., wind_dir.keiyou, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.keiyou, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.keiyou, by = "datetime")

names(train.keiyou)[2] <- "response"

# 宇都宮線
train.uchibou  <- dplyr::select(train.orig, c(datetime, uchibou )) %>%
  dplyr::full_join(., temperature.uchibou, by = "datetime") %>%
  dplyr::full_join(., precipitation.uchibou, by = "datetime") %>%
  dplyr::full_join(., humidity.uchibou, by = "datetime") %>%
  dplyr::full_join(., mwgs.uchibou, by = "datetime") %>%
  dplyr::full_join(., windspeed.uchibou, by = "datetime") %>%
  dplyr::full_join(., wind_dir.uchibou, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.uchibou, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.uchibou, by = "datetime")

names(train.uchibou)[2] <- "response"

# 埼京川越線
train.saikyoukawagoe  <- dplyr::select(train.orig, c(datetime, saikyoukawagoe )) %>%
  dplyr::full_join(., temperature.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., precipitation.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., humidity.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., mwgs.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., windspeed.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., wind_dir.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.saikyoukawagoe, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.saikyoukawagoe, by = "datetime")

names(train.saikyoukawagoe)[2] <- "response"

train <-  rbind(train.tyuou, train.keihintohoku) %>%
  rbind(., train.keiyou) %>%
  rbind(., train.uchibou) %>%
  rbind(., train.saikyoukawagoe) %>%
  dplyr::left_join(. , datetime ,by="datetime")
  
sapply(train, function(x) sum(is.na(x)))
 

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