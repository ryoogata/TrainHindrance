# Packages ----------------------------------------------------------------

require(dplyr)

# 指数表示の回避
options(scipen=10)

source("script/R/fun/functions.R")
source("variables.R")


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

# 外房線
sotobou <- makeDF("temperature", centralid.sotobou)
sotobou$temperature <- as.numeric(sotobou$temperature)
sotobou.na <- which(is.na(sotobou$temperature))

# checkdata(sotobou)

for(i in sotobou.na){
  sotobou[i,"temperature"] <- dataCompletion(sotobou, "temperature", i) 
}

# checkdata(sotobou)

temperature.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,temperature.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,temperature.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean, temperature.median, temperature.sd))

# checkdata(sotobou)

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線
yamanote <- makeDF("temperature", centralid.yamanote)
yamanote$temperature <- as.numeric(yamanote$temperature)
yamanote.na <- which(is.na(yamanote$temperature))

# checkdata(yamanote)

for(i in yamanote.na){
  yamanote[i,"temperature"] <- dataCompletion(yamanote, "temperature", i) 
}

# checkdata(yamanote)

temperature.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,temperature.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,temperature.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean, temperature.median, temperature.sd))

# checkdata(yamanote)

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線
utsunomiya <- makeDF("temperature", centralid.utsunomiya)
utsunomiya$temperature <- as.numeric(utsunomiya$temperature)
utsunomiya.na <- which(is.na(utsunomiya$temperature))


for(i in utsunomiya.na){
  utsunomiya[i,"temperature"] <- dataCompletion(utsunomiya, "temperature", i) 
}


temperature.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,temperature.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,temperature.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean, temperature.median, temperature.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))


# 湘南新宿ライン
shounanshinjuku <- makeDF("temperature", centralid.shounanshinjuku )
shounanshinjuku$temperature <- as.numeric(shounanshinjuku$temperature)
shounanshinjuku.na <- which(is.na(shounanshinjuku$temperature))


for(i in shounanshinjuku.na){
  shounanshinjuku[i,"temperature"] <- dataCompletion(shounanshinjuku, "temperature", i) 
}


temperature.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,temperature.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,temperature.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean, temperature.median, temperature.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))


# 高崎線 ----
takasaki <- makeDF("temperature", centralid.takasaki)
takasaki$temperature <- as.numeric(takasaki$temperature)
takasaki.na <- which(is.na(takasaki$temperature))


for(i in takasaki.na){
  takasaki[i,"temperature"] <- dataCompletion(takasaki, "temperature", i) 
}


temperature.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,temperature.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,temperature.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean, temperature.median, temperature.sd))


# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))


# 降水量データ ====

# 外房線 ----
sotobou <- makeDF("precipitation", centralid.sotobou)
sotobou$precipitation <- as.numeric(sotobou$precipitation)
sotobou.na <- which(is.na(sotobou$precipitation))


for(i in sotobou.na){
  sotobou[i,"precipitation"] <- dataCompletion(sotobou, "precipitation", i) 
}


precipitation.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,precipitation.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,precipitation.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean, precipitation.median, precipitation.sd))


# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線 ----
yamanote <- makeDF("precipitation", centralid.yamanote)
yamanote$precipitation <- as.numeric(yamanote$precipitation)
yamanote.na <- which(is.na(yamanote$precipitation))


for(i in yamanote.na){
  yamanote[i,"precipitation"] <- dataCompletion(yamanote, "precipitation", i) 
}


precipitation.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,precipitation.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,precipitation.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean, precipitation.median, precipitation.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線 ----
utsunomiya <- makeDF("precipitation", centralid.utsunomiya)
utsunomiya$precipitation <- as.numeric(utsunomiya$precipitation)
utsunomiya.na <- which(is.na(utsunomiya$precipitation))


for(i in utsunomiya.na){
  utsunomiya[i,"precipitation"] <- dataCompletion(utsunomiya, "precipitation", i) 
}


precipitation.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,precipitation.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,precipitation.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean, precipitation.median, precipitation.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))


# 湘南新宿ライン ----
shounanshinjuku <- makeDF("precipitation", centralid.shounanshinjuku )
shounanshinjuku$precipitation <- as.numeric(shounanshinjuku$precipitation)
shounanshinjuku.na <- which(is.na(shounanshinjuku$precipitation))


for(i in shounanshinjuku.na){
  shounanshinjuku[i,"precipitation"] <- dataCompletion(shounanshinjuku, "precipitation", i) 
}


precipitation.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,precipitation.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,precipitation.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean, precipitation.median, precipitation.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))


# 高崎線 ----
takasaki <- makeDF("precipitation", centralid.takasaki)
takasaki$precipitation <- as.numeric(takasaki$precipitation)
takasaki.na <- which(is.na(takasaki$precipitation))


for(i in takasaki.na){
  takasaki[i,"precipitation"] <- dataCompletion(takasaki, "precipitation", i) 
}


precipitation.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,precipitation.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,precipitation.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean, precipitation.median, precipitation.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))


# 湿度データ ====

# 外房線
sotobou <- makeDF("humidity", centralid.sotobou)
sotobou$humidity <- as.numeric(sotobou$humidity)
sotobou.na <- which(is.na(sotobou$humidity))


for(i in sotobou.na){
  sotobou[i,"humidity"] <- dataCompletion(sotobou, "humidity", i) 
}


humidity.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,humidity.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,humidity.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean, humidity.median, humidity.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線
yamanote <- makeDF("humidity", centralid.yamanote)
yamanote$humidity <- as.numeric(yamanote$humidity)
yamanote.na <- which(is.na(yamanote$humidity))


for(i in yamanote.na){
  yamanote[i,"humidity"] <- dataCompletion(yamanote, "humidity", i) 
}


humidity.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,humidity.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,humidity.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean, humidity.median, humidity.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線
utsunomiya <- makeDF("humidity", centralid.utsunomiya)
utsunomiya$humidity <- as.numeric(utsunomiya$humidity)
utsunomiya.na <- which(is.na(utsunomiya$humidity))


for(i in utsunomiya.na){
  utsunomiya[i,"humidity"] <- dataCompletion(utsunomiya, "humidity", i) 
}


humidity.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,humidity.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,humidity.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean, humidity.median, humidity.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))


# 湘南新宿ライン
shounanshinjuku <- makeDF("humidity", centralid.shounanshinjuku )
shounanshinjuku$humidity <- as.numeric(shounanshinjuku$humidity)
shounanshinjuku.na <- which(is.na(shounanshinjuku$humidity))


for(i in shounanshinjuku.na){
  shounanshinjuku[i,"humidity"] <- dataCompletion(shounanshinjuku, "humidity", i) 
}


humidity.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,humidity.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,humidity.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean, humidity.median, humidity.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))


# 高崎線 ----
takasaki <- makeDF("humidity", centralid.takasaki)
takasaki$humidity <- as.numeric(takasaki$humidity)
takasaki.na <- which(is.na(takasaki$humidity))


for(i in takasaki.na){
  takasaki[i,"humidity"] <- dataCompletion(takasaki, "humidity", i) 
}


humidity.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,humidity.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,humidity.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean, humidity.median, humidity.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))


# 風向データ ====

# 外房線
sotobou <- makeDF("wind_dir", centralid.sotobou)
sotobou$direction16 <- as.numeric(sotobou$direction16)
sotobou.na <- which(is.na(sotobou$direction16))


for(i in sotobou.na){
  sotobou[i,"direction16"] <- dataCompletion(sotobou, "direction16", i) 
}


wind_dir.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,wind_dir.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
             ,wind_dir.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, wind_dir.mean, wind_dir.sd))


# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線
yamanote <- makeDF("wind_dir", centralid.yamanote)
yamanote$direction16 <- as.numeric(yamanote$direction16)
yamanote.na <- which(is.na(yamanote$direction16))


for(i in yamanote.na){
  yamanote[i,"direction16"] <- dataCompletion(yamanote, "direction16", i) 
}


wind_dir.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,wind_dir.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
             ,wind_dir.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, wind_dir.mean, wind_dir.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線
utsunomiya <- makeDF("wind_dir", centralid.utsunomiya)
utsunomiya$direction16 <- as.numeric(utsunomiya$direction16)
utsunomiya.na <- which(is.na(utsunomiya$direction16))


for(i in utsunomiya.na){
  utsunomiya[i,"direction16"] <- dataCompletion(utsunomiya, "direction16", i) 
}


wind_dir.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,wind_dir.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
             ,wind_dir.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, wind_dir.mean, wind_dir.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))


# 湘南新宿ライン
shounanshinjuku <- makeDF("wind_dir", centralid.shounanshinjuku )
shounanshinjuku$direction16 <- as.numeric(shounanshinjuku$direction16)
shounanshinjuku.na <- which(is.na(shounanshinjuku$direction16))


for(i in shounanshinjuku.na){
  shounanshinjuku[i,"direction16"] <- dataCompletion(shounanshinjuku, "direction16", i) 
}


wind_dir.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,wind_dir.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
             ,wind_dir.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, wind_dir.mean, wind_dir.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))


# 高崎線 ----
takasaki <- makeDF("wind_dir", centralid.takasaki)
takasaki$direction16 <- as.numeric(takasaki$direction16)
takasaki.na <- which(is.na(takasaki$direction16))


for(i in takasaki.na){
  takasaki[i,"direction16"] <- dataCompletion(takasaki, "direction16", i) 
}


wind_dir.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "direction16") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,wind_dir.mean = apply(dplyr::select(., -c(datetime)), 1, mean)
             ,wind_dir.sd = apply(dplyr::select(., -c(datetime)), 1, sd)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, wind_dir.mean, wind_dir.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))


# 風速データ ====

# 外房線
sotobou <- makeDF("windspeed", centralid.sotobou)
sotobou$windspeed <- as.numeric(sotobou$windspeed)
sotobou.na <- which(is.na(sotobou$windspeed))


for(i in sotobou.na){
  sotobou[i,"windspeed"] <- dataCompletion(sotobou, "windspeed", i) 
}


windspeed.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,windspeed.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,windspeed.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean, windspeed.median, windspeed.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線
yamanote <- makeDF("windspeed", centralid.yamanote)
yamanote$windspeed <- as.numeric(yamanote$windspeed)
yamanote.na <- which(is.na(yamanote$windspeed))


for(i in yamanote.na){
  yamanote[i,"windspeed"] <- dataCompletion(yamanote, "windspeed", i) 
}


windspeed.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,windspeed.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,windspeed.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean, windspeed.median, windspeed.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線
utsunomiya <- makeDF("windspeed", centralid.utsunomiya)
utsunomiya$windspeed <- as.numeric(utsunomiya$windspeed)
utsunomiya.na <- which(is.na(utsunomiya$windspeed))


for(i in utsunomiya.na){
  utsunomiya[i,"windspeed"] <- dataCompletion(utsunomiya, "windspeed", i) 
}


windspeed.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,windspeed.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,windspeed.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean, windspeed.median, windspeed.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))


# 湘南新宿ライン
shounanshinjuku <- makeDF("windspeed", centralid.shounanshinjuku )
shounanshinjuku$windspeed <- as.numeric(shounanshinjuku$windspeed)
shounanshinjuku.na <- which(is.na(shounanshinjuku$windspeed))


for(i in shounanshinjuku.na){
  shounanshinjuku[i,"windspeed"] <- dataCompletion(shounanshinjuku, "windspeed", i) 
}


windspeed.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,windspeed.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,windspeed.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean, windspeed.median, windspeed.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))


# 高崎線 ----
takasaki <- makeDF("windspeed", centralid.takasaki)
takasaki$windspeed <- as.numeric(takasaki$windspeed)
takasaki.na <- which(is.na(takasaki$windspeed))


for(i in takasaki.na){
  takasaki[i,"windspeed"] <- dataCompletion(takasaki, "windspeed", i) 
}


windspeed.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,windspeed.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,windspeed.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean, windspeed.median, windspeed.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))


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

# 評価用路線毎の雷データ ( CG：対地雷 )の作成 
thunder.CG.sotobou <- makeThunderCG(centralid.sotobou)
thunder.CG.yamanote <- makeThunderCG(centralid.yamanote)
thunder.CG.utsunomiya <- makeThunderCG(centralid.utsunomiya)
thunder.CG.shounanshinjuku <- makeThunderCG(centralid.shounanshinjuku)
thunder.CG.takasaki <- makeThunderCG(centralid.takasaki)



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

# 評価用路線毎の雷データ ( IC：対地雷 )の作成 
#thunder.IC.sotobou <- makeThunderIC(centralid.sotobou)
thunder.IC.sotobou <- makeThunderIC(centralid.sotobou[-which(centralid.sotobou == 33129448)])
thunder.IC.yamanote <- makeThunderIC(centralid.yamanote)
thunder.IC.utsunomiya <- makeThunderIC(centralid.utsunomiya)
thunder.IC.shounanshinjuku <- makeThunderIC(centralid.shounanshinjuku)
thunder.IC.takasaki <- makeThunderIC(centralid.takasaki)


# 最大瞬間風速データ(最大瞬間風速) ====

# 外房線 ----
sotobou <- makeDF("mwgs", centralid.sotobou)
sotobou$mwgs <- as.numeric(sotobou$mwgs)
sotobou.na <- which(is.na(sotobou$mwgs))

for(i in sotobou.na){
  sotobou[i,"mwgs"] <- dataCompletion(sotobou, "mwgs", i) 
}

mwgs.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,mwgs.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,mwgs.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean, mwgs.median, mwgs.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線 ----
yamanote <- makeDF("mwgs", centralid.yamanote)
yamanote$mwgs <- as.numeric(yamanote$mwgs)
yamanote.na <- which(is.na(yamanote$mwgs))


for(i in yamanote.na){
  yamanote[i,"mwgs"] <- dataCompletion(yamanote, "mwgs", i) 
}


mwgs.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,mwgs.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,mwgs.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean, mwgs.median, mwgs.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線 ----
utsunomiya <- makeDF("mwgs", centralid.utsunomiya)
utsunomiya$mwgs <- as.numeric(utsunomiya$mwgs)
utsunomiya.na <- which(is.na(utsunomiya$mwgs))


for(i in utsunomiya.na){
  utsunomiya[i,"mwgs"] <- dataCompletion(utsunomiya, "mwgs", i) 
}


mwgs.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,mwgs.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,mwgs.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean, mwgs.median, mwgs.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))


# 湘南新宿ライン ----
shounanshinjuku <- makeDF("mwgs", centralid.shounanshinjuku )
shounanshinjuku$mwgs <- as.numeric(shounanshinjuku$mwgs)
shounanshinjuku.na <- which(is.na(shounanshinjuku$mwgs))


for(i in shounanshinjuku.na){
  shounanshinjuku[i,"mwgs"] <- dataCompletion(shounanshinjuku, "mwgs", i) 
}


mwgs.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,mwgs.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,mwgs.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean, mwgs.median, mwgs.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))


# 高崎線 ----
takasaki <- makeDF("mwgs", centralid.takasaki)
takasaki$mwgs <- as.numeric(takasaki$mwgs)
takasaki.na <- which(is.na(takasaki$mwgs))


for(i in takasaki.na){
  takasaki[i,"mwgs"] <- dataCompletion(takasaki, "mwgs", i) 
}


mwgs.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,mwgs.median = apply(dplyr::select(., -c(datetime)), 1, median, na.rm = TRUE)
             ,mwgs.sd = apply(dplyr::select(., -c(datetime)), 1, sd, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean, mwgs.median, mwgs.sd))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))


# データ結合 ====

# 外房線
test.sotobou <- dplyr::select(train.orig, c(datetime)) %>%
  dplyr::full_join(., temperature.sotobou, by = "datetime") %>%
  dplyr::full_join(., precipitation.sotobou, by = "datetime") %>%
  dplyr::full_join(., humidity.sotobou, by = "datetime") %>%
  dplyr::full_join(., mwgs.sotobou, by = "datetime") %>%
  dplyr::full_join(., windspeed.sotobou, by = "datetime") %>%
  dplyr::full_join(., wind_dir.sotobou, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.sotobou, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.sotobou, by = "datetime") %>%
  dplyr::left_join(. , datetime ,by="datetime")

# 7/1 分データ削除
test.sotobou <- test.sotobou[-nrow(test.sotobou),]


if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/test/test.sotobou.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/test/test.sotobou.csv")
}

test.sotobou$datetime <- paste0("sotobou_", test.sotobou$datetime)
test.sotobou[1,-1] <- test.sotobou[2,-1]

test.sotobou[apply(test.sotobou, 1, function(x){anyNA(x)}),]

# write.table(test.sotobou, file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)


# 山手線
test.yamanote <- dplyr::select(train.orig, c(datetime)) %>%
  dplyr::full_join(., temperature.yamanote, by = "datetime") %>%
  dplyr::full_join(., precipitation.yamanote, by = "datetime") %>%
  dplyr::full_join(., humidity.yamanote, by = "datetime") %>%
  dplyr::full_join(., mwgs.yamanote, by = "datetime") %>%
  dplyr::full_join(., windspeed.yamanote, by = "datetime") %>%
  dplyr::full_join(., wind_dir.yamanote, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.yamanote, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.yamanote, by = "datetime") %>%
  dplyr::left_join(. , datetime ,by="datetime")

# 7/1 分データ削除
test.yamanote <- test.yamanote[-nrow(test.yamanote),]

if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/test/test.yamanote.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/test/test.yamanote.csv")
} 

test.yamanote$datetime <- paste0("yamanote_", test.yamanote$datetime)
test.yamanote[1,-1] <- test.yamanote[2,-1]

# write.table(test.yamanote, file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)


# 宇都宮線
test.utsunomiya <- dplyr::select(train.orig, c(datetime)) %>%
  dplyr::full_join(., temperature.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., precipitation.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., humidity.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., mwgs.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., windspeed.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., wind_dir.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.utsunomiya, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.utsunomiya, by = "datetime") %>%
  dplyr::left_join(. , datetime ,by="datetime")

# 7/1 分データ削除
test.utsunomiya <- test.utsunomiya[-nrow(test.utsunomiya),]

if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/test/test.utsunomiya.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/test/test.utsunomiya.csv")
} 

test.utsunomiya$datetime <- paste0("utsunomiya_", test.utsunomiya$datetime)
test.utsunomiya[1,-1] <- test.utsunomiya[2,-1]

# write.table(test.utsunomiya, file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)


# 湘南新宿ライン
test.shounanshinjuku <- dplyr::select(train.orig, c(datetime)) %>%
  dplyr::full_join(., temperature.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., precipitation.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., humidity.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., mwgs.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., windspeed.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., wind_dir.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.shounanshinjuku, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.shounanshinjuku, by = "datetime") %>%
  dplyr::left_join(. , datetime ,by="datetime")

# 7/1 分データ削除
test.shounanshinjuku <- test.shounanshinjuku[-nrow(test.shounanshinjuku),]

if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/test/test.shounanshinjuku.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/test/test.shounanshinjuku.csv")
} 

test.shounanshinjuku$datetime <- paste0("shounanshinjuku_", test.shounanshinjuku$datetime)
test.shounanshinjuku[1,-1] <- test.shounanshinjuku[2,-1]

# write.table(test.shounanshinjuku, file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)


# 高崎線
test.takasaki <- dplyr::select(train.orig, c(datetime)) %>%
  dplyr::full_join(., temperature.takasaki, by = "datetime") %>%
  dplyr::full_join(., precipitation.takasaki, by = "datetime") %>%
  dplyr::full_join(., humidity.takasaki, by = "datetime") %>%
  dplyr::full_join(., mwgs.takasaki, by = "datetime") %>%
  dplyr::full_join(., windspeed.takasaki, by = "datetime") %>%
  dplyr::full_join(., wind_dir.takasaki, by = "datetime") %>%
  dplyr::full_join(., thunder.CG.takasaki, by = "datetime") %>%
  dplyr::full_join(., thunder.IC.takasaki, by = "datetime") %>%
  dplyr::left_join(. , datetime ,by="datetime")

# 7/1 分データ削除
test.takasaki <- test.takasaki[-nrow(test.takasaki),]

if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/test/test.takasaki.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/test/test.takasaki.csv")
} 

test.takasaki$datetime <- paste0("takasaki_", test.takasaki$datetime)
test.takasaki[1,-1] <- test.takasaki[2,-1]

# write.table(test.takasaki, file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)

test <- rbind(test.sotobou, test.yamanote)  %>%
  rbind(., test.utsunomiya) %>%
  rbind(., test.shounanshinjuku) %>%
  rbind(., test.takasaki)

sapply(test, function(x) sum(is.na(x)))

if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  SAVE_DIR = paste0("/Users/ryo/Desktop/deepanalytics/test/test.csv")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/test/test.csv")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  SAVE_DIR = paste0("test/test.csv")
}

write.table(test, file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)
