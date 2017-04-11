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


# 気温データ ====

# 外房線
sotobou <- makeDF("temperature", centralid.sotobou)
sotobou$temperature <- as.numeric(sotobou$temperature)
sotobou.na <- which(is.na(sotobou$temperature))

dim(sotobou)

sapply(sotobou, function(x) sum(is.na(x)))

for(i in sotobou.na){
  sotobou[i,"temperature"] <- dataCompletion(sotobou, "temperature", i) 
}

sotobou[apply(sotobou, 1, function(x){anyNA(x)}),]
sapply(sotobou, function(x) sum(is.na(x)))

temperature.sotobou <- sotobou %>%
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

sapply(temperature.sotobou, function(x) sum(is.na(x)))

dim(temperature.sotobou)
# [1] 78768     4

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))


# 山手線
yamanote <- makeDF("temperature", centralid.yamanote)
yamanote$temperature <- as.numeric(yamanote$temperature)
yamanote.na <- which(is.na(yamanote$temperature))

sapply(yamanote, function(x) sum(is.na(x)))

for(i in yamanote.na){
  yamanote[i,"temperature"] <- dataCompletion(yamanote, "temperature", i) 
}

sapply(yamanote, function(x) sum(is.na(x)))

temperature.yamanote <- yamanote %>%
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

sapply(temperature.yamanote, function(x) sum(is.na(x)))

dim(temperature.yamanote)
# [1] 78768     4

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))


# 宇都宮線
utsunomiya <- makeDF("temperature", centralid.utsunomiya)
utsunomiya$temperature <- as.numeric(utsunomiya$temperature)
utsunomiya.na <- which(is.na(utsunomiya$temperature))

sapply(utsunomiya, function(x) sum(is.na(x)))

for(i in utsunomiya.na){
  utsunomiya[i,"temperature"] <- dataCompletion(utsunomiya, "temperature", i) 
}

sapply(utsunomiya, function(x) sum(is.na(x)))

temperature.utsunomiya <- utsunomiya %>%
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

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))

dim(temperature.utsunomiya)
# [1] 78768     4


# 湘南新宿ライン
shounanshinjuku <- makeDF("temperature", centralid.shounanshinjuku )
shounanshinjuku$temperature <- as.numeric(shounanshinjuku$temperature)
shounanshinjuku.na <- which(is.na(shounanshinjuku$temperature))

sapply(shounanshinjuku, function(x) sum(is.na(x)))

for(i in shounanshinjuku.na){
  shounanshinjuku[i,"temperature"] <- dataCompletion(shounanshinjuku, "temperature", i) 
}

sapply(shounanshinjuku, function(x) sum(is.na(x)))

temperature.shounanshinjuku <- shounanshinjuku %>%
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

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))

dim(temperature.shounanshinjuku)
# [1] 78768     4


# 高崎線 ( 要データ修正 ) ----
takasaki <- makeDF("temperature", centralid.takasaki)
takasaki$temperature <- as.numeric(takasaki$temperature)
takasaki.na <- which(is.na(takasaki$temperature))

dim(takasaki)
# [1] 235955      6

sapply(takasaki, function(x) sum(is.na(x)))

for(i in takasaki.na){
  takasaki[i,"temperature"] <- dataCompletion(takasaki, "temperature", i) 
}

sapply(takasaki, function(x) sum(is.na(x)))

# temperature.takasaki <- takasaki %>%
#   reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
#   add10minute(.) %>%
#   data.frame(.
#              ,datetime = dplyr::select(., c(datetime))
#              ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
#              ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
#              ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
#              ,stringsAsFactors = FALSE
#   ) %>%
#   dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean))

temperature.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "temperature") %>%
  full_join(data.frame(datetime = train.orig[,"datetime"], stringsAsFactors = FALSE)
            ,. , by = "datetime") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,temperature.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,temperature.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,temperature.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, temperature.max, temperature.min, temperature.mean))

temperature.takasaki[apply(temperature.takasaki, 1, function(x){anyNA(x)}),]

temperature.takasaki.na <- which(is.na(temperature.takasaki$temperature))



# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))

dim(temperature.takasaki)
# [1] 78746     4


# 降水量データ ====

# 外房線 ( 要データ修正 ) ----
sotobou <- makeDF("precipitation", centralid.sotobou)
sotobou$precipitation <- as.numeric(sotobou$precipitation)
sotobou.na <- which(is.na(sotobou$precipitation))

dim(sotobou)
# [1] 236229      6

sapply(sotobou, function(x) sum(is.na(x)))

for(i in sotobou.na){
  sotobou[i,"precipitation"] <- dataCompletion(sotobou, "precipitation", i) 
}

sapply(sotobou, function(x) sum(is.na(x)))

precipitation.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean))

sapply(precipitation.sotobou, function(x) sum(is.na(x)))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))

dim(precipitation.sotobou)
# [1] 78756     4


# 山手線 ( 要データ修正 ) ----
yamanote <- makeDF("precipitation", centralid.yamanote)
yamanote$precipitation <- as.numeric(yamanote$precipitation)
yamanote.na <- which(is.na(yamanote$precipitation))

dim(yamanote)
# [1] 314382      6

sapply(yamanote, function(x) sum(is.na(x)))

for(i in yamanote.na){
  yamanote[i,"precipitation"] <- dataCompletion(yamanote, "precipitation", i) 
}

sapply(yamanote, function(x) sum(is.na(x)))

precipitation.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))

dim(precipitation.yamanote)
# [1] 78757     4


# 宇都宮線 ( 要データ修正 ) ----
utsunomiya <- makeDF("precipitation", centralid.utsunomiya)
utsunomiya$precipitation <- as.numeric(utsunomiya$precipitation)
utsunomiya.na <- which(is.na(utsunomiya$precipitation))

dim(utsunomiya)
# [1] 787122      6

sapply(utsunomiya, function(x) sum(is.na(x)))

for(i in utsunomiya.na){
  utsunomiya[i,"precipitation"] <- dataCompletion(utsunomiya, "precipitation", i) 
}

sapply(utsunomiya, function(x) sum(is.na(x)))

precipitation.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))

dim(precipitation.utsunomiya)
# [1] 78757     4


# 湘南新宿ライン ( 要データ修正 ) ----
shounanshinjuku <- makeDF("precipitation", centralid.shounanshinjuku )
shounanshinjuku$precipitation <- as.numeric(shounanshinjuku$precipitation)
shounanshinjuku.na <- which(is.na(shounanshinjuku$precipitation))

sapply(shounanshinjuku, function(x) sum(is.na(x)))

for(i in shounanshinjuku.na){
  shounanshinjuku[i,"precipitation"] <- dataCompletion(shounanshinjuku, "precipitation", i) 
}

sapply(shounanshinjuku, function(x) sum(is.na(x)))

precipitation.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))

dim(precipitation.shounanshinjuku)
# [1] 78757     4


# 高崎線 ( 要データ修正 ) ----
takasaki <- makeDF("precipitation", centralid.takasaki)
takasaki$precipitation <- as.numeric(takasaki$precipitation)
takasaki.na <- which(is.na(takasaki$precipitation))

sapply(takasaki, function(x) sum(is.na(x)))

for(i in takasaki.na){
  takasaki[i,"precipitation"] <- dataCompletion(takasaki, "precipitation", i) 
}

sapply(takasaki, function(x) sum(is.na(x)))

precipitation.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "precipitation") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,precipitation.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,precipitation.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,precipitation.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, precipitation.max, precipitation.min, precipitation.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))

dim(precipitation.takasaki)
# [1] 78735     4


# 湿度データ ====

# 外房線
sotobou <- makeDF("humidity", centralid.sotobou)
sotobou$humidity <- as.numeric(sotobou$humidity)
sotobou.na <- which(is.na(sotobou$humidity))

sapply(sotobou, function(x) sum(is.na(x)))

for(i in sotobou.na){
  sotobou[i,"humidity"] <- dataCompletion(sotobou, "humidity", i) 
}

sapply(sotobou, function(x) sum(is.na(x)))

humidity.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))

dim(humidity.sotobou)
# [1] 78768     4


# 山手線
yamanote <- makeDF("humidity", centralid.yamanote)
yamanote$humidity <- as.numeric(yamanote$humidity)
yamanote.na <- which(is.na(yamanote$humidity))

sapply(yamanote, function(x) sum(is.na(x)))

for(i in yamanote.na){
  yamanote[i,"humidity"] <- dataCompletion(yamanote, "humidity", i) 
}

sapply(yamanote, function(x) sum(is.na(x)))

humidity.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))

dim(humidity.yamanote)
# [1] 78768     4


# 宇都宮線
utsunomiya <- makeDF("humidity", centralid.utsunomiya)
utsunomiya$humidity <- as.numeric(utsunomiya$humidity)
utsunomiya.na <- which(is.na(utsunomiya$humidity))

sapply(utsunomiya, function(x) sum(is.na(x)))

for(i in utsunomiya.na){
  utsunomiya[i,"humidity"] <- dataCompletion(utsunomiya, "humidity", i) 
}

sapply(utsunomiya, function(x) sum(is.na(x)))

humidity.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))

dim(humidity.utsunomiya)
# [1] 78768     4


# 湘南新宿ライン
shounanshinjuku <- makeDF("humidity", centralid.shounanshinjuku )
shounanshinjuku$humidity <- as.numeric(shounanshinjuku$humidity)
shounanshinjuku.na <- which(is.na(shounanshinjuku$humidity))

sapply(shounanshinjuku, function(x) sum(is.na(x)))

for(i in shounanshinjuku.na){
  shounanshinjuku[i,"humidity"] <- dataCompletion(shounanshinjuku, "humidity", i) 
}

sapply(shounanshinjuku, function(x) sum(is.na(x)))

humidity.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))

dim(humidity.shounanshinjuku)
# [1] 78768     4


# 高崎線 ( 要データ修正 ) ----
takasaki <- makeDF("humidity", centralid.takasaki)
takasaki$humidity <- as.numeric(takasaki$humidity)
takasaki.na <- which(is.na(takasaki$humidity))

sapply(takasaki, function(x) sum(is.na(x)))

for(i in takasaki.na){
  takasaki[i,"humidity"] <- dataCompletion(takasaki, "humidity", i) 
}

sapply(takasaki, function(x) sum(is.na(x)))

humidity.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "humidity") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,humidity.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,humidity.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,humidity.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, humidity.max, humidity.min, humidity.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))

dim(humidity.takasaki)
# [1] 78746     4


# 風速データ ====

# 外房線
sotobou <- makeDF("windspeed", centralid.sotobou)
sotobou$windspeed <- as.numeric(sotobou$windspeed)
sotobou.na <- which(is.na(sotobou$windspeed))

sapply(sotobou, function(x) sum(is.na(x)))

for(i in sotobou.na){
  sotobou[i,"windspeed"] <- dataCompletion(sotobou, "windspeed", i) 
}

sapply(sotobou, function(x) sum(is.na(x)))

windspeed.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))

dim(windspeed.sotobou)
# [1] 78768     4


# 山手線
yamanote <- makeDF("windspeed", centralid.yamanote)
yamanote$windspeed <- as.numeric(yamanote$windspeed)
yamanote.na <- which(is.na(yamanote$windspeed))

sapply(yamanote, function(x) sum(is.na(x)))

for(i in yamanote.na){
  yamanote[i,"windspeed"] <- dataCompletion(yamanote, "windspeed", i) 
}

sapply(yamanote, function(x) sum(is.na(x)))

windspeed.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))

dim(windspeed.yamanote)
# [1] 78768     4


# 宇都宮線
utsunomiya <- makeDF("windspeed", centralid.utsunomiya)
utsunomiya$windspeed <- as.numeric(utsunomiya$windspeed)
utsunomiya.na <- which(is.na(utsunomiya$windspeed))

sapply(utsunomiya, function(x) sum(is.na(x)))

for(i in utsunomiya.na){
  utsunomiya[i,"windspeed"] <- dataCompletion(utsunomiya, "windspeed", i) 
}

sapply(utsunomiya, function(x) sum(is.na(x)))

windspeed.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))

dim(windspeed.utsunomiya)
# [1] 78768     4


# 湘南新宿ライン
shounanshinjuku <- makeDF("windspeed", centralid.shounanshinjuku )
shounanshinjuku$windspeed <- as.numeric(shounanshinjuku$windspeed)
shounanshinjuku.na <- which(is.na(shounanshinjuku$windspeed))

sapply(shounanshinjuku, function(x) sum(is.na(x)))

for(i in shounanshinjuku.na){
  shounanshinjuku[i,"windspeed"] <- dataCompletion(shounanshinjuku, "windspeed", i) 
}

sapply(shounanshinjuku, function(x) sum(is.na(x)))

windspeed.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))

dim(windspeed.shounanshinjuku)
# [1] 78768     4


# 高崎線 ( 要データ修正 ) ----
takasaki <- makeDF("windspeed", centralid.takasaki)
takasaki$windspeed <- as.numeric(takasaki$windspeed)
takasaki.na <- which(is.na(takasaki$windspeed))

sapply(takasaki, function(x) sum(is.na(x)))

for(i in takasaki.na){
  takasaki[i,"windspeed"] <- dataCompletion(takasaki, "windspeed", i) 
}

sapply(takasaki, function(x) sum(is.na(x)))

windspeed.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "windspeed") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,windspeed.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,windspeed.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,windspeed.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, windspeed.max, windspeed.min, windspeed.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))

dim(windspeed.takasaki)
# [1] 78746     4


# 最大瞬間風速データ(最大瞬間風速) ====

# 外房線 ( 要データ修正 ) ----
sotobou <- makeDF("mwgs", centralid.sotobou)
sotobou$mwgs <- as.numeric(sotobou$mwgs)
sotobou.na <- which(is.na(sotobou$mwgs))

sapply(sotobou, function(x) sum(is.na(x)))

for(i in sotobou.na){
  sotobou[i,"mwgs"] <- dataCompletion(sotobou, "mwgs", i) 
}

sapply(sotobou, function(x) sum(is.na(x)))

mwgs.sotobou <- sotobou %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^sotobou$"))

dim(mwgs.sotobou)
# [1] 78756     4


# 山手線 ( 要データ修正 ) ----
yamanote <- makeDF("mwgs", centralid.yamanote)
yamanote$mwgs <- as.numeric(yamanote$mwgs)
yamanote.na <- which(is.na(yamanote$mwgs))

sapply(yamanote, function(x) sum(is.na(x)))

for(i in yamanote.na){
  yamanote[i,"mwgs"] <- dataCompletion(yamanote, "mwgs", i) 
}

sapply(yamanote, function(x) sum(is.na(x)))

mwgs.yamanote <- yamanote %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^yamanote$"))

dim(mwgs.yamanote)
# [1] 78757     4


# 宇都宮線 ( 要データ修正 ) ----
utsunomiya <- makeDF("mwgs", centralid.utsunomiya)
utsunomiya$mwgs <- as.numeric(utsunomiya$mwgs)
utsunomiya.na <- which(is.na(utsunomiya$mwgs))

sapply(utsunomiya, function(x) sum(is.na(x)))

for(i in utsunomiya.na){
  utsunomiya[i,"mwgs"] <- dataCompletion(utsunomiya, "mwgs", i) 
}

sapply(utsunomiya, function(x) sum(is.na(x)))

mwgs.utsunomiya <- utsunomiya %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^utsunomiya$"))

dim(mwgs.utsunomiya)
# [1] 78757     4


# 湘南新宿ライン ( 要データ修正 ) ----
shounanshinjuku <- makeDF("mwgs", centralid.shounanshinjuku )
shounanshinjuku$mwgs <- as.numeric(shounanshinjuku$mwgs)
shounanshinjuku.na <- which(is.na(shounanshinjuku$mwgs))

sapply(shounanshinjuku, function(x) sum(is.na(x)))

for(i in shounanshinjuku.na){
  shounanshinjuku[i,"mwgs"] <- dataCompletion(shounanshinjuku, "mwgs", i) 
}

sapply(shounanshinjuku, function(x) sum(is.na(x)))

mwgs.shounanshinjuku <- shounanshinjuku %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^shounanshinjuku$"))

dim(mwgs.shounanshinjuku)
# [1] 78757     4


# 高崎線 ( 要データ修正 )  ----
takasaki <- makeDF("mwgs", centralid.takasaki)
takasaki$mwgs <- as.numeric(takasaki$mwgs)
takasaki.na <- which(is.na(takasaki$mwgs))

sapply(takasaki, function(x) sum(is.na(x)))

for(i in takasaki.na){
  takasaki[i,"mwgs"] <- dataCompletion(takasaki, "mwgs", i) 
}

sapply(takasaki, function(x) sum(is.na(x)))

mwgs.takasaki <- takasaki %>%
  reshape2::dcast(., datetime ~ centralid, value.var = "mwgs") %>%
  add10minute(.) %>%
  data.frame(.
             ,datetime = dplyr::select(., c(datetime))
             ,mwgs.max = apply(dplyr::select(., -c(datetime)), 1, max, na.rm = TRUE)
             ,mwgs.min = apply(dplyr::select(., -c(datetime)), 1, min, na.rm = TRUE)
             ,mwgs.mean = apply(dplyr::select(., -c(datetime)), 1, mean, na.rm = TRUE)
             ,stringsAsFactors = FALSE
  ) %>%
  dplyr::select(.,c(datetime, mwgs.max, mwgs.min, mwgs.mean))

# 不要なオブジェクトの削除
rm(list = ls(pattern = "^takasaki$"))

dim(mwgs.takasaki)
# [1] 78735     4


# データ結合 ====

# 外房線
test.sotobou <- dplyr::select(train.orig, c(datetime)) %>%
  dplyr::full_join(., temperature.sotobou, by = "datetime") %>%
  dplyr::full_join(., precipitation.sotobou, by = "datetime") %>%
  dplyr::full_join(., humidity.sotobou, by = "datetime") %>%
  dplyr::full_join(., mwgs.sotobou, by = "datetime") %>%
  dplyr::full_join(., windspeed.sotobou, by = "datetime")

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
  dplyr::full_join(., windspeed.yamanote, by = "datetime")

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
  dplyr::full_join(., windspeed.utsunomiya, by = "datetime")

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
  dplyr::full_join(., windspeed.shounanshinjuku, by = "datetime")

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
  dplyr::full_join(., windspeed.takasaki, by = "datetime")

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