# 詳細区分支障データ ====
detail <- read.csv("./data/detail.csv"
               ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
dim(detail)
# [1] 78768     6
sapply(detail, function(x) sum(is.na(x)))

names(detail) <- c("datetime","tyuou.detail","keihintohoku.detail","keiyou.detail","uchibou.detail","saikyoukawagoe.detail")

# 支障区分ID参照用ファイル ====
# reference <- read.csv("./data/reference.tsv"
reference <- read.csv("~/Desktop/deepanalytics/reference.tsv"
               ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8", sep = "\t")
dim(reference)
# [1] 55  4
# sapply(reference, function(x) sum(is.na(x)))


# 応募用サンプルファイル ====
#sample_submit <- read.csv("./data/sample_submit.csv"
sample_submit <- read.csv("/Users/r-ogata/Desktop/deepanalytics/sample_submit.csv"
               ,header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
dim(sample_submit)
# [1] 393839      5
# sapply(sample_submit, function(x) sum(is.na(x)))


# 気象観測地点情報ファイル ====
if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Mac の場合
  observation_point <- read.csv("~/Desktop/deepanalytics/observation_point.tsv"
               ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8", sep = "\t")
} else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Windows の場合
  observation_point <- read.csv("/Users/r-ogata/Desktop/deepanalytics/observation_point.tsv"
               ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8", sep = "\t")
} else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
  # 実行環境が Ubuntu の場合
  observation_point <- read.csv("./data/observation_point.tsv"
               ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8", sep = "\t")
}


dim(observation_point)
# [1] 219  12
# 
names(observation_point) <- c("id", "centralid", "municipality", "address", "latitude", "longitude", "elevation",
                              "buildinghigh", "temperature", "humidity", "precipitation", "windinfo")
sapply(observation_point, function(x) sum(is.na(x)))

observation_point$buildinghigh <- as.numeric(observation_point$buildinghigh)
googlemap <- dplyr::select(observation_point, c(latitude, longitude, centralid))
names(googlemap) <- c("緯度", "経度", "局ID")

all.no <- observation_point[which(observation_point$風向風速 == "なし" & observation_point$降水量 == "なし"),]
any.no <- observation_point[which(observation_point$風向風速 == "なし" | observation_point$降水量 == "なし"),]
all.yes <- observation_point[which(observation_point$風向風速 == "あり" & observation_point$降水量 == "あり"),]

write.table(googlemap, file = "~/Desktop/deepanalytics/observation.csv", sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)

write.table(all.no, file = "/Users/r-ogata/Desktop/deepanalytics/all.no.csv", sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)
write.table(any.no, file = "/Users/r-ogata/Desktop/deepanalytics/any.no.csv", sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)
write.table(all.yes , file = "/Users/r-ogata/Desktop/deepanalytics/all.yes.csv", sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)