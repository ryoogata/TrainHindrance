checkdata <- function(dataframe){
  print("dim(dataframe) ----")
  print(dim(dataframe))
  Str <- readline("Enter : ")
  
  print("sapply(dataframe, function(x) sum(is.na(x))) ----")
  print(sapply(dataframe, function(x) sum(is.na(x))))
  Str <- readline("Enter : ")
  
  print("dataframe[apply(dataframe, 1, function(x){anyNA(x)}),] ----")
  print(dataframe[apply(dataframe, 1, function(x){anyNA(x)}),])
}

splitsave <- function(dataframe){
  
  datetime <- read.csv("datetime.csv"
              ,header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")

  id <- unique(dataframe$centralid)
  for(i in id){
    df.name <- deparse(substitute(dataframe)) 
    name <- paste0(df.name, ".", i)
    assign(x = name, value = dataframe[which(dataframe$centralid == i),])
    assign(x = name, value = full_join(datetime, eval(parse(text = name)), by = "datetime"))
    
    eval(parse(text = paste0(name, "$centralid <- ", i)))
    
    if ( length(grep("os x", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Mac の場合
      SAVE_DIR = paste0("~/Desktop/deepanalytics/", deparse(substitute(dataframe)), "/", name, ".csv")
    } else if ( length(grep("windows", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Windows の場合
      SAVE_DIR = paste0("/Users/r-ogata/Desktop/deepanalytics/", deparse(substitute(dataframe)), "/", name, ".csv")
    } else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Ubuntu の場合
      SAVE_DIR = paste0("data/", deparse(substitute(dataframe)), "/", name, ".csv")
    }
    write.table(eval(parse(text=name)) , file = SAVE_DIR, sep = ",", row.names = FALSE, append = FALSE, quote = FALSE)
  }
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
    } else if ( length(grep("Ubuntu", ignore.case = TRUE, sessionInfo()$running)) != 0 ) {
      # 実行環境が Ubuntu の場合
      FILE = paste0("data/", weatherdata, "/", weatherdata, ".", i, ".csv")
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

 
# 列:datetime に 10 分加算した data.frame を返す
add10minute <- function(dataframe){
  dataframe$datetime <- as.character(as.POSIXlt(dataframe$datetime) + 600)
  return(dataframe)
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
