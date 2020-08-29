selection1 <- readline("请选择导入方式：(1-从xls导入；2-从csv导入)")
if(selection1 == 1){
  library(xlsx)
  raw.data <- read.xlsx(gsub("file://", "", readline("请输入文件路径：")),sheetIndex = 1)[-1,-c(1:2)]
}else if(selection1 == 2){
  raw.data <- read.csv(readline("请输入文件路径："))
}
raw.data2 <- raw.data
NCx <- sum(raw.data2[1:2,1])/2
PCx <- sum(raw.data2[3:4,1])/2
raw.data2[raw.data2 == "( + )"] <- PCx
raw.data2 <- sapply(sapply(raw.data2,as.numeric), as.numeric)
raw.data <- unlist(raw.data)
if(PCx - NCx >= 0.150 && NCx <= 0.250){
  SN <- raw.data2 - NCx
  result <- SN
  result[result <= 0.100] <- '-'
  result[result > 0.100 & result <=0.300] <- '±'
  result[result > 0.300] <- '+'
  SUMMARY <- data.frame("rawdata" = raw.data, "S—N" = SN, result = result)
  selection2 <- readline("请输入样品编号：(1-从xls文件导入；2-一次输入；3-逐个输入）")
  if(selection2 == 1){
    library(xlsx)
    NameFile <- gsub("file://", "",readline("文件路径: "))
    FileSheet <- as.numeric(readline("工作表: "))
    numb <- read.xlsx(NameFile,sheetIndex = FileSheet)
    if(is.data.frame(numb) == TRUE){
      numb <- unlist(numb)
      row.names(SUMMARY) <- numb
    }else{
      row.names(SUMMARY) <- numb
    }
  }else if(selection2 == 2){
    from <- as.numeric(readline("从: "))
    fromnum <- as.numeric(readline("编号: "))
    to <- as.numeric(readline("到: "))
    tonum <- as.numeric(readline("编号: "))
    row.names(SUMMARY)[from : to]
    row.names(SUMMARY) <- c(fromnum : tonum)
  }else if(selection2 == 3){
    row.names(SUMMARY) <- scan()
  }
  write.csv(SUMMARY,"/home/linyk/文档/CSFVag-result.csv")
  SUMMARY
}else{
  cat("实验结果不成立")
}
