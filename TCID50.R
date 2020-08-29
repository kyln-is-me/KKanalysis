# row to start the inputing and total number of the samples
ask.clear <- readline("clean up the formeral data?(y/n)")
if(ask.clear == 'y' || ask.clear == ''){
  TCID50.lg <- c()
}
method <- readline("Type 'n' for which row to input, or any other keys to continue: ")
if(method == 'n'){
  row.start <- readline("Type which row to input: ")
}else{
  row.start = 1
  sample.total <- as.numeric(readline("Type total number of the samples: "))
}

# input CPE.happen and cumsum it
CPE.happen <- matrix(nrow = sample.total, ncol = 8, dimnames = list(c(1:sample.total),c("10^-2","10^-3","10^-4","10^-5","10^-6","10^-7","10^-8","10^-9")))
for(row.recent in row.start:sample.total){
  row.recent.buf <- row.recent
  cat(paste("now's importing No.", row.recent, "sample's CPE number: \n"))
  min.dilusion <- as.numeric(readline("Minimum dilution is 10^-"))
  sample.input <- as.numeric(scan())
  sample.input.sort <- sort(sample.input,decreasing = T)
  distance.max <- 10 - min.dilusion
  while (identical(sample.input, sample.input.sort) == F || length(sample.input) > distance.max || max(sample.input) != 8){
    cat(paste(" data error!\n 1. The sequance should be descending.\n 2. the length should be suitable.\n 3. at least one col is 8.\n please import again.\n"))
    cat(paste("now's importing No.", row.recent, "sample's CPE number again: \n"))
    sample.input <- as.numeric(scan())
    sample.input.sort <- sort(sample.input,decreasing = T)
  }
  col.area <- (min.dilusion - 1):(min.dilusion + length(sample.input) - 2)
  col.area.reversed <- (min.dilusion + length(sample.input) - 2):(min.dilusion - 1)
  sample.input -> CPE.happen[row.recent, col.area]
  CPE.happen.cum <- CPE.happen
  happen.recent <- CPE.happen[row.recent, col.area.reversed]
  happen.cumdata <- sort(cumsum(happen.recent),decreasing = T)  
  happen.cumdata -> CPE.happen.cum[row.recent, col.area]
  
#input CPE.none and cumsum it
  CPE.none <- 8 - CPE.happen
  CPE.none.cum <- CPE.none
  none.recent <- CPE.none[row.recent, col.area]
  none.cumdata <- cumsum(none.recent)
  none.cumdata -> CPE.none.cum[row.recent, col.area]

#calculating distance
CPE.percent <- CPE.happen.cum / (CPE.happen.cum + CPE.none.cum)
CPE.percent.buffer <- CPE.percent - 0.5
CPE.percent.buffer.row <- CPE.percent.buffer[row.recent,]
CPE.percent.higher50 <- min(CPE.percent.buffer.row[CPE.percent.buffer.row > 0],na.rm = T)
CPE.percent.lower50 <- max(CPE.percent.buffer[CPE.percent.buffer < 0],na.rm = T)
TCID50.distance <- round(CPE.percent.higher50 / (CPE.percent.higher50 - CPE.percent.lower50),2)

#finding the right logarithm
TCID50.integer.station <- max(which(CPE.percent.buffer[row.recent,col.area] == CPE.percent.higher50, arr.ind = T))
TCID50.integer <-TCID50.integer.station + min.dilusion
TCID50.lg.output <- TCID50.integer + TCID50.distance 
cat(paste("The No.", row.recent, "'s result is 10^",TCID50.lg.output,"/mL\n"))
TCID50.lg <- c(TCID50.lg, TCID50.lg.output)
row.recent <- row.recent.buf
}
TCID50.lg <- matrix(TCID50.lg ,nrow = sample.total)
TCID50 <- 10^TCID50.lg
result <- cbind(CPE.happen, lgTCID50 = TCID50.lg,TCID50)
ask.rowname <- readline("Do you need to rename the samples?(y/n)\n")
while(ask.rowname == 'y' || ask.rowname == ''){
  row.result.change <- as.numeric(readline("which to change: \n"))
  row.names(result)[row.result.change] <- as.numeric(readline("type new one: \n"))
  ask.rowname <- readline("Do you need to rename another?(y/n)\n")
}
View(result)
write.csv(result, "/home/linyk/文档/TCLD50result.csv")
cat("TCID50 result has been export as /home/linyk/文档/TCLD50result.csv\n")

