#Initialization
# ImportDf <- function() {
#   
# }
#delete data
if(exists("dataSum") == TRUE && nrow(dataSum) != 0){
  askDel <- askYesNo("delete?")
  if(askDel == TRUE){
    askRow <- as.numeric(readline("Which row to delete?"))
    dataSum <- dataSum[-askRow,]
  }
}else{
  dataSum <- data.frame(svv = numeric(), all = numeric(), percent = numeric(), clump = numeric())
}

#import data
ifelse(nrow(dataSum)== 0, askRow <- 0,
       askRow <- as.numeric(readline("Which row to import?"))-1)
ask.next <- TRUE
while (ask.next == TRUE) {
  askRow <- askRow + 1
  ask.again <- TRUE
  while (ask.again == TRUE) {
    cat("No.", nrow(dataSum) + 1, "Input hours, survival, all and clump.")
    dataInp <- scan()
    while(length(dataInp) != 3){
      cat("Input hours, survival, all and clump.\nInput again\n")
      dataInp <-scan()
    }
    dataSum[askRow, c(1,2,4)] <- dataInp
    dataSum[askRow, 3] <- round(dataSum[askRow,1] / dataSum[askRow,2],2)
    ask.again <- askYesNo("reInput?")
  }
  ask.next <- askYesNo("Next one?")
}
dataSum