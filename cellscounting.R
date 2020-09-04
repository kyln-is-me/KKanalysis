# Initialization
# ImportDf <- function() {
#   
# }
library(deSolve)
library(ggplot2)


# create dataSum
if(exists("dataSum") == FALSE){
  dataSum <- data.frame(hours = numeric(), svv = numeric(), all = numeric(), percent = numeric(), clump = numeric())
}

askAgain <- askYesNo("Input?")
if(askAgain == TRUE){
# import data
# ask if input in sequence
  while (askAgain == TRUE){
    askMod <- askYesNo("Input in sequence?")
    if(askMod == FALSE || is.numeric(askMod) == TRUE) {
      askRow <- as.numeric(readline(paste0("Which row to import?The latest is No.", nrow(dataSum),seq = "")))
    }else{  askRow <- nrow(dataSum) + 1
    }
  cat("No.", askRow, "Input hours, survival, all and clump.")
  dataInp <- scan()
  while (length(dataInp) != 4){
    cat("No.", askRow, "Input hours, survival, all and clump.\nInput again\n")
    dataInp <-scan()
  }
  dataSum[askRow, c(1, 2, 3, 5)] <- dataInp
  dataSum[askRow, 4] <- round(dataSum[askRow,2] / dataSum[askRow,3],2)
  askAgain <- askYesNo("Next?")
  }
}

svv <- dataSum$svv
hours <- dataSum$hours
print(dataSum)


# Estimate initial valu es of parameters
SS <- getInitial(svv ~ SSlogis(hours, alpha, xmid, scale), data = dataSum)
K_start <- SS["alpha"]
R_start <- 1/SS["scale"]
N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)

# Fitting function equation
formulaModel <- formula(svv ~ K * N0 * exp(R * hours) / (K + N0 * (exp(R * hours) - 1)))
paraSum <- nls(formulaModel, start = list(K = K_start, R = R_start, N0 = N0_start))

# get fitting function
K <- coef(paraSum)[[1]]
R <- coef(paraSum)[[2]]
N0 <- coef(paraSum)[[3]]
KN0 <- K * N0
subEnv <- list(K = K, R = R, N0 = N0, KN0 = KN0)
formulaFit <- substitute(K * N0 * exp(R * x) / (K + N0 * (exp(R * x) - 1)), subEnv)
funcPredict <- function (x) {
  y <- eval(parse(text = (as.expression(formulaFit))))
  dataPredict <- data.frame(x=x,y=y)
  return(dataPredict)
}
x <- 0:72
dataPredict <- funcPredict(x)
dataPredict[hours + 1, 3] <- svv
colnames(dataPredict)[3] <- "exp"

# edit expression
R2 <- cor(svv, predict(paraSum))
subEnv2 <- list(K = round(K,2), R = round(R,2), N0 = round(N0,2), KN0 = round(KN0,2), R2 = R2)

expressFit <- as.character(as.expression(substitute(y == frac(KN0 %*% "e"^(R * x ), K + N0 * (e^(R * x ) - 1))~","~"R"^2 == R2, subEnv2)))

  # painting chart
p1 <- ggplot(dataPredict)+
      geom_line(aes(x,y))+
      geom_point(aes(x=x,y=exp,na.rm =T))+
      theme_bw()+
      theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
      scale_x_continuous(breaks = hours)+
      geom_text(aes(x=max(hours) / 3,y=max(svv) - 1),label=expressFit, parse=TRUE)
p1