#initial
library(ggplot2)
library(Cairo)
if(exists("exp.name") == F){
  exp.name <- readline("exp.name?\n")
}
path <- paste("/home/linyk/git projects/KKanalysis/Tempdata/",
              exp.name,date(),sep = "")
dir.create(path = path)
setwd(dir = path)
if(exists("exp.name") == FALSE){
  exp.name <- readline("Experience name?\n")  
}
if(exists("temp") == FALSE)
{
  num <- c()
temp <- c()
}

#edit data frame
ask.delete <- readline("Delete?(y/n)\n")
if(ask.delete == 'y' || ask.delete == '') {
  cat("which one to delete?\n")
  whichDay  <- scan()
  temp <- temp[-c((whichDay * 2-1), (whichDay * 2))]
}

ask.import <- readline("Import?(y/n)")
while (ask.import == 'y' || ask.import == '') {
  input.Num <- as.numeric(readline("Num:\n"))
  num <- c(num, input.Num)
  cat("temp:\n")
  input.Temp <- scan()
  temp <- c(temp,input.Temp)
  ask.import <- readline("Import?(y/n)")
}
Day <- rep(1:(length(temp)/2), each = 2)
df <- data.frame(Day,temp)
