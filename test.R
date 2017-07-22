
source("getExtremePoints.R")

# read file
data <- read.table("test.txt")
head(data)

colnames(data) <- c("Time", "A")

th = 0.2
ep <- getExtremePoints( data[,"A"], threshold = th ) # 

if(!is.null(ep)){
  #plot line
  plot(data[,"Time"], data[,"A"],
        type = "l", lwd = 3,
        xlab = "Time", ylab = "A",
        xlim = c(min(data[,"Time"]), max(data[,"Time"])), ylim = c(min(data[,"A"]), max(data[,"A"]))
      )
  par(new=T)
  #plot extreme points
  plot(data[ep$index,"Time"], data[ep$index,"A"],
        pch = 19, col = "red", cex = 2, ann = F,
        xlim = c(min(data[,"Time"]), max(data[,"Time"])), ylim = c(min(data[,"A"]), max(data[,"A"]))
      )
}