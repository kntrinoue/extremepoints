
#threshold 1e-2 //threshold for judging whether increase or decrease, ex. 1e-2 = 1% change
BOTTOM <- 0
TOP <- 1

getExtremePoints <- function(y, threshold=0.01){
  
#  //------Initialization
  index <- array()
  bottomOrTop <- array()
  numOfExtremePoints <- 0
  numOfBottom <- 0
  numOfTop <- 0
  thresholdValue <- 0.0
  arraySize <- length(y)
  
#  //------calculate range of values
  maxValue <- max(y)
  minValue <- min(y)
  thresholdValue <- threshold*(maxValue-minValue)
  
#  //------whether increase or decrease initially
  flag <- 0
  k1 <- 1
  k2 <- 1
  temp_max <- y[1]
  temp_min <- y[1]
  for(j in 2:arraySize){
    if(temp_max < y[j])
      temp_max <- y[j]
    if(temp_min > y[j])
      temp_min <- y[j]
    
    if( (temp_min + thresholdValue) < y[j]){#//increase
      flag <- 1
      k1 <- j
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfBottom <- numOfBottom + 1
      index[numOfExtremePoints] <- 1
      bottomOrTop[numOfExtremePoints] <- BOTTOM
      break
    }
    if( (temp_max - thresholdValue) > y[j]){#//decrease
      flag <- 3
      k2 <- j
      numOfExtremePoints <- numOfExtremePoints + 1
      numOfTop <- numOfTop + 1
      index[numOfExtremePoints] <- 1
      bottomOrTop[numOfExtremePoints] <- TOP
      break
    }
  }

  if(j == arraySize){#// No change
    return (NULL);
  }
  
#  //------main
  for(j in 2:arraySize-1){
    
    if(flag==1){
      if(y[j] > y[j+1]){#//point from increase to decrease is regarded as temporary top 
        flag <- 2
        k2 <- j
      }
    }
    else if(flag==2){
      if(y[k2] < y[j]){#//If y[j] is more than y[k2], y[j] is regarded as temporary top.(update)
        k2 <- j
      }
      if( (y[k2] - thresholdValue) > y[j]  ){#// top is decided when decrease of more than threshold*(max-min) from temporary top
        flag <- 3
        numOfExtremePoints <- numOfExtremePoints + 1
        numOfTop <- numOfTop + 1
        index[numOfExtremePoints] <- k2
        bottomOrTop[numOfExtremePoints] <- TOP
      }
    }
    else if(flag==3){
      if(y[j] < y[j+1]){#//point from decrease to increase is regarded as temporary bottom 
        flag <- 4
        k1 <- j
      }
    }
    else if(flag==4){
      if(y[k1] > y[j]){#//If y[j] is less than y[k1], y[j] is regarded as temporary bottom.(update)
        k1 <- j
      }
      if( (y[k1] + thresholdValue) < y[j]  ){#//botom is decided when increase of more than threshold*(max-min) from temporary bottom
        flag <- 1
        numOfExtremePoints <- numOfExtremePoints + 1
        numOfBottom <- numOfBottom + 1
        index[numOfExtremePoints] <- k1
        bottomOrTop[numOfExtremePoints] <- BOTTOM
      }
    }
    
  }
  
  #index: index of extreme points in y
  #bottomOrTop: bottom(0) or top (1) at index
  #numOfExtremePoints: Number of extreme points
  #numOfBottom: Number of bottoms at extreme points
  #numOfTop: Number of tops at extreme points
  #threshold: threshold value (ratio) for judging whether increase or decrease
  #threhsoldValue: threshold value to y in threshold
  return ( list(index=index, bottomOrTop=bottomOrTop, numOfExtremePoints=numOfExtremePoints,
                numOfBottom=numOfBottom, numOfTop=numOfTop, threshold=threshold, thresholdValue=thresholdValue) );
  
}