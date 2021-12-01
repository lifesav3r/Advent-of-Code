#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

test <- readLines("test.txt");
input15 <- readLines("input.txt");

numSpoken <- function(x,num){
  spokenList <- as.numeric(unlist(strsplit(x,",")))
  counter <- length(spokenList)
  lastSpoken <- tail(spokenList,1)
  while(counter < num){
    if(length(which(spokenList == lastSpoken)) == 1){
      lastSpoken <- 0
      spokenList <- append(spokenList,lastSpoken)
      counter <- counter + 1
    } else{
      lastTwo <- tail(which(spokenList == lastSpoken),2)
      lastSpoken <- lastTwo[2] - lastTwo[1]
      spokenList <- append(spokenList, lastSpoken)
      counter <- counter + 1
    }
  }
  return(lastSpoken)
}

numSpoken(input15,2020)

#Part2

numSpoken <- function(x,num){
  start.time <- Sys.time()
  spokenList <- as.numeric(unlist(strsplit(x,",")))
  hash <- new.env(hash = TRUE, parent = emptyenv(), size = num)
  i <- 1
  for(elem in spokenList){
    assign(as.character(elem), i, hash)
    i <- i + 1
  }

  lastSpoken <- 0

  for(current in (length(spokenList) + 1):(num - 1)){
    last <- hash[[as.character(lastSpoken)]]
    hash[[as.character(lastSpoken)]] <- current
    if(is.null(last)){
      lastSpoken <- 0
    } else{
      lastSpoken <- current - last
    }
  }
  end.time <- Sys.time()
  print(end.time - start.time)
  return(lastSpoken)
}

numSpoken(input15,30000000) #Time difference of 6.137589 mins
