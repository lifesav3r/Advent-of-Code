#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(Rmpfr);

test <- readLines("test.txt");
input13 <- readLines("input.txt");

firstBus <- function(x){
  depTime <- as.numeric(x[1])
  busList <- unlist(strsplit(x[2],","))
  bestOpt <- NA
  for(i in 1:length(busList)){
    if(busList[i] != "x"){
      potBest <- (ceiling((depTime / as.numeric(busList[i]))) * as.numeric(busList[i])) - depTime
      if(is.na(bestOpt) | potBest < bestOpt){
        bestOpt <- potBest
        busID <- as.numeric(busList[i])
      }
    }
  }
  return(bestOpt * busID)
}

firstBus(input13);

#Part2

numCalc <- function(y){
  mpfr(y, precBits = 200)
}

allBuses <- function(x){
  busList <- unlist(strsplit(x[2],","))
  numBus <- which(busList != "x") - 1
  busID <- as.double(busList[busList != "x"])

  target <- numCalc(prod(busID))
  index <- target / busID
  rem <- -numBus %% busID
  inverse <- rep(NA, length(busID))

  for(i in 1:length(busID)){
    remainder <- ((index[i] %% busID[i]) * (1:(busID[i] - 1))) %% busID[i]
    inverse[i] <- which(remainder == 1)
  }
  return(sum(index * rem * inverse) %% target)
}

allBuses(input13);
