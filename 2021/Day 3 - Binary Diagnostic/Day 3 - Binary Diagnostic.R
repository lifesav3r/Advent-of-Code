#R version 4.1.2 (2021-11-01) -- "Bird Hippie"
#Copyright (C) 2021 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

library(dplyr);
library(compositions);

test <- read.table("N:/Advent of Code/2021/Day 3 - Binary Diagnostic/test.txt", quote="\"", comment.char="", colClasses = "character");
test <- data.frame(do.call("rbind", strsplit(as.character(test$V1), "", fixed = TRUE)));
test <- mutate_all(test, function(x) as.numeric(as.character(x)));

data <- read.table("N:/Advent of Code/2021/Day 3 - Binary Diagnostic/data.txt", quote="\"", comment.char="", colClasses = "character");
data <- data.frame(do.call("rbind", strsplit(as.character(data$V1), "", fixed = TRUE)));
data <- mutate_all(data, function(x) as.numeric(as.character(x)));

#Part 1

binDiag <- function(x){
  threshold <- (nrow(x) / 2)
  xSums <- colSums(x)
  gamma <- ""
  epsilon <- ""
  for(i in 1:length(xSums)){
    if(xSums[i] > threshold){
      gamma <- paste0(gamma,"1")
      epsilon <- paste0(epsilon,"0")
    }
    else{
      gamma <- paste0(gamma,"0")
      epsilon <- paste0(epsilon,"1")
    }
  }
  gamma <- unbinary(gamma)
  epsilon <- unbinary(epsilon)
  print(gamma * epsilon)
}

binDiag(data);

#Part 2

binDiag2 <- function(x){
  oxyVal <- x
  CO2Val <- x
  while(nrow(oxyVal) != 1){
    for(i in 1:length(oxyVal)){
      threshold <- (nrow(oxyVal) / 2)
      sums <- colSums(oxyVal)
      if(sums[i] >= threshold){
        oxyVal <- subset(oxyVal, oxyVal[,i] == 1)
      }
      else{
        oxyVal <- subset(oxyVal, oxyVal[,i] == 0)
      }
      if(nrow(oxyVal) == 1) break
    }
  }
  while(nrow(CO2Val) != 1){
    for(j in 1:length(CO2Val)){
      threshold <- (nrow(CO2Val) / 2)
      sums <- colSums(CO2Val)
      if(sums[j] >= threshold){
        CO2Val <- subset(CO2Val, CO2Val[,j] == 0)
      }
      else{
        CO2Val <- subset(CO2Val, CO2Val[,j] == 1)
      }
      if(nrow(CO2Val) == 1) break
    }
  }

  oxyVal <- c(oxyVal, sep = "")
  oxyVal <- do.call(paste, oxyVal)
  oxyVal <- unbinary(oxyVal)

  CO2Val <- c(CO2Val, sep = "")
  CO2Val <- do.call(paste, CO2Val)
  CO2Val <- unbinary(CO2Val)

  print(oxyVal * CO2Val)
}

binDiag2(data);
