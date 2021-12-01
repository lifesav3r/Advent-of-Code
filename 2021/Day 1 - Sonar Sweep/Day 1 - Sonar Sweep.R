#R version 4.1.2 (2021-11-01) -- "Bird Hippie"
#Copyright (C) 2021 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

test <- read.table("N:/Advent of Code/2021/Day 1 - Sonar Sweep/test.txt", quote="\"", comment.char="");
data <- read.table("N:/Advent of Code/2021/Day 1 - Sonar Sweep/data.txt", quote="\"", comment.char="");

#Part 1

sonarSweep <- function(x){
  counter <- 0
  for(i in 2:nrow(x)){
    if(x[i,] > x[i-1,]){
      counter <- counter + 1
    }
  }
  print(counter)
}

sonarSweep(data);

#Part 2

sonarSweep <- function(x){
  counter <- -1
  storedWindow <- 0
  for(i in 2:(nrow(x)-1)){
    currentWindow <- x[i-1,] + x[i,] + x[i+1,]
    if(currentWindow > storedWindow){
      counter <- counter + 1
    }
    storedWindow <- currentWindow
  }
  print(counter)
}

sonarSweep(data);
