#R version 4.1.2 (2021-11-01) -- "Bird Hippie"
#Copyright (C) 2021 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

test <- read.table("~/Documents/GitHub/Advent-of-Code/2021/Day 2 - Dive!/test.txt", quote="\"", comment.char="");
data <- read.table("~/Documents/GitHub/Advent-of-Code/2021/Day 2 - Dive!/data.txt", quote="\"", comment.char="");

#Part 1

subPos <- function(input){
  x <- 0
  z <- 0
  for(i in 1:nrow(input)){
    if(input[i,1] == "forward"){
      x <- x + input[i,2]
    }
    else if(input[i,1] == "down"){
      z <- z + input[i,2]
    }
    else if(input[i,1] == "up"){
      z <- z - input[i,2]
    }
  }
  print(x * z)
}

subPos(data);

#Part 2

subPos <- function(input){
  x <- 0
  z <- 0
  aim <- 0
  for(i in 1:nrow(input)){
    if(input[i,1] == "forward"){
      x <- x + input[i,2]
      z <- z + (aim * input[i,2])
    }
    else if(input[i,1] == "down"){
      aim <- aim + input[i,2]
    }
    else if(input[i,1] == "up"){
      aim <- aim - input[i,2]
    }
  }
  print(x * z)
}

subPos(data);
