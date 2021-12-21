# R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: x86_64-apple-darwin17.0 (64-bit)

test <- data.matrix(read.fwf("~/Documents/GitHub/Advent-of-Code/2021/Day 9 - Smoke Basin/test.txt", widths = rep(1,10)));
data <- data.matrix(read.fwf("~/Documents/GitHub/Advent-of-Code/2021/Day 9 - Smoke Basin/data.txt", widths = rep(1,100)));

#Part 1

riskLevel <- function(x){
  totalRisk <- 0
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      curVal <- x[[i,j]]
      if( (i <= 1 || curVal < x[[i - 1, j]]) &&
          (j <= 1 || curVal < x[[i, j - 1]]) &&
          (i >= nrow(x) || curVal < x[[i + 1, j]]) &&
          (j >= ncol(x) || curVal < x[[i, j + 1]])){
            totalRisk <- totalRisk + curVal + 1
      }
    }
  }
  totalRisk
}

riskLevel(data);

#Part 2

basinFinder <- function(x,y){
  if(data[[x,y]] == 9){
    return(0)
  }
  data[x,y] <<- 9
  n <- 1
  if(x > 1){
    n <- n + basinFinder(x - 1, y)
  }
  if(y > 1){
    n <- n + basinFinder(x, y - 1)
  }
  if(x < nrow(data)){
    n <- n + basinFinder(x + 1, y)
  }
  if(y < ncol(data)){
    n <- n + basinFinder(x, y + 1)
  }
  return(n)
}

basinSize <- function(input){
  basins <- c()
  for(i in 1:nrow(input)){
    for(j in 1:ncol(input)){
      n <- basinFinder(i,j)
      if(n > 0){
        basins <- c(basins, n)
      }
    }
  }
  prod(head(sort(basins, decreasing = TRUE), 3))
}

basinSize(data);
