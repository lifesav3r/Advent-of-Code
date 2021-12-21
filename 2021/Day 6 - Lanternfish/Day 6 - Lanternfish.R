# R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: x86_64-apple-darwin17.0 (64-bit)

library(bit64);

test <- as.numeric(scan("~/Documents/GitHub/Advent-of-Code/2021/Day 6 - Lanternfish/test.txt", what = "", sep = ","));
data <- as.numeric(scan("~/Documents/GitHub/Advent-of-Code/2021/Day 6 - Lanternfish/data.txt", what = "", sep = ","));

#Part 1

lanternfish <- function(x, days){
  for(i in 1:days){
    x <- x - 1
    if(any(x < 0)){
      x <- c(x, rep(8, sum(x < 0)))
      x[x < 0] <- 6
    }
  }
  length(x)
}

lanternfish(data,80);

#Part 2

lanternfish2 <- function(x,days){
  table <- integer64(9)
  table[1:9] <- as.vector(table(factor(x,0:8)))
  for(i in 1:days){
    table2 <- table
    table2[1:8] <- table[2:9]
    table2[9] <- table[1]
    table2[7] <- table[8] + table[1]
    table <- table2
  }
  sum(table)
}

lanternfish2(data,256);
