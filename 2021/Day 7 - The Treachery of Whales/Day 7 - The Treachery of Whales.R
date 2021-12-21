# R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: x86_64-apple-darwin17.0 (64-bit)

test <- as.numeric(scan("~/Documents/GitHub/Advent-of-Code/2021/Day 7 - The Treachery of Whales/test.txt", what = "", sep = ","));
data <- as.numeric(scan("~/Documents/GitHub/Advent-of-Code/2021/Day 7 - The Treachery of Whales/data.txt", what = "", sep = ","));

#Part 1

crabFuel <- function(x){
  crabRange <- range(x)
  calcTotal <- NULL
  for(i in crabRange[1]:crabRange[2]){
    calcTotal <- append(calcTotal,sum(abs(x - i)))
  }
  min(calcTotal)
}

crabFuel(data);

#Part2

crabFuel2 <- function(x){
  crabRange <- range(x)
  calcTotal <- NULL
  for(i in crabRange[1]:crabRange[2]){
    calcTotal <- append(calcTotal,sum(sapply(abs(x - i),function(y)sum(seq_len(y)))))
  }
  min(calcTotal)
}

crabFuel2(data);
