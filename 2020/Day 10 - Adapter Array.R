#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(tidyverse);

test <- read.table("test.txt")[,1];
input10 <- read.table("input.txt")[,1];

joltChain <- function(x){
  chain <- sort(c(0,x,max(x) + 3))
  oneJolt <- 0
  threeJolt <- 0
  for(i in 2:length(chain)){
    if((chain[i] - chain[i-1]) == 1){
      oneJolt <- oneJolt + 1
    }
    if((chain[i] - chain[i-1]) == 3){
      threeJolt <- threeJolt + 1
    }
  }
  return((oneJolt) * (threeJolt))
}

#Part2

joltArr <- function(x){
  chain <- sort(c(0, x, max(x) + 3))
  extAdapters <- chain[-c(1, which(diff(chain) == 3), which(diff(chain) == 3) + 1, length(chain))]
  total <- prod(pmin(2^table(cumsum(c(0, diff(extAdapters) > 2))), 7))
  return(total)
}
