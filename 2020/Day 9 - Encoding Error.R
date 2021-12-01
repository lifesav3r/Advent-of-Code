#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(gtools)

test <- as.numeric(readLines("test.txt"));
input9 <- as.numeric(readLines("input.txt"));

findInvalid <- function(x,preamble,consider){
  for(i in (preamble + 1):length(x)){
    curValues <- x[(i - consider - 1):(i - 1)]
    curValues <- combn(curValues, 2, FUN = sum)
    if(!(x[i] %in% curValues)){
      return(x[i])
    }
  }
}

#Part2

xMAS <- function(x,preamble,consider){
  badVal <- findInvalid(x,preamble,consider)
  for(i in 1:length(x)){
    calc <- x[i]
    for(j in (i+1):length(x)){
      calc <- append(calc,x[j])
      if(sum(calc) == badVal){
        return(max(calc) + min(calc))
      }
    }
  }
}
