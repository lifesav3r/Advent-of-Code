#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(mgsub);
library(stringr);
library(splitstackshape);

passPolicy <- function(x){
  numValid <- 0
  table <- mgsub(x, c("-",":"), c(" ",""))
  table <- concat.split(data = table, split.col = "V1", sep = " ", drop = TRUE)
  for(i in 1:nrow(table)){
    curPass <- str_count(as.character(table[i,2]), as.character(table[i,1]))
      if((curPass >= table[i,3]) & (curPass <= table[i,4])){
        numValid <- numValid + 1
      }
    }
  print(numValid)
}

passPolicy(input2)

#Part2

passPolicy <- function(x){
  numValid <- 0
  table <- mgsub(x, c("-",":"), c(" ",""))
  table <- concat.split(data = table, split.col = "V1", sep = " ", drop = TRUE)
  for(i in 1:nrow(table)){
    numElements <- 0
    curPass <- which(strsplit(as.character(table[i,2]), "")[[1]]==as.character(table[i,1]))
    for(j in 1:length(curPass)){
      if(is.element(as.numeric(table[i,3]), curPass)){
        numElements <- numElements + 1
      }
      if(is.element(as.numeric(table[i,4]), curPass)){
        numElements <- numElements + 1
      }
      if(numElements == 1){
        numValid <- numValid + 1
      }
    }
  }
  print(numValid)
}

passPolicy(input2)
