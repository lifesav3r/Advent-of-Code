#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(binaryLogic)

test <- readLines("test.txt");
input14 <- readLines("input.txt");

memInit <- function(x){
  memReg <- matrix(, ncol = 2)
  for(i in 1:length(x)){
    if(grepl("mask",x[i])){
      mask <- unlist(strsplit(unlist(strsplit(x[i],"mask = "))[2],""))
    } else{

      index <- as.numeric(str_extract(x[i],"[0-9]+"))
      value <- as.numeric(unlist(strsplit(x[i]," = "))[2])
      binValue <- as.binary(value,n = 36)

      for(j in 1:length(mask)){
        if(mask[j] == "0"){
          binValue[j] <- FALSE
        }
        if(mask[j] == "1"){
          binValue[j] <- TRUE
        }
      }
      value <- as.numeric(binValue)

      if(length(which(memReg[,1] == index)) == 0){
        memReg <- rbind(memReg, c(index,value))
      } else{
        memReg[which(memReg[,1] == index),2] <- value
      }
    }
  }
  memReg <- memReg[-1,]
  return(sum(memReg[,2]))
}

memInit(input14);

#Part2

dec2bin <- function(x) as.numeric(rev(intToBits(as.numeric(x))))

memInit <- function(x){
  memIndex <- NULL
  memValue <- NULL

  for (i in seq_along(x)) {
    if (grepl("mask", x[i])) {
      mask <- strsplit(sub("mask = ", "", x[i]), "")[[1]]
    } else {
      index <- c(rep("0", 4), dec2bin(sub("mem.(\\d+).*", "\\1", x[i])))
      value <- as.integer(sub("[^=]*= (.*)$", "\\1", x[i]))
      index[which(mask != "0")] <- mask[which(mask != "0")]
      floatValue <- unlist(sapply(0:sum(mask == "X"),
      function(z) combn(2^(36 - which(index == "X")), z, sum)))
      memIndex <- c(memIndex, sum((index == "1") * 2^(35:0)) + floatValue)
      memValue <- c(memValue, rep(value, length(floatValue)))
    }
  }
  return(sum(aggregate(memValue, by = list(memIndex), FUN = tail, 1)[,2]), 16)
}
