#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

input6 <- readLines("input.txt");

custYes <- function(x){
  group <- NULL
  total <- 0
  for(i in 1:length(x)){
    if(x[i] == ""){
      total <- total + length(group)
      group <- NULL
    }
    if(i == length(x)){
      group <- union(group,unlist(strsplit(x[i], split = "")))
      total <- total + length(group)
    } else{
        group <- union(group,unlist(strsplit(x[i], split = "")))
      }
    }
    return(total)
  }

custYes(input6);

#Part2

allYes <- function(x){
  group <- NULL
  groupsize <- 0
  total <- 0
  for(i in 1:length(x)){
    if(x[i] == ""){
      total <- total + sum(summary(as.factor(group)) == groupsize)
      group <- NULL
      groupsize <- 0
    } else{
    if(i == length(x)){
      groupsize <- groupsize + 1
      group <- append(group,unlist(strsplit(x[i], split = "")))
      total <- total + sum(summary(as.factor(group)) == groupsize)
    } else{
        group <- append(group,unlist(strsplit(x[i], split = "")))
        groupsize <- groupsize + 1
      }
    }
  }
    return(total)
  }

allYes(input6);
