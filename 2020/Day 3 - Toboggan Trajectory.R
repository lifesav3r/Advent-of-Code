#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

tobTraj <- function(x){
  table <- strsplit(x[,1], split = "(?=.)", perl=TRUE)
  table <- data.frame(do.call(rbind, table))
  expFactor <- ceiling((nrow(table)*3)/length(table))
  map <- do.call("cbind",replicate(expFactor,table,simplify = FALSE))
  trees <- 0
  j <- 1
  for(i in 1:nrow(table)){
    if(map[i,j] == "#"){
      trees <- trees + 1
    }
      j <- j + 3
  }
  print(trees)
}

tobTraj(input3)

#Part2

tobTraj <- function(x,right,down){
  table <- strsplit(x[,1], split = "(?=.)", perl=TRUE)
  table <- data.frame(do.call(rbind, table))
  expFactor <- ceiling((nrow(table)*right)/(length(table)/down))
  map <- do.call("cbind",replicate(expFactor,table,simplify = FALSE))
  trees <- 0
  i <- 1
  j <- 1
  while(i <= nrow(table)){
    if(map[i,j] == "#"){
      trees <- trees + 1
    }
      i <- i + down
      j <- j + right
  }
  return(trees)
}

tobTrajMult <- function(x){
  trees <- tobTraj(x,1,1) * tobTraj(x,3,1) * tobTraj(x,5,1) * tobTraj(x,7,1) * tobTraj(x,1,2)
  print(trees)
}

tobTrajMult(input3)
