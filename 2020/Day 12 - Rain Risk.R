#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

library(spdep)

#Part1

test <- readLines("test.txt")
input12 <- readLines("input.txt");

shipMove <- function(dir,val,dx,dy){
  if(dir == 0){
    return(dy + val)
  }
  if(dir == 90){
    return(dx + val)
  }
  if(dir == 180){
    return(dy - val)
  }
  if(dir == 270){
    return(dx - val)
  }
}

shipNav <- function(x){
  navInst <- str_extract(x, "[aA-zZ]+")
  navVal <- as.numeric(str_extract(x, "[0-9]+"))
  faceDir <- 90
  dx <- 0
  dy <- 0
  for(i in 1:length(navInst)){
    if(navInst[i] == "F"){
      if(faceDir == 90 | faceDir == 270){
        dx <- shipMove(faceDir,navVal[i],dx,dy)
      }
      if(faceDir == 0 | faceDir == 180){
        dy <- shipMove(faceDir,navVal[i],dx,dy)
      }
    }
    if(navInst[i] == "L"){
      faceDir <- faceDir - navVal[i]
      if(faceDir < 0){faceDir <- faceDir + 360}
    }
    if(navInst[i] == "R"){
      faceDir <- faceDir + navVal[i]
      if(faceDir > 359){faceDir <- faceDir - 360}
    }
    if(navInst[i] == "E"){
      dx <- dx + navVal[i]
    }
    if(navInst[i] == "W"){
      dx <- dx - navVal[i]
    }
    if(navInst[i] == "N"){
      dy <- dy + navVal[i]
    }
    if(navInst[i] == "S"){
      dy <- dy - navVal[i]
    }
  }
  manDist <- abs(dx) + abs(dy)
  return(manDist)
}

#Part2

shipNav <- function(x){
  navInst <- str_extract(x, "[aA-zZ]+")
  navVal <- as.numeric(str_extract(x, "[0-9]+"))
  faceDir <- 90
  wpx <- 10
  wpy <- 1
  dx <- 0
  dy <- 0
  for(i in 1:length(navInst)){
    if(navInst[i] == "F"){
      dx <- dx + (wpx * navVal[i])
      dy <- dy + (wpy * navVal[i])
    }
    if(navInst[i] == "L"){
      rotMatrix <- matrix(c(wpx,wpy), nrow = 1, ncol = 2)
      wpx <- Rotation(rotMatrix, navVal[i] * pi / 180)[1]
      wpy <- Rotation(rotMatrix, navVal[i] * pi / 180)[2]
    }
    if(navInst[i] == "R"){
      rotMatrix <- matrix(c(wpx,wpy), nrow = 1, ncol = 2)
      wpx <- Rotation(rotMatrix, -navVal[i] * pi / 180)[1]
      wpy <- Rotation(rotMatrix, -navVal[i] * pi / 180)[2]
    }
    if(navInst[i] == "E"){
      wpx <- wpx + navVal[i]
    }
    if(navInst[i] == "W"){
      wpx <- wpx - navVal[i]
    }
    if(navInst[i] == "N"){
      wpy <- wpy + navVal[i]
    }
    if(navInst[i] == "S"){
      wpy <- wpy - navVal[i]
    }
  }
  manDist <- abs(dx) + abs(dy)
  return(manDist)
}
