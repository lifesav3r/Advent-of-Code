#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

seatID <- function(x){
  max <- 0
  for(i in 1:nrow(x)){
    rows <- c(1:128)
    cols <- c(1:8)
    for(j in 1:10){
      if(substring(x[i,1],j,j) == "F"){
        rows <- head(rows, n = 0.5 * length(rows))
      }
      if(substring(x[i,1],j,j) == "B"){
        rows <- tail(rows, n = 0.5 * length(rows))
      }
      if(substring(x[i,1],j,j) == "L"){
        cols <- head(cols, n = 0.5 * length(cols))
      }
      if(substring(x[i,1],j,j) == "R"){
        cols <- tail(cols, n = 0.5 * length(cols))
      }
    }
    current <- (rows - 1) * 8 + (cols - 1)
    if(current > max){
      max <- current
    }
  }
  return(max)
}

seatID(input4)

#Part2

seatID <- function(x){
  max <- 0
  remainID <- NULL
  for(i in 1:nrow(x)){
    rows <- c(1:128)
    cols <- c(1:8)
    for(j in 1:10){
      if(substring(x[i,1],j,j) == "F"){
        rows <- head(rows, n = 0.5 * length(rows))
      }
      if(substring(x[i,1],j,j) == "B"){
        rows <- tail(rows, n = 0.5 * length(rows))
      }
      if(substring(x[i,1],j,j) == "L"){
        cols <- head(cols, n = 0.5 * length(cols))
      }
      if(substring(x[i,1],j,j) == "R"){
        cols <- tail(cols, n = 0.5 * length(cols))
      }
    }
    currentID <- (rows - 1) * 8 + (cols - 1)
    remainID <- append(remainID, currentID)
    }
    expID <- c(min(remainID):max(remainID))
    remainID <- setdiff(expID, remainID)
    return(remainID)
  }

seatID(input4)
