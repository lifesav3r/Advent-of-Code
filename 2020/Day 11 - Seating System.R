#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(raster)

test <- readLines("test.txt")
input11 <- readLines("input.txt");

initSeats <- function(x){
  seatingArr <- strsplit(x,"")
  seatingArr <- matrix(".", nrow = length(x) + 2, ncol = nchar(x[1]) + 2)
  for(i in 1:length(x)){
    seatingArr[i + 1, 2:(nchar(x[i]) + 1)] <- strsplit(x[i],"")[[1]]
  }
  return(seatingArr)
}

seatStabilizer <- function(x){
  seatingArr <- initSeats(x)
  temp <- seatingArr
  while(TRUE){
    for(i in 2:(nrow(seatingArr) - 1)){
      for(j in 2:(ncol(seatingArr) - 1)){
        adjSeats <- seatingArr[(i - 1):(i + 1), (j - 1):(j + 1)]
        if(seatingArr[i,j] == "L" && !(any(adjSeats == "#"))){
          temp[i,j] <- "#"
        }
        if(seatingArr[i,j] == "#" && length(which(adjSeats == "#")) > 4){
          temp[i,j] <- "L"
        }
      }
    }
    if(identical(temp, seatingArr)){
      return(length(which(seatingArr == "#")))
    } else{
      seatingArr <- temp
    }
  }
}

#Part2

seatStabilizer <- function(x){
  seatingArr <- initSeats(x)
  temp <- seatingArr
  while (TRUE) {
    temp <- seatingArr
    for (i in 2:(nrow(seatingArr) - 1)) {
      for (j in 2:(ncol(seatingArr) - 1)) {
        if (seatingArr[i, j] != ".") {
          left <- seatingArr[i, max(which(seatingArr[i, 1:(j - 1)] != "."))]
          right <- seatingArr[i, (min(which(seatingArr[i, (j + 1):ncol(seatingArr)] != ".")) + j)]
          top <- seatingArr[max(which(seatingArr[1:(i - 1), j] != ".")), j]
          bot <- seatingArr[min(which(seatingArr[(i + 1):nrow(seatingArr), j] != ".")) + i, j]

          topleft <- topright <- botleft <- botright <- NA
          for (k in 1:min(nrow(seatingArr), ncol(seatingArr))) {
            if (is.na(topleft) &&
                i - k > 1 && j - k > 1 && seatingArr[i - k, j - k] != ".") {
              topleft <- seatingArr[i - k, j - k]
            }
            if (is.na(topright) &&
                i + k < nrow(seatingArr) &&
                j - k > 1 && seatingArr[i + k, j - k] != ".") {
              topright <- seatingArr[i + k, j - k]
            }
            if (is.na(botright) &&
                i + k < nrow(seatingArr) &&
                j + k < ncol(seatingArr) && seatingArr[i + k, j + k] != ".") {
              botright <- seatingArr[i + k, j + k]
            }
            if (is.na(botleft) &&
                i - k > 1 &&
                j + k < ncol(seatingArr) && seatingArr[i - k, j + k] != ".") {
              botleft <- seatingArr[i - k, j + k]
            }
          }

          if (seatingArr[i, j] == "L") {
            if ((is.na(bot) || bot == "L") &&
                (is.na(botleft) || botleft == "L") &&
                (is.na(botright) || botright == "L") &&
                (is.na(left) || left == "L") &&
                (is.na(right) || right == "L") &&
                (is.na(top) || top == "L") &&
                (is.na(topleft) || topleft == "L") &&
                (is.na(topright) || topright == "L")) {
              temp[i, j] <- "#"
            }
          }
          if (seatingArr[i, j] == "#") {
            count <- 0
            if (!is.na(bot) && bot == "#")
              count <- count + 1
            if (!is.na(botleft) && botleft == "#")
              count <- count + 1
            if (!is.na(botright) && botright == "#")
              count <- count + 1
            if (!is.na(left) && left == "#")
              count <- count + 1
            if (!is.na(right) && right == "#")
              count <- count + 1
            if (!is.na(top) && top == "#")
              count <- count + 1
            if (!is.na(topleft) && topleft == "#")
              count <- count + 1
            if (!is.na(topright) && topright == "#")
              count <- count + 1
            if (count > 4) {
              temp[i, j] <- "L"
            }
          }
        }
      }
    }
    if(identical(temp, seatingArr)){
      return(length(which(seatingArr == "#")))
    } else{
      seatingArr <- temp
    }
  }
}
