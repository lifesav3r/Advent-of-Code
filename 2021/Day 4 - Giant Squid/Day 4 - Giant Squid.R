#R version 4.1.2 (2021-11-01) -- "Bird Hippie"
#Copyright (C) 2021 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

testNumbers <- read.csv("N:/Advent of Code/2021/Day 4 - Giant Squid/testNumbers.txt", header=FALSE);
testBoards <- read.table("N:/Advent of Code/2021/Day 4 - Giant Squid/testBoards.txt", quote="\"");

numbers <- read.csv("N:/Advent of Code/2021/Day 4 - Giant Squid/numbers.txt", header=FALSE);
boards <- read.table("N:/Advent of Code/2021/Day 4 - Giant Squid/boards.txt", quote="\"");

#Part 1

boardMarker <- function(board, number){
  marked <- is.na(board)
  if(all(c(rowMeans(marked), colMeans(marked)) != 1)){
    return(0)
  }
  sum(board, na.rm = TRUE) * number
}

bingo <- function(boards, numbers){
  numBoards <- nrow(boards) / 5
  boardID <- rep(1:numBoards, each = 5)

  for(i in numbers){
    boards[boards == i] <- NA
    score <- unname(sapply(split(boards, boardID), boardMarker, i))
    if(any(score > 0)){
      return(score[score > 0])
    }
  }
}

bingo(boards, numbers);

#Part 2

bingo2 <- function(boards, numbers){
  for(i in numbers){
    numBoards <- nrow(boards) / 5
    boardID <- rep(1:numBoards, each = 5)
    boards[boards == i] <- NA
    score <- unname(sapply(split(boards, boardID), boardMarker, i))
    if(any(score > 0)){
      if(numBoards == 1){
        return(score[score > 0])
      }
      boards <- boards[boardID %in% which(score == 0),]
    }
  }
}
