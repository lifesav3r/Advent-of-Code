#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

maxExpense <- function(x){
  PHTB <- combn(x,2)
  correctCol <- (colSums(PHTB) == 2020)
  correctCol <- which(correctCol == TRUE)
  result <- prod(PHTB[,correctCol])
  print(result)
}

maxExpense(input1)

#Part2

maxExpense <- function(x){
  PHTB <- combn(x,3)
  correctCol <- (colSums(PHTB) == 2020)
  correctCol <- which(correctCol == TRUE)
  result <- prod(PHTB[,correctCol])
  print(result)
}

maxExpense(input1)
