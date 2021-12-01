#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

test <- readLines("test.txt");
input8 <- readLines("input.txt");

bootCode <- function(x){
  code <- strsplit(x, " ")
  position <- 1
  usedPos <- NULL
  acc <- 0
  while(TRUE){
    if(position %in% usedPos){
      return(acc)
    } else if(code[[position]][1] == "nop"){
      usedPos <- append(usedPos,position)
      position <- position + 1
    } else if(code[[position]][1] == "acc"){
      usedPos <- append(usedPos,position)
      acc <- acc + as.numeric(code[[position]][2])
      position <- position + 1
    } else if(code[[position]][1] == "jmp"){
      usedPos <- append(usedPos,position)
      position <- position + as.numeric(code[[position]][2])
    }
  }
}

bootCode(input8)

#Part2

codeChanger <- function(x,pos){
  original <- strsplit(x, " ")
  out <- original
  if(original[[pos]][1] == "nop"){
    out[[pos]][1] <- "jmp"
    return(out)
  } else if(original[[pos]][1] == "jmp"){
    out[[pos]][1] <- "nop"
    return(out)
  } else if(original[[pos]][1] == "acc"){
    return("go next")
  }
}

bootCode <- function(x){
  for(i in 1:length(x)){
    code <- codeChanger(x,i)
    position <- 1
    counter <- 0
    acc <- 0
    if(code != "go next"){
      while(counter < length(x) * 10){
        if(position == (length(x) + 1)){
          return(acc)
        } else if(code[[position]][1] == "nop"){
          position <- position + 1
          counter <- counter + 1
        } else if(code[[position]][1] == "acc"){
          acc <- acc + as.numeric(code[[position]][2])
          position <- position + 1
          counter <- counter + 1
        } else if(code[[position]][1] == "jmp"){
          position <- position + as.numeric(code[[position]][2])
          counter <- counter + 1
        }
      }
    }
  }
}
