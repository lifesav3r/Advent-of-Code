# R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: x86_64-apple-darwin17.0 (64-bit)

test <- readLines("~/Documents/GitHub/Advent-of-Code/2021/Day 8 - Seven Segment Search/test.txt");
data <- readLines("~/Documents/GitHub/Advent-of-Code/2021/Day 8 - Seven Segment Search/data.txt");

#Part 1

segDisplay <- function(x){
  x <- do.call(rbind, strsplit(x, "\\|"))
  display <- strsplit(gsub("^ ", "", x[,2]), " ")
  sum(sapply(display, function(y) sum(nchar(y) %in% c(2,3,4,7))))
}

segDisplay(data);

#Part 2

segDisplay2 <- function(x){
  x <- do.call(rbind, strsplit(x, "\\|"))
  outputAll <- c()
  for(i in 1:nrow(x)){
    display <- as.list(rep(NA,7))
    code <- strsplit(gsub("^ ", "", x[i,]), " ")
    numChar <- sapply(code,nchar)
    codeList <- unlist(code)
    numCharList <- unlist(numChar)
    output <- rep(NA, length(code[[2]]))
    numCharOutput <- tail(numCharList, length(output))
    codeOutput <- tail(codeList, length(output))
    if(any(numCharList == 2)){
      ONE <- unique(unlist(strsplit(codeList[which(numCharList == 2)], "")))
      display[[3]] <- display[[6]] <- ONE
    }
    if(any(numCharList == 3)){
      SEVEN <- unique(unlist(strsplit(codeList[which(numCharList == 3)], "")))
      if(!is.na(display[[3]][[1]])){
        display[[1]] <- SEVEN[!SEVEN %in% ONE]
      } else{
        display[[1]] <- display[[3]] <- display[[6]] <- ONE
      }
    }
    if(any(numCharList == 4)){
      FOUR <- unique(unlist(strsplit(codeList[which(numCharList == 4)], "")))
      if(!is.na(display[[3]][[1]])){
        display[[2]] <- display[[4]] <- FOUR[!FOUR %in% display[[3]]]
      } else{
        display[[2]] <- display[[4]] <- display[[3]] <- display[[6]] <- FOUR
      }
    }
    if(any(numCharList == 6)){
      check069 <- strsplit(codeList[which(numCharList == 6)], "")
      check09 <- sapply(check069, function(y) all(display[[3]] %in% y))
      check69 <- sapply(check069, function(y) all(display[[4]] %in% y))
      if(any(!check09)){
        SIX <- check069[!check09][[1]]
        display[[3]] <- letters[1:7][!letters[1:7] %in% SIX]
        display[[6]] <- display[[6]][!display[[6]] %in% display[[3]]]
      }
      if(any(!check69)){
        ZERO <- check069[!check69][[1]]
        display[[4]] <- letters[1:7][!letters[1:7] %in% ZERO]
        display[[2]] <- display[[2]][!display[[2]] %in% display[[4]]]
      }
    }
    output[numCharOutput == 2] <- 1
    output[numCharOutput == 3] <- 7
    output[numCharOutput == 4] <- 4
    output[numCharOutput == 7] <- 8
    if(length(display[[3]]) == 1){
      output[numCharOutput == 6 & !grepl(display[[3]], codeOutput)] <- 6
    }
    if(length(display[[4]]) == 1){
      output[numCharOutput == 6 & !grepl(display[[4]], codeOutput)] <- 0
    }
    output[numCharOutput == 6 & !output %in% c(0,6)] <- 9
    if(length(display[[3]]) == 1){
      output[numCharOutput == 5 & !grepl(display[[6]], codeOutput)] <- 2
      output[numCharOutput == 5 & grepl(display[[3]],codeOutput) & grepl(display[[6]], codeOutput)] <- 3
      output[numCharOutput == 5 & !grepl(display[[3]], codeOutput)] <- 5
    } else{
      output[numCharOutput == 5 & sapply(codeOutput, function(y) sum(display[[3]] %in% el(strsplit(y,"")))) == 2] <- 3
      if(length(display[[2]]) == 1){
        output[numCharOutput == 5 & output != 3 & !grepl(display[[2]], codeOutput)] <- 2
      }else{
        output[numCharOutput == 5 & output != 3 & sapply(codeOutput, function(y) sum(display[[2]] %in% el(strsplit(y,"")))) == 1] <- 2
      }
      output[numCharOutput == 5 & !output %in% c(2,3)] <- 5
    }
    outputAll[i] <- paste(output, collapse = "")
  }
  sum(as.integer(outputAll))
}
