# R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Copyright (C) 2021 The R Foundation for Statistical Computing
# Platform: x86_64-apple-darwin17.0 (64-bit)

test <- readLines("~/Documents/GitHub/Advent-of-Code/2021/Day 10 - Syntax Scoring/test.txt");
data <- readLines("~/Documents/GitHub/Advent-of-Code/2021/Day 10 - Syntax Scoring/data.txt");

#Part 1

syntaxScoring <- function(x){
  errScore <- c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)
  while(grepl("(\\(\\))|(\\{\\})|(\\[\\])|(<>)", x)){
    x <- gsub("(\\(\\))|(\\{\\})|(\\[\\])|(<>)", "", x)
  }
  errScore[head(el(strsplit(gsub("^[[({<]+", "", x), "")), 1)]
}

sum(unlist(sapply(data,syntaxScoring)));

#Part 2

syntaxScoring2 <- function(x){
  errScore <- 0
  while(grepl("(\\(\\))|(\\{\\})|(\\[\\])|(<>)", x)){
    x <- gsub("(\\(\\))|(\\{\\})|(\\[\\])|(<>)", "", x)
  }
  incChar <- rev(chartr("([{<", ")]}>", el(strsplit(x, ""))))
  for(i in seq_along(incChar)){
    errScore <- errScore * 5 + switch(incChar[i], ")" = 1 , "]" = 2, "}" = 3, ">" = 4)
  }
  errScore
}

part1 <- sapply(data, syntaxScoring);
median(sapply(data[sapply(part1, length) == 0], syntaxScoring2));
