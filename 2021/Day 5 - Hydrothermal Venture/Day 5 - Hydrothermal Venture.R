#R version 4.1.2 (2021-11-01) -- "Bird Hippie"
#Copyright (C) 2021 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

library(reshape);

test <- read.table("N:/Advent of Code/2021/Day 5 - Hydrothermal Venture/test.txt", quote="\"", comment.char="");
test <- test[c(1,3)];
test <- within(test, V1<-data.frame(do.call('rbind', strsplit(as.character(V1), ',', fixed=TRUE))));
test <- as.matrix(within(test, V3<-data.frame(do.call('rbind', strsplit(as.character(V3), ',', fixed=TRUE)))));
colnames(test) <- c("x1", "y1", "x2", "y2");

data <- read.table("N:/Advent of Code/2021/Day 5 - Hydrothermal Venture/data.txt", quote="\"", comment.char="");
data <- data[c(1,3)];
data <- within(data, V1<-data.frame(do.call('rbind', strsplit(as.character(V1), ',', fixed=TRUE))));
data <- as.matrix(within(data, V3<-data.frame(do.call('rbind', strsplit(as.character(V3), ',', fixed=TRUE)))));
colnames(data) <- c("x1", "y1", "x2", "y2");

#Part 1

overlapCounter <- function(lines){
  lines <- subset(as.data.frame(lines), x1 == x2 | y1 == y2)
  x <- mapply(seq, lines$x1, lines$x2)
  y <- mapply(seq, lines$y1, lines$y2)
  xy <- do.call(rbind, mapply(cbind, x, y))
  sum(table(as.data.frame(xy)) > 1)
}

overlapCounter(data);

#Part 2

overlapCounter <- function(lines){
  lines <- as.data.frame(lines)
  x <- mapply(seq, lines$x1, lines$x2)
  y <- mapply(seq, lines$y1, lines$y2)
  xy <- do.call(rbind, mapply(cbind, x, y))
  sum(table(as.data.frame(xy)) > 1)
}

overlapCounter(data);
