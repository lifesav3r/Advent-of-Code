#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

test <- readLines("test.txt")
input7 <- readLines("input.txt");

bagInit <- function(x){
  x <- strsplit(x, split = " ")[[1]]
  bag <- list(N = as.numeric(x[1]), color = paste(x[2], x[3], sep = "_"))
  return(bag)
}

guideCreator <- function(x, sep = ",", word = "contain"){
  x <- strsplit(x, split = word)[[1]]
  outside <- strsplit(x[1], split = " ")[[1]]
  outside <- paste(outside[1], outside[2], sep = "_")
  inside <- strsplit(x[2], split = sep)[[1]]
  inside <- gsub("^ ", "", inside)
  if(length(inside) == 1){
    if(grepl("no other", inside)){
      color <- "none"
      numBags <- -999
    } else{
      contents <- bagInit(inside)
      color <- contents$color
      numBags <- contents$N
    }
  } else{
    contents <- lapply(inside, FUN = bagInit)
    color <- sapply(contents,"[[", "color")
    numBags <- sapply(contents, "[[", "N")
  }
  table <- data.frame(outside = outside, color = color, N = numBags, stringsAsFactors = FALSE)
  return(table)
}

searchBag <- function(bag, guide){
  curBag <- which(guide$color == bag)
  if(length(curBag) > 0){
    col1 <- guide$outside[curBag]
    col2 <- lapply(col1,searchBag,guide = guide)
    bagOut <- c(col1, do.call('c', col2))
  } else{
    bagOut <- character(length = 0)
  }
  return(bagOut)
}

guide <- lapply(input7, FUN = guideCreator)
guide <- do.call('rbind', guide)
allBags <- searchBag("shiny_gold",guide[which(guide$N > 0),])
numColors <- unique(allBags)
length(numColors)

#Part2

bagContents <- function(color, guide, mult = 1){
  curBag <- which(guide$outside == color)
  N <- guide$N[curBag]
  inside <- guide$color[curBag]
  if(length(curBag) == 1){
    if(N > 0){
      total <- bagContents(inside, guide, N)
    } else{
      total <- 0
    }
  } else if(length(curBag) > 1){
    total <- mapply(FUN = function(col, m){
      bagContents(col, guide, m)
      }, col = inside, m = N, SIMPLIFY = TRUE)
  }
  total <- mult + sum(total * mult)
  return(total)
}

allBags <- sort(unique(guide$outside))
contents <- sapply(allBags, bagContents, guide, 1)
totalBags <- data.frame(color = allBags, numBags = contents - 1, row.names = NULL, stringsAsFactors = FALSE)
totalBags[which(totalBags$color == "shiny_gold"),]
