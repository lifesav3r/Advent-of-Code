#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

library(stringr)
library(dplyr)

#Part1

test <- readLines("test.txt");
input16 <- readLines("input.txt");

checkValid <- function(x){
  myTicket <- x[(which(grepl("^your ticket", x)) + 1):length(x)]
  nearbyTickets <- str_split_fixed(x[(which(grepl("^nearby", x)) + 1):length(x)], ",", Inf)
  mode(nearbyTickets) <- "integer"
  fields <- x[1:(which(grepl("^your ticket", x)) - 2)]
  fields <- str_match_all(fields, "^([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)")
  fields <- as.data.frame(t(sapply(fields, function(x) matrix(x[,-1], ncol = 5))))
  fields[,2:5] <- lapply(fields[,2:5], as.integer)
  names(fields) <- c("field","min1", "max1", "min2", "max2")
  validList <- sapply(nearbyTickets, function(y) any((fields$min1 <= y & y <= fields$max1) | (fields$min2 <= y & y <= fields$max2)))
  dim(validList) <- dim(nearbyTickets)
  errRate <- sum(unlist(sapply(1:nrow(nearbyTickets), function(z) if(!all(validList[z,])) nearbyTickets[z, !validList[z,]])))
  return(errRate)
}

checkValid(input16)

#Part2

checkValid <- function(x){
  myTicket <- x[(which(grepl("^your ticket", x)) + 1):length(x)]
  nearbyTickets <- str_split_fixed(x[(which(grepl("^nearby", x)) + 1):length(x)], ",", Inf)
  mode(nearbyTickets) <- "integer"
  fields <- x[1:(which(grepl("^your ticket", x)) - 2)]
  fields <- str_match_all(fields, "^([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)")
  fields <- as.data.frame(t(sapply(fields, function(x) matrix(x[,-1], ncol = 5))))
  fields[,2:5] <- lapply(fields[,2:5], as.integer)
  names(fields) <- c("field","min1", "max1", "min2", "max2")
  validList <- sapply(nearbyTickets, function(y) any((fields$min1 <= y & y <= fields$max1) | (fields$min2 <= y & y <= fields$max2)))
  dim(validList) <- dim(nearbyTickets)

  validTickets <- nearbyTickets[apply(validList, 1, all),]
  candidates <- apply(fields, 1, function(row)
      which(
          apply(matrix(between(validTickets,row[2], row[3]) | between(validTickets, row[4], row[5]),
                       nrow = nrow(validTickets),
                       ncol = ncol(validTickets)),
                       2, all)
            )
  )
  names(candidates) <- fields$class

  ord <- names(sort(sapply(candidates, length)))
  candidates <- candidates[ord]
  for(i in 1:(length(candidates)-1)){
      candidates[(i+1):length(candidates)] <- lapply(candidates[(i+1):length(candidates)], function(x) setdiff(x, candidates[[i]]))
  }
  dep <- grep('departure', names(candidates))
  prod(as.integer(str_split(myTicket, ",")[[1]])[unlist(candidates[dep])])
}
