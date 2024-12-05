library(data.table);

test <- as.matrix(read.fwf(file = "test.txt", widths = rep(1,10)));
input <- as.matrix(read.fwf(file = "input.txt", widths = rep(1, 140)));

#Part 1

wordTabler <- function(x, y){
  data.table(which(y == x, arr.ind = T))[,
 let(N = row-1, E = col+1, S = row+1, W = col-1)]
}
wordTest <- lapply(c("X", "M", "A", "S"), wordTabler, test);
wordTable <- lapply(c("X", "M", "A", "S"), wordTabler, input);

find <- function(x, y, addX = TRUE) {
  ns <- fcase(y %like% "N", "N", y %like% "S", "S", default = "row")
  ew <- fcase(y %like% "E", "E", y %like% "W", "W", default = "col")
  res <- x[[1]][, .(row = get(ns), col = get(ew))][
      x[[2]], on = .(row, col), .(row = get(ns),
      col = get(ew), ar = row, ac = col), nomatch = NULL][
      x[[3]], on = .(row, col), .(row = get(ns),
      col = get(ew), ar, ac), nomatch = NULL]
  if (addX) {
    res[x[[4]], on = .(row, col), .(row = get(ns),
    col = get(ew), ar, ac), nomatch = NULL]
  } else res
} 

rbindlist(lapply( c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), find, x = wordTable))[, .N];

# Part 2
MASTable <- rbindlist(lapply(c("NE", "SE", "SW", "NW"), find, x = wordTable[2:4], addX = F), idcol = T);
MASTable[MASTable, on = .(ar, ac)][.id != i..id][, .N/2];