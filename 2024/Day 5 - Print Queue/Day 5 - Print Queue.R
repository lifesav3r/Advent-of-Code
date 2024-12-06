test <- readLines("test.txt");
input <- readLines("input.txt");

order <- sapply(strsplit(input[grep("\\|", input)], "\\|"), as.integer);
update <- lapply(strsplit(input[grep(",", input)], ","), as.integer);

#Part 1

pageOrder <- function(x){
    ordSubset <- order[,apply(order, 2, \(z) sum(x %in% z) == 2L)]
    sortedList <- as.integer(names(sort(-table(ordSubset[1, ]))))
    sortedList <- c(sortedList, setdiff(x, ordSubset[1,]))
    sortedList[(length(x) + 1) / 2] * if (identical(sortedList, x)) 1 else -1
}

ans <- sapply(update, pageOrder);
sum(ans[ans > 0])

#Part 2
sum(-ans[ans < 0])