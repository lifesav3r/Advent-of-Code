test <- read.table("test.txt", sep = "", header = FALSE);
input <- read.table("input.txt", sep = "", header = FALSE);

#Part 1

listDistance <- function(input){
    col1 <- sort(as.numeric(input[,1]))
    col2 <- sort(as.numeric(input[,2]))
    ans <- sum(abs(col1-col2))
    print(ans)
}

#Part 2

col1 <- sort(as.numeric(input[,1]))
col2 <- sort(as.numeric(input[,2]))

listComparer <- sapply(col1, function(x){
    val <- sum(col2 == x)
    val * x
})

sum(listComparer);