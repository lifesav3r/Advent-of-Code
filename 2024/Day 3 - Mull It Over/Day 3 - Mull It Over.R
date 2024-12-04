library(dplyr);

test <- readLines("test.txt");
input <- readLines("input.txt");

#Part 1

memoryFix <- function(x){
    memory <- regmatches(x, gregexpr("mul\\(\\K\\d+,\\d+(?=\\))", x, perl = TRUE))[[1]]
    memory <- memory %>%
        strsplit(.,",") %>%
        unlist() %>%
        as.numeric() %>%
        matrix(., ncol = 2, byrow = TRUE)
    ans <- sum(memory[,1] * memory[,2])
    print(ans)
}

#Part 2

memoryFix2 <- function(x){
    memIndex <- gregexpr("mul\\(\\K\\d+,\\d+(?=\\))", x, perl = TRUE)
    memory <- regmatches(x, memIndex)[[1]]
    memIndex <- as.vector(memIndex[[1]])

    do <- as.vector(gregexpr("do\\(\\)", x, perl = TRUE)[[1]])
    dont <- as.vector(gregexpr("don't\\(\\)", x, perl = TRUE)[[1]])

    memory <- memory %>%
        strsplit(.,",") %>%
        unlist() %>%
        as.numeric() %>%
        matrix(., ncol = 2, byrow = TRUE)

    enabled <- 1
    ans <- 0
    for(i in sort(c(memIndex, do, dont))){
        if(i %in% do){
            enabled <- 1
        }
        else if(i %in% dont){
            enabled <- 0
        }
        else{
            j <- which(memIndex == i)
            ans <- ans + enabled * (memory[j, 1] * memory[j, 2])
        }
    }
    print(ans)
}