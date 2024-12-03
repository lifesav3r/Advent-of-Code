test <- readLines("test.txt");
input <- readLines("input.txt");

test <- unlist(lapply(test, strsplit, split = " "), recursive = FALSE)
input <- unlist(lapply(input, strsplit, split = " "), recursive = FALSE)

test <- lapply(test, as.numeric)
input <- lapply(input, as.numeric)

#Part 1

reportChecker <- function(x){
    diffCheck <- diff(x)
    if(length(unique(sign(diffCheck))) == 1){
        if(max(abs(diffCheck)) <= 3 && min(abs(diffCheck)) >= 1){
            return(1)
        } else{
            return(0)
        }
    } else {
        return(0)
    }
}

sum(unlist(lapply(input, reportChecker)));

#Part 2