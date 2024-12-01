test <- readLines("test.txt");
data <- readLines("input.txt");

#Part 1

input <- data;

x <- c(0, which(input == ""));
elves <- list();
for(i in seq_along(x)){
    y <- ifelse(i == length(x), length(input), x[i+1] - 1)
    elves[[i]] <- as.integer(input[(x[i] + 1):y])
}

calories <- sapply(elves, sum);
max(calories);

#Part 2

sum(tail(sort(calories), 3));