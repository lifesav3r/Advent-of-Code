test <- scan("test.txt","");
data <- scan("input.txt","");

#Part 1

input <- matrix(data, nc = 2, byrow = TRUE);

points <- apply(input, 1, function(x) switch(x[1], 
    "A" = switch(x[2], "X" = 3, "Y" = 6, "Z" = 0),
    "B" = switch(x[2], "X" = 0, "Y" = 3, "Z" = 6),
    "C" = switch(x[2], "X" = 6, "Y" = 0, "Z" = 3)));

sum(points+sapply(input[,2], function(x) switch(x, "X" = 1, "Y" = 2, "Z" = 3)));

#Part 2

points <- apply(input, 1, function(x) switch(x[1], 
    "A" = switch(x[2], "X" = 3, "Y" = 1, "Z" = 2),
    "B" = switch(x[2], "X" = 1, "Y" = 2, "Z" = 3),
    "C" = switch(x[2], "X" = 2, "Y" = 3, "Z" = 1)));

sum(points+sapply(input[,2], function(x) switch(x, "X" = 0, "Y" = 3, "Z" = 6)));