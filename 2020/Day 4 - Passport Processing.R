#R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Part1

library(tidyverse);

validPass <- function(x){
  valid <- 0
  table <- str_split(x, "\r\n\r")
  table <- unlist(table)
  table <- str_split(table,"( )|(\\r)")
  for(i in 1:length(table)){
    if(any(str_detect(table[[i]],"byr")) & any(str_detect(table[[i]],"iyr")) & any(str_detect(table[[i]],"eyr")) & any(str_detect(table[[i]],"hgt")) & any(str_detect(table[[i]],"hcl")) & any(str_detect(table[[i]],"ecl")) & any(str_detect(table[[i]],"pid"))){
      valid <- valid + 1
    }
  }
  return(valid)
}

validPass(input4);

#Part2

validPass <- function(x){
  valid <- 0
  table <- str_split(x, "\r\n\r")
  table <- unlist(table)
  table <- str_split(table,"( )|(\\r)")
  for(i in 1:length(table)){
    if(any(str_detect(table[[i]],"(byr:((19[2-9][0-9]$)|(200[0-2])$))")) &
      any(str_detect(table[[i]],"(iyr:((201[0-9]$)|(2020$)))")) &
      any(str_detect(table[[i]],"(eyr:((202[0-9]$)|(2030$)))")) &
      any(str_detect(table[[i]],"(hgt:((((15[0-9])|(1[6-8][0-9])|(19[0-3]))cm$)|(((59)|(6[0-9])|(7[0-6]))in$)))")) & any(str_detect(table[[i]],"(hcl:#[0-9a-f]{6}$)")) & any(str_detect(table[[i]],"(ecl:((amb$)|(blu$)|(brn$)|(gry$)|(grn$)|(hzl$)|(oth$)))")) & any(str_detect(table[[i]],"(pid:[0-9]{9}$)"))){
      valid <- valid + 1
    }
  }
  return(valid)
}

validPass(input4);
