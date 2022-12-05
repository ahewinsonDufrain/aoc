library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 5

input5 %<>% standard_parse_text()
test5 %<>% standard_parse_text()

# day 5 utils -------------------------------------------------------------
parse_stacks <- function(x) {
  stackIDs <- which(str_detect(x, "^[ 0-9]+$"))
  m <- x[1:(stackIDs-1)] %>% 
    str_split("") %>% 
    unlist() %>% 
    matrix(ncol = stackIDs-1)
  m[seq(2, nrow(m), 4), ] %>% 
    apply(1, function(q) q[q != " "])
}
stevedor9000 <- function(x) {
  stackIDs <- which(str_detect(x, "^[ 0-9]+$"))
  stacks <- parse_stacks(x)
  instructions <- x[-1:-stackIDs] %>% 
    str_extract_all("[0-9]+") %>% 
    lapply(as.integer)
  
  for (i in instructions) {
    for (n in 1:i[1]) {
      stacks[[i[3]]] <- append(stacks[[i[3]]], stacks[[i[2]]][1], 0)
      stacks[[i[2]]] <- stacks[[i[2]]][-1]
    }
  }
  sapply(stacks, function(q) q[1]) %>% paste(collapse="")
  
}
stevedor9001 <- function(x) {
  stackIDs <- which(str_detect(x, "^[ 0-9]+$"))
  stacks <- parse_stacks(x)
  instructions <- x[-1:-stackIDs] %>% 
    str_extract_all("[0-9]+") %>% 
    lapply(as.integer)
  
  for (i in instructions) {
    stacks[[i[3]]] <- append(stacks[[i[3]]], stacks[[i[2]]][1:i[1]], 0)
    stacks[[i[2]]] <- stacks[[i[2]]][-1:-i[1]]
  }
  sapply(stacks, function(q) q[1]) %>% paste(collapse="")
  
}

# test5 %>% stevedor() %>% print()
# #input5 %>% stevedor() %>% print()
# stop(98)

# part a ------------------------------------------------------------------
## solve function for part a
solve5a <- function(inp) {
  stevedor9000(inp)
}

## test part a
test_answer_a <- solve5a(test5)
expected_a    <- "CMZ"

## solve part a
pp_try(test_answer_a, expected_a, solve5a(input5), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve5b <- function(inp) {
  stevedor9001(inp)
}

## test part b
test_answer_b <- solve5b(test5)
expected_b    <- "MCD"

## solve part b
pp_try(test_answer_b, expected_b, solve5b(input5), day, "b")
