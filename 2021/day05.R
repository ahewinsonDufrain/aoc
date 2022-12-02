library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2021)
day <- 5

input5 %<>% standard_parse_text()
test5 %<>% standard_parse_text()

# day 5 utilities ---------------------------------------------------------
parse_hydros <- function(x) {
  x %>% 
    str_split(" -> ") %>% 
    lapply(function(q) unlist(str_split(q, ","))) %>% 
    lapply(as.numeric)
}
is_diagonal <- function(x) {
  (x[1] != x[3]) & (x[2] != x[4])
}

# part a ------------------------------------------------------------------
## solve function for part a
solve5a <- function(inp) {
  hydros <- inp %>% parse_hydros()
  hydros <- hydros[!sapply(hydros, is_diagonal)]
  
}

solve5a(test5) %>% print()
stop(98)

## test part a
test_answer_a <- solve5a(test5)
expected_a    <- 5

## solve part a
pp_try(test_answer_a, expected_a, solve5a(input5), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve5b <- function(inp) {
  98
}

## test part b
test_answer_b <- solve5b(test5)
expected_b    <- -1

## solve part b
pp_try(test_answer_b, expected_b, solve5b(input5), day, "b")

