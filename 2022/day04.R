library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 4

input4 %<>% standard_parse_text()
test4 %<>% standard_parse_text()

# day 4 utils -------------------------------------------------------------
get_lims <- function(x) {
  x %>% 
    str_split(",") %>% 
    lapply(str_split, "-") %>% 
    unlist() %>% 
    as.numeric() %>% 
    matrix(4)
}
test_full_overlap <- function(x) {
  (x[1] >= x[3] & x[2] <= x[4]) | (x[3] >= x[1] & x[4] <= x[2])
}
test_partial_overlap <- function(x) {
  x[1] <= x[4] & x[2] >= x[3]
}
find_common <- function(m) {
  intersect(m[, 1], m[, 2])
}

# part a ------------------------------------------------------------------
## solve function for part a
solve4a <- function(inp) {
  m <- get_lims(inp)
  apply(m, 2, test_full_overlap) %>% sum()
}

## test part a
test_answer_a <- solve4a(test4)
expected_a    <- 2

## solve part a
pp_try(test_answer_a, expected_a, solve4a(input4), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve4b <- function(inp) {
  m <- get_lims(inp)
  apply(m, 2, test_partial_overlap) %>% sum()
}

## test part b
test_answer_b <- solve4b(test4)
expected_b    <- 4

## solve part b
pp_try(test_answer_b, expected_b, solve4b(input4), day, "b")
