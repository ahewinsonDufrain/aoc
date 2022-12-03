library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 4

input4 %<>% standard_parse_text()
test4 %<>% standard_parse_text()

# day 4 utils -------------------------------------------------------------
find_common <- function(m) {
  intersect(m[, 1], m[, 2])
}

# part a ------------------------------------------------------------------
## solve function for part a
solve4a <- function(inp) {
  98
}

## test part a
test_answer_a <- solve4a(test4)
expected_a    <- -1

## solve part a
pp_try(test_answer_a, expected_a, solve4a(input4), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve4b <- function(inp) {
  98
}

## test part b
test_answer_b <- solve4b(test4)
expected_b    <- -1

## solve part b
pp_try(test_answer_b, expected_b, solve4b(input4), day, "b")
