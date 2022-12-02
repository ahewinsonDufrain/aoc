library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 3

input3 %<>% standard_parse_text()
test3 %<>% standard_parse_text()

# day 3 utils -------------------------------------------------------------

# part a ------------------------------------------------------------------
## solve function for part a
solve3a <- function(inp) {
  98
}

## test part a
test_answer_a <- solve3a(test3)
expected_a    <- -1

## solve part a
pp_try(test_answer_a, expected_a, solve3a(input3), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve3b <- function(inp) {
  98
}

## test part b
test_answer_b <- solve3b(test3)
expected_b    <- -1

## solve part b
pp_try(test_answer_b, expected_b, solve3b(input3), day, "b")

