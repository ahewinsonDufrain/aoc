library(tidyverse)
library(magrittr)

source("utils/inputs.R")
source("utils/printing.R")
readInputs(2022)
day <- 1

input1 %<>% standard_parse_number()
test1 %<>% standard_parse_number()

## solve function for part a
solve1a <- function(inp) {
  98
}

## test part a
test_answer_a <- solve1a(test1)
expected_a    <- -1

## solve part a
pp_try(test_answer_a, expected_a, solve1a(input1, 1), day, "a")

## solve function for part b
solve1b <- function(inp) {
  98
}

## test part b
test_answer_b <- solve1b(test1)
expected_b    <- -1

## solve part b
pp_try(test_answer_b, expected_b, solve1b(input1), day, "b")

