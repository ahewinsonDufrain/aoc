library(tidyverse)
library(magrittr)

source("utils/inputs.R")
source("utils/printing.R")
readInputs(2022)
day <- 2

input2 %<>% split_double_line_break()
test2 %<>% split_double_line_break()

stop("check inputs look ok")

# part a ------------------------------------------------------------------
## solve function for part a
solve2a <- function(inp) {
  98
}

## test part a
test_answer_a <- solve2a(test2)
expected_a    <- -1

## solve part a
pp_try(test_answer_a, expected_a, solve2a(input2), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve2b <- function(inp) {
  98
}

## test part b
test_answer_b <- solve2b(test2, 3)
expected_b    <- -1

## solve part b
pp_try(test_answer_b, expected_b, solve2b(input2, 3), day, "b")

