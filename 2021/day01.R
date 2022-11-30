library(tidyverse)
library(magrittr)

source("utils/inputs.R")
source("utils/printing.R")
readInputs(2021)
day <- 1

input1 %<>% standard_parse_number()
test1 %<>% standard_parse_number()

solve1a <- function(inp) {
  toret <- inp %>% as.numeric() %>% diff() %>% `>`(0) %>% sum()
  toret
}

solve1a <- function(inp, offset) {
  early <- inp[1:(length(inp)-offset)]
  late <- inp[(1+offset):length(inp)]
  sum(late > early)
}
solve1b <- solve1a

## test part a
test_answer_a <- solve1a(test1, 1)
expected_a    <- 7

## solve part a
pp_try(test_answer_a, expected_a, solve1a(input1, 1), day, "a")

## test part b
test_answer_b <- solve1b(test1, 3)
expected_b    <- 5

## solve part b
pp_try(test_answer_b, expected_b, solve1b(input1, 3), day, "b")

