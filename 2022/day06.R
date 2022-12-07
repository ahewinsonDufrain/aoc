library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 6

input6 %<>% standard_parse_text()
test6 %<>% standard_parse_text()

# day 6 utils -------------------------------------------------------------
parse_signal <- function(x) {
  x %>% str_split("") %>% unlist()
}

# part a ------------------------------------------------------------------
## solve function for part a
solve6a <- function(inp) {
  for (i in 4:length(parse_signal(inp))) {
    if (length(unique(parse_signal(inp)[(i-3):i])) == 4) return(i)
  }
  NULL
}

## test part a
test_answer_a <- solve6a(test6)
expected_a    <- 7

## solve part a
pp_try(test_answer_a, expected_a, solve6a(input6), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve6b <- function(inp) {
  for (i in 14:length(parse_signal(inp))) {
    if (length(unique(parse_signal(inp)[(i-13):i])) == 14) return(i)
  }
  NULL
}

## test part b
test_answer_b <- solve6b(test6)
expected_b    <- 19

## solve part b
pp_try(test_answer_b, expected_b, solve6b(input6), day, "b")
