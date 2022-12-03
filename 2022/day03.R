library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 3

input3 %<>% standard_parse_text()
test3 %<>% standard_parse_text()

# day 3 utils -------------------------------------------------------------
find_common <- function(m) {
  intersect(m[, 1], m[, 2])
}

# part a ------------------------------------------------------------------
## solve function for part a
solve3a <- function(inp) {
  sapply(inp, function(q) {
    str_split(q, "") %>% 
      unlist() %>% 
      matrix(ncol = 2) %>% 
      find_common() %>% 
      match(c(letters, LETTERS))
  }) %>% sum()
}

## test part a
test_answer_a <- solve3a(test3)
expected_a    <- 157

## solve part a
pp_try(test_answer_a, expected_a, solve3a(input3), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve3b <- function(inp) {
  badges <- inp %>% matrix(3) %>% apply(2, function(q) {
    ql <- q %>% str_split("")
    ql[[1]] %>% intersect(ql[[2]]) %>% intersect(ql[[3]])
  })
  badges %>% match(c(letters, LETTERS)) %>% sum()
}

## test part b
test_answer_b <- solve3b(test3)
expected_b    <- 70

## solve part b
pp_try(test_answer_b, expected_b, solve3b(input3), day, "b")
