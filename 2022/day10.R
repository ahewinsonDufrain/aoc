library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 10

input10 %<>% standard_parse_text()
test10 %<>% standard_parse_text()

# day 10 utils -------------------------------------------------------------

# part a ------------------------------------------------------------------
## solve function for part a
solve10a <- function(inp) {
  expanded <- lapply(inp, function(q) {
    if (str_detect(q, "noop")) 0
    else c(0, str_replace(q, "[^0-9-]+", "") %>% as.numeric())
  })
  sum((1 + c(unlist(expanded)) %>% cumsum())[seq(19, 221, 40)] * seq(20, 220, 40))
}

## test part a
test_answer_a <- solve10a(test10)
expected_a    <- 13140

## solve part a
pp_try(test_answer_a, expected_a, solve10a(input10), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve10b <- function(inp) {
  expanded <- lapply(inp, function(q) {
    if (str_detect(q, "noop")) 0
    else c(0, str_replace(q, "[^0-9-]+", "") %>% as.numeric())
  }) %>% 
    unlist() %>% 
    c(1, .) %>% cumsum()

  sapply(1:240, function(cycle) {
    cyclemod <- 1+((cycle-1) %% 40)
    if (abs(expanded[cycle] - cyclemod + 1) < 2) 1 else 0
  }) %>% 
    matrix(40) %>% 
    t()
}

## test part b
test_answer_b <- solve10b(test10)
test_answer_b %>% apply(1, rev) %>% apply(1, rev) %>% apply(1, rev) %>% image() %>% print()

## solve part b
solve10b(input10) %>% apply(1, rev) %>% apply(1, rev) %>% apply(1, rev) %>% image() %>% print()
