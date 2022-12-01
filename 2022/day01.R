library(tidyverse)
library(magrittr)

source("utils/inputs.R")
source("utils/printing.R")
readInputs(2022)
day <- 1

input1 %<>% split_double_line_break()
test1 %<>% split_double_line_break()

# part a ------------------------------------------------------------------
## solve function for part a
solve1a <- function(inp, topn = 1) {
  inp %>%
    lapply(as.numeric) %>%
    lapply(replace_na, 0) %>% 
    sapply(sum) %>% 
    sort(decreasing = TRUE) %>% 
    `[`(1:topn) %>% 
    sum()
}

## test part a
test_answer_a <- solve1a(test1)
expected_a    <- 24000

## solve part a
pp_try(test_answer_a, expected_a, solve1a(input1), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve1b <- solve1a

## test part b
test_answer_b <- solve1b(test1, 3)
expected_b    <- 45000

## solve part b
pp_try(test_answer_b, expected_b, solve1b(input1, 3), day, "b")

