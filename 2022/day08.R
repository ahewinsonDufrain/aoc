library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 8

input8 %<>% standard_parse_text()
test8 %<>% standard_parse_text()

# day 8 utils -------------------------------------------------------------
parse_copse <- function(x) {
  x %>% 
    str_split("") %>% 
    unlist() %>% 
    as.integer() %>% 
    matrix(length(x)) %>% 
    t()
}
is_highest <- function(x) {
  if (length(x) == 1) TRUE
  else all(x[1] > x[-1])
}
check_view <- function(x) {
  if (length(x) == 1) return(0)
  for (i in 2:length(x)) {
    if (x[i] >= x[1]) return(i-1)
  }
  i-1
}

# part a ------------------------------------------------------------------
## solve function for part a
solve8a <- function(inp) {
  copse <- parse_copse(inp)
  left_highs <- apply(copse, 1, function(q) sapply(seq_along(q), function(r) {
    is_highest(rev(q[1:r]))
  })) %>% t()
  right_highs <- apply(copse, 1, function(q) sapply(seq_along(q), function(r) {
    is_highest(rev(q)[r:1])
  }) %>% rev()) %>% t()
  top_highs <- apply(copse, 2, function(q) sapply(seq_along(q), function(r) {
    is_highest(rev(q[1:r]))
  })) #%>% t()
  bottom_highs <- apply(copse, 2, function(q) sapply(seq_along(q), function(r) {
    is_highest(rev(q)[r:1])
  }) %>% rev()) #%>% t()
  sum(left_highs | right_highs | top_highs | bottom_highs)
}

## test part a
test_answer_a <- solve8a(test8)

expected_a    <- 21

## solve part a
pp_try(test_answer_a, expected_a, solve8a(input8), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve8b <- function(inp) {
  copse <- parse_copse(inp)
  views <- copse * 0
  for (r in 1:nrow(copse)) {
    for (c in 1:ncol(copse)) {
      right_view <- copse[r, c:ncol(copse)]
      left_view <- copse[r, 1:c] %>% rev()
      down_view <- copse[r:nrow(copse), c]
      up_view <- copse[1:r, c] %>% rev()
      views[r, c] <- check_view(right_view) * check_view(left_view) * check_view(down_view) * check_view(up_view)
    }
  }
  views %>% max()
}

## test part b
test_answer_b <- solve8b(test8)
expected_b    <- 8

## solve part b
pp_try(test_answer_b, expected_b, solve8b(input8), day, "b")
