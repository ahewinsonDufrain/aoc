library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2021)
day <- 4

input4 %<>% split_double_line_break()
test4 %<>% split_double_line_break()

# day 4 utilities ---------------------------------------------------------
score_grid <- function(grid, draws) {
  for (qq in seq_along(draws)) {
    q <- draws[qq]
    grid[grid==q] <- NA
    NA_grid <- is.na(grid)
    horizontal_NAs <- apply(NA_grid, 1, sum)
    vertical_NAs <- apply(NA_grid, 2, sum)
    if (any(c(horizontal_NAs, vertical_NAs) == 5)) {
      return(list(n=qq, score=sum(grid, na.rm = TRUE) * q))
    }
  }
}

# part a ------------------------------------------------------------------
## solve function for part a
solve4a <- function(inp) {
  drawn <- inp[[1]] %>% 
    str_split(",") %>% 
    unlist() %>% 
    as.numeric()
  
  grids <- inp[-1] %>% remove_empty_strings()
  grids <- grids %>% 
    lapply(str_trim) %>% 
    lapply(remove_empty_strings) %>% 
    lapply(str_split, " +") %>% 
    lapply(unlist) %>% 
    lapply(as.numeric) %>% 
    lapply(function(q) t(matrix(q, 5)))

  scored_grids <- lapply(grids, score_grid, drawn)
  ns <- sapply(scored_grids, function(q) q$n)
  scores <- sapply(scored_grids, function(q) q$score)
  scores[which.min(ns)]  
}

## test part a
test_answer_a <- solve4a(test4)
expected_a    <- 4512

## solve part a
pp_try(test_answer_a, expected_a, solve4a(input4), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve4b <- function(inp) {
  drawn <- inp[[1]] %>% 
    str_split(",") %>% 
    unlist() %>% 
    as.numeric()
  
  grids <- inp[-1] %>% remove_empty_strings()
  grids <- grids %>% 
    lapply(str_trim) %>% 
    lapply(remove_empty_strings) %>% 
    lapply(str_split, " +") %>% 
    lapply(unlist) %>% 
    lapply(as.numeric) %>% 
    lapply(function(q) t(matrix(q, 5)))
  
  scored_grids <- lapply(grids, score_grid, drawn)
  ns <- sapply(scored_grids, function(q) q$n)
  scores <- sapply(scored_grids, function(q) q$score)
  scores[which.max(ns)]  
}

## test part b
test_answer_b <- solve4b(test4)
expected_b    <- 1924

## solve part b
pp_try(test_answer_b, expected_b, solve4b(input4), day, "b")

