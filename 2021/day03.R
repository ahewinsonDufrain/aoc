library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2021)
day <- 3

input3 %<>% standard_parse_text()
test3 %<>% standard_parse_text()

# part a ------------------------------------------------------------------
## solve function for part a
solve3a <- function(inp) {
  n <- length(inp)
  gamma_2 <- inp %>% 
    str_split("") %>% 
    unlist() %>% 
    matrix(ncol = n) %>% 
    apply(1, modal)
  epsilon_2 <- gamma_2 %>% as.numeric() %>% `-`(1, .)
  
  gamma <- gamma_2 %>% paste(collapse="") %>% strtoi(base = 2)
  epsilon <- epsilon_2 %>% paste(collapse="") %>% strtoi(base = 2)
  
  gamma * epsilon
}
solve3a(test3) %>% print()

## test part a
test_answer_a <- solve3a(test3)
expected_a    <- 198

## solve part a
pp_try(test_answer_a, expected_a, solve3a(input3), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve3b <- function(inp) {
  n <- length(inp)
  ge_matrix <- inp %>% 
    str_split("") %>% 
    unlist() %>% 
    matrix(ncol = n)
  
  g_matrix <- ge_matrix
  for (q in 1:nrow(ge_matrix)) {
    if (ncol(g_matrix)==1) break
    gamma_2 <- g_matrix %>% apply(1, modal)
    g_matrix <- g_matrix[, g_matrix[q, ] == gamma_2[q], drop=FALSE]
  }
  oxygen <- g_matrix %>% apply(2, function(q) q %>% paste(collapse="")) %>% strtoi(base = 2)

  e_matrix <- ge_matrix
  for (q in 1:nrow(ge_matrix)) {
    if (ncol(e_matrix)==1) break
    epsilon_2 <- e_matrix %>% apply(1, modal) %>% as.numeric() %>% `-`(1, .) %>% as.character()
    e_matrix <- e_matrix[, e_matrix[q, ] == epsilon_2[q], drop=FALSE]
  }
  co2 <- e_matrix %>% apply(2, function(q) q %>% paste(collapse="")) %>% strtoi(base = 2)
  
  oxygen * co2
}

## test part b
test_answer_b <- solve3b(test3)
expected_b    <- 230

## solve part b
pp_try(test_answer_b, expected_b, solve3b(input3), day, "b")
