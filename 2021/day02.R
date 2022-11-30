library(tidyverse)
library(magrittr)

source("utils/inputs.R")
source("utils/printing.R")
readInputs(2021)
day <- 2

input2 %<>% standard_parse_text()
test2 %<>% standard_parse_text()

solve2a <- function(inp) {
  sub_f <- inp %>% str_extract_all(pattern = "forward .*", simplify = TRUE)
  sub_u <- inp %>% str_extract_all(pattern = "up .*", simplify = TRUE)
  sub_d <- inp %>% str_extract_all(pattern = "down .*", simplify = TRUE)

  horizontal <- sub_f %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric() %>% replace_na(0) %>% sum()
  down       <- sub_d %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric() %>% replace_na(0) %>% sum()
  up         <- sub_u %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric() %>% replace_na(0) %>% sum()
  vertical   <- down - up

  horizontal * vertical
}

## test part a
test_answer_a <- solve2a(test2)
expected_a    <- 150

## solve part a
pp_try(test_answer_a, expected_a, solve2a(input2), day, "a")

solve2b <- function(inp) {
  sub_f <- inp %>% str_extract_all(pattern = "forward .*", simplify = TRUE)
  sub_u <- inp %>% str_extract_all(pattern = "up .*", simplify = TRUE)
  sub_d <- inp %>% str_extract_all(pattern = "down .*", simplify = TRUE)
  
  move <- sub_f %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric() %>% replace_na(0)
  down <- sub_d %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric() %>% replace_na(0)
  up   <- sub_u %>% str_extract_all("[0-9]+", simplify = TRUE) %>% as.numeric() %>% replace_na(0)
  aim  <- down - up
  horizontal <- sum(move)
  vertical   <- sum(move * cumsum(aim))
  
  horizontal * vertical
}

solve2b(test2) %>% print()

## test part b
test_answer_b <- solve2b(test2)
expected_b    <- 900

## solve part b
pp_try(test_answer_b, expected_b, solve2b(input2), day, "b")

