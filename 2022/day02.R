library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 2

input2 %<>% standard_parse_text()
test2 %<>% standard_parse_text()

# day 2 utils -------------------------------------------------------------
rps_resolve <- function(o, m) {
  case_when(o=="A" & m=="X" ~ 1,
            o=="B" & m=="X" ~ 0,
            o=="C" & m=="X" ~ 2,
            o=="A" & m=="Y" ~ 2,
            o=="B" & m=="Y" ~ 1,
            o=="C" & m=="Y" ~ 0,
            o=="A" & m=="Z" ~ 0,
            o=="B" & m=="Z" ~ 2,
            o=="C" & m=="Z" ~ 1) * 3
}
rps_choose <- function(o, m) {
  case_when(o=="A" & m=="X" ~ 3,
            o=="B" & m=="X" ~ 1,
            o=="C" & m=="X" ~ 2,
            o=="A" & m=="Y" ~ 1,
            o=="B" & m=="Y" ~ 2,
            o=="C" & m=="Y" ~ 3,
            o=="A" & m=="Z" ~ 2,
            o=="B" & m=="Z" ~ 3,
            o=="C" & m=="Z" ~ 1)
}

# part a ------------------------------------------------------------------
## solve function for part a
solve2a <- function(inp) {
  inp %<>% str_split(" ")
  sapply(inp, function(q) {
    choice_score <- case_when(q[2] == "X" ~ 1 + rps_resolve(q[1], q[2]),
                              q[2] == "Y" ~ 2 + rps_resolve(q[1], q[2]),
                              q[2] == "Z" ~ 3 + rps_resolve(q[1], q[2]))
              
  }) %>% sum()
}

## test part a
test_answer_a <- solve2a(test2)
expected_a    <- 15

## solve part a
pp_try(test_answer_a, expected_a, solve2a(input2), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve2b <- function(inp) {
  inp %<>% str_split(" ")
  sapply(inp, function(q) {
    choice_score <- case_when(q[2] == "X" ~ 0 + rps_choose(q[1], q[2]),
                              q[2] == "Y" ~ 3 + rps_choose(q[1], q[2]),
                              q[2] == "Z" ~ 6 + rps_choose(q[1], q[2]))
    
  }) %>% sum()
}

## test part b
test_answer_b <- solve2b(test2)
expected_b    <- 12

## solve part b
pp_try(test_answer_b, expected_b, solve2b(input2), day, "b")

