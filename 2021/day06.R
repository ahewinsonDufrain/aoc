library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2021)
day <- 6

input6 %<>% standard_parse_scan()
test6 %<>% standard_parse_scan()

# day 6 utils -------------------------------------------------------------
grow_recursive <- function(fish, days) {
  #print(glue("resolving a fish of value {fish} in {days} days"))
  if (fish >= days) 1
  else {
    sum(sapply(seq(days-fish, 1, by=-7), grow_recursive, fish=9)) + 1
    #grow_recursive(8, days-fish) + 1
  }
}

count_fish <- function(x, days=256, recurred=FALSE) {
  if (!recurred) print(x)
  if (days <= x) return(1)
  v <- seq(x-1, by=-1, length.out=days)
  v[v < 0] <- v[v < 0] %% 7
  generators <- which((v %>% rev() %>% `[`(-1) %>% rev())==0)+1
  to_add <- sum(unlist(sapply(days - generators, count_fish, x=8, recurred=TRUE)))+1
  to_add
}

sapply(c(3,4,3,1,2), count_fish) %>% 
  sum() %>% 
  print()


#sapply(1:30, grow_recursive, fish=6) %>% paste(collapse=",") #%>% sum() %>% print()
stop(98)

# part a ------------------------------------------------------------------
## solve function for part a
solve6a <- function(inp) {
  sum(sapply(inp, grow_recursive, days=80))
}

## test part a
test_answer_a <- solve6a(test6)
expected_a    <- 5934

## solve part a
pp_try(test_answer_a, expected_a, solve6a(input6), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve6b <- function(inp) {
  #sum(sapply(inp, grow_recursive, days=256))
  sum(sapply(inp, function(q) {print(q);grow_recursive(q, days=256)}))
}

## test part b
test_answer_b <- solve6b(test6)
expected_b    <- 26984457539

## solve part b
pp_try(test_answer_b, expected_b, solve6b(input6), day, "b")
