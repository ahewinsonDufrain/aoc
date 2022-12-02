library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2021)
day <- 5

input5 %<>% standard_parse_text()
test5 %<>% standard_parse_text()

# day 5 utilities ---------------------------------------------------------
parse_hydros <- function(x) {
  x %>% 
    str_split(" -> ") %>% 
    lapply(function(q) unlist(str_split(q, ","))) %>% 
    lapply(as.numeric)
}
is_diagonal <- function(x) {
  (x[1] != x[3]) & (x[2] != x[4])
}
draw_line <- function(m, coords) {
  runs <- case_when(
    (coords[1] != coords[3]) & (coords[2] != coords[4]) ~ list(xseq = coords[1]:coords[3], yseq = coords[2]:coords[4]),
    (coords[1] != coords[3])                            ~ list(xseq = coords[1]:coords[3], yseq = rep(coords[2], length(coords[1]:coords[3]))),
    (coords[2] != coords[4])                            ~ list(xseq = rep(coords[1], length(coords[2]:coords[4])), yseq = coords[2]:coords[4])
  )
  for (q in seq_along(runs[[1]])) {
    m[runs[[2]][q], runs[[1]][q]] <- m[runs[[2]][q], runs[[1]][q]] + 1
  }
  m
}

# part a ------------------------------------------------------------------
## solve function for part a
solve5a <- function(inp, discard_diagonals = TRUE) {
  hydros <- inp %>% parse_hydros()
  if (discard_diagonals) hydros <- hydros[!sapply(hydros, is_diagonal)]
  minx <- sapply(hydros, function(q) q[c(1, 3)]) %>% min()
  miny <- sapply(hydros, function(q) q[c(2, 4)]) %>% min()
  maxx <- sapply(hydros, function(q) q[c(1, 3)]) %>% max()
  maxy <- sapply(hydros, function(q) q[c(2, 4)]) %>% max()
  
  m <- matrix(0, nrow = 1+maxy, ncol = 1+maxx)
  
  for (linje in hydros) {
    m <- draw_line(m, linje+1)
  }
  sum(m > 1)
}

## test part a
test_answer_a <- solve5a(test5)
expected_a    <- 5

## solve part a
pp_try(test_answer_a, expected_a, solve5a(input5), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve5b <- function(inp) solve5a(inp, discard_diagonals = FALSE)

## test part b
test_answer_b <- solve5b(test5)
expected_b    <- 12

## solve part b
pp_try(test_answer_b, expected_b, solve5b(input5), day, "b")

