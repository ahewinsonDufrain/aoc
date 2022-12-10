library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 9

input9 %<>% standard_parse_text()
test9 %<>% standard_parse_text()

# day 9 utils -------------------------------------------------------------
move_matrix <- function(x) {
  d <- str_extract(x, "[RLUD]")
  n <- str_extract(x, "\\d") %>% as.integer()
  m <- rep(d, n)
  hx <- case_when(m=="R" ~ 1, m=="L" ~ -1, TRUE ~ 0) %>% cumsum()
  hy <- case_when(m=="U" ~ 1, m=="D" ~ -1, TRUE ~ 0) %>% cumsum()
  cbind(m=m, hx=hx, hy=hy, tx=rep(0, length(m)), ty=rep(0, length(m)))
}
move_matrix(input9)[2891:2900, ]

# part a ------------------------------------------------------------------
## solve function for part a
solve9a <- function(inp) {
  mm <- move_matrix(inp) #[1:25, ]
  #return(mm)
  
  for (i in 2:nrow(mm)) {
    dx = mm[i, "hx"] - mm[i-1, "tx"]
    dy = mm[i, "hy"] - mm[i-1, "ty"]
    shifts <- case_when(
      dx %in% -1:1 & dy %in% -1:1 ~ c(mm[i-1, "tx"], mm[i-1, "ty"]),
      TRUE ~ c(mm[i-1, "tx"] + sign(dx), mm[i-1, "ty"] + sign(dy))
    )
    mm[i, "tx"] <- shifts[1]
    mm[i, "ty"] <- shifts[2]
  }
  
  mm[, c("tx", "ty")] #%>% unique() %>% nrow()

}

## test part a
test_answer_a <- solve9a(test9)
expected_a    <- 13

#test_answer_a %>% print()
# testm <- matrix(0, 5, 5)
# for (q in 1:nrow(test_answer_a)) {
#   #print(q)
#   testm[test_answer_a[q,2]+1, test_answer_a[q,1]+1] <- 1
# }
# testm <- apply(testm, 2, rev)
# print(testm)
# stop(98)

## solve part a
pp_try(test_answer_a, expected_a, solve9a(input9), day, "a") ## 3024 is too low

# part b ------------------------------------------------------------------
## solve function for part b
solve9b <- function(inp) {
  98
}

## test part b
test_answer_b <- solve9b(test9)
expected_b    <- -1

## solve part b
pp_try(test_answer_b, expected_b, solve9b(input9), day, "b")
