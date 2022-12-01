library(magrittr)

modal <- function(x) {
  x <- x %>% table() %>% sort(decreasing = TRUE) #%>% names()
  x <- x[x==x[1]]
  names(x) %>% sort(decreasing = TRUE) %>% `[`(1)
}
