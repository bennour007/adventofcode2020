# day1

library(tidyverse)

data <- read_lines("day1/input.txt") %>% as.numeric()

# part 1
################################################################################

lista_1 <- map(data,function(x){
  tmp <- rep(x, length(data))
  tmpp <- tmp + data
  return(tmpp)
})


search <- lista %>%
  map(., function(x){
    if_else(2020 %in% x, "its here", "nada")
  })



which(search == "its here")


data[82] * data[93]


# part 2
################################################################################

v1 <- data
v2 <- data
v3 <- data

combos <- crossing(v1, v2, v3) %>%
  mutate(sum_3 = v1 + v2 + v3)
  
combos %>%
  filter(sum_3 == 2020) %>% 
  .[1,] %>%
  mutate(mul = v1*v2*v3)


