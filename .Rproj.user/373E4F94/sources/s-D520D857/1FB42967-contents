library(tidyverse)

data <- read_lines("day2/input.txt")

################################################################################
# part 1
data %>%
  as_tibble() %>%
  separate(value, sep = ': ', into = c("condition", "password")) %>%
  separate(condition, sep = " ", into = c("length", "letter")) %>%
  separate(length, sep = "-", into = c("min", "max")) %>%
  mutate(count_letter = str_count(password, letter)) %>%
  mutate(min = as.numeric(min)) %>%
  mutate(max = as.numeric(max)) %>%
  mutate(validity_min = if_else(count_letter >= min, 1,0)) %>%
  mutate(validity_max = if_else(count_letter <= max, 1,0)) %>%
  mutate(t_validity = validity_max*validity_min) %>%
  filter(t_validity == 1) %>%
  nrow()


################################################################################
# part 2

dd <- data %>%
  as_tibble() %>%
  separate(value, sep = ': ', into = c("condition", "password")) %>%
  separate(condition, sep = " ", into = c("length", "letter")) %>%
  separate(length, sep = "-", into = c("pos_1", "pos_2")) %>%
  mutate(locations = str_locate_all(password, letter)) %>%
  mutate(locations = map(locations, function(x){x[,1]})) 

tmp_1 <- vector(length = 1000)
tmp_2 <- vector(length = 1000)

for(i in 1:1000){
    tmp_1[i] <- dd$pos_1[i] %in% as.matrix(unlist(dd$locations[i]))
    tmp_2[i] <- dd$pos_2[i] %in% as.matrix(unlist(dd$locations[i]))
}

dd %>%
  mutate(val_1 = tmp_1) %>%
  mutate(val_2 = tmp_2) %>%
  filter(val_1 != val_2)

# I really struggled in this second part, not sure how am gonna cope in the next challenges lol



