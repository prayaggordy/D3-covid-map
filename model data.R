library(tidyverse)

statewide <- read.csv("data/md_statewide.csv")
recovered <- read.csv("data/md_isolation.csv") %>% 
  mutate(recovered = count - lag(count, default = first(count)))

statewide <- full_join(statewide, recovered) %>% 
  select(!"count")