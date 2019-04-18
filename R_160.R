library(ggplot2)
install.packages("ggplot2")
library(dplyr)
midwest<- as.data.frame(ggplot2::midwest)
midwest
midwest_a <- midwest %>% mutate(child = 100 - (popadults / poptotal * 100))
midwest_a
View(midwest_a)
midwest_a %>% arrange(desc(child)) %>%  select(county, child) %>% head(5)
midwest_a <- midwest_a %>% 
  mutate(grade = ifelse(child >= 40, "large",
                        ifelse(child >= 30, "middle", "small")))
midwest_a
table(midwest_a$grade)
midwest_a %>% 
  mutate(ratio_asian = (popasian / poptotal)* 100) %>% 
  arrange(ratio_asian) %>% 
  select(state, county, ratio_asian) %>% 
  head(10)