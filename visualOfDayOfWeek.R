pacman::p_load(COVID19, tidyverse, lubridate, recipes, magrittr)

df = covid19(country = "US", start = "2020-03-01")
df$newCases = c(NA, diff(df$confirmed))
df$day = wday(df$date, label = TRUE) %>% factor(ordered = F)

df %<>% mutate_at(seq(11,21), as.character) %>% mutate_at(seq(11,21), as.factor)

windows()
df %>% filter(date < "2020-08-27") %>% 
  ggplot(aes(x = date, y = newCases, color = day, group = day)) +
  geom_line(size = 2) + 
  scale_color_brewer(palette = "Paired", type = "qual") + 
  theme_bw()

prep = recipe(~., data = df) %>% 
  step_dummy(c(day, school_closing), one_hot = F) %>% 
  prep(df, retain = T)

dfWithDummies = juice(prep)

