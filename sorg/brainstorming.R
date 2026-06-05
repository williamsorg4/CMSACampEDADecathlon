# test file

library(tidyverse)
library(lubridate)

dec_performances <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/dec_performances.csv")

dec_performances <- dec_performances %>% 
  mutate(dob = mdy(dob),
         pos_full = pos_full %>% 
           gsub("f.*", "", .) %>% 
           gsub("ce", "", .) %>% 
           as.numeric(),
         date = mdy(date),
         men_1500_sec = seconds(men_1500) / 60,
         year = year(date),
         age = as.numeric(date - dob) / 365.25) %>% 
  mutate(age = ifelse(age > 60, NA, age))


# EDA ---------

# Sample size increases after 2008
dec_performances %>% 
  ggplot(aes(x = year)) +
  geom_bar()


# Not a big change in how points are scored
dec_performances %>% 
  # filter(pos_full < 4) %>% 
  mutate(group = case_when(year <= 2005 ~ "G1",
                           year <= 2010 ~ "G2",
                           year <= 2015 ~ "G3",
                           .default = "G4")) %>% 
  group_by(group) %>% 
  select(contains("score"), -world_athletics_event_ranking_score) %>% 
  summarise(across(men_100_score:overall_score, ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(across(men_100_score:overall_score, ~ .x / overall_score)) %>% 
  mutate(group = as.factor(group)) %>% 
  select(-overall_score) %>% 
  pivot_longer(cols = men_100_score:men_1500_score ,
               names_to = "event",
               values_to = "prop_score") %>% 
  ggplot(aes(x = group, y = prop_score, fill = event)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired")


dec_performances %>% 
  filter(pos_full < 2, world_athletics_event_ranking_score > 1100) %>% 
  ggplot(aes(x = date, y = overall_score)) +
  geom_point() +
  geom_smooth()


dec_performances %>% 
  ggplot(aes(x = men_100, y = men_1500)) +
  geom_point(alpha = 0.1)


dec_performances %>% 
  select(contains("score"), -c(world_athletics_event_ranking_score, overall_score)) %>% 
  GGally::ggpairs(alpha = 0.1)

dec_performances %>% 
  count(competitor) %>% 
  arrange(desc(n))

dec_performances %>% 
  add_count(competitor) %>% 
  select(competitor, n) %>% 
  distinct() %>% 
  ggplot(aes(x = n)) +
  geom_bar()


dec_performances %>% 
  add_count(competitor) %>% 
  group_by(competitor) %>% 
  filter(n > 10) %>% 
  mutate(best = max(overall_score),
         rel_score = overall_score - best) %>% 
  ggplot(aes(x = age, y = rel_score)) +
  geom_point(alpha = 0.25) +
  geom_smooth()



dec_performances %>% 
  add_count(competitor) %>% 
  group_by(competitor) %>% 
  filter(n > 15) %>% 
  mutate(across(contains("score"), ~ (.x - max(.x)))) %>% 
  ungroup() %>% 
  select(contains("score"), age, -c(world_athletics_event_ranking_score, overall_score)) %>% 
  pivot_longer(cols = contains("score"),
               names_to = "event",
               values_to = "rel_score") %>% 
  filter(rel_score > -500 | event == "overall_score") %>%
  ggplot(aes(x = age, y = rel_score))+
  geom_point(alpha = 0.05) +
  geom_smooth() +
  facet_wrap(~ event, 
             # scales = 'free_y', 
             nrow = 2) +
  labs(x = "Age", 
       y = "Score Difference from Career Best") +
  theme_minimal()
