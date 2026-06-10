library(tidyverse)
library(lubridate)
library(janitor)
library(patchwork)
library(GGally)


# Load in data -------------------
dec_performances <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/dec_performances.csv")

dec_performances <- dec_performances  |>  
  mutate(dob = mdy(dob),
         pos_full = pos_full |> 
           gsub("f.*", "",x = _) |>
           gsub("ce", "", x=  _) |>
           as.numeric(),
         date = mdy(date),
         men_1500_sec = seconds(men_1500) / 60,
         year = year(date),
         age = as.numeric(date - dob) / 365.25) |> 
  mutate(age = ifelse(age > 60, NA, age))



event_map <- c("men_100_score" = "100m", 
               "men_110h_score" = "100m Hurdles", 
               "men_400_score" = "400m", 
               "men_1500_score" = "1500m",
               "men_hj_score" = "High Jump",
               "men_lj_score" = "Long Jump", 
               "men_pv_score" = "Pole Vault", 
               "men_sp_score" = "Shot Put",
               "men_jt_score" = "Javelin Throw", 
               "men_dt_score" = "Discus Throw")

event_map <- event_map |> 
  as.factor() |> 
  fct_relevel(event_map)


dec_performances |> 
  count(competitor) |> 
  nrow()


# All races EDA ---------

# Sample size increases after 2008
dec_performances |> 
  ggplot(aes(x = year)) +
  geom_bar()


# Not a big change in how points are scored
dec_performances |> 
  # filter(pos_full < 4) |> 
  mutate(group = case_when(year <= 2005 ~ "Pre-2006",
                           year <= 2010 ~ "2006-2010",
                           year <= 2015 ~ "2011-2015",
                           .default = "Post 2015")) |> 
  group_by(group) |> 
  select(contains("score"), -world_athletics_event_ranking_score) |> 
  summarise(across(men_100_score:overall_score, ~ sum(.x, na.rm = TRUE))) |> 
  mutate(across(men_100_score:overall_score, ~ .x / overall_score)) |> 
  mutate(group = as.factor(group)) |> 
  select(-overall_score) |> 
  pivot_longer(cols = men_100_score:men_1500_score ,
               names_to = "event",
               values_to = "prop_score") |> 
  ggplot(aes(x = group, y = prop_score, fill = event)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired")


# Winning times in major events haven't changed significantly
dec_performances |> 
  filter(pos_full < 2, world_athletics_event_ranking_score > 1100) |> 
  ggplot(aes(x = date, y = overall_score)) +
  geom_point() +
  geom_smooth()


# No notable relationship between 100 & 1500
dec_performances |> 
  ggplot(aes(x = men_100, y = men_1500)) +
  geom_point(alpha = 0.1)


# Most correlated events - dt & sp, jt & dt, jt & sp, 400 & 100, 100 & LJ, 100 & 110h > .5
dec_performances |> 
  select(contains("score"), -c(world_athletics_event_ranking_score, overall_score)) |> 
  GGally::ggpairs(aes(alpha = 0.1))


# Age curves ------
# How many performances per athlete 
dec_performances |> 
  count(competitor) |> 
  arrange(desc(n))

dec_performances |> 
  add_count(competitor) |> 
  select(competitor, n) |> 
  distinct() |> 
  ggplot(aes(x = n)) +
  geom_bar()


dec_performances |> 
  add_count(competitor) |> 
  select(competitor, n) |> 
  distinct() |> 
  mutate(count = min_rank(desc(n))) |> 
  select(n, count) |> 
  distinct() |> 
  ggplot(aes(x = n, y = count)) +
  geom_col() +
  geom_hline(yintercept = 250, linetype = 2) +
  scale_x_reverse() 

# 299 athletes w/ 10+ performances
dec_performances |> 
  add_count(competitor) |> 
  filter(n > 9) |> 
  distinct(competitor)

# Overall age trend
cols <- c("darkred", "#005AB5")


A <-dec_performances |> 
  add_count(competitor) |> 
  group_by(competitor) |> 
  filter(n > 9) |> 
  mutate(best = max(overall_score),
         rel_score = best - overall_score) |> 
  ggplot(aes(x = age, y = rel_score)) +
  geom_point(alpha = 0.15, col = cols[1]) +
  geom_smooth(col = cols[2]) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(NA, 35)) +
  labs(x = 'Age',
       y = "Points Off Career Best",
       title = "Overall Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = 'bold',
                                  margin = margin(b = -10)))
A
curve <- ggplot_build(A)$data[[2]]
peak <- curve[which.max(curve$y),]
peak$x
peak$y

# A +
#   geom_vline(xintercept = peak$x, linetype = 2)

# A <- A +
  # geom_curve(aes(xend = 26.3, yend = -peak$y, x= 28, y = 600),
  #            curvature = -0.3,
  #            arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  # geom_label(aes(label = "25.98", x = 28, y = -625))

B <- dec_performances |> 
  add_count(competitor) |> 
  group_by(competitor) |> 
  filter(n > 15) |> 
  mutate(across(contains("score"), ~ (max(.x) - .x))) |> 
  ungroup() |> 
  select(contains("score"), age, -c(world_athletics_event_ranking_score, overall_score)) |> 
  pivot_longer(cols = contains("score"),
               names_to = "event",
               values_to = "rel_score") |> 
  mutate(event = fct_relevel(event, names(event_map))) |> 
  filter(rel_score > -500 | event == "overall_score") |>
  ggplot(aes(x = age, y = rel_score))+
  geom_point(alpha = 0.05, color = cols[1]) +
  geom_smooth(color = cols[2], linewidth = 1, se = FALSE) +
  facet_wrap(~ event, 
             nrow = 2,
             labeller = as_labeller(event_map)) +
  coord_cartesian(ylim = c(0, 400)) +
  scale_x_continuous(limits = c(NA, 35)) +
  scale_y_reverse() +
  labs(x = "Age", 
       y = "") +
  theme_minimal() +
  theme(strip.text = element_text(face = 'bold'))

B
curves_event <- ggplot_build(B)$data[[2]] |> as_tibble()

event_peaks <- curves_event |> 
  group_by(PANEL) |> 
  slice_max((y))

curves_event |> 
  group_by(PANEL) |> 
  mutate(y = percent_rank(y)) |> 
  filter(y > 0.9) |> 
  view()
  summarise(peak = x[which.max(y)],
            peak_length = max(x) - min(x))
  slice_max(y)
  

A + B + plot_layout(nrow = 1, widths = c(1,2))








ggsave("sorg/ageCurves.png", height = 1250, width = 3750, units = 'px')



# Where points come from: Winners vs bums --------
dec_performances |> 
  ggplot(aes(x = pos_full)) +
  geom_bar()

quantile(dec_performances$pos_full, .66, na.rm = TRUE)
quantile(dec_performances$pos_full, .66, na.rm = TRUE)

dec_performances |> 
  mutate(pos_group = case_when(pos_full <= 2 ~ "top2",
                               pos_full <= 6 ~ "top6",
                               .default = "outside6")) |>
  mutate(pos_group = fct_relevel(as.factor(pos_group), 
                             "top2", "top6", "outside6")) |> 
  group_by(pos_group) |> 
  select(contains("score"), -world_athletics_event_ranking_score) |> 
  summarise(across(men_100_score:overall_score, ~ sum(.x, na.rm = TRUE))) |> 
  mutate(across(men_100_score:overall_score, ~ .x / overall_score)) |> 
  mutate(group = as.factor(pos_group)) |> 
  select(-overall_score) |> 
  pivot_longer(cols = men_100_score:men_1500_score ,
               names_to = "event",
               values_to = "prop_score") |> 
  ggplot(aes(x = pos_group, y = prop_score, fill = event)) +
  geom_col(position = "dodge")+
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()




# Grouping -------
# throws most correlated with jumps
dec_performances |> 
  rowwise() |> 
  mutate(avg_running_score = mean(men_100_score, men_110h_score, 
                                  men_1500_score, men_400_score),
         avg_jump_score = mean(men_pv_score, men_lj_score, men_hj_score),
         avg_throw_score = mean(men_dt_score, men_jt_score, men_sp_score)) |> 
  ungroup() |> 
  select(contains("avg")) |> 
  GGally::ggpairs(aes(alpha = 0.07))



# PCA -----
pca <- prcomp(dec_performances |> 
         select(contains("score"), -world_athletics_event_ranking_score, - overall_score) |> 
         na.omit(), 
       scale. = TRUE, center = TRUE)

screeplot(pca)

pca$x |> 
  as_tibble() |> 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.1)


biplot(pca)


pca_athlete <- dec_performances |> 
  group_by(competitor) |> 
  mutate(num_races = n()) |> 
  filter(num_races > 10) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE))) |> 
  ungroup() |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score)) |> 
  prcomp(scale. = TRUE, center = TRUE)


biplot(pca_athlete)  
screeplot(pca_athlete)

pca_athlete$x |> 
  as_tibble() |> 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.6)



dec_performances |> 
  group_by(competitor) |> 
  mutate(num_races = n()) |> 
  filter(num_races > 10) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE))) |> 
  ungroup() |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score)) |> 
  rowwise() |> 
  mutate(avg_running_score = mean(men_100_score, men_110h_score, 
                                  men_1500_score, men_400_score),
         avg_jump_score = mean(men_pv_score, men_lj_score, men_hj_score),
         avg_throw_score = mean(men_dt_score, men_jt_score, men_sp_score)) |> 
  ungroup() |> 
  select(contains("avg")) |> 
  GGally::ggpairs(aes(alpha = 0.07))



dec_performances |> 
  group_by(competitor) |> 
  mutate(num_races = n()) |> 
  filter(num_races > 3) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE))) |> 
  select(contains("score"), -c(world_athletics_event_ranking_score, overall_score)) |> 
  GGally::ggpairs(aes(alpha = 0.1))



percentiles <- dec_performances |> 
  group_by(competitor) |> 
  mutate(num_races = n()) |> 
  filter(num_races > 4) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE))) |> 
  ungroup() |> 
  reframe(competitor, across(contains("score"), percent_rank))


top10in2022 <- c("Pierce Lepage", "Damian Warner", "Zachery Ziemek",
                 "Lindon Victor", "Niklas Kaul", "Ayden Owens-Delerme",
                 "Simon Ehammer", "Garrett Scantling", "Maicel Uibo",
                 "Cedric Dubler")

top10in2022 <- fct_relevel(top10in2022, top10in2022)

percentiles |> 
  arrange(desc(overall_score)) |> 
  filter(competitor %in% top10in2022) |> 
  # head(10) |>
  # slice(1:5, (nrow(percentiles) - 4):nrow(percentiles)) |> 
  # slice_sample(n = 10) |>
  select(-c(overall_score, world_athletics_event_ranking_score)) |> 
  pivot_longer(cols = contains("score"),
               names_to = "event",
               values_to = "ptile") |> 
  ggplot(aes(x = event, y = competitor, fill = ptile)) +
  geom_tile() +
  scale_y_discrete(labels = rev(top10in2022)) +
  labs(x = "Event", 
       y= "Athlete",
       title = "Top 10 Decathletes on 10/25/2022",
       fill = "Percentile") +
  coord_equal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


percentiles |> 
  select(contains("score"), -c(world_athletics_event_ranking_score, overall_score)) |> 
  GGally::ggpairs(aes(alpha = 0.01))


# Wind effects -----

# 100 effect > long jump
dec_performances |> 
  group_by(competitor) |> 
  mutate(num_races = n(),
         mean_100 = mean(men_100),
         rel_100 = men_100 - mean_100) |> 
  filter(num_races > 9) |> 
  ggplot(aes(y = rel_100, x = men_100_wind)) +
  geom_point(alpha = 0.25)


dec_performances |> 
  group_by(competitor) |> 
  mutate(num_races = n(),
         mean_lj = mean(men_lj),
         rel_lj = men_lj - mean_lj) |> 
  filter(num_races > 9) |> 
  ggplot(aes(x = men_lj_wind, y = rel_lj)) +
  geom_point(alpha = 0.25)



#
ath_means <- dec_performances |> 
  group_by(competitor) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE)),
            num_races = n()) |> 
  filter(num_races > 9) |> 
  ungroup()


std_scores <- ath_means |> 
  select(-c(competitor, world_athletics_event_ranking_score, num_races)) |> 
  mutate(across(everything(), ~ .x / overall_score)) |> 
  scale() |> 
  as_tibble() |> 
  select(-overall_score)


std_prop_kmean <- std_scores |> 
  kmeans(centers = 2, nstart = 30)


ath_means |> 
  mutate(cluster = std_prop_kmean$cluster |> 
           as.factor()) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE)),
            size = n(),
            .by = cluster) |> 
  mutate(across(contains("score"), ~ .x / overall_score)) |>
  select(-c(overall_score, world_athletics_event_ranking_score)) |> 
  pivot_longer(cols = contains("score"), names_to = "event", values_to = "score") |> 
  ggplot(aes(y = event, x = score, color = cluster)) +
  geom_point() +
  geom_segment(aes(x = 0.08, xend = score, y = event, yend = event)) +
  facet_wrap(~ cluster)


ath_means |> 
  mutate(cluster = std_prop_kmean$cluster |> 
           as.factor()) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE)),
            size = n(),
            .by = cluster) |> 
  mutate(across(contains("score"), ~ .x / overall_score)) |>
  select(-c(overall_score, world_athletics_event_ranking_score)) |> 
  pivot_longer(cols = contains("score"), names_to = "event", values_to = "score") |> 
  ggplot(aes(x = event, y = score, fill = cluster)) +
  geom_col() +
  coord_polar() +
  facet_wrap(~cluster)



library(ggradar)

ath_means |> 
  mutate(cluster = std_prop_kmean$cluster |> 
           as.factor()) |> 
  summarise(across(contains("score"), ~ mean(.x, na.rm = TRUE)),
            size = n(),
            .by = cluster) |> 
  mutate(across(contains("score"), ~ .x / overall_score)) |>
  mutate(across(contains("score"), ~ scales::rescale(.x, to = c(0, 1),
                                                     from = c(0.08, 0.12)))) |> 
  select(-c(overall_score, world_athletics_event_ranking_score, size)) |> 
  ggradar(values.radar = c("8%", "10%", "12%"),
          legend.position = "bottom") 



ath_means |> 
  mutate(cluster = std_prop_kmean$cluster |> 
           as.factor(),
         across(contains("score"), ~ .x / overall_score)) |> 
  ggparcoord(columns = 3:12, groupColumn = 15, order = 'anyClass',
             alphaLines = 0.3, scale = "globalminmax") +
  facet_wrap(~ cluster) +
  scale_color_viridis_d() +
  theme_minimal()

library(factoextra)

# 2 clusters
ath_means |> 
  mutate(across(contains("score"), percent_rank)) |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score)) |> 
  scale() |> 
  as_tibble() |> 
  fviz_nbclust(kmeans)


ath_means |> 
  mutate(across(contains("score"), percent_rank)) |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score)) |> 
  scale() |> 
  as_tibble() |> 
  fviz_nbclust(kmeans, method = 'wss')

ath_means |> 
  mutate(across(contains("score"), percent_rank)) |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score)) |> 
  scale() |> 
  as_tibble() |> 
  fviz_nbclust(kmeans)

kmean_ptile2 <- ath_means |> 
  mutate(across(contains("score"), percent_rank)) |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score)) |> 
  scale() |> 
  kmeans(centers = 2, nstart = 30)


ath_means |> 
  mutate(across(contains("score"), percent_rank)) |> 
  mutate(cluster = std_prop_kmean$cluster |> 
           as.factor()) |> 
  select(-c(competitor, world_athletics_event_ranking_score, overall_score, num_races)) |> 
  select(cluster, names(event_map)) |> 
  ggradar(fill = 1, fill.alpha = 0.1,
          axis.labels = event_map) +
  guides(fill = 'none') +
  theme(legend.position = 'bottom')


ath_means |> 
  mutate(across(contains("score"), percent_rank)) |> 
  mutate(cluster = std_prop_kmean$cluster |> 
           as.factor()) |> 
  group_by(cluster) |> 
  ggplot(aes(x = cluster, y = overall_score)) +
  ggbeeswarm::geom_beeswarm() +
  theme_minimal()
