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
               "men_110h_score" = "110m Hurdles", 
               "men_400_score" = "400m", 
               "men_1500_score" = "1500m",
               "men_hj_score" = "High Jump",
               "men_lj_score" = "Long Jump", 
               "men_pv_score" = "Pole Vault", 
               "men_sp_score" = "Shot Put",
               "men_jt_score" = "Javelin Throw", 
               "men_dt_score" = "Discus Throw")

# event_map <- event_map |>
#   as.factor() |>
#   fct_relevel(event_map)




# Age curves ------

# 299 athletes w/ 10+ performances
dec_performances |> 
  add_count(competitor) |> 
  filter(n > 9) |> 
  distinct(competitor)

# Overall age trend
cols <- c("darkred", "#005AB5")


# Big plot (overall trend) --------
A <- dec_performances |> 
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


# Get curve data A ------
curve <- ggplot_build(A)$data[[2]]
peak <- curve[which.max(curve$y),]
peak$x
peak$y

A <- A +
  geom_vline(xintercept = peak$x, linetype = 2) +
  geom_curve(aes(xend = 26.3, yend = -peak$y, x= 28, y = 600),
           curvature = -0.3,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("label", label = "25.98", x = 28, y = 625)



# Event specific curves -----

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


# Get event curve data ------
curves_event <- ggplot_build(B)$data[[2]] |> as_tibble()

event_peaks <- curves_event |> 
  group_by(PANEL) |> 
  slice_max((y))


event_peaks <- event_peaks |> 
  ungroup() |> 
  mutate(event = names(event_map)) |> 
  select(x, y, event) |> 
  rename(peak = x) |> 
  mutate(event = fct_relevel(event, names(event_map)))


B <- B +
  geom_vline(data = event_peaks, aes(xintercept = peak), linetype = 2) +
  geom_curve(data = event_peaks,
             aes(xend = peak, yend = -y, x= peak + 3, y = -(y - 100)),
             curvature = -0.3,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_label(data = event_peaks, 
             aes(label = round(peak, 2), x = peak + 5, y = -(y - 100)))






A + B + plot_layout(nrow = 1, widths = c(1,2))





ggsave("sorg/ageCurves.png", height = 1250, width = 3750, units = 'px')

