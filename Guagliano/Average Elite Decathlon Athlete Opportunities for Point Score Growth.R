## Average Elite Decathlon Athlete Opportunities for Point Score Growth
## June 9th 2026
## By Chloe Guagliano

# reading in the data
library(tidyverse)
dec_performances <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/dec_performances.csv")

# converting 1500m run time object into a seconds value 
library(lubridate)
dec_performances = dec_performances |>
  mutate(
    men_1500 = men_1500 |> 
      gsub(pattern = ":(?=[0-9]+$)", replacement = ".", perl = TRUE) |> 
      ms() |> 
      as.numeric()  
  )

# collecting performance score columns to average out
# (to attain average elite decathlon athlete)
colEndings = c('score','wind')
averageScores = dec_performances |>
  select(starts_with('men_')) |>
  select(! ends_with(colEndings)) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

columnNames = names(averageScores)

# lists with column names by event grouping (running vs non-running events)
runningCols = c('men_100','men_400', 'men_110h', 'men_1500')
nonRunningCols = columnNames[!columnNames %in% runningCols]

# mutating across the averageScores tbl to calculate the marginal increase
# and make those new columns (non-running cols)
averageScores = averageScores |>
  mutate(across(all_of(nonRunningCols), ~(.x/100)+.x, .names = '{.col}_marginalInc'))

# mutating across the runningCols vector to calculate the marginal decrease
# (because decrease is an improvement for running cols) and make new columns
averageScores = averageScores |>
  mutate(across(all_of(runningCols), ~ .x -(.x/100), .names = '{.col}_marginalDec'))

# decathlon point calculation by performance formulas by event
men_100_func = function(p) 25.4347*((18-p)^1.81)
men_400_func = function(p) 1.53775*((82-p)^1.81)
hurdles_110_func = function(p) 5.74352*((28.5-p)^1.92)
men_1500_func = function(p) 0.03768*((480-p)^1.85)
longjump_func = function(p) 0.14354*(((p*100)-220)^1.4)
shotput_func = function(p) 51.39*((p-1.5)^1.05)
highjump_func = function(p) 0.8465*(((p*100)-75)^1.42)
discus_func = function(p) 12.91*((p-4)^1.1)
polevault_func = function(p) 0.2797*(((p*100)-100)^1.35)
javelin_func = function(p) 10.14*((p-7)^1.08)

# using the decathlon point scoring functions to convert all the marginal
# increases into new scores
averageScores = averageScores |>
  # pointScores for the 1% increases
  mutate(men_100_pointInc = men_100_func(men_100_marginalDec)) |>
  mutate(men_400_pointInc = men_400_func(men_400_marginalDec)) |>
  mutate(men_110h_pointInc = hurdles_110_func(men_110h_marginalDec)) |>
  mutate(men_1500_pointInc = men_1500_func(men_1500_marginalDec)) |>
  mutate(men_lj_pointInc = longjump_func(men_lj_marginalInc)) |>
  mutate(men_sp_pointInc = shotput_func(men_sp_marginalInc)) |>
  mutate(men_hj_pointInc = highjump_func(men_hj_marginalInc)) |>
  mutate(men_dt_pointInc = discus_func(men_dt_marginalInc)) |>
  mutate(men_pv_pointInc = polevault_func(men_pv_marginalInc)) |>
  mutate(men_jt_pointInc = javelin_func(men_jt_marginalInc)) |>
  
  # pointScores for the initial averaged starting scores for hypothetical athlete
  mutate(men_100_score = men_100_func(men_100)) |>
  mutate(men_400_score = men_400_func(men_400))|>
  mutate(men_110h_score = hurdles_110_func(men_110h)) |>
  mutate(men_1500_score = men_1500_func(men_1500)) |>
  mutate(men_lj_score = longjump_func(men_lj)) |>
  mutate(men_sp_score = shotput_func(men_sp)) |>
  mutate(men_hj_score = highjump_func(men_hj)) |>
  mutate(men_dt_score = discus_func(men_dt)) |>
  mutate(men_pv_score = polevault_func(men_pv)) |>
  mutate(men_jt_score = javelin_func(men_jt))

# calculating difference b/n improved point score and initial point score
unneededColSuffixes = c('marginalInc','marginalDec','pointInc','score')
averageScores = averageScores |>
  mutate(men_100_diff = men_100_pointInc - men_100_score,
         men_400_diff = men_400_pointInc - men_400_score,
         men_110h_diff = men_110h_pointInc - men_110h_score,
         men_1500_diff = men_1500_pointInc - men_1500_score,
         men_lj_diff = men_lj_pointInc - men_lj_score,
         men_sp_diff = men_sp_pointInc - men_sp_score,
         men_hj_diff = men_hj_pointInc - men_hj_score,
         men_dt_diff = men_dt_pointInc - men_dt_score,
         men_pv_diff = men_pv_pointInc - men_pv_score,
         men_jt_diff = men_jt_pointInc - men_jt_score) |>
  select(! ends_with(unneededColSuffixes))

# using pivot longer to organize data for graphing (event and roi)
roi_long = averageScores |>
  select(ends_with('diff')) |>
  pivot_longer(
    cols = everything(),
    names_to = "event",
    values_to = "roi"
  ) |>
  mutate(
    event = recode(event,
                   men_100_diff = '100m Run',
                   men_400_diff = '400m Run',
                   men_110h_diff = '110m Hurdles',
                   men_1500_diff = '1500m Run',
                   men_lj_diff = 'Long Jump',
                   men_sp_diff = 'Shot Put',
                   men_hj_diff = 'High Jump',
                   men_dt_diff = 'Discus Throw',
                   men_pv_diff = 'Pole Vault',
                   men_jt_diff = 'Javelin Throw')
  ) |>
  arrange(desc(roi))

# pivot longer for the actual performances
# using pivot longer to organize data for graphing (event and roi)
performances_long = averageScores |>
  select(!ends_with('diff')) |>
  pivot_longer(
    cols = everything(),
    names_to = "event",
    values_to = "performance"
  ) |>
  mutate(
    event = recode(event,
                   men_100 = '100m Run',
                   men_400 = '400m Run',
                   men_110h = '110m Hurdles',
                   men_1500 = '1500m Run',
                   men_lj = 'Long Jump',
                   men_sp = 'Shot Put',
                   men_hj = 'High Jump',
                   men_dt = 'Discus Throw',
                   men_pv = 'Pole Vault',
                   men_jt = 'Javelin Throw')
  )

# joining bestPerformances to the existing roi_long tbl
roi_long = left_join(roi_long,performances_long, by = 'event')


# pulling max/min info (depending on event) and joining them to existing table;
# also on the event column
runningEvents = c('men_100','men_400','men_110h','men_1500')
fieldEvents = c('men_lj','men_sp','men_hj','men_dt','men_pv','men_jt')

bestRunningPerformances = dec_performances |>
  select(runningEvents) |>
  summarise(across(where(is.numeric), min, na.rm = TRUE)) |>
  pivot_longer(
    cols = everything(),
    names_to = "event",
    values_to = "bestPerformance"
  ) |>
  mutate(
    event = recode(event,
                   men_100 = '100m Run',
                   men_400 = '400m Run',
                   men_110h = '110m Hurdles',
                   men_1500 = '1500m Run',
                   men_lj = 'Long Jump',
                   men_sp = 'Shot Put',
                   men_hj = 'High Jump',
                   men_dt = 'Discus Throw',
                   men_pv = 'Pole Vault',
                   men_jt = 'Javelin Throw')
  )

bestFieldPerformances = dec_performances |>
  select(fieldEvents) |>
  summarise(across(where(is.numeric), max, na.rm = TRUE)) |>
  pivot_longer(
    cols = everything(),
    names_to = "event",
    values_to = "bestPerformance"
  ) |>
  mutate(
    event = recode(event,
                   men_100 = '100m Run',
                   men_400 = '400m Run',
                   men_110h = '110m Hurdles',
                   men_1500 = '1500m Run',
                   men_lj = 'Long Jump',
                   men_sp = 'Shot Put',
                   men_hj = 'High Jump',
                   men_dt = 'Discus Throw',
                   men_pv = 'Pole Vault',
                   men_jt = 'Javelin Throw')
  )


bestPerformances = bind_rows(
  bestRunningPerformances,
  bestFieldPerformances)

# joining bestPerformances to the existing roi_long tbl
roi_long = left_join(roi_long,bestPerformances, by = 'event')

# mutating the difficulty factor
renamedRunningEvents = c('100m Run','400m Run','110m Hurdles','1500m Run')

roi_long = roi_long |>
  mutate(difficulty = if_else(
    event %in% renamedRunningEvents,
    bestPerformance / performance,
    performance/bestPerformance)
  )

# plotting the base bar graph with y-axis: ROI in Points on 1% Increase,
# x-axis: decathalon event
averageElite = roi_long |>
  ggplot(aes(x= fct_reorder(event,roi,.desc=TRUE), y= roi, fill= difficulty)) + 
  geom_col() + 
  scale_fill_continuous(low = '#fdcf58', high = '#ff0000') +
  labs(x = 'Event',
       y = 'ROI in Points on 1% Increase',
       title = 'Average Elite Decathlon Athlete: Opportunities for Point Score Growth',
       caption = 'Data courtesy of Battles, Noble and Chapman',
       fill = 'Difficulty of 1% Increase') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


