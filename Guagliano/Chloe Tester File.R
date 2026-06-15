# Decathalon EDA Project
# June 4th, 2026
# By Chloe Guagliano

## loading in the data and making necessary adjustments
library(tidyverse)
library(lubridate)
dec_performances <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/dec_performances.csv")

## converting dates to a lexiographic format for sorting purposes
dec_performances = dec_performances |>
  mutate(dob = format(as.Date(dob,format='%m/%d/%Y'))) |>
  mutate(date = format(as.Date(date,format='%m/%d/%Y')))

## adding yearofComp column which indicates the competition year
dec_performances = dec_performances |>
  mutate(yearofComp = year(date))

## adding age column which uses DOB and date to calculate age at time of competition
dec_performances = dec_performances |>
  mutate(age = interval(dob,date) %/% years(1))




## general information about the data (non-event specific)
generalColumns = c('rank','competitor','dob','nat','sec','pos_sec','pos_full','venue','date',
                   'world_athletics_event_ranking_score','overall_score','average_wind','yearofComp')

length(unique(dec_performances$date)) ## 1298 unique decathalon competitions in dataset


## graphing number of 'elite' (>6400) decathalon performances by year
eliteByYear = dec_performances |>
  count(yearofComp, name='num_elite_performances')

eliteByYear |>
  ggplot(aes(x=yearofComp, y=num_elite_performances)) + 
  geom_line(color='firebrick', linewidth = 1.5) +
  labs(x= 'Year',
       y= 'Number of Elite Performances (>6400)',
       title = 'Number of Elite Performances by Year in Decathalon',
       caption = 'Data courtesy of Battles, Noble and Chapman') + 
  theme_bw()

## graphing number of 'elite' performances (>6400) by age
dec_performances |>
  ggplot(aes(x=age)) +
  geom_bar(fill = 'firebrick') + 
  labs(x= 'Age',
       y= 'Number of Elite Performances (>6400)',
       title = 'Number of Elite Performances by Age in Decathalon',
       caption = 'Data courtesy of Battles, Noble and Chapman') + 
  theme_bw()

## graphing bar chart to show number of 'elite' performances (>6400) by nationality
eliteByNationality = dec_performances |>
  count(nat) |>
  mutate(prop = n/sum(n)) |>
  filter(prop >= 0.01) |>
  mutate(country = fct_reorder(nat,n))

eliteByNationality |>
  ggplot(aes(x=reorder(nat,n),y=n)) + 
    geom_col(fill='firebrick') + 
    coord_flip() + ## makes bars horizontal for easier reading of country names
    theme_bw() + 
    labs(
      title = 'Elite Performances by Country (min. 1% of total elite performances)',
      x = 'Country',
      y = 'Number of Elite Performances (>=6400)',
      caption = 'Data courtesy of Battles, Noble and Chapman'
    )

## which competitors have the highest number of elite performances b/n 2001-2022
dec_performances |>
  count(competitor, name = 'num_elitePerformances') |>
  summary(num_elitePerformances)

dec_performances |>
  count(competitor,name='num_elitePerformances') |>
  arrange(desc(num_elitePerformances)) |>
  ggplot(aes(x = num_elitePerformances)) + 
  geom_boxplot() + 
  theme(axis.text.y = element_blank()) + 
  labs(x = 'Number of Elite Performances by Athlete')

## 100m dash specific research (USING THE DATASET ITSELF)
dec_performances |>
  select(all_of(generalColumns),starts_with('men_100')) |>
  ggplot(aes(x=men_100, y=men_100_score)) + 
  geom_point(color = 'firebrick', alpha = 0.1) + 
  labs(x= "Run Time",
       y = "Point Score",
       title = "Men's 100m Point Score by Run Time",
       caption = "Data courtesy of Battles, Noble and Chapman")

## plotting the 100m scoring function 
library(Deriv)
men_100_func = function(p) 25.4347*((18-p)^1.81)
men_100_derivative = Deriv(men_100_func)

# plotting the function itself
ggplot() + 
  geom_function(fun = men_100_func, xlim= c(18,10),
                colour = 'firebrick') +
  labs(x='Run Time',
       y= 'Point Score',
       title = "Men's 100m Race: Point Score by Run Time (using formula)",
       caption = 'Data courtesy of Battles, Noble and Chapman')

ggplot() + 
  geom_function(fun = men_100_derivative, xlim= c(18,10),
                colour = 'firebrick') +
  labs(x = "Run Time (seconds)",
    y = "Points Gained per Decrease in Time",
    title = "Rate of Change of Decathlon Points with Respect to 100m Time"
  )



## FUNCTIONS FOR OTHER EVENTS !!!!!!!!!!!!!!

## mens 400m
library(Deriv)
men_400_func = function(p) 1.53775*((82-p)^1.81)
men_400_derivative = Deriv(men_400_func)

## 110m hurdles
library(Deriv)
hurdles_110_func = function(p) 5.74352*((28.5-p)^1.92)
hurdles_110_derivative = Deriv(hurdles_110_func)

## mens 1500m
library(Deriv)
men_1500_func = function(p) 0.03768*((480-p)^1.85)
men_1500_derivative = Deriv(men_1500_func)


## field events starting here:
## long jump
longjump_func = function(p) 0.14354*((p-220)^1.4)
longjump_derivative = Deriv(longjump_func)

## shot put
shotput_func = function(p) 51.39*((p-1.5)^1.05)
shotput_derivative = Deriv(shotput_func)

## high jump
highjump_func = function(p) 0.8465*((p-75)^1.42)
highjump_derivative = Deriv(highjump_func)

## discus throw
discus_func = function(p) 12.91*((p-4)^1.1)
discus_derivative = Deriv(discus_func)

## pole vault
polevault_func = function(p) 0.2797*(((p*100)-100)^1.35)
polevault_derivative = Deriv(polevault_func)

polevault = dec_performances |>
  select(starts_with('men_pv')) |>
  mutate(marginalImprovement = men_pv + (men_pv/100)) |> # marginal improvement in PERFORMANCE where marginal = 1%
  mutate (pointsForMarginal = polevault_func(marginalImprovement)) |> ## the point total associated with the 1% increase
  mutate(marginalPointIncrease = pointsForMarginal - men_pv_score)

polevault |> 
  ggplot(aes(x = men_pv, y = marginalPointIncrease)) + 
  geom_line(color = 'firebrick') +
  # geom_point(color='firebrick',alpha = 0.3) + 
  labs(x = 'Pole Vault Performance',
       y = 'Marginal Point Increase for 1% Gain',
       title = 'Marginal Point Increases for Pole Vault')





## javelin throw
javelin_func = function(p) 10.14*((p-7)^1.08)
javelin_derivative = Deriv(javelin_func)

ggplot() + 
  geom_function(fun = javelin_func, color = 'firebrick', xlim = c(7,80)) +
  labs(x = 'Javelin Throw Distance',
       y = 'Point Score',
       title = 'Event Point Score by Javelin Score Distance',
       caption = 'Data courtesy of Battles, Noble and Chapman')

ggplot() + 
  geom_function(fun = javelin_derivative, color = 'firebrick', xlim= c(7,80)) +
  labs(x = 'Javelin Throw Distance',
       y = 'Rate of Change in Point Score',
       title = 'Rate of Change in Event Point Score by Javelin Score Distance',
       caption = 'Data courtesy of Battles, Noble and Chapman')
  
## trying the 1% marginal increase way
javelin = dec_performances |>
  select(starts_with('men_jt')) |>
  mutate(marginalImprovement = men_jt + (men_jt/100)) |> # marginal improvement in PERFORMANCE where marginal = 1%
  mutate (pointsForMarginal = javelin_func(marginalImprovement)) |> ## the point total associated with the 1% increase
  mutate(marginalPointIncrease = pointsForMarginal - men_jt_score)
  
javelin |> 
  ggplot(aes(x = men_jt, y = marginalPointIncrease)) + 
  geom_point(color='firebrick',alpha = 0.1) + 
  labs(x = 'Javelin Throw Performance',
       y = 'Marginal Point Increase for 1% Gain',
       title = 'Marginal Point Increases for Javelin Throw')






