library("dplyr")
library("tidyverse")
library("ggplot2")


filename <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_trends <- read.csv(filename, header = T, stringsAsFactors = F)
View(incarceration_trends)


#Variable 1: State with the highest incarceration rate
highest_incarceration_state <- incarceration_trends %>%
  group_by(state) %>%
  summarise(total_pop = sum(total_jail_pop, na.rm = T)) %>%
  filter(total_pop == max(total_pop)) %>%
  pull(state)

highest_incarceration_state


#Variable 2: adult vs juvenile incarceration rate
fem_adult <- sum(incarceration_trends$female_adult_jail_pop, na.rm = T)
mal_adult <- sum(incarceration_trends$male_adult_jail_pop, na.rm = T)
fem_juv <- sum(incarceration_trends$female_juvenile_jail_pop, na.rm = T)
mal_juv <- sum(incarceration_trends$male_juvenile_jail_pop, na.rm = T)
age <- c(fem_adult, fem_juv, mal_adult, mal_juv)
data <- data.frame(age = c("Adult female", "Adult male", "juvenile female", "juvenile male"),
                   pop = c(fem_adult, mal_adult, fem_juv, mal_juv))

highest_jail_pop <- data %>%
  filter(pop == max(pop)) %>%
  pull(age)

highest_jail_pop


#Variable 3: county with the highest jail population 
highest_county_pop <- incarceration_trends %>%
  group_by(county_name) %>%
  summarise(total = sum(total_pop)) %>%
  filter(total == max(total)) %>%
  pull(county_name)

highest_county_pop


#Variable 4: state with the lowest incarceration rates in 2012
state_low_jail_pop <- incarceration_trends %>%
  group_by(state) %>%
  filter(year == 2012) %>%
  summarize(total_jail = sum(total_jail_pop, na.rm = T)) %>%
  filter(total_jail == min(total_jail, na.rm = T)) %>%
  pull(state)

state_low_jail_pop 
  


#Variable 5: total incarceration population in Florida  
total_incarceration_FL <- incarceration_trends %>% 
  filter(state == "FL") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, black_jail_pop, native_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

total_incarceration_FL



#trends over time: native jail population from 1970-2018
na_incarceration<- incarceration_trends %>% group_by(year) %>% 
  select(year, native_jail_pop) %>%
  gather(key = race, value = population, -year, na.rm = TRUE) %>%
  group_by(year, race) %>%
  summarise(population = sum(population))


na_incarceration$race <- na_incarceration$race %>% 
  factor(levels = c("native_jail_pop"))


native_jail_pop_time <- ggplot(na_incarceration)+
  geom_col(mapping = aes(x = year, y = population, fill = race), position = "dodge") +
  scale_alpha_continuous(5, 10) +
  labs(title = "Native Jail Population Over Time", 
       x = "year",
       y = "total incarcerations", fill = "incarcerations")
native_jail_pop_time


#variable comparison chart: adult female vs adult male
female_vs_male <- incarceration_trends %>%
  group_by(state) %>%
  filter(year == 2000) %>% 
  summarize(female_adult_jail_pop = mean(female_adult_jail_pop, na.rm = T),
            male_adult_jail_pop = mean(male_adult_jail_pop, na.rm = T)) %>% 
  na.omit()

adult_fem_v_mal <- ggplot(female_vs_male, aes(x = female_adult_jail_pop,
                                                      y = male_adult_jail_pop)) +
  geom_line(color = "red") + labs(title = "Adult Female vs. Adult Male Jail Population in 2000", x = "female population", 
                                             y = "male population")

adult_fem_v_mal


#map: aapi across the us 
library(usmap)


aapi_map <- incarceration_trends %>%
  select(year, state, aapi_jail_pop) %>%
  group_by(state) %>%
  filter(year == 2010) %>% 
  summarize(aapi_jail_pop = sum(aapi_jail_pop, na.rm = T))

us_map <- plot_usmap(data = aapi_map, values = "aapi_jail_pop", color = "white") + 
  labs(title = "Number of Asian Americans and Pacific Islanders People Incarcerated by State") +
  scale_fill_continuous(name = "Number of Incarcerations")
                        
  
us_map
