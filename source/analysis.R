library(tidyverse)
library(ggplot2)
library(scales)

#loading the data set
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

view(incarceration_trends)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

#County with lowest jail population

View(incarceration_trends)

#lowest_pop_county <- total_jail_pop %>% 
 # filter(total_jail_pop == max(total_jail_pop)) %>% 
  #pull(county_names)

#County with highest jail population

#highest_pop_county <- total_jail_pop %>% 
  #filter(total_jail_pop == max(total_jail_pop)) %>% 
  #pull(county_names)

#County with highest juvenile population

#highest_juvinile_pop_county <- total_jail_pop %>% 
  #filter(total_jail_pop == max(male_juvenile_population)) %>% 
  #pull(county_names)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function ... 
get_year_jail_pop <- incarceration_trends %>%
  group_by(year) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))

View(get_year_jail_pop)

# This function ...
plot_jail_pop_for_us <- ggplot(data = get_year_jail_pop) +
  geom_col(mapping = aes(x = year, y = total_jail_pop)) +
  scale_y_continuous(labels=comma) +
  labs(title= "Jail Population Rise in the United States",
       x = "Year",
       y = "Total Jail Population", 
       caption = "Figure 1. This chart depicts the increase of indiviuals in prison over the past fifty years in the United States.") +
         theme(plot.caption = element_text(hjust = 0.5))

  # TODO: Implement this function 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- incarceration_trends %>% 
  group_by(state,year) %>% 
  summarize(total_jail_pop_state = sum(total_jail_pop, na.rm=TRUE)) %>% 
  return(year,state, total_jail_pop_state)

View(get_jail_pop_by_states)

plot_jail_pop_by_states <- {
  ggplot(data = get_jail_pop_by_states) +
    geom_line(mapping = aes(x = year, y = total_jail_pop_state, group = state, color = state)) +
    labs(title = "Jail Population Rise by State",
         x = "Year",
         y = "Jail Population",
         caption = "Figure 2. Rise of Jail Pop. By State from 1970-2018. This shows that there has been a rise in the population.") +
    theme(plot.caption = element_text(hjust = 0.5))
}


plot(plot_jail_pop_by_states)

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#

# Extracting Needed Variables
relevant_variables <- incarceration_trends %>% 
  select(state, year, region, total_pop, black_jail_pop, latinx_jail_pop,
         native_jail_pop, white_jail_pop)


# Finding State Total Population for Each Race 
black_jail_pop_per_state <- relevant_variables %>% group_by(state) %>% 
  summarise(black_jail_pop_per_state = sum(black_jail_pop, na.rm = T))

latinx_jail_pop_per_state <- relevant_variables %>% group_by(state) %>% 
  summarise(latinx_jail_pop_per_state = sum(latinx_jail_pop, na.rm = T))

native_jail_pop_per_state <- relevant_variables %>% group_by(state) %>% 
  summarise(native_jail_pop_per_state = sum(native_jail_pop, na.rm = T))

white_jail_pop_per_state <- relevant_variables %>% group_by(state) %>% 
  summarise(white_jail_pop_per_state = sum(white_jail_pop, na.rm = T))

total_pop_per_state <- relevant_variables %>% group_by(state) %>% 
  summarise(total_pop_per_state = sum(total_pop, na.rm = T))


# Creating Data Frame
jail_pop_per_state <- black_jail_pop_per_state

jail_pop_per_state<- left_join(jail_pop_per_state, latinx_jail_pop_per_state, by = "state")

jail_pop_per_state <- left_join(jail_pop_per_state, native_jail_pop_per_state, by = "state")

jail_pop_per_state <- left_join(jail_pop_per_state, white_jail_pop_per_state, by = "state")

jail_pop_per_state <- left_join(jail_pop_per_state, total_pop_per_state, by = "state")


jail_pop_per_state <- jail_pop_per_state %>% 
  mutate(
    total_jail_pop_per_state = black_jail_pop_per_state + 
      latinx_jail_pop_per_state + 
      native_jail_pop_per_state +
      white_jail_pop_per_state
  )

# chart 1: Black vs. White Jail Population in Washington (over time)

wa_data <- relevant_variables %>% 
  filter(state == "WA") %>% 
  select(state, year, black_jail_pop, white_jail_pop)

# creating plot
jail_pop_over_time <- wa_data %>% 
  ggplot() +
  geom_line(mapping = aes( x = year, y = black_jail_pop), color = "Blue") +
  geom_line(mapping = aes( x = year, y = white_jail_pop), color = "Red")+
  labs(title = "Black jail population vs. White jail population over time",
       subtitle = "Data is only from Washington State \n Red lines are black population \n Blue lines are white population",
       x = "Year",
       y = "Jail Population")
jail_pop_over_time

# Chart2 : White vs. Black Jail Population Per State

jail_pop_per_state <- jail_pop_per_state %>% 
  mutate(black_jail_approx = case_when(
    black_jail_pop_per_state > 200000 ~ "greater than 200",
    black_jail_pop_per_state > 150000 & black_jail_pop_per_state <= 200000 ~ "between 150 and 200",
    black_jail_pop_per_state > 100000 & black_jail_pop_per_state <= 50000 ~ "between 50 and 100",
    black_jail_pop_per_state <= 50000 ~ "less than 50"
  ))

jail_pop_per_state <- jail_pop_per_state %>% 
  mutate(white_jail_approx = case_when(
    white_jail_pop_per_state > 200000 ~ "greater than 200",
    white_jail_pop_per_state > 150000 & black_jail_pop_per_state <= 200000 ~ "between 150 and 200",
    white_jail_pop_per_state > 100000 & black_jail_pop_per_state <= 50000 ~ "between 50 and 100",
    white_jail_pop_per_state <= 50000 ~ "less than 50"
  ))

balck_vs_black_jail_pop_per_state <- jail_pop_per_state %>% 
  ggplot()+
  geom_bar(mapping = aes( x = black_jail_approx, fill = "Black")) +
  geom_bar(mapping = aes( x = white_jail_approx, fill = "White", alpha = 0.3)) +
  labs(title = "Black vs. White Jail Pop. per State",
       x = "Jail Population per State in tens of thousands",
       y = "Count"
  )
balck_vs_black_jail_pop_per_state


# Section 6: Making the Map

perc_state_data <- jail_pop_per_state %>% 
  select(state, black_jail_pop_per_state, total_jail_pop_per_state)

perc_state_data <- perc_state_data %>% 
  group_by(state) %>% 
  summarise(percent_black_jail_pop = 
              (sum(black_jail_pop_per_state) / sum(total_jail_pop_per_state)) * 100)

state_shape <- map_data("state")
state_abvs <- data.frame(state.abb, state.name)

perc_state_data <-left_join(perc_state_data, state_abvs, by = c('state' = 'state.abb'))

perc_state_data <- perc_state_data %>% 
  mutate(
    region = tolower(state.name)
  )

state_shape <- left_join(state_shape, perc_state_data)

map <- state_shape %>% 
  ggplot() +
  geom_polygon( mapping = aes( x = long, y = lat, group = group, fill = percent_black_jail_pop)) +
  scale_fill_continuous( low = 'Blue', high = 'Red') +
  coord_map() +
  labs(
    title = "Percentage of Each State's Black Jail Population"
  ) 
map



