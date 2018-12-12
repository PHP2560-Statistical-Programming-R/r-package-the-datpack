# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


library(MASS)
library(epiR)
library(epitools)
library(incidence)
library(Epi)
library(epiDisplay)
library(devtools)
library(roxygen2)
library(epicalc)
library(maps)
library(usmap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rprev)
library(ggmap)
library(mapdata)
library(viridis)


#Load sample dataset available in the AER package

library(AER)
data(Fatalities)

#NOTE: no data for Alaska or Hawaii

#Cleaning the state variable data

us_map <- map_data("state")

tbl <- state.x77 %>%
  as_tibble(rownames = "state") %>%
  bind_cols(state_name = str_to_lower(state.abb)) %>%
  rename(value_x = Income) %>%
  select(state_name, value_x)

state_abbs <- tibble(state_full = str_to_lower(state.name), abb = str_to_lower(state.abb))
tbl_m <- left_join(tbl, state_abbs, by = c("state_name" = "abb")) %>%
  rename(id = state_full)

Fatalities_clean <- Fatalities %>%
  left_join(state_abbs, by = c("state" = "abb"))
#right_join(us_map, by = c("state_full" = "region"))

Fatalities_clean %>%
  ggplot(aes(x = long, y = lat, group = group, fill = `fatal`)) +
  geom_polygon(color = "white") + ggtitle("National Fatalities 1982-1988") +
  theme_void() +
  scale_fill_viridis(name = "Fatalities (Count)")

#National Prevalence Function
map_national_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%
    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("National Prevalence of Car Fatalities 1982-1988") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}

map_national_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state_full", year="year")


#Mapping for Region: Northeast
map_northeast_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("massachusetts","rhode island", "connecticut", "maine",
                                "vermont", "new hampshire","new york","new jersey",
                                "pennsylvania")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the Northeast") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}

map_northeast_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state_full", year="year")


#Mapping for Region: South
map_south_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("delaware","maryland","west virginia", "virginia", "kentucky",
                                "tennessee","north carolina","south carolina",
                                "georgia","alabama","mississippi","arkansas",
                                "louisiana","florida","texas","oklahoma")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the South") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}

map_south_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state_full", year="year")


#Map for Region: Midwest
map_midwest_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("ohio","michigan", "indiana", "wisconsin", "illinois",
                                "minnesota","iowa","missouri","north dakota",
                                "south dakota","nebraska","kansas")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the Midwest") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}

map_midwest_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state_full", year="year")


#Map for Region: West
map_west_prevalence <- function(data,existing_cases,population,state,year) {
  us_map <- map_data("state")
  data %>%
    group_by(!! sym(state), !! sym(year)) %>%
    mutate(prevalence = !! sym(existing_cases) / !!sym(population)) %>%

    #left_join(state_abbs, by = c("state" = "abb")) %>%
    right_join(us_map, by = c("state_full" = "region")) %>%

    filter(!! sym(state) %in% c("montana","wyoming","colorado","new mexico", "arizona",
                                "utah","california","idaho","nevada","oregon","washington",
                                "hawaii","alaska")) %>%
    ggplot(aes(x = long, y = lat, group = group, fill = `prevalence`)) +
    geom_polygon(color = "white") + ggtitle("Prevalence in the West") +
    theme_void() +
    scale_fill_viridis(name = "Prevalence")
}

map_west_prevalence(Fatalities_clean,existing_cases= "fatal",population="pop", state="state_full", year="year")

