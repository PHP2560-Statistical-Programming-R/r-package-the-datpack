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
library(RColorBrewer)
library(plotly)


#Load sample dataset available in the AER package

library(AER)
data(Fatalities)


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

#Function for plotting
epiplot<- function(data, x, y, graph, fill, title, xlab, ylab, legend){    ###add argument for error bars
  data[complete.cases(data), ]
  if(graph == "bar"){                           #boxplot function
    pic<-  ggplot(data=data, aes(x=x, fill=x)) +
      geom_bar( ) +
      scale_fill_brewer(palette = "Paired")+
      labs(title="title", x="xlab", y="ylab")
    #table1 = table(data$x)  ## get the cross tab
    #pic<-barplot(table1, beside = TRUE, legend = levels(data$x), col=c("lightblue","darkblue"),main="title", xlab="xlab", ylab = "ylab")
    return(pic)
    #barplot(table(data$x), col=c("lightblue","darkblue"),main="title", xlab="xlab", ylab = "ylab")
  } else if(graph=="bargroup"){                                           #works, but has column for NAs....why???
    pic<-ggplot(data=data, aes(x=x, y=y, fill=fill)) +
      geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Paired")+theme_bw()+facet_wrap(~"fill")
    return(pic)
  } else if(graph=="barstack"){                                           #works, but has column for NAs....why???
    pic<-ggplot(data=data, aes(fill=fill, y=y, x=x)) +
      geom_bar( stat="identity")
    return(pic)
  } else if(graph=="boxplot"){                                            #GOOD
    pic<-boxplot(y~x, data=data, notch=TRUE,
                 main="title", xlab="xlab", ylab="ylab")
    return(pic)
  } else if(graph=="dotboxplot"){                                         #GOOD
    pic<-plot_ly(y = ~y, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8)
    return(pic)
  } else if(graph=="hist"){                                               #GOOD
    pic<-ggplot(data=data, aes(x)) +
      geom_histogram(col="black", aes(fill=..count..)) +
      scale_fill_gradient("Count", low="light blue", high="navy")+
      labs(title="title", x="xlab", y="ylab")
    return(pic)
  } else if(graph=="densityhist"){                                          #GOOD
    pic<-ggplot(data=data, aes(x)) +
      geom_histogram(aes(y =..density..),col="blue", fill="light blue", alpha=.5) +
      geom_density(col=2) +
      labs(title="title", x="xlab", y="ylab")
    return(pic)
  } else if(graph=="scatter"){                                            #GOOD
    pic<-ggplot(data, aes(x, y, color = fill)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      #scale_color_gradient(color = "Blues")+
      labs(title="title", x="xlab", y="ylab", color = "legend")
    return(pic)
  } else if(graph=="scatterline"){                                        #GOOD
    pic<-ggplot(data, aes(x, y, color = fill)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      scale_color_gradient(low = "light blue", high = "dark blue")+
      labs(title="title", x="xlab", y="ylab", color = "legend")+geom_smooth()
    return(pic)
  } else if(graph=="linreg"){                                             #GOOD
    pic<-ggplot(data, aes(x, y, color = fill)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      scale_color_gradient(low = "light blue", high = "dark blue")+
      labs(title="title", x="xlab", y="ylab", color = "legend")+ geom_smooth(method = 'lm', se = TRUE)
    return(pic)
  }
}



