# Script for Airbnb data

library(dplyr)
library(leaflet)
library(htmltools)
library(ggplot2)
library(plotly)

# Read in our data
data <- read.csv("data/listings.csv")

#####################################
# FORMULATING QUESTIONS OF INTEREST #
#####################################

# How many Airbnbs are in Seattle?

total_listings <- nrow(data)

# Average price of an Airbnb in Seattle?

avg_price <- data %>%
  summarise(avg = mean(price)) %>%
  pull(avg)

# How many total reviews have been written?

total_reviews <- sum(data$number_of_reviews)

##################
# CREATING A MAP #
##################

# You'll build an interactive map that shows a marker at the location of each shooting.
# On your map, manipulate the size of the markers based on the underlying dataset
# (# injured, # killed, etc.). When hovered or clicked on, each point should provide
# at least 3 pieces of information about the incident (with a line break -- <br> --
# between each piece of information) and no irrelevant information.

# Let's display a map where a marker is placed on a listing,
# and the marker's size is determined by its price.

udub_data <- data %>%
  filter(neighbourhood == "University District") %>%
  filter(price > 200) %>%
  mutate(on_hover = paste0(
    "Listing: ", name, "<br>",
    "Host: ", host_name
  ))

price_map <- leaflet(udub_data) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~ (price / 10),
    lat = ~latitude, # Specify where to place the marker
    lng = ~longitude,
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~on_hover # Show the listing & host info when we click on the circle
  )

####################
# CREATING A GRAPH #
####################

# x axis: neighborhood
# y axis: avg num of reviews per neighborhood

x_graph <- unique(data$neighbourhood_group)

y_graph <- data %>%
  group_by(neighbourhood_group) %>%
  summarise(avg = mean(number_of_reviews)) %>%
  pull(avg)

reviews_graph <- data %>%
  plot_ly(
    type = "bar",
    x = ~x_graph,
    y = ~y_graph
  ) %>%
  layout(
    title = "Average Number of Reviews per Neighborhood",
    xaxis = list(
      title = "Neighborhood"
    ),
    yaxis = list(
      title = "Average Number of Reviews"
    )
  )

############################
# CREATING A SUMMARY TABLE #
############################

# Table of neighborhoods versus avg price per neighborhood

price_table <- data %>%
  select(neighbourhood, price) %>%
  group_by(neighbourhood) %>%
  summarize(avg_price = mean(price)) %>%
  arrange(-avg_price) # Sort ascending
