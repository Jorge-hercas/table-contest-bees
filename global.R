


library(shiny)
library(echarts4r)
library(dplyr)
library(shinymaterial)
library(shinyWidgets)
library(reactable)
library(mapboxer)


states <- sf::read_sf("data/USA_States/USA_States.shp")
bees <-vroom::vroom("data/bees.csv")
stressor <- vroom::vroom("data/stressor.csv")
options(scipen = 999)

style <- "mapbox://styles/mapbox/light-v11"
token_mapa <- "pk.eyJ1Ijoiam9yZ2VoZGV6MTk5OCIsImEiOiJja2o2dnZzdmowemRsMzNueW5zNmJ6ZmdoIn0.s3BJeDpXW5GMy2Kln139Eg"
id<- "bees_map"

base_color <-
states |> 
  select(-one_of("STATE_FIPS","STATE_ABBR")) |> 
  left_join(
    bees |> 
      group_by(
        state
      ) |> 
      summarise(
        mean_col = mean(colony_n, na.rm =T)
      ),
    by = c("STATE_NAME" = "state")
  ) |> 
  filter(is.na(mean_col) == F)



elecciones <- unique(base_color$STATE_NAME)

opts <- list(
  `actions-box` = TRUE,
  `live-search`=TRUE)

source("functs.R")


