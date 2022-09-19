## Set up ----

library(tidyverse)
library(rvest)
library(gt)

team_colors <-
  structure(
    list(
      team = c(
        "Arsenal",
        "Manchester United",
        "Aston Villa",
        "Liverpool",
        "Tottenham Hotspur",
        "West Ham United",
        "Manchester City",
        "Chelsea",
        "Leicester City",
        "Everton",
        "Brighton & Hove Albion",
        "Reading"
      ),
      color_hex = c(
        "#EF0107",
        "#DA291C",
        "#670E36",
        "#C8102E",
        "#132257",
        "#7A263A",
        "#6CABDD",
        "#034694",
        "#003090",
        "#003399",
        "#0057B8",
        "#004494"
      ),
      color_main = c(
        "Red",
        "Red",
        "Red",
        "Red",
        "Blue",
        "Red",
        "Blue",
        "Blue",
        "Blue",
        "Blue",
        "Blue",
        "Blue"
      )
    ),
    row.names = c(NA,-12L),
    class = c("tbl_df", "tbl", "data.frame")
  )

team_names <-
  structure(
    list(
      team = c(
        "Arsenal",
        "Manchester United",
        "Aston Villa",
        "Liverpool",
        "Tottenham Hotspur",
        "West Ham United",
        "Manchester City",
        "Chelsea",
        "Leicester City",
        "Everton",
        "Brighton & Hove Albion",
        "Reading"
      ),
      guardian_name = c(
        "Arsenal Women",
        "Man Utd Women",
        "Aston Villa Women",
        "Liverpool FC Women",
        "Tottenham Hotspur Women",
        "West Ham Women",
        "Man City Women",
        "Chelsea Women",
        "Leicester Women",
        "Everton Women",
        "Brighton & Hove Albion Women",
        "Reading Women"
      ),
      short_name = c(
        "ARS",
        "MUN",
        "AVL",
        "LIV",
        "TOT",
        "WHU",
        "MCI",
        "CHE",
        "LEI",
        "EVE",
        "BHA",
        "REA"
      )
    ),
    row.names = c(NA,-12L),
    class = c("tbl_df", "tbl", "data.frame")
  )

## Scrape WSL table from Guardian website ----

guardian_url <- "https://www.theguardian.com/football/womens-super-league/table"

wsl_tab <- 
  guardian_url %>%
  read_html() %>%
  html_elements("table.table--football.table--league-table.table--responsive-font.table--striped") %>%
  html_table()

wsl_tab <- wsl_tab[[1]]

# Merge in data with team names and colors
full_tab <-
  wsl_tab %>%
  left_join(., team_names, by = c("Team" = "guardian_name")) %>%
  left_join(., team_colors, by = "team")

full_tab$Team <- full_tab$team

# arrange colors in alphabetical order by team name
color_palette <-
  full_tab %>%
  arrange(., Team) %>%
  select(color_hex)

## Create table ----

my_table <- 
  full_tab %>%
  mutate(color_hex = fct_inorder(color_hex)) %>%
  select(1:10) %>%
  gt() %>%
  data_color(columns = Team,
             colors = pull(color_palette)) %>%
  tab_header(
    title = md("Women's Super League table"),
    subtitle = md("Points after match week 1")
  )

my_table

# save table

my_table %>%
  gtsave(., 
         filename = "wsl-red-blues.png")
