## Set up ----

library(tidyverse) # for data wrangling
library(rvest) # for web-scraping
library(gganimate) # for animation

theme_set(theme_bw()) # set ggplot theme

# save team names
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
        "Reading",
        "Birmingham City"
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
        "Reading Women",
        "Birmingham City Women"
      ),
      bbc_name = c(
        "Arsenal",
        "Man Utd",
        "Aston Villa",
        "Liverpool",
        "Tottenham",
        "West Ham",
        "Man City",
        "Chelsea",
        "Leicester City",
        "Everton",
        "Brighton",
        "Reading"
        "B'ham City"
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
        "REA",
        "BIR"
      )
    ),
    row.names = c(NA,-12L),
    class = c("tbl_df", "tbl", "data.frame")
  )


# save team colors
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
        "Reading",
        "Birmingham City"
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
        "#004494",
        "#0000FF"
      )
    ),
    row.names = c(NA,-13L),
    class = c("tbl_df", "tbl", "data.frame")
  )

## Scrape WSL 2021-22 results from BBC website ----

bbc_results <- "https://www.bbc.co.uk/sport/football/womens-super-league/scores-fixtures/2021-09"
bbc_base_url <- "https://www.bbc.co.uk"

# grab all hyperlinks on page
bbc_links <-
  bbc_results %>%
  read_html() %>%
  html_elements("a") %>%
  html_attr("href")

# save links of all matches by month (Sep 2021 - May 2022)
bbc_links_2122 <- 
  paste0(bbc_base_url, bbc_links[119:127])

# initialize empty list to store match results in
out <- list()

for (j in 1:length(bbc_links_2122)) {

  # for each month link, scrape a list of match urls
  bbc_match_links <-
    bbc_links_2122[j] %>%
    read_html() %>%
    html_elements("a") %>%
    html_attr("href") %>%
    tibble() %>%
    filter(str_detect(., "^/sport/football/[0-9]{8}$")) %>%
    pull() %>%
    paste0(bbc_base_url, .)

  # for each match url, extract:
  ## (a) Home and Away teams
  ## (b) Date
  ## (c) Score
  
  # initialize empty vectors
  match_date <- rep(NA, length(bbc_match_links))
  home_team <- rep(NA, length(bbc_match_links))
  away_team <- rep(NA, length(bbc_match_links))
  home_goals <- rep(NA, length(bbc_match_links))
  away_goals <- rep(NA, length(bbc_match_links))


  for (i in 1:length(bbc_match_links)) {
  
    temp <-
      bbc_match_links[i] %>%
      read_html() %>%
      html_elements(".sp-c-fixture__team.sp-c-fixture__team--home") %>%
      html_text()
    
    home_team[i] <- str_split(temp[1], " Women")[[1]][1]
      
    temp <- 
      bbc_match_links[i] %>%
      read_html() %>%
      html_elements(".sp-c-fixture__team.sp-c-fixture__team--away") %>%
      html_text()
    
    away_team[i] <- str_split(temp[1], " Women")[[1]][1]
    
    match_date[i] <-
      bbc_match_links[i] %>%
      read_html() %>%
      html_elements(".sp-c-fixture__date.gel-minion") %>%
      html_attr("datetime")
    
    home_goals[i] <-
      bbc_match_links[i] %>%
      read_html() %>%
      html_element(".sp-c-fixture__number.sp-c-fixture__number--home.sp-c-fixture__number--ft") %>%
      html_text()
    
    away_goals[i] <- 
      bbc_match_links[i] %>%
      read_html() %>%
      html_element(".sp-c-fixture__number.sp-c-fixture__number--away.sp-c-fixture__number--ft") %>%
      html_text()
  }

  out[[j]] <-
    tibble(match_date,
         home_team,
         away_team,
         home_goals,
         away_goals,
         bbc_match_links)
}

## Tidy data ----

# combine list into one dataframe
temp <- 
  do.call("bind_rows", out)

# define points based on match result
temp <-
  temp %>%
  mutate(
    home_points = case_when(home_goals > away_goals ~ 3,
                            home_goals == away_goals ~ 1,
                            home_goals < away_goals ~ 0),
    away_points = case_when(away_goals > home_goals ~ 3,
                            away_goals == home_goals ~ 1,
                            away_goals < home_goals ~ 0)
      )

# pivot dataset from wide format (1 row = 1 match),
# into long format (1 row = 1 result for each team)
out_df <- 
  temp %>%
  pivot_longer(cols = home_team:away_team,
               names_to = "home_away",
               values_to = "team") %>%
  mutate(
    goals = case_when(home_away == "home_team" ~ home_goals,
                      home_away == "away_team" ~ away_goals),
    points = case_when(home_away == "home_team" ~ home_points,
                      home_away == "away_team" ~ away_points)
  ) %>%
  select(-c(home_goals, away_goals, home_points, away_points)) %>%
  mutate(
    # replace NA's with 0's so the cumulative sum works
    goals = ifelse(is.na(goals), 0, goals),
    points = ifelse(is.na(points), 0, points),
  ) %>%
  group_by(team) %>%
  arrange(match_date) %>%
  mutate(
    cumulative_goals = cumsum(goals),
    cumulative_points = cumsum(points)
  ) %>%
  ungroup() %>%
  select(match_date, team, cumulative_goals, cumulative_points) %>%
  mutate(
    match_date = as.Date(match_date)
  ) %>%
  # fill in missing dates so we have data for each team on each match day
  complete(
    expand(., match_date, team)
    ) %>%
  group_by(team) %>%
  arrange(match_date) %>%
  # copy down the cumulative points/goals for these new rows
  fill(cumulative_goals, cumulative_points) %>%
  ungroup() %>%
  mutate(
    cumulative_goals = ifelse(is.na(cumulative_goals), 0, cumulative_goals),
    cumulative_points = ifelse(is.na(cumulative_points), 0, cumulative_points)
    )


# define the table rankings on each match date
out_df2 <-
  out_df %>%
  group_by(match_date) %>%
  # *1.0 to convert this variable from integer to double
  mutate(ordering = row_number(cumulative_points) * 1.0) %>%
  ungroup()

# define vector of team colors (in alphabetical order)
# to feed into scale_fill_manual()
wsl_palette <-
  team_colors %>%
  filter(team != "Liverpool") %>%
  arrange(team) %>%
  select(color_hex) %>%
  pull()

## Create animation ----

my_anim <-
  out_df2 %>%
  ggplot(data = .,
         aes(x = ordering, group = team)
  ) +
  # use geom_tile() rather than geom_col() or geom_bar() 
  # because the animation is smoother
  geom_tile(
    aes(y = cumulative_points/2,
        height = cumulative_points,
        fill = team),
    width = 0.9,
    alpha = 1
    ) +
  scale_fill_manual(
    values = wsl_palette
    ) +
  ylim(c(0, 80)) +
  geom_text(
    aes(
      y = cumulative_points, 
      label = str_pad(
        team,
        width = max(nchar(unique(out_df2$team))),
        side = "right"
        )
      ), 
    vjust = 0.5,
    hjust = -0.2,
    color = "black",
    ) +
  annotate(
    geom = "text",
    x = c(1:12),
    y = 0,
    hjust = 1,
    color = "black",
    label = str_pad(c(12:1), width = 4, side = "right")
    ) +
  labs(title = "Women's Super League table: 2021-22",
       subtitle = 'Date: {frame_time}', 
       x = "",
       y = "Cumulative points",
       caption = "Credit: @woso_stats") +
  theme(plot.title = element_text(size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  # add animation
  transition_time(match_date) +
  ease_aes('cubic-in-out') +
  coord_flip(clip = "off", expand = FALSE) +
  theme(
    plot.margin = margin(2, 2, 2, 2, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.border = element_blank(),
    panel.background = element_rect(
      fill = "white",
      color = "white", size = 2),    
    ) +
  guides(fill = "none")

my_anim

# use animate() to define:
# -duration (overall length in seconds) 
# -fps (smoothness)
animate(my_anim, 
        duration = 25, 
        fps  =  10,
        start_pause = 0,
        end_pause = 50)

anim_save(filename = "wsl-2021-22-animation.gif")
