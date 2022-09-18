## Set up ----

library(tidyverse) # data wrangling
library(gghighlight) # highlighting points on graph

theme_set(theme_bw()) # set graph theme

# Load in raw data (input manually)
shots <-
  structure(
    list(
      date = structure(
        c(
          19253,
          19120,
          19113,
          19110,
          19106,
          19085,
          19078,
          19067,
          19064,
          19061,
          19034,
          19029,
          19018,
          19015,
          18972,
          18952,
          18945,
          18937,
          18910,
          18902,
          18896,
          18882,
          18875
        ),
        class = "Date"
      ),
      opponent = c(
        "Liverpool",
        "Manchester United",
        "Birmingham City",
        "Tottenham Hotspur",
        "Tottenham Hotspur",
        "Reading",
        "Leicester City",
        "Everton",
        "Aston Villa",
        "West Ham",
        "Arsenal",
        "Manchester City",
        "West Ham",
        "Brighton",
        "Reading",
        "Birmingham City",
        "Manchester City",
        "Aston Villa",
        "Leicester City",
        "Brighton",
        "Manchester United",
        "Everton",
        "Arsenal"
      ),
      competition = c(
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL",
        "WSL"
      ),
      total_shots = c(
        10,
        17,
        20,
        24,
        12,
        24,
        25,
        16,
        22,
        13,
        11,
        11,
        19,
        26,
        34,
        25,
        13,
        16,
        22,
        18,
        16,
        27,
        15
      ),
      shots_on_target = c(4, 4, 3, 9, 6, 8, 14, 5, 8,
                          6, 3, 4, 10, 5, 5, 13, 6, 5, 9, 6, 9, 6, 4),
      gs = c(1, 4, 1,
             2, 3, 5, 9, 3, 1, 4, 0, 1, 2, 0, 0, 5, 4, 1, 2, 3, 6, 4, 2),
      possession = c(
        0.69,
        0.46,
        0.67,
        0.56,
        0.45,
        0.76,
        0.77,
        0.64,
        0.57,
        0.59,
        0.5,
        0.43,
        0.57,
        0.68,
        0.77,
        0.75,
        0.45,
        0.75,
        0.71,
        0.62,
        0.49,
        0.57,
        0.59
      )
    ),
    row.names = c(NA,-23L),
    class = c("tbl_df", "tbl", "data.frame")
  )

# Take a look:
glimpse(shots)

# Tidy up
shots <-
  shots %>%
  mutate(
    shots_on_target_pc = shots_on_target / total_shots,
    row_nums = 1:23,
    highlight = case_when(row_nums %in% c(1, 11, 15, 17, 18, 22) ~ 1,
                          TRUE ~ 0),
    opponent = case_when(opponent == "Manchester City" ~ "Man City",
                         TRUE ~ opponent)
  )

## Make graph ---- 

shots %>%
  ggplot(data = .,
         aes(
           x = possession * 100,
           y = total_shots,
           size = gs,
           color = opponent
         )) +
  geom_point() +
  labs(
    x = "Possession (%)",
    y = "Shots",
    title = "Chelsea WFC: possession vs shots",
    subtitle = "2021-22 WSL season, plus 2022-23 opener vs Liverpool",
    caption = "Point size indicates # goals scored by Chelsea.\nCredit: @woso_stats"
  ) +
  guides(size = "none") +
  scale_size(range = c(2, 9)) +
  gghighlight(highlight == 1) +
  scale_color_manual(values = c("black",
                                "black",
                                "black",
                                "#C8102E",
                                "black",
                                "black")) +
  xlim(c(40, 80)) +
  ylim(c(0, 40))

# Save graph 
ggsave(filename = "chelsea-shots.png",
       height = 4,
       width = 6)
