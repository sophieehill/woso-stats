## Set up ----

# Load packages
library(tidyverse) # for data wrangling
library(magick) # for image layering

# Load in raw data (collected manually)
dat <- structure(
  list(
    Date = structure(c(18510, 18644, 18734, 18951,
                       19007, 19253), class = "Date"),
    GA = c(2, 7, 8, 5, 3, 3),
    GS = c(0,
           0, 0, 0, 0, 4),
    Competition = c("WSL", "WSL", "FA Cup", "WSL",
                    "WSL", "WSL")
  ),
  row.names = c(NA,-6L),
  class = c("tbl_df", "tbl",
            "data.frame")
)

# Chart aesthetics
my_width <- 40
theme_set(theme_bw())

# Clean data
dat <-
  dat %>%
  mutate(result = paste0(GA, "-", GS),
         GA = GA * -1,
         Date = as.Date(Date))

## Create plot ----

dat %>%
  ggplot(aes(x = Date),
         data = .) +
  geom_col(aes(y = GA),
           fill = "lightblue",
           width = my_width) +
  geom_col(aes(y = GS),
           fill = "darkred",
           width = my_width) +
  labs(
    y = "Goals",
    x = "",
    title = "Head to Head: Man City v. Aston Villa",
    caption = "Credit: @woso_stats"
  ) +
  geom_hline(yintercept = 0) +
  geom_label(aes(x = Date,
                 y = 0,
                 label = result),
             size = 3) +
  theme(panel.grid = element_blank()) +
  scale_x_date(breaks = dat$Date) +
  scale_y_continuous(breaks = c(-8, -4, 0, 4)) +
  coord_flip()

# Save plot
ggsave(filename = "AstonVilla-ManCity-H2H.png",
       width = 5,
       height = 4)

## Add club logos ----

# First read back in the chart
plot <- image_read("~/Downloads/AstonVilla-ManCity-H2H.png")

# Next read in club logos and scale them
logo_av <-
  image_read(
    "https://upload.wikimedia.org/wikipedia/en/thumb/f/f9/Aston_Villa_FC_crest_%282016%29.svg/1200px-Aston_Villa_FC_crest_%282016%29.svg.png"
  ) %>%
  image_scale("130")

logo_mc <-
  image_read(
    "https://upload.wikimedia.org/wikipedia/en/thumb/e/eb/Manchester_City_FC_badge.svg/1200px-Manchester_City_FC_badge.svg.png"
  ) %>%
  image_scale("180")

# Layer images
out <- image_composite(plot, logo_av, offset = "+1200+280")
out <- image_composite(out, logo_mc, offset = "+400+250")

# Save final image
image_write(out, "AstonVilla-ManCity-H2H.png")
