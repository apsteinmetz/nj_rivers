
create_analog_clock <- function(this_time_str = "2025-07-14 01:00:00") {
  # one minute granularity
  this_time <- as.POSIXct(this_time_str)
  hour <- lubridate::hour(this_time)
  minute <- lubridate::minute(this_time)
  i <- hour * 60 + minute + 1
  
  hour.pos <- seq(0, 12, 12 / (12 * 60))[1:720]
  min.pos <- seq(0, 12, 12 / 60)[1:60]
  all.hours <- rep(hour.pos, 2)
  all.times <- cbind(all.hours, min.pos, 24)
  
  cur.time <- data.frame(list(
    times = c(all.times[i, 1], all.times[i, 2]),
    hands = c(.5, 1),
    hand_type = c("hour", "minute")
  ))
  
  arrow_spec <- arrow(angle = 30, length = unit(0.25, "inches"),
        ends = "last", type = "open")
  
  ggplot(cur.time, aes(xmin = times, xmax = times + 0.1, ymin = 0, ymax = hands, fill = hand_type)) +
    # geom_rect(alpha = 1) +
    geom_segment(
      aes(x = times, xend = times, y = 0, yend = hands),
      size = 5, color = c("black","red"), alpha = 0.8,lineend = "round"
    ) +
    scale_fill_manual(values = c("hour" = "black", "minute" = "red")) +
    # annotate with AM/PM text
    annotate("text", x = 6, y = 0.5, label = ifelse(hour < 12, "AM", "PM"), size = 6, fontface = "bold") +
    scale_x_continuous(
      limits = c(0, all.hours[length(all.hours)]),
      breaks = 0:11,
      labels = c(12, 1:11)
    ) +
    scale_y_continuous(limits = c(0, 1.1)) +
    theme_bw() +
    coord_polar() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank()
    )
}
create_analog_clock("2025-07-14 11:00:01") |>
  print()


library(gapminder)
library(dplyr)
library(ggplot2)
library(gganimate)

gapminder_interpolated <- gapminder |>
  group_by(country) |>
  tidyr::complete(year = seq(min(gapminder$year), max(gapminder$year), 
                             by = 1)) |>
  tidyr::fill(continent, .direction = "downup") |>
  mutate(lifeExp = approx(year, lifeExp, year, rule = 2)$y,
         pop = approx(year, pop, year, rule = 2)$y,
         gdpPercap = approx(year, gdpPercap, year, rule = 2)$y
  ) |>
  ungroup()

ggplot(gapminder_interpolated,
       aes(gdpPercap, lifeExp, size = pop, color = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  geom_text(aes(x = 1000, y = 80, label = as.character(year)),
            hjust = 0, vjust = 1, size = 3.5, color = 'grey50') +
  labs(title = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'life expectancy') +
  transition_time(as.integer(year)) +
  ease_aes('linear')
