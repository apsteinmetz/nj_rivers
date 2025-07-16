# visualize stream data
library(tidyverse)
library(grid)
library(lubridate)
library(gganimate)
library(cowplot)

# load metadata
#load river_data
load(file = "data/nj_river_sites_metadata.RData")
load(file = "data/nj_river_data.RData")


# plot elevation for all sites
sites_metadata |> 
  arrange(desc(altitude))|>
  mutate(site_name = as_factor(site_name)) |> 
  ggplot(aes(y = altitude, x = site_name,color=basin_name,group = basin_name)) +
  # facet_grid(basin_name ~ ., scales = "free_y") +
  # remove x axis text
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_line() +
  labs(title = "Elevation of NJ River Sites",
       y = "Elevation (ft)",
       x = "Site Name")

# join stream data with metadata
stream_data_full <- stream_data |> 
  # remove rows with NA in gage_height_ft
  filter(datetime >= as.POSIXct("2025-07-14 12:00:00")) |>
  filter(minute(datetime) %% 15 == 0) |> 
  filter(!is.na(gage_height_ft)) |>
  # reduce rows to every 15 minutes
  # complete missing datetime rows, filling the other columns with the previous value
  complete(site_number, datetime) |> 
  group_by(site_number) |>
  fill(where(is.numeric), .direction = "down") |>
  left_join(sites_metadata, by = "site_number") |> 
  select(basin_name, map_name, site_name,everything()) |> 
  select(-basin_code, -site_number) |> 
  arrange(desc(altitude))|>
  mutate(site_name = as_factor(site_name)) |> 
  mutate(basin_name = as_factor(basin_name)) |> 
  # add a column for gage height change from initial value
  ungroup() |> 
  arrange(datetime) |>
  group_by(site_name) |>
  mutate(gage_height_change = gage_height_ft - first(gage_height_ft)) |>
  as_tibble() 


plot_gage_height <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[1000]) {
  stream_data_full |> 
    # filter by basin and time of day
    filter(datetime == this_time) |> 
    filter(basin_name == basin) |>
    ggplot(aes(x = site_name, y = gage_height_change)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Gage Height Change From Baseline-", basin),
         subtitle = this_time,
         x = "< - Upstream - Downstream ->",
         y = "Gage Height (ft)") +
    # ylim to max range of gage height change
    ylim(0,max(stream_data_full$gage_height_change+1))
    
}


plot_gage_height()
# create an animation of plot_gage_height where each frame is a different time
plot_gage_height_animation <- function(basin = stream_data_full$basin_name[2]) {
  stream_data_full |> 
    filter(basin_name == basin) |>
    ggplot(aes(x = site_name, y = gage_height_change, group = datetime, fill = site_name)) +
    geom_col() +
    scale_fill_viridis_d() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # remove legend
    theme(legend.position = "none") +
    labs(title = paste("River Heights -", basin),
         x = "Site Name",
         y = "River Height Increase(ft)") +
    # add time to title
    labs(subtitle = "Time: {format(frame_time, '%Y-%m-%d %H:%M:%S')}") +
    #annotation_custom(create_analog_clock_grob("{format(frame_time, '%Y-%m-%d %H:%M:%S')}"), 
    #                  xmin = 5, xmax = 9, ymin = 8, ymax = 12) +
  
    transition_time(datetime) +
    # transition_reveal(datetime) +
    ease_aes('linear')
}

plot_gage_height_animation(basin = "Passaic River Basin") |> 
  animate(nframes = length(unique(stream_data_full$datetime)), fps = 10, width = 800, height = 600)

# create a grob that is a digital clock
create_digital_clock <- function(time) {
  time_str <- format(time, "%H:%M:%S")
  grid::textGrob(time_str, gp = grid::gpar(fontsize = 20, fontface = "bold"), 
                 just = "right", x = unit(1, "npc") - unit(0.05, "inches"), 
                 y = unit(1, "npc") - unit(0.05, "inches"))
}
# create a plot with a digital clock in the top right corner
plot_gage_height_with_clock <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[2]) {
  p <- plot_gage_height(basin, this_time)
  
  # create a digital clock
  clock_grob <- create_digital_clock(this_time)
  
  # add the clock to the plot
  p + annotation_custom(clock_grob, xmin = Inf, xmax = Inf, ymin = Inf, ymax = Inf)
}
plot_gage_height_with_clock(basin = "Passaic River Basin", this_time = stream_data_full$datetime[2])


# create a grob with a circular analog 12-hour clock
create_analog_clock <- function(this_time_str = "2025-07-14 01:00:00") {
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
      size = 3, color = c("black","red"), alpha = 0.8,lineend = "round"
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
# test
grid.newpage()
clock_grob <- create_analog_clock("2025-07-14 13:00:01")

clock_grob |> 
  grid::grid.draw()


# create a plot with an analog clock in the top right corner
plot_gage_height_with_analog_clock <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[100]) {
  p <- plot_gage_height(basin, this_time)
  
  # create an analog clock
  clock <- create_analog_clock(this_time)
  clock_grob <- ggplotGrob(clock)
  # add the clock to the plot, p, as an inset
  p <- p + 
    annotation_custom(
      grob = clock_grob, 
      xmin = 0, xmax =4, ymin = 8, ymax = 12
    )
p
}
gg <- plot_gage_height_with_analog_clock(basin = "Passaic River Basin", this_time = stream_data_full$datetime[1000])
gg
# save the plot with the analog clock
ggsave("img/gage_height_with_analog_clock.png", plot = gg, width = 10, height = 8)


# plot gage height over time for all sites in raritan basin
plot_gage_height_over_time <- function(basin = stream_data_full$basin_name[2]) {
  stream_data_full |> 
    filter(basin_name == basin) |>
    ggplot(aes(x = datetime, y = gage_height_change, color = site_name)) +
    geom_line() +
    labs(title = paste("Gage Height Over Time -", basin),
         x = "Datetime",
         y = "Gage Height Change(ft)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
plot_gage_height_over_time("Rahway River Basin")
