# visualize stream data
library(tidyverse)
library(grid)
library(lubridate)
library(gganimate)

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


plot_gage_height <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[100]) {
  stream_data_full |> 
    # filter by basin and time of day
    filter(datetime == this_time) |> 
    filter(basin_name == basin) |>
    ggplot(aes(x = site_name, y = gage_height_ft)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Gage Height -", basin),
         subtitle = this_time,
         x = "Site Name",
         y = "Gage Height (ft)")
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
create_analog_clock_grob <- function(this_time_str = "2025-07-14 12:00:00") {
  print(paste("Creating clock for time:", this_time_str))
  this_time <- as.POSIXct(this_time_str)
  hour <- hour(this_time) %% 12
  minute <- minute(this_time)
  # Calculate angles for hour and minute hands
  hour_angle <- -(hour + minute / 70) * (360 / 12)
  minute_angle <- -minute * (360 / 60)
  # Create a circular clock face
  clock_face <- grid::circleGrob(r = unit(1, "npc") / 2, gp = grid::gpar(fill = "darkgrey", col = "black"))
  hour_numbers <- lapply(1:12, function(i) {
    i_inverse <- 13 - i # Invert the hour number for correct positioning
    angle <- (i * 30) + 60 # Adjusting for 12 o'clock at the top
    x <- 0.5 + 0.3 * cos(pi * angle / 180)
    y <- 0.5 + 0.45 * sin(pi * angle / 180)
    grid::textGrob(as.character(i_inverse), x = x, y = y, 
                   gp = grid::gpar(fontsize = 30, fontface = "bold"))
  })
  # Create hour hand
  hour_hand <- grid::linesGrob(
    x = c(0.5, 0.5 + 0.2 * cos(pi * (hour_angle + 90) / 180)),
    y = c(0.5, 0.5 + 0.2 * sin(pi * (hour_angle + 90) / 180)),
    gp = grid::gpar(col = "white", lwd = 5)
  )
  
  # Create minute hand
  minute_hand <- grid::linesGrob(
    x = c(0.5, 0.5 + 0.25 * cos(pi * (minute_angle+90) / 180)),
    y = c(0.5, 0.5 + 0.45 * sin(pi * (minute_angle+90) / 180)),
    gp = grid::gpar(col = "red", lwd = 3)
  )
  
  # Combine all elements into a single grob
  grid::gTree(children = do.call(gList, c(list(clock_face), hour_numbers, list(hour_hand, minute_hand))))
}

# create a plot with an analog clock in the top right corner
plot_gage_height_with_analog_clock <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[2]) {
  p <- plot_gage_height(basin, this_time)
  
  # create an analog clock
  clock_grob <- create_analog_clock_grob(this_time)
  
  # add the clock to the plot
  p + annotation_custom(clock_grob, xmin = 5, xmax = 9, ymin = 8, ymax = 12)
}
plot_gage_height_with_analog_clock(basin = "Passaic River Basin", this_time = stream_data_full$datetime[200])


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
