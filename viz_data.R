# visualize stream data
library(tidyverse)
library(grid)
library(lubridate)
# library(gganimate)
library(cowplot)
library(magick)
library(ggmap)
library(ggforce)

# load metadata
#load river_data
load(file = "data/nj_river_sites_metadata.RData")
load(file = "data/nj_river_data.RData")

# plot elevation for all sites
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
  ungroup() |> 
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

# ==============================================================================

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

plot_gage_height <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[1000]) {
  stream_data_full |> 
    # filter by basin and time of day
    filter(datetime == this_time) |> 
    filter(basin_name == basin) |>
    ggplot(aes(x = site_name, y = gage_height_change)) +
    geom_col(fill = "navy") +
    theme_minimal_hgrid(color = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # panel background color is dark green
    theme(panel.background = element_rect(fill = "lightgreen", color = "black")) +
    # plot background color is light green
    theme(plot.background = element_rect(fill = "lightgrey", color = "black")) +
    labs(title = paste(basin,"- Gage Height Change From Baseline"),
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

#plot_gage_height_animation(basin = "Passaic River Basin") |> 
#  animate(nframes = length(unique(stream_data_full$datetime)), fps = 10, width = 800, height = 600)

# ==============================================================================

# create a grob with a circular analog 12-hour clock
# adapted from code by Drew Conway
# https://www.r-bloggers.com/2011/08/create-an-animated-clock-in-r-with-ggplot2-and-ffmpeg/
create_analog_clock <- function(this_time_str = "2025-07-14 01:00:00") {
  this_time <- as.POSIXct(this_time_str)
  hour <- lubridate::hour(this_time)
  minute <- lubridate::minute(this_time)
  dow <- format(this_time,"%a")
  i <- hour * 60 + minute + 1
  
  # one minute granularity
  hour.pos <- seq(0, 12, 12 / (12 * 60))[1:720]
  min.pos <- seq(0, 12, 12 / 60)[1:60]
  all.hours <- rep(hour.pos, 2)
  all.times <- cbind(all.hours, min.pos, 24)
  
  cur.time <- data.frame(list(
    times = c(all.times[i, 1], all.times[i, 2]),
    hands = c(1.2,1), # thickness of hand
    hand_type = c("hour", "minute")
  ))
  #arrow_spec <- arrow(angle = 30, length = unit(0.25, "inches"),
  #                    ends = "last", type = "open")

    ggplot(cur.time, aes(xmin = times, xmax = times + 0.1, ymin = 0, ymax = hands, fill = hand_type)) +
    # geom_rect(alpha = 1) +
      # annotate with AM/PM text
      annotate("text", x = 6, y = 0.5, 
               label = paste0(dow," - ",ifelse(hour < 12, "AM", "PM")),
               fontface = "bold", size = 3) +
      scale_x_continuous(
        limits = c(0, all.hours[length(all.hours)]),
        breaks = 0:11,
        labels = c(12, 1:11)
      ) +
      geom_segment(
      data = filter(cur.time, hand_type == "hour"),
      aes(x = times, xend = times, y = 0, yend = .9),linewidth = 2,
      color = "darkgrey", alpha = .8, lineend = "round"
    ) +
    geom_segment(
      data = filter(cur.time, hand_type == "minute"),
      aes(x = times, xend = times, y = 0, yend = 1),linewidth = 1,
      color = "red", alpha = 0.6, lineend = "round"
    ) +
    scale_fill_manual(values = c("hour" = "black", "minute" = "red")) +
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
# ==============================================================================
# # test
# grid.newpage()
# clock_grob <- create_analog_clock("2025-07-14 12:00")
# 
# clock_grob |> 
#   grid::grid.draw()
# 
# 
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

# gg <- plot_gage_height_with_analog_clock()
# gg
# ggsave(plot_gage_height_with_analog_clock(), filename = "img/aa_test.png", width = 10, height = 8)

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
# plot_gage_height_over_time("Raritan River Basin")

reduce_image_size <- function(fname, scale = 0.5) {
  img <- image_read(fname)
  img <- image_scale(img, paste0(scale * 100, "%"))
  image_write(img, fname)
}

save_gage_height_plot <- function(basin = stream_data_full$basin_name[2], this_time = stream_data_full$datetime[100]) {
  gg <- plot_gage_height(basin, this_time)
  
  # create an analog clock
  clock <- create_analog_clock(this_time)
  clock_grob <- ggplotGrob(clock)
  # add the clock to the plot, p, as an inset
  gg <- gg+ 
    annotation_custom(
      grob = clock_grob, 
      xmin = 0, xmax =4, ymin = 8, ymax = 12
    )
  
  # compute the number of minutes between this_time and the first time in the dataset
  time_diff <- sprintf("%04d", as.numeric(difftime(this_time, min(stream_data_full$datetime), units = "mins")))
  
  # save gg with the filename pattern "img/gage_height_change_<basin>_<time>.png"
  filename <- paste0("img/gage_height_change_",basin,"_",time_diff,".png")
  print(filename)
  # save the plot as filename
 ggsave(gg, filename = filename, width = 10, height = 8)
 reduce_image_size(filename)
 
}


# load fname an reduce pixel size by 50%, then resave

# combine files into a gif
combine_gage_height_plots <- function(basin = BASIN) {
  fps <- 5
  # get all files in img directory that match the pattern
  files <- list.files("img", pattern = paste0("gage_height_change_", basin,".*.png"), full.names = TRUE)
  
  # read the images
  cat("reading ",basin," images from", length(files), "files...\n")
  images <- image_read(files)
  
  # combine into a gif
  cat("combining images into gif...\n")
  gif <- image_animate(images, fps = fps)
  
  # save the gif
  cat("saving gif...\n")
  image_write(gif, paste0("gif/gage_height_change_", basin, ".gif"))
  
  cat("Saved GIF to img/gage_height_change_", basin, ".gif\n")
}

# ==============================================================================
basins <- levels(stream_data_full$basin_name)
times <- stream_data_full |> 
  filter(datetime >= as.POSIXct("2025-07-14 16:00:00")) |>
  filter(datetime <= as.POSIXct("2025-07-15 08:00:00")) |> 
  pull(datetime) |> 
  unique()

# read weather data from csv
rainfall <- read_csv("data/EWR_weather_2025-07-14.csv") |> 
  select(datetime, precip) |> 
  # set datetime to US/Eastern timezone
  mutate(datetime = with_tz(datetime, tzone = "US/Eastern")) |> 
  # change datetime to nearest 15 minutes
  rename(datetime_orig = datetime) |>
  mutate(datetime = round_date(datetime_orig, "15 minutes")) |> 
  # add a column for cumulative precipitation
  mutate(precip_cum = cumsum(precip))
  

cum_rainfall <- enframe(times,value = "datetime",name = NULL)  |> 
  left_join(rainfall, by = "datetime") |> 
  arrange(datetime) |> 
  fill(precip_cum, .direction = "down") |>
  # replace NA values in precip_cum with 0
  mutate(precip_cum = ifelse(is.na(precip_cum), 0, precip_cum)) |>
  select(datetime, precip_cum)
  
  
# function to return bbox from extent of sites_metadata as a parameter
get_bbox_from_metadata <- function(metadata = sites_metadata) {
  bbox <- c(
    min(metadata$longitude, na.rm = TRUE),
    min(metadata$latitude, na.rm = TRUE),
    max(metadata$longitude, na.rm = TRUE),
    max(metadata$latitude, na.rm = TRUE)
  )
  return(bbox)
}

# function to locate a smaller box in bbox with with a certain percentage size and a text quadrant parameter with "topright" as the default
get_inset_loc <- function(bbox, size = 0.2, quadrant = "topleft") {
  width <- (bbox[3] - bbox[1]) * size
  height <- (bbox[4] - bbox[2]) * size
  
  loc <- if (quadrant == "topright") {
    return(c(bbox[3] - width, bbox[4] - height, bbox[3], bbox[4]))
  } else if (quadrant == "topleft") {
    return(c(bbox[1], bbox[4] - height, bbox[1] + width, bbox[4]))
  } else if (quadrant == "bottomright") {
    return(c(bbox[3] - width, bbox[2], bbox[3], bbox[2] + height))
  } else if (quadrant == "bottomleft") {
    return(c(bbox[1], bbox[2], bbox[1] + width, bbox[2] + height))
  } else {
    stop("Invalid quadrant specified. Use 'topright', 'topleft', 'bottomright', or 'bottomleft'.")
  }
  return(loc)
}


# function to compute the geographic center of each basin
get_basin_centers <- function(metadata) {
  metadata |> 
    group_by(basin_name) |> 
    summarise(
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE)
    ) |> 
    # mutate basin name to remove "river" and put a new line before Basin
    mutate(basin_name = str_replace_all(basin_name, " River ", "\n"))
}

get_metadata_center <- function(metadata = sites_metadata) {
  metadata |> 
    summarise(
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE)
    )
}
get_metadata_center()

bbox <- get_bbox_from_metadata(sites_metadata)
inset_loc <- bbox |>  get_inset_loc()

size_precip_bar <- function(this_time = times[1], rainfall = cum_rainfall,inset = inset_loc) {
  # scale the cumulative precipitation to a range of 0 to 1
  inset_range <- inset_loc[4] - inset_loc[2]
  scaled <- pull(filter(rainfall,datetime == this_time),precip_cum)/max(rainfall$precip_cum)*inset_range
  return(scaled)
}

# get basemap

# basemap1 <- get_stadiamap(bbox = bbox,
#                         zoom = 10, maptype = "stamen_terrain")
basemap2 <- get_stadiamap(bbox = bbox,
                          zoom = 10, maptype = "alidade_bright")
#basemap3 <- get_googlemap(center = as.numeric(paste(get_metadata_center())),
#                          zoom = 10, maptype = "roadmap")

map_gage_height <- function(this_time = stream_data_full$datetime[1]) {
  gg <- ggmap(basemap2) +
    coord_cartesian() +
    # add the clock to the plot as an inset
    annotation_custom(
      grob = ggplotGrob(create_analog_clock(this_time)),
      # use inset_loc to position annotation
      xmin = inset_loc[1], xmax = inset_loc[3],
      ymin = inset_loc[2],
      ymax = inset_loc[4]) +

    geom_point(
      data = stream_data_full |> filter(datetime == this_time),
      aes(
        x = longitude,
        y = latitude,
        size = gage_height_change,
        color = basin_name
      ),
      alpha = .9
    ) +
    scale_size_continuous(range = c(1, 25), limits = c(0, max(stream_data_full$gage_height_change))) + 
    ggforce::geom_mark_hull(
      data = sites_metadata,
      aes(x = longitude, y = latitude, group = basin_name, fill = basin_name),
      concavity = 2,
      expand = unit(.3, "cm"),
      alpha = 0.1
    ) +
    geom_text(
      data = get_basin_centers(sites_metadata),
      aes(x = longitude, y = latitude, label = basin_name),
      size = 5,
      hjust = -0.1
    ) +
    # add a narrow rectangle to the right of the  annotation proportional to the height of the cumulative rainfall
    annotate("rect",
             xmin = inset_loc[3],
             xmax = inset_loc[3] + 0.05,
             ymin = inset_loc[2], 
             ymax = inset_loc[2] + size_precip_bar(this_time),
             fill = "navy", alpha = .9) +
    # draw a white box around the text
    annotate("rect",
             xmin = inset_loc[3] - .02,
             xmax = inset_loc[3] + 0.07,
             ymin = inset_loc[2] - 0.05, 
             ymax = inset_loc[2] - 0.01,
             fill = "white", alpha = 1) +
    annotate("text",
             x = inset_loc[3] + 0.025,
             y = inset_loc[2] - 0.01,
             label = paste0("Rain At EWR\n",
                            pull(filter(cum_rainfall,datetime == this_time),precip_cum),
                            " in."),
             fontface = "bold",
             hjust = 0.5,
             vjust = 1,
             size = 3) +
    # end of precipitation bar drawing
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    # add a title
    labs(title = "Gage Height Change",
         subtitle = paste("Time:", this_time),
         caption = "Data from USGS and NJDEP")
  gg + coord_sf()
}

map_gage_height(this_time = times[2])

# save the map with gage height change for a specific time
save_map_gage_height <- function(this_time = stream_data_full$datetime[1000]) {
  cat("Saving...\n")
  gg <- map_gage_height(this_time)
  
  # compute the number of minutes between this_time and the first time in the dataset
  time_diff <- sprintf("%04d", as.numeric(difftime(this_time, min(times), units = "mins")))
  
  # save gg with the filename pattern "img/map_gage_height_change_<time>.png"
  filename <- paste0("img/map_gage_height_change_", time_diff, ".png")
  print(filename)
  # save the plot as filename
  cat("Saving...\n")
  ggsave(gg, filename = filename, width = 10, height = 8)
  cat("Reducing File Size...\n")
  reduce_image_size(filename)
}

walk(times,save_map_gage_height)

# combine files into a gif
combine_gage_height_maps <- function() {
  fps <- 5
  # get all files in img directory that match the pattern
  files <- list.files("img", pattern = ("map.*.png"), full.names = TRUE)
  
  # read the images
  cat("reading ", length(files), "files...\n")
  images <- image_read(files)
  
  # combine into a gif
  cat("combining images into gif...\n")
  gif <- image_animate(images, fps = fps)
  
  # save the gif
  cat("saving gif...\n")
  image_write(gif, paste0("gif/gage_height_map.gif"))
  
  cat("Saved GIF to img/gage_height_map.gif\n")
}

# combine
combine_gage_height_maps()

  
# convert gif to mp4 using external ffmpeg
system("ffmpeg -i gif/gage_height_map.gif -movflags faststart -pix_fmt yuv420p -vf scale=1280:-1 gif/gage_height_map.mp4")




# create a leaflet map of metadata sites
library(leaflet)

# Create color palette for basins
pal <- colorFactor(palette = "Set1", domain = sites_metadata$basin_name)

leaflet(sites_metadata) |> 
  addTiles() |> 
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    radius = ~altitude / 100,
    color = ~pal(basin_name), fillOpacity = 0.5,
    popup = ~paste0("<strong>Site Name:</strong> ", site_name, "<br>",
                    "<strong>Basin:</strong> ", basin_name, "<br>",
                    "<strong>Altitude:</strong> ", altitude, " ft")
  ) |> 
  addLegend("bottomright", pal = pal, values = ~basin_name,
            title = "Basin", opacity = 1)


