library(ggplot2)

# Define a function that returns multiple annotate layers as a list
my_annotations <- function(label_positions, text_color = "blue", font_size = 4) {
  lapply(label_positions, function(pos) {
    annotate("text", x = pos$x, y = pos$y, label = pos$label, 
             color = text_color, size = font_size)
  })
}

# Example usage
label_data <- list(
  list(x = 2, y = 4, label = "Point A"),
  list(x = 5, y = 7, label = "Point B"),
  list(x = 8, y = 2, label = "Point C")
)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  my_annotations(label_data)


custom_column <- function(this_time = times[1], inset = inset_loc) {
  
  size_precip_bar <- function(this_time = times[1], rainfall = cum_rainfall,inset = inset_loc) {
    # scale the cumulative precipitation to a range of 0 to 1
    inset_range <- inset_loc[4] - inset_loc[2] - .01
    scaled <- pull(filter(rainfall,datetime == this_time),precip_cum)/max(rainfall$precip_cum)*inset_range
    return(scaled)
  }
  
list(
  # add a narrow rectangle to the right of the  annotation proportional to the height of the cumulative rainfall
  annotate("rect",
           xmin = inset[3],
           xmax = inset[3] + 0.05,
           ymin = inset[2], 
           ymax = inset[2] + size_precip_bar(this_time),
           fill = "navy", alpha = .9),
  # draw a white box around the text
  annotate("rect",
           xmin = inset[3] - .02,
           xmax = inset[3] + 0.07,
           ymin = inset[2] - 0.05, 
           ymax = inset[2] ,
           color = "black",
           fill = "white", alpha = 1),
  annotate("text",
           x = inset[3] + 0.025,
           y = inset[2] - 0.01,
           label = paste0("Rain At EWR\n",
                          pull(filter(cum_rainfall,datetime == this_time),precip_cum),
                          " in."),
           fontface = "bold",
           hjust = 0.5,
           vjust = 1,
           size = 3)
)
}
custom_column()
