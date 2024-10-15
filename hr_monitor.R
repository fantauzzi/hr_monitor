library(ggplot2)
library(tidyr)
library(readr)
library(scales)
library(gridExtra)
library(grid)

filepath = '/home/fanta/Documents/workouts/SleepU 2722_20241015015033.csv'

log =  read_csv(filepath)

# Replace '--' with NA and convert columns to numeric (except Time)
log$`Oxygen Level` = as.numeric(ifelse(log$`Oxygen Level` == '--', NA, log$`Oxygen Level`))
log$`Pulse Rate`   = as.numeric(ifelse(log$`Pulse Rate` == '--', NA, log$`Pulse Rate`))
log$Motion         = as.numeric(ifelse(log$Motion == '--', NA, log$Motion))

# Display the structure of the updated dataframe

# Convert 'Time' column to POSIXct assuming format 'hh:mm:ss dd/mm/yyyy'
log$Time = as.POSIXct(log$Time, format = "%H:%M:%S %d/%m/%Y")

# Reshape the data into long format for faceting
log_long = log %>%
  pivot_longer(
    cols = c(`Pulse Rate`, `Oxygen Level`),
    names_to = "Metric",
    values_to = "Value"
  )

# Calculate the time range in minutes to determine appropriate X-axis ticks
time_range = difftime(max(log$Time), min(log$Time), units = "mins")

# Create the plot
p_lines = ggplot(log_long, aes(x = Time, y = Value, color = Metric)) +
  geom_line() +  # Plot lines for both Pulse Rate and Oxygen Level
  facet_grid(rows = vars(Metric), scales = "free_y") +  # Stack the plots, separate Y-axes
  scale_color_manual(values = c("Pulse Rate" = "red", "Oxygen Level" = "blue")) +  # Assign colors
  theme_minimal() +  # Use minimalist theme
  labs(x = "Time", y = NULL) +  # Label X-axis, remove Y-axis label
  theme(legend.position = "none", # Remove the legend
        strip.text.y = element_blank())  # Remove facet labels

# Adjust the X-axis tick marks based on time range
if (time_range > 90) {
  p_lines = p_lines + scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H:%M"))
} else {
  p_lines = p_lines + scale_x_datetime(breaks = date_breaks("5 mins"), labels = date_format("%H:%M"))
}

# Display the plot
print(p_lines)

# Zone 1 (50–60%): 83–100 bpm
# Zone 2 (60–70%): 100–116 bpm
# Zone 3 (70–80%): 116–133 bpm
# Zone 4 (80–90%): 133–149 bpm
# Zone 5 (90–100%): 149–166 bpm

zones = list(
  zone1 = 83,
  zone2 = 100,
  zone3 = 116,
  zone4 = 133,
  zone5 = 149
)

# Define the breaks for the bins
breaks = c(-Inf,
           zones$zone1,
           zones$zone2,
           zones$zone3,
           zones$zone4,
           zones$zone5,
           Inf)

# Create bin labels
bin_labels = c(
  paste0("<", zones$zone1),
  paste0(zones$zone1, "-", zones$zone2 - 1),
  paste0(zones$zone2, "-", zones$zone3 - 1),
  paste0(zones$zone3, "-", zones$zone4 - 1),
  paste0(zones$zone4, "-", zones$zone5 - 1),
  paste0("≥", zones$zone5)
)

# Classify Pulse Rate values into bins, ignoring NAs
log$Pulse_bin = cut(
  log$`Pulse Rate`,
  breaks = breaks,
  labels = bin_labels,
  include.lowest = TRUE,
  right = FALSE
)

# Remove NA values from the dataset
log_filtered = na.omit(log)

# Define a color palette from yellow to red
color_palette = c("#99FF99",
                  "#FFD966",
                  "#FFA500",
                  "#FF7F50",
                  "#FF4500",
                  "#FF0000")

# Create the histogram
p_hist = ggplot(log_filtered, aes(x = Pulse_bin, fill = Pulse_bin)) +
  geom_bar() +  # Create bars with the fill mapped to Pulse_bin
  scale_x_discrete(drop = FALSE) +  # Ensure all bins are shown, even if they have count 0
  scale_fill_manual(values = color_palette) +  # Manually assign colors to each bin
  theme_minimal() +  # Minimalist theme
  labs(x = "Pulse Rate Bins", y = "Count") +  # Axis labels
  theme(legend.position = "none")  #''''''''''''''''' Remove the legend

print(p_hist)


# Assuming the first time stamp in the log dataframe (Time column)
first_timestamp <- log$Time[1]

# Calculate statistics for Pulse Rate (rounded to 1 decimal place)
pulse_min <- round(min(log$`Pulse Rate`, na.rm = TRUE), 1)
pulse_max <- round(max(log$`Pulse Rate`, na.rm = TRUE), 1)
pulse_mean <- round(mean(log$`Pulse Rate`, na.rm = TRUE), 1)

# Calculate statistics for Oxygen Level (rounded to 1 decimal place)
oxygen_min <- round(min(log$`Oxygen Level`, na.rm = TRUE), 1)
oxygen_max <- round(max(log$`Oxygen Level`, na.rm = TRUE), 1)
oxygen_mean <- round(mean(log$`Oxygen Level`, na.rm = TRUE), 1)

# Convert time_range to a more readable format (minutes)
time_elapsed <- round(as.numeric(time_range, units = "mins"), 1)

# Open a PDF device with A4 dimensions (in inches: 8.27 x 11.69 for A4)
pdf("report.pdf", width = 8.27, height = 11.69)  # A4 dimensions

# Create a grob (graphical object) for the filepath text
filepath_grob <- textGrob(filepath, gp = gpar(fontsize = 12, fontface = "bold"), just = "center")

# Create a grob for the time stamp text (converting to string if necessary)
timestamp_grob <- textGrob(paste("Time of first entry:", first_timestamp), gp = gpar(fontsize = 10), just = "center")

# Create grobs for Pulse Rate and Oxygen Level statistics, and elapsed time
pulse_stats_grob <- textGrob(paste("Pulse Rate - Min:", pulse_min, "Max:", pulse_max, "Mean:", pulse_mean),
                             gp = gpar(fontsize = 10), just = "left", x = unit(0, "npc"))
oxygen_stats_grob <- textGrob(paste("Oxygen Level - Min:", oxygen_min, "Max:", oxygen_max, "Mean:", oxygen_mean),
                              gp = gpar(fontsize = 10), just = "left", x = unit(0, "npc"))
time_elapsed_grob <- textGrob(paste("Time Elapsed:", time_elapsed, "minutes"),
                              gp = gpar(fontsize = 10), just = "left", x = unit(0, "npc"))

# Arrange the elements in the layout
arranged_grobs <- arrangeGrob(
  filepath_grob,            # File path at the top
  timestamp_grob,           # Time stamp on the next line
  p_lines,                  # First plot (line plot)
  p_hist,                   # Second plot (histogram)
  pulse_stats_grob,         # Pulse Rate stats
  oxygen_stats_grob,        # Oxygen Level stats
  time_elapsed_grob,        # Time elapsed
  nrow = 7,                 # Use 7 rows for the layout
  heights = c(0.1, 0.05, 0.4, 0.4, 0.05, 0.05, 0.05)  # Allocate relative heights
)

# Create a grob that adds margins around the arranged content
# Use `unit()` to specify the top, right, bottom, and left margins
margins_grob <- gTree(children = gList(arranged_grobs), vp = viewport(width = 0.85, height = 0.85))

# Draw the arranged grobs with margins on the PDF
grid.draw(margins_grob)

# Close the PDF device
dev.off()
