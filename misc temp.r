# Install required libraries if needed
# install.packages(c("tidyverse", "jsonlite", "httr", "lubridate"))

library(tidyverse)
library(jsonlite)
library(httr)
library(lubridate)

# ==========================================
# 0. CREATE DIRECTORIES IF THEY DON'T EXIST
# ==========================================
if (!dir.exists("csv")) {
  dir.create("csv")
}
if (!dir.exists("images")) {
  dir.create("images")
}

# ==========================================
# 1. FETCH COLUMBUS WEATHER DATA (1979 - 2021)
# ==========================================
cat("Fetching climate data from NOAA ACIS API for Columbus, OH...\n")

api_url <- "http://data.rcc-acis.org/StnData"
query_params <- list(
  sid = "USW00014821", # John Glenn Columbus International Airport (CMH)
  sdate = "1979-01-01",
  edate = "2021-12-31",
  elems = "maxt"       # Maximum temperature
)

response <- POST(api_url, body = query_params, encode = "json")
data_json <- content(response, "parsed")

# Robust unpacking using map_chr to avoid unnest_wide version conflicts
weather_df <- tibble(
  date_raw = map_chr(data_json$data, ~ .x[[1]]),
  max_temp_raw = map_chr(data_json$data, ~ .x[[2]])
) %>%
  mutate(
    date = as.Date(date_raw),
    max_temp = as.numeric(max_temp_raw)
  ) %>%
  filter(!is.na(max_temp))

# ==========================================
# 2. OUTPUT ORGANIZED RAW DATA TO CSV FOLDER
# ==========================================
csv_filename <- "csv/01_columbus_temperatures_1979_2021.csv"
write_csv(weather_df, csv_filename)
cat("Raw historical weather data successfully saved to:", csv_filename, "\n")

# ==========================================
# 3. PREPARE GEOMETRY FOR THE PLOTS
# ==========================================
plot_df <- weather_df %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    # Map everyone to a proxy timeline year (2020 handles leap day alignments)
    dummy_date = as.Date(paste(2020, month, day, sep = "-"))
  ) %>%
  filter(!is.na(dummy_date))

# Isolate the extreme 2021 Heatwave event points ("Last 3 Days")
heatwave_days <- plot_df %>%
  filter(year == 2021, month == 6, day %in% c(26, 27, 28))

historical_data <- plot_df %>%
  anti_join(heatwave_days, by = "date")

# ==========================================
# 4. GRAPH 1: ORIGINAL MAXIMUM TEMPERATURE CLOUD
# ==========================================
plt1 <- ggplot() +
  geom_point(
    data = historical_data, 
    aes(x = dummy_date, y = max_temp), 
    color = "#6c757d", alpha = 0.15, size = 1.1, shape = 16
  ) +
  geom_point(
    data = heatwave_days, 
    aes(x = dummy_date, y = max_temp), 
    color = "#b22222", alpha = 1, size = 3.5, shape = 16
  ) +
  annotate(
    "text", x = as.Date("2020-07-02"), y = 104, 
    label = "Last 3 days", color = "#b22222", fontface = "bold", hjust = 0, size = 4.5
  ) +
  scale_y_continuous(
    limits = c(10, 125), breaks = c(20, 40, 60, 80, 100, 120),
    labels = c("20", "40", "60", "80", "100", "120 deg. Fahrenheit")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b.") +
  labs(title = "Daily maximum temperatures in Columbus, 1979-2021", x = NULL, y = NULL) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#212529", hjust = 0, margin = margin(b = 20)),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "#ebebeb", size = 0.5),
    axis.text.x = element_text(size = 11, color = "#495057"), axis.text.y = element_text(size = 11, color = "#495057"),
    axis.ticks.x = element_line(color = "#212529", size = 0.5), axis.line.x = element_line(color = "#212529", size = 0.6)
  )

ggsave("images/01_columbus_max_temp_distribution.png", plot = plt1, width = 11, height = 8, dpi = 300, bg = "white")

# ==========================================
# 5. GRAPH 2: AVERAGE TEMPERATURES BY MONTH (LINE GRAPH)
# ==========================================
monthly_avg <- plot_df %>%
  group_by(month) %>%
  summarize(avg_max = mean(max_temp, na.rm = TRUE)) %>%
  mutate(dummy_date = as.Date(paste(2020, month, "15", sep = "-")))

plt2 <- ggplot(monthly_avg, aes(x = dummy_date, y = avg_max)) +
  geom_line(color = "#2b6cb0", size = 1.5) +
  geom_point(color = "#2b6cb0", size = 3) +
  scale_y_continuous(limits = c(30, 90), breaks = seq(30, 90, 10)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b.") +
  labs(
    title = "Average Historical Maximum Temperatures by Month",
    subtitle = "Columbus, OH (1979-2021 Mean Baseline)",
    x = NULL, y = "Degrees Fahrenheit"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#212529"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "#212529", size = 0.6)
  )

ggsave("images/02_columbus_monthly_averages.png", plot = plt2, width = 11, height = 8, dpi = 300, bg = "white")

# ==========================================
# 6. GRAPH 3: YEARLY MAXIMUM TEMPERATURE TRENDS (PEAK TRACKING)
# ==========================================
yearly_peaks <- plot_df %>%
  group_by(year) %>%
  summarize(peak_temp = max(max_temp, na.rm = TRUE))

plt3 <- ggplot(yearly_peaks, aes(x = year, y = peak_temp)) +
  geom_line(color = "#4a5568", size = 0.8, alpha = 0.7) +
  geom_point(color = "#e53e3e", size = 2) +
  geom_smooth(method = "lm", color = "#2d3748", linetype = "dashed", se = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  labs(
    title = "Highest Temperature Recorded Each Year",
    subtitle = "Columbus, OH (1979-2021 Peak Spikes with Trendline)",
    x = "Year", y = "Degrees Fahrenheit"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#212529"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "#212529", size = 0.6)
  )

ggsave("images/03_columbus_yearly_max_trends.png", plot = plt3, width = 11, height = 8, dpi = 300, bg = "white")

# ==========================================
# 7. GRAPH 4: ANOMALY DAYS COUNT (DAYS EXCEEDING 90°F BY YEAR)
# ==========================================
anomaly_counts <- plot_df %>%
  # We define an "anomaly day" here as an unusually extreme hot day exceeding 90F
  filter(max_temp >= 90) %>%
  group_by(year) %>%
  count(name = "days_above_90")

# Fill in missing years if any year had 0 days above 90
all_years <- tibble(year = 1979:2021)
anomaly_counts <- all_years %>%
  left_join(anomaly_counts, by = "year") %>%
  replace_na(list(days_above_90 = 0))

plt4 <- ggplot(anomaly_counts, aes(x = year, y = days_above_90)) +
  geom_col(fill = "#dd6b20", width = 0.7) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  labs(
    title = "Annual Anomaly Heat Count",
    subtitle = "Number of Days Per Year Reaching or Exceeding 90°F in Columbus",
    x = "Year", y = "Number of Days"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#212529"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "#212529", size = 0.6)
  )

ggsave("images/04_columbus_anomaly_days_count.png", plot = plt4, width = 11, height = 8, dpi = 300, bg = "white")

cat("All numbered visualization files generated in the 'images/' folder successfully!\n")