rm(list = ls(all.names = TRUE))

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(zoo)
  library(plotly)
  library(htmlwidgets)
  library(htmltools)
  library(gridExtra)
  library(usethis)
  library(epiR)
})

# ----------------------------
# User options
# ----------------------------
agg_mode <- "rolling"   # "rolling" or "monthly"
k_days   <- 28          # rolling window if agg_mode == "rolling"

# FFT y-axis limits (temperature spectrum, ggplot only)
fft_ymin <- 0
fft_ymax <- 100

# Restrict FFT periods to (0, 700)
max_period <- 700

# Cincinnati, OH
lat <- 39.1031
lon <- -84.5120
tz  <- "America/New_York"

end_date   <- Sys.Date()
start_date <- as.Date("2015-12-01")

season_colors <- c(
  Winter = "#3A6EA5",
  Spring = "#4AA96C",
  Summer = "#E1A100",
  Fall   = "#C17F59"
)

# ----------------------------
# Helper: fetch daily series from Open-Meteo ERA5
# ----------------------------
get_daily_series <- function(lat, lon, start_date, end_date, what, tz = "UTC") {
  base_daily  <- "https://archive-api.open-meteo.com/v1/era5"
  base_hourly <- "https://archive-api.open-meteo.com/v1/archive"
  
  daily_param <- switch(
    what,
    "rain"     = "precipitation_sum",
    "temp"     = "temperature_2m_mean",
    "humidity" = "relative_humidity_2m_mean",
    stop("Invalid 'what'. Use 'temp', 'rain', or 'humidity'.")
  )
  
  daily_url <- sprintf(
    paste0("%s?latitude=%s&longitude=%s&start_date=%s&end_date=%s",
           "&daily=%s&temperature_unit=fahrenheit&timezone=%s"),
    base_daily, lat, lon, start_date, end_date, daily_param, URLencode(tz)
  )
  
  dat <- fromJSON(daily_url)
  
  has_daily <- !is.null(dat$daily) && length(dat$daily$time) > 0
  has_var   <- has_daily && !is.null(dat$daily[[daily_param]])
  
  if (!has_daily || !has_var) {
    stop("No daily data returned or variable missing for '", what, "'.")
  }
  
  tibble(
    date  = as.Date(dat$daily$time),
    value = as.numeric(dat$daily[[daily_param]])
  )
}

# ----------------------------
# Retrieve daily temp and rain
# ----------------------------
daily_temp <- get_daily_series(lat, lon, start_date, end_date, what = "temp", tz = tz)
daily_rain <- get_daily_series(lat, lon, start_date, end_date, what = "rain", tz = tz)

# ----------------------------
# Aggregate: monthly means OR n-day rolling means
# ----------------------------
if (agg_mode == "monthly") {
  monthly_temp <- daily_temp %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(
      temp_series = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(time = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
    select(time, temp_series)
  
  monthly_rain <- daily_rain %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(
      rain_series = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(time = as.Date(sprintf("%04d-%02d-01", year, month))) %>%
    select(time, rain_series)
  
  df <- monthly_temp %>%
    inner_join(monthly_rain, by = "time") %>%
    arrange(time)
  
  main_title <- "Cincinnati weather: monthly mean temperature and rainfall"
  ylab_temp  <- "Monthly mean temperature (°F)"
  ylab_rain  <- "Monthly mean rainfall (mm)"
  
} else if (agg_mode == "rolling") {
  roll_temp <- daily_temp %>%
    arrange(date) %>%
    mutate(temp_series = zoo::rollmean(value, k_days, align = "right", fill = NA_real_)) %>%
    select(time = date, temp_series)
  
  roll_rain <- daily_rain %>%
    arrange(date) %>%
    mutate(rain_series = zoo::rollmean(value, k_days, align = "right", fill = NA_real_)) %>%
    select(time = date, rain_series)
  
  df <- roll_temp %>%
    inner_join(roll_rain, by = "time") %>%
    filter(!is.na(temp_series) & !is.na(rain_series)) %>%
    arrange(time)
  
  main_title <- sprintf("Cincinnati weather: %d-day rolling temperature and rainfall", k_days)
  ylab_temp  <- sprintf("%d-day rolling mean temperature (°F)", k_days)
  ylab_rain  <- sprintf("%d-day rolling mean rainfall (mm)", k_days)
  
} else {
  stop("agg_mode must be 'monthly' or 'rolling'.")
}

# ----------------------------
# Season classification (for shading)
# ----------------------------
season_df <- df %>%
  mutate(
    month_num = month(time),
    season = case_when(
      month_num %in% c(12, 1, 2)  ~ "Winter",
      month_num %in% c(3, 4, 5)   ~ "Spring",
      month_num %in% c(6, 7, 8)   ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  arrange(time) %>%
  mutate(
    season_change = season != lag(season, default = first(season)),
    grp = cumsum(season_change)
  ) %>%
  group_by(season, grp) %>%
  summarise(
    xmin = min(time),
    xmax = max(time),
    .groups = "drop"
  )

if (agg_mode == "monthly") {
  season_df <- season_df %>%
    mutate(xmax = xmax %m+% months(1) - days(1))
}

# ----------------------------
# Time-series axis ranges
# ----------------------------
temp_min <- 20
temp_max <- 80
rain_min <- 0
rain_max <- 20

# ----------------------------
# ggplot: main time series (static)
# ----------------------------
p_main <- ggplot() +
  geom_rect(
    data = season_df,
    aes(xmin = xmin, xmax = xmax, ymin = temp_min, ymax = temp_max, fill = season),
    alpha = 0.12,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = df,
    aes(x = time, y = temp_series, color = "Temperature"),
    linewidth = 0.9
  ) +
  geom_line(
    data = df,
    aes(
      x = time,
      y = (rain_series - rain_min) / (rain_max - rain_min) * (temp_max - temp_min) + temp_min,
      color = "Rainfall"
    ),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  scale_fill_manual(values = season_colors, name = "Season") +
  scale_color_manual(
    values = c("Temperature" = "black", "Rainfall" = "darkblue"),
    name   = NULL
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand      = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = ylab_temp,
    limits = c(temp_min, temp_max),
    sec.axis = sec_axis(
      ~ (.-temp_min) / (temp_max - temp_min) * (rain_max - rain_min) + rain_min,
      name = ylab_rain
    )
  ) +
  labs(
    title    = main_title,
    subtitle = sprintf("From %s to %s",
                       format(min(df$time), "%Y-%m-%d"),
                       format(max(df$time), "%Y-%m-%d")),
    x        = NULL,
    caption  = "Source: Open-Meteo ERA5 archive"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank()
  )

# ----------------------------
# Hanning-windowed FFT on aggregated series
# ----------------------------
x_temp <- df$temp_series
x_rain <- df$rain_series
N <- length(x_temp)

if (length(x_rain) != N) stop("Temp and rain series must have same length.")
if (N < 4) stop("Time series too short for meaningful spectrum.")

n <- 0:(N - 1)
hanning <- 0.5 - 0.5 * cos(2 * pi * n / (N - 1))

x_temp_win <- x_temp * hanning
x_rain_win <- x_rain * hanning

X_temp <- fft(x_temp_win)
X_rain <- fft(x_rain_win)

k_idx <- 2:floor(N / 2)
j <- k_idx - 1

freq <- j / N
period_samples <- 1 / freq

if (agg_mode == "monthly") {
  period_x    <- period_samples          # months
  xlab_period <- "Period (months)"
} else {
  period_x    <- period_samples          # days
  xlab_period <- "Period (days)"
}

P_temp <- 2 * (Mod(X_temp[k_idx])^2) / N^2
P_rain <- 2 * (Mod(X_rain[k_idx])^2) / N^2

spec_df <- tibble(
  period = period_x,
  P_temp = P_temp,
  P_rain = P_rain
) %>%
  filter(is.finite(period), period > 0, period <= max_period)

# -------- ggplot FFT (physical units, dual axis) ----------
temp_min_spec <- min(spec_df$P_temp, na.rm = TRUE)
temp_max_spec <- max(spec_df$P_temp, na.rm = TRUE)
rain_min_spec <- min(spec_df$P_rain, na.rm = TRUE)
rain_max_spec <- max(spec_df$P_rain, na.rm = TRUE)

if (!is.finite(rain_max_spec - rain_min_spec) || (rain_max_spec - rain_min_spec) == 0) {
  spec_df <- spec_df %>%
    mutate(P_rain_scaled = temp_min_spec)
} else {
  spec_df <- spec_df %>%
    mutate(
      P_rain_scaled = (P_rain - rain_min_spec) /
        (rain_max_spec - rain_min_spec) *
        (temp_max_spec - temp_min_spec) + temp_min_spec
    )
}

p_spec <- ggplot(spec_df, aes(x = period)) +
  geom_line(aes(y = P_temp,        color = "Temperature")) +
  geom_line(aes(y = P_rain_scaled, color = "Rainfall"), linetype = "dashed") +
  scale_color_manual(
    values = c("Temperature" = "black", "Rainfall" = "darkblue"),
    name   = NULL
  ) +
  scale_x_reverse(
    name   = xlab_period,
    limits = c(max_period, 0),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name   = "Spectral density (temperature)",
    limits = c(fft_ymin, fft_ymax),
    sec.axis = sec_axis(
      ~ (.-fft_ymin) / (fft_ymax - fft_ymin) *
        (rain_max_spec - rain_min_spec) + rain_min_spec,
      name = "Spectral density (rainfall)"
    )
  ) +
  labs(
    title = "Hanning-windowed Fourier spectra of aggregated temp & rainfall",
    subtitle = if (agg_mode == "monthly")
      "FFT input: monthly means"
    else
      sprintf("FFT input: %d-day rolling means", k_days)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank()
  )

# -------- Plotly FFT: normalized spectra in [0,1] (clearly linear) --------
spec_df_norm <- spec_df %>%
  mutate(
    temp_norm = if (all(is.na(P_temp))) NA_real_ else
      (P_temp - min(P_temp, na.rm = TRUE)) /
      (max(P_temp, na.rm = TRUE) - min(P_temp, na.rm = TRUE)),
    rain_norm = if (all(is.na(P_rain))) NA_real_ else
      (P_rain - min(P_rain, na.rm = TRUE)) /
      (max(P_rain, na.rm = TRUE) - min(P_rain, na.rm = TRUE))
  )

# ============================================================
# Plotly figure with true dual axes (time-series) + normalized FFT
# ============================================================

# Seasonal shading as Plotly shapes for top panel
shapes_main <- lapply(seq_len(nrow(season_df)), function(i) {
  s <- season_df$season[i]
  list(
    type      = "rect",
    xref      = "x",
    yref      = "y",
    x0        = season_df$xmin[i],
    x1        = season_df$xmax[i],
    y0        = temp_min,
    y1        = temp_max,
    fillcolor = season_colors[[s]],
    opacity   = 0.12,
    line      = list(width = 0),
    layer     = "below"
  )
})

fig <- plot_ly()

# Top panel: temperature (left axis) and rainfall (right axis)
fig <- fig %>%
  add_lines(
    data = df,
    x    = ~time,
    y    = ~temp_series,
    name = "Temperature (°F)",
    xaxis = "x",
    yaxis = "y",
    line  = list(color = "black")
  ) %>%
  add_lines(
    data = df,
    x    = ~time,
    y    = ~rain_series,
    name = "Rainfall (mm)",
    xaxis = "x",
    yaxis = "y2",
    line  = list(color = "darkblue", dash = "dash")
  )

# Bottom panel: normalized FFT temp (left) and rain (right), period (0,700)
fig <- fig %>%
  add_lines(
    data = spec_df_norm,
    x    = ~period,
    y    = ~temp_norm,
    name = "Temp spectrum (norm.)",
    xaxis = "x2",
    yaxis = "y3",
    line  = list(color = "black")
  ) %>%
  add_lines(
    data = spec_df_norm,
    x    = ~period,
    y    = ~rain_norm,
    name = "Rain spectrum (norm.)",
    xaxis = "x2",
    yaxis = "y4",
    line  = list(color = "darkblue", dash = "dash")
  )

fig <- fig %>%
  layout(
    # Top panel axes
    xaxis = list(
      title   = "",
      type    = "date",
      domain  = c(0, 1),
      anchor  = "y"
    ),
    yaxis = list(
      title   = ylab_temp,
      domain  = c(0.45, 1),
      range   = c(temp_min, temp_max),
      type    = "linear"
    ),
    yaxis2 = list(
      title      = ylab_rain,
      domain     = c(0.45, 1),
      overlaying = "y",
      side       = "right",
      range      = c(rain_min, rain_max),
      type       = "linear"
    ),
    # Bottom panel axes (normalized, explicitly linear 0–1)
    xaxis2 = list(
      title     = xlab_period,
      domain    = c(0, 1),
      anchor    = "y3",
      autorange = FALSE,
      range     = c(max_period, 0),
      position  = 0.35
    ),
    yaxis3 = list(
      title   = "Normalized spectral density (temp)",
      domain  = c(0, 0.3),
      range   = c(0, 1),
      type    = "linear"
    ),
    yaxis4 = list(
      title      = "Normalized spectral density (rain)",
      domain     = c(0, 0.3),
      overlaying = "y3",
      side       = "right",
      range      = c(0, 1),
      type       = "linear"
    ),
    # Shared layout
    shapes = shapes_main,
    legend = list(orientation = "h", x = 0, y = -0.05),
    margin = list(t = 60, r = 60, b = 60, l = 60),
    title  = list(text = main_title, x = 0)
  )

# ----------------------------
# Show both plots on a single page
# ----------------------------
print(grid.arrange(p_main, p_spec, ncol = 1))

# Show in RStudio when sourcing
if (interactive()) {
  browsable(fig)
}

# Save HTML
out_file <- "cincy_weather_timeseries_spectrum_seasons.html"
saveWidget(fig, out_file, selfcontained = TRUE)
cat("Saved HTML to:", normalizePath(out_file), "\n")
