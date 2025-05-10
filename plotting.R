library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(visdat)
library(zoo)
library(signal)
library(ggplot2)

standard_columns <- c("timestamp", "heartRate", "respirationRate", "pulseOx", "IBI",
                      "accelxyz", "stressScore", "bodyBattery")

file_list <- list.files("//path/to/files/", pattern = "*.json", full.names = TRUE)

df_all <- bind_rows(lapply(file_list, function(file) {
  data <- fromJSON(file)
  
  # print(data)
  # str(data)
  # summary(data)
  
  for (col in standard_columns) {
    if (!col %in% names(data)) {
      data[[col]] <- NA  # Fehlende Werte mit NA auffüllen
    }
  }
  
  print(data)
  print(names(data))
  
  df_single <- data.frame(
    timestamp = data$timestamp, # as.POSIXct(data$timestamp, origin = "1970-01-01", tz = "UTC")
    pulseOx = data$pulseOx,
    stressScore = data$stressScore,
    heartRate = data$heartRate,
    bodyBattery = data$bodyBattery,
    respirationRate = data$respirationRate
  )
  
  df_IBI_mean <- if (!is.null(data$IBI) && length(data$IBI) > 0 && !all(is.na(data$IBI))) {
    data.frame(
      timestamp = df_single$timestamp,
      IBI = mean(data$IBI, na.rm = TRUE)
    )
  } else {
    data.frame(
      timestamp = df_single$timestamp,
      IBI = NA
    )
  }
  
  df_IBI_all <- if (!is.na(data$IBI[1])) {
    data.frame(
      timestamp = df_single$timestamp,
      IBI = data$IBI 
    )
  } else {
    data.frame(timestamp = df_single$timestamp, IBI = NA)
  }
  
  df_accel <- data.frame(
    timestamp = df_single$timestamp,
    accel_x = if (!is.na(data$accelxyz[1])) data$accelxyz[1] else NA,
    accel_y = if (!is.na(data$accelxyz[2])) data$accelxyz[2] else NA,
    accel_z = if (!is.na(data$accelxyz[3])) data$accelxyz[3] else NA
  )
  
  df_final <- left_join(df_IBI, df_single, by = "timestamp") %>%
    left_join(df_accel, by = "timestamp")
  
  # AUSREIßER HERAUSFILTERN basically bandpassfilter
  df_final <- df_final %>%
    mutate(
      heartRate = ifelse(heartRate < 40 | heartRate > 220, NA, heartRate),
      IBI = ifelse(IBI < 300 | IBI > 1500, NA, IBI),
      respirationRate = ifelse(respirationRate < 5 | respirationRate > 40, NA, respirationRate), # 5-65
      pulseOx = ifelse(pulseOx < 70 | pulseOx > 100, NA, pulseOx),
      accel_x = ifelse(accel_x < -20 | accel_x > 20, NA, accel_x),
      accel_y = ifelse(accel_y < -20 | accel_y > 20, NA, accel_y),
      accel_z = ifelse(accel_z < -20 | accel_z > 20, NA, accel_z)
    )
  
  # wenn alle in 1 zeile NA sind wird die ganze Zeile entfernt
  # df_final <- df_final %>%
  # filter(!is.na(heartRate) & !is.na(IBI) & !is.na(respirationRate) & !is.na(pulseOx))
  
  df_final <- df_final %>%
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))
  
  return(df_final)
  
}))

print(df_all)

# Sortiere nach Zeit
df_all <- df_all %>%
  arrange(timestamp)

# Erstelle zwei neue Dataframes für alle IBI-Werte und die Mittelwerte
df_IBI_all_combined <- bind_rows(lapply(file_list, function(file) {
  data <- fromJSON(file)
  if (!is.null(data$IBI) && length(data$IBI) > 0 && !all(is.na(data$IBI))) {
    data.frame(
      timestamp = rep(as.POSIXct(data$timestamp, origin = "1970-01-01", tz = "UTC"), length(data$IBI)),
      IBI = data$IBI
    )
  } else {
    data.frame(timestamp = NA, IBI = NA)
  }
}))

df_IBI_mean_combined <- bind_rows(lapply(file_list, function(file) {
  data <- fromJSON(file)
  if (!is.null(data$IBI) && length(data$IBI) > 0 && !all(is.na(data$IBI))) {
    data.frame(
      timestamp = as.POSIXct(data$timestamp[1], origin = "1970-01-01", tz = "UTC"),
      IBI = mean(data$IBI, na.rm = TRUE)
    )
  } else {
    data.frame(timestamp = NA, IBI = NA)
  }
}))

# Entferne NA-Werte
df_IBI_all_combined <- na.omit(df_IBI_all_combined)
df_IBI_mean_combined <- na.omit(df_IBI_mean_combined)

# Plot
ggplot() +
  geom_line(data = df_IBI_all_combined, aes(x = timestamp, y = IBI, color = "IBI Alle")) +
  geom_line(data = df_IBI_mean_combined, aes(x = timestamp, y = IBI, color = "IBI Mittelwert")) +
  labs(title = "IBI Werte über die Zeit",
       x = "Zeitstempel",
       y = "IBI (ms)",
       color = "Legende") +
  theme_minimal()

