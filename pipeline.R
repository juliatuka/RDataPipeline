library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(visdat)
library(zoo)
library(signal)

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

df_IBI <- if (!is.null(data$IBI) && length(data$IBI) > 0 && !all(is.na(data$IBI))) {
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

df_accel <- data.frame(
  timestamp = df_single$timestamp,
  accel_x = if (!is.na(data$accelxyz[1])) data$accelxyz[1] else NA,
  accel_y = if (!is.na(data$accelxyz[2])) data$accelxyz[2] else NA,
  accel_z = if (!is.na(data$accelxyz[3])) data$accelxyz[3] else NA
)

df_final <- left_join(df_IBI, df_single, by = "timestamp") %>%
  left_join(df_accel, by = "timestamp")

# AUSREIßER HERAUSFILTERN bandpassfilter
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

print(head(df_all))

# lineare interpolation 
df_all$heartRate <- na.approx(df_all$heartRate, na.rm = FALSE)
df_all$IBI <- na.approx(df_all$IBI, na.rm = FALSE)
df_all$respirationRate <- na.approx(df_all$respirationRate, na.rm = FALSE)
df_all$pulseOx <- na.approx(df_all$pulseOx, na.rm = FALSE)

# für randwerte da hier keine 'Nachbarswerte' vorhanden sind 
df_all$heartRate[is.na(df_all$heartRate)] <- median(df_all$heartRate, na.rm = TRUE)
df_all$IBI[is.na(df_all$IBI)] <- median(df_all$IBI, na.rm = TRUE)
df_all$respirationRate[is.na(df_all$respirationRate)] <- median(df_all$respirationRate, na.rm = TRUE)
df_all$pulseOx[is.na(df_all$pulseOx)] <- median(df_all$pulseOx, na.rm = TRUE)

# Last Observation Carried Forward
# df_all$bodyBattery <- zoo::na.locf(df_all$bodyBattery, na.rm = FALSE)
# df_all$stressScore <- zoo::na.locf(df_all$stressScore, na.rm = FALSE)

# df_all$accel_x <- zoo::na.locf(df_all$accel_x, na.rm = FALSE)
# df_all$accel_y <- zoo::na.locf(df_all$accel_y, na.rm = FALSE)
# df_all$accel_z <- zoo::na.locf(df_all$accel_z, na.rm = FALSE)

df_all <- df_all %>%
  mutate(across(c(bodyBattery, stressScore, accel_x, accel_y, accel_z), ~na.locf(.x, na.rm = FALSE)))

# falls noch immer fehlende werte bestehen 
# df_all <- df_all %>% mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE)))

# colSums(is.na(df_all))
# vis_miss(df_all)

# falls daraufhin noch immer fehlende Werte existieren kann diese zeile evetuell auch entfernt werden 
# df_all <- na.omit(df_all)

# will man die zeilen entfernen in der nur eine bestimmte Spalte NA enthällt kann A = IBI zb. 
# df_clean <- df %>% drop_na(A)

# SIGNAL SMOOTHING mit moving average über 5 werte 
df_all <- df_all %>% 
  mutate(
    heartRate_smooth = rollapply(heartRate, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    IBI_smooth = rollapply(IBI, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    respirationRate_smooth = rollapply(respirationRate, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    accel_x_smooth = rollapply(accel_x, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    accel_y_smooth = rollapply(accel_y, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    accel_z_smooth = rollapply(accel_z, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE)
  )

# mit Tiefpassfilter 
butter_filter <- butter(2, 0.25, type = "low")

apply_filter <- function(signal, filter, padding = 10) {
  padded_signal <- c(rev(signal[1:padding]), signal, rev(signal[(length(signal)-padding+1):length(signal)]))
  filtered_signal <- filtfilt(filter, padded_signal)
  return(filtered_signal[(padding+1):(length(filtered_signal)-padding)])
}

# df_all <- df_all %>%
  # mutate(
    # heartRate_filtered = apply_filter(heartRate, butter_filter),
    # IBI_filtered = apply_filter(IBI, butter_filter),
    # respirationRate_filtered = apply_filter(respirationRate, butter_filter)
  # )

# n_padding <- 10  # Anzahl der zusätzlichen Werte

# padded_heartRate <- c(
  # rev(df_all$heartRate[1:n_padding]),  # Spiegel Anfang
  # df_all$heartRate,                    # Originaldaten
  # rev(df_all$heartRate[(nrow(df_all)-n_padding+1):nrow(df_all)])  # Spiegel Ende
# )

# padded_HR_filtered <- filtfilt(butter_filter, padded_heartRate)

# padded_IBI <- c(
  # rev(df_all$IBI[1:n_padding]),  # Spiegel Anfang
  # df_all$IBI,                    # Originaldaten
  # rev(df_all$IBI[(nrow(df_all)-n_padding+1):nrow(df_all)])  # Spiegel Ende
# )

# padded_IBI_filtered <- filtfilt(butter_filter, padded_IBI)

# padded_respirationRate <- c(
  # rev(df_all$respirationRate[1:n_padding]),  # Spiegel Anfang
  # df_all$respirationRate,                    # Originaldaten
  # rev(df_all$respirationRate[(nrow(df_all)-n_padding+1):nrow(df_all)])  # Spiegel Ende
# )

# padded_respirationRate_filtered <- filtfilt(butter_filter, padded_respirationRate)

# vorwärts und rückwärtsfilter 
df_all <- df_all %>%
  mutate(
    # heartRate_filtered = padded_HR_filtered[(n_padding+1):(length(padded_HR_filtered)-n_padding)],
    # IBI_filtered = padded_IBI_filtered[(n_padding+1):(length(padded_IBI_filtered)-n_padding)],
    # respirationRate_filtered = padded_respirationRate_filtered[(n_padding+1):(length(padded_respirationRate_filtered)-n_padding)],
    heartRate_filtered = apply_filter(heartRate, butter_filter),
    IBI_filtered = apply_filter(IBI, butter_filter),
    respirationRate_filtered = apply_filter(respirationRate, butter_filter),
    accel_x_filtered = filtfilt(butter_filter, accel_x),
    accel_y_filtered = filtfilt(butter_filter, accel_y),
    accel_z_filtered = filtfilt(butter_filter, accel_z) # die benötgen kein padding
  )

# Die beschleunigungsdaen sollten nicht mit tiefpassfilter geglättet werden da dieser
# bei hochfrequenten daten zu viel Information entfernt
df_all <- df_all %>%
  mutate(
    accel_x_smooth = rollapply(accel_x, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    accel_y_smooth = rollapply(accel_y, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
    accel_z_smooth = rollapply(accel_z, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE)
  )

# PLOTTING
# Herzfrequenz
ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = heartRate, color = "Original")) +
  geom_line(aes(y = heartRate_smooth, color = "Moving Average")) +
  geom_line(aes(y = heartRate_filtered, color = "Butterworth")) +
  # scale_color_manual(values = c("Original" = "green", "Moving Average" = "blue", "Butterworth" = "red")) +
  labs(title = "Herzfrequenz Glättung", y = "Herzfrequenz (bpm)", x = "Zeit") +
  theme_minimal()

# IBI (Inter-Beat-Intervall)
ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = IBI, color = "Original")) +
  geom_line(aes(y = IBI_smooth, color = "Moving Average")) +
  geom_line(aes(y = IBI_filtered, color = "Butterworth")) +
  labs(title = "IBI Glättung", y = "IBI (ms)", x = "Zeit") +
  theme_minimal()

# Respiration Rate
ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = respirationRate, color = "Original")) +
  geom_line(aes(y = respirationRate_smooth, color = "Moving Average")) +
  geom_line(aes(y = respirationRate_filtered, color = "Butterworth")) +
  labs(title = "Atemfrequenz Glättung", y = "Atemfrequenz (bpm)", x = "Zeit") +
  theme_minimal()

# Accel X
ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = accel_x, color = "Original")) +
  geom_line(aes(y = accel_x_smooth, color = "Moving Average")) +
  geom_line(aes(y = accel_x_filtered, color = "Butterworth")) +
  labs(title = "Beschleunigung X Glättung", y = "Beschleunigung (m/s²)", x = "Zeit") +
  theme_minimal()

# Accel Y
ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = accel_y, color = "Original")) +
  geom_line(aes(y = accel_y_smooth, color = "Moving Average")) +
  geom_line(aes(y = accel_y_filtered, color = "Butterworth")) +
  labs(title = "Beschleunigung Y Glättung", y = "Beschleunigung (m/s²)", x = "Zeit") +
  theme_minimal()

# Accel Z
ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = accel_z, color = "Original")) +
  geom_line(aes(y = accel_z_smooth, color = "Moving Average")) +
  geom_line(aes(y = accel_z_filtered, color = "Butterworth")) +
  labs(title = "Beschleunigung Z Glättung", y = "Beschleunigung (m/s²)", x = "Zeit") +
  theme_minimal()


# NORMALISIERUNG
normalize <- function(x) {
  if (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) == 0) {
    return(rep(0.5, length(x)))  # Falls alle Werte gleich sind → Setze auf 0.5
  } else {
    return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}

df_all <- df_all %>%
  mutate(
    heartRate_norm = normalize(heartRate_filtered),
    IBI_norm = normalize(IBI_filtered),
    respirationRate_norm = normalize(respirationRate_filtered)
    )

df_all <- df_all %>% 
  mutate(
    accel_x_norm = (accel_x_filtered - min(accel_x_filtered, na.rm = TRUE)) / 
      (max(accel_x_filtered, na.rm = TRUE) - min(accel_x_filtered, na.rm = TRUE)),
    accel_y_norm = (accel_y_filtered - min(accel_y_filtered, na.rm = TRUE)) / 
      (max(accel_y_filtered, na.rm = TRUE) - min(accel_y_filtered, na.rm = TRUE)),
    accel_z_norm = (accel_z_filtered - min(accel_z_filtered, na.rm = TRUE)) / 
      (max(accel_z_filtered, na.rm = TRUE) - min(accel_z_filtered, na.rm = TRUE))
  )

# STANDARDISIERUNG Z-Score Normalization
standardize <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    return(rep(0, length(x)))  # Falls Standardabweichung 0 ist → Setze alle Werte auf 0
  } else {
    return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  }
}

df_all <- df_all %>%
  mutate(
    heartRate_std = standardize(heartRate_filtered),
    IBI_std = standardize(IBI_filtered),
    respirationRate_std = standardize(respirationRate_filtered)
  )

df_all <- df_all %>%
  mutate(
    accel_x_std = ((accel_x_filtered - mean(accel_x_filtered, na.rm = TRUE)) / sd(accel_x_filtered, na.rm = TRUE)),
    accel_y_std = ((accel_y_filtered - mean(accel_y_filtered, na.rm = TRUE)) / sd(accel_y_filtered, na.rm = TRUE)),
    accel_z_std = ((accel_z_filtered - mean(accel_z_filtered, na.rm = TRUE)) / sd(accel_z_filtered, na.rm = TRUE))
  )

# colSums(is.na(df_all))

summary(df_all[, c("heartRate_norm", "IBI_norm", "respirationRate_norm")])
# summary(df_all[, c("heartRate_std", "IBI_std", "respirationRate_std")])

# FEATURE ENGINEERING

# summary für die statistischen Merkmale 
# SDNN = Standard Deviation of the NN Intervall

df_all <- df_all %>%
  mutate(HRV_SDNN = rollapply(IBI_filtered, width = 10, FUN = sd, fill = NA, align = "right", partial = TRUE))

# print(df_all$HRV_SDNN)

df_all <- df_all %>%
  mutate(accel_total = sqrt(accel_x_filtered^2 + accel_y_filtered^2 + accel_z_filtered^2))

# print(df_all$accel_total)

df_all <- df_all %>%
  mutate(
    heartRate_mean_10s = rollapply(heartRate_filtered, width = 10, FUN = mean, fill = NA, align = "right", partial = TRUE),
    heartRate_std_10s = rollapply(heartRate_filtered, width = 10, FUN = sd, fill = NA, align = "right", partial = TRUE),
    
    accel_total_mean_10s = rollapply(accel_total, width = 10, FUN = mean, fill = NA, align = "right", partial = TRUE),
    accel_total_std_10s = rollapply(accel_total, width = 10, FUN = sd, fill = NA, align = "right", partial = TRUE)
  )

# RMSSD = Root Mean Square of Successive Differences
df_all <- df_all %>%
  mutate(HRV_RMSSD = rollapply(IBI_filtered, 10, function(x) sqrt(mean(diff(x)^2)), fill = NA, align = "right", partial = TRUE))

# um die NAs zu füllen
df_all <- df_all %>%
  mutate(
   HRV_SDNN = ifelse(is.na(HRV_SDNN), median(HRV_SDNN, na.rm = TRUE), HRV_SDNN),
   heartRate_std_10s = ifelse(is.na(heartRate_std_10s), median(heartRate_std_10s, na.rm = TRUE), heartRate_std_10s),
   accel_total_std_10s = ifelse(is.na(accel_total_std_10s), median(accel_total_std_10s, na.rm = TRUE), accel_total_std_10s),
   HRV_RMSSD = ifelse(is.na(HRV_RMSSD), median(HRV_RMSSD, na.rm = TRUE), HRV_RMSSD)
  )

ggplot(df_all, aes(x = timestamp)) +
  geom_line(aes(y = heartRate, color = "Herzfrequenz")) +
  geom_line(aes(y = HRV_SDNN, color = "HFV_SDNN")) +
  geom_line(aes(y = HRV_RMSSD, color = "HFV_RMSSD")) +
  geom_line(aes(y = accel_total, color = "Gesamtbeschleunigung")) +
  labs(title = "HRV & Bewegung", y = "Werte", x = "Zeit") +
  theme_minimal()

# hf 

write.csv(df_all, file = "/Desktop/df_all_preprocessed.csv", row.names = FALSE)

