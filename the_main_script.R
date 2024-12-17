install.packages("dispRity")
install.packages("hrbrthemes")
install.packages("lubridate")
library(dispRity)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)

bat_data <- read.csv(file = "2020_2024_bat_sampling - Sheet1.csv")

#Filtering data for only ANPA and PAHE
pallids_and_canyons <- filter(bat_data, Species %in% c("ANPA","PAHE"))
head(pallids_and_canyons)

#Configuring emergence time
pallids_and_canyons$Time <- lubridate::hm(pallids_and_canyons$Time)

#Plotting emergence times of ANPA and PAHE
ggplot(pallids_and_canyons, aes(x=Time, fill = Species)) +
  geom_histogram(position = "identity", alpha=0.5, bins=20) +
  scale_x_time(labels = scales::time_format("%H:%M"))

#Significance testing for difference in emergence times & preparing to overlay on a graph
wilcox_result <- wilcox.test(pallids_and_canyons$Time ~ pallids_and_canyons$Species,
            data = pallids_and_canyons, exact = FALSE)
wilcox_p <- wilcox_result$p.value
wilcox_label <- sprintf("Wilcox p = %.3f", wilcox_p)

#Preparing for an overlap test (Bhattacharyya coeff.) by separating emergence times
pallids_and_canyons <- mutate(pallids_and_canyons,
                              ANPA_Time = case_when(
                                Species == "ANPA" ~ Time
                                )
                              )
pallids_and_canyons <- mutate(pallids_and_canyons,
                              PAHE_Time = case_when(
                                Species == "PAHE" ~ Time
                                )
                              )

#Storing as numeric (since the function doesn't accept the lubridate data format)
pallids_and_canyons$ANPA_Time <- as.numeric(pallids_and_canyons$ANPA_Time)
pallids_and_canyons$PAHE_Time <- as.numeric(pallids_and_canyons$PAHE_Time)

#New dataframes to remove NA values for the Bhattacharyya function
emergence_times_ANPA <- select(pallids_and_canyons, ANPA_Time)
emergence_times_ANPA <- drop_na(emergence_times_ANPA, ANPA_Time)

emergence_times_PAHE <- select(pallids_and_canyons, PAHE_Time)
emergence_times_PAHE <- drop_na(emergence_times_PAHE, PAHE_Time)

bhatt_result <- bhatt.coeff(emergence_times_ANPA$ANPA_Time, emergence_times_PAHE$PAHE_Time)
bhatt_label <- sprintf("Bhatt. Coeff. = %.3f", bhatt_result)

#Plotting the same emergence times, with statistical overlays
ggplot(pallids_and_canyons, aes(x=Time, fill = Species)) +
  geom_histogram(position = "identity", alpha=0.5, bins=20) +
  scale_x_time(labels = scales::time_format("%H:%M")) +
  annotate("text", x = 85000, y = 16, label = wilcox_label, hjust = 1, size = 5, color = "black") +
  annotate("text", x = 85000, y = 14, label = bhatt_label, hjust = 1, size = 5, color = "black")

#Separating out by reproductive status for females
repro_status <- filter(pallids_and_canyons, Reproductive_status %in% c("L","P","NR","P/L") & Sex == "F")

ggplot(repro_status, aes(x=Time, fill = Species)) +
  geom_histogram(position = "identity", alpha=0.5, bins=20) +
  facet_wrap(~Reproductive_status) +
  scale_x_time(labels = scales::time_format("%H:%M"))

