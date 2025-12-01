
library(tidyverse)
library(lubridate)
library(viridis)
library(ggridges)

traffic <- read.csv("~/Homework 4/MetroInterstateTrafficVolume.csv", 
                    header = TRUE, sep = ",")


daily_data <- traffic %>%
  mutate(date = as_date(date_time),
         year = year(date_time),
         month = month(date_time)) %>%
  group_by(date, year, month) %>%
  summarise(daily_volume = sum(traffic_volume), .groups = "drop") %>%
  mutate(day_of_year = yday(date)) %>%
  filter(year != 2012) # filter out 2012, full years of data start in 2013

############ Daily volume - Facet by year ############

ggplot(daily_data, aes(x = day_of_year, y = daily_volume, color = factor(month))) +
  geom_point(size = 2.5, alpha = 0.75) +
  facet_wrap(~year, ncol = 3) +
  scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = month.abb) +
  scale_color_viridis_d(labels = month.abb, option = "viridis") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Daily Traffic Volume by Year",
       y = "Daily Volume",
       color = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
        )

  
################### Holidays ################### 
### Look up how to fill boxes using viridis scale filler ###
### look up how to center title ###

daily_data

holidays <- 
traffic %>%
  mutate(date = as.Date(date_time)) %>%
  select(holiday, date) %>%
  filter(holiday != "None")

holiday_data <- 
daily_data %>%
  left_join(holidays, by = 'date') %>%
  filter(!is.na(holiday))

ggplot(data = holiday_data, aes(x = holiday, y = daily_volume)) + 
  geom_boxplot(aes(fill = holiday), alpha = 0.75) +
  scale_fill_viridis_d(option = "viridis") +
  scale_y_continuous(labels = scales::label_comma(),
                     breaks = c(20000, 40000, 60000, 80000, 100000, 120000),
                     limits = c(20000, 120000)) +
  labs(x = "",
       y = "Daily Volume",
       title = "Distribution of Daily Traffic by Holiday")+
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
        )

####### mountain chart for weather by month & season ####### 

daily_weather <- traffic %>%
  mutate(date = as_date(date_time)) %>%
  group_by(date, weather_main) %>%
  summarise(hours = n(), .groups = "drop_last") %>%
  slice_max(hours, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(day_of_year = yday(date)) %>%
  filter(year(date) != 2012)

# Create ridgeline plot
ggplot(daily_weather, aes(x = day_of_year, y = weather_main, fill = weather_main)) +
  geom_density_ridges(alpha = 0.75) +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = month.abb) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.28)))+
  labs(title = "Weather Distribution Throughout the Year",
       x = "Month",
       y = "Weather Type") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"))


###### scatterplot daily volume on snow days with dot size for snowfall amount ###### 

###### diverging bar chart for time (hours) with pos above and neg below for a given day ######


###### Temperature ######

# convert temperature to Fahrenheit and use average temp as daily temperature
traffic_converted <- 
traffic %>%
  filter(temp != 0) %>%
  mutate(temperature_f = (temp * (9/5)) - 459.67,
         date = as_date(date_time)) %>%
  group_by(date) %>%
  summarize(avg_temp_f = mean(temperature_f, na.rm = TRUE),
            total_precip = sum(rain_1h + snow_1h),
            daily_volume = sum(traffic_volume), .groups = 'drop') %>%
  mutate(day_of_year = yday(date),
         year = year(date),
         month = month(date, label = TRUE, abbr = FALSE)) %>% 
  filter(year != 2012) 

min(traffic_converted$avg_temp_f)
max(traffic_converted$avg_temp_f)

ggplot(traffic_converted, aes(x = avg_temp_f, y = month, fill = after_stat(x))) + 
  geom_density_ridges_gradient(alpha = 0.75, scale = 1.5) +
  scale_fill_viridis_c(option = "inferno") +
  scale_y_discrete(limits = rev(month.name)) +
  labs(y = "",
       fill = "",
       x = "Average Daily Temperature (°F)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14)
  )
  
         




