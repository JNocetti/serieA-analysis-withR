library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(ggpubr)


df <- read_csv("data/matches_serie_A.csv")

# Rename columns and convert
df_transformed <- df %>%
  transmute(
    date       = ymd(Date),
    season     = year(date) + (month(date) >= 8),  # 2020/21→2021, 21/22→2022, etc.
    venue      = Venue,                             
    result     = Result,                         
    attendance = Attendance
  ) 

glimpse(df_transformed)

# Remove missing values
df_cleaned <- df_transformed %>%
  filter(
    !is.na(date),
    !is.na(season),
    !is.na(venue),
    !is.na(result),
    !is.na(attendance)
  )

# Remove duplicated
df_cleaned <- df_cleaned %>% distinct()

df_cleaned <- df_transformed  %>%             
  mutate(
    home_match = venue == "Home",                 # TRUE = was played at home
    home_win   = result == "W",                   # TRUE = team win
    with_crowd = replace_na(attendance > 0, FALSE)# TRUE = were spectators
  )

glimpse(df_cleaned)

# Only home matches
home_matches <- df_cleaned %>% filter(home_match) 

# i. Frequency table of a categorical variable (with_crowd)
freq_wc <- home_matches %>%
  count(with_crowd) %>%
  mutate(
    relative_frequency = n / sum(n)
  )
print(freq_wc)

# ii. Descriptive statistics of Attendance
desciptive_attributes <- home_matches %>%
  filter(!is.na(attendance)) %>%
  summarise(
    mean_att   = mean(attendance),
    median_att = median(attendance),
    sd_att     = sd(attendance) 
  )
print(desciptive_attributes)


#  ggplot2 plots

# 1. Bar plot of with_crowd
bar_plot_number_matches_with_crowd <- ggplot(freq_wc, aes(
  x = factor(with_crowd, levels=c(FALSE,TRUE), labels=c("Without crowd","With crowd")),
  y = n
)) +
  geom_col(fill=c("#EF5350","#42A5F5")) +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(
    title = "Number of matches at home with crowd vs without crowd",
    x     = "",
    y     = "Match quantity"
  ) +
  theme_minimal()


# Scatterplot plot:  Goals for vs. goals against and correlation coefficient
season_performance <- df_cleaned %>%
  filter(home_match) %>%
  group_by(season) %>%
  summarise(
    avg_attendance = mean(attendance, na.rm = TRUE),
    win_rate       = mean(home_win,    na.rm = TRUE),
    .groups = "drop"
  )
print(season_performance)

correlation_value <- cor(season_performance$avg_attendance, season_performance$win_rate)
print(paste("Correlation Pearson:", round(corr_value, 3)))

# Scatterplot: 
scatterplot_attendance_home_win_rate_and_season <- ggplot(season_performance, aes(x = avg_attendance, y = win_rate)) +
  geom_point(size = 3, color = "#2C3E50") +
  geom_text(aes(label = season),
            vjust = -0.5,      
            size  = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#E74C3C") +
  stat_cor(
    method  = "pearson",
    label.x = min(season_performance$avg_attendance),
    label.y = max(season_performance$win_rate)
  ) +
  scale_y_continuous(labels = percent_format(1)) +
  scale_x_continuous(labels = comma_format()) +
  labs(
    title    = "Attendance vs. Home Win Rate by Season",
    subtitle = paste0("Pearson Correlation = ", round(corr_value, 3)),
    x        = "Average Attendance (Home Matches)",
    y        = "Home Win Rate"
  ) +
  theme_minimal(base_size = 14)


# Boxplot: Home attendance with crowd.
boxplot_home_attendance <- ggplot(home_matches, aes(
  x     = factor(with_crowd, levels = c(FALSE, TRUE),
                 labels = c("No Crowd", "With Crowd")),
  y     = attendance,
  fill  = factor(with_crowd, levels = c(FALSE, TRUE),
                 labels = c("No Crowd", "With Crowd"))
)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(
    values = c("No Crowd"   = "#EF5350",  "With Crowd" = "#42A5F5") 
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Home Attendance Distribution",
    x     = "",
    y     = "Attendance",
    fill  = ""
  ) +
  theme_minimal(base_size = 14)

# Save plots
ggsave("figs/Question1/bar_plot_number_matches_with_crowd.jpg", 
       bar_plot_number_matches_with_crowd, width = 8, height = 5)
ggsave("figs/Question1/scatterplot_attendance_home_win_rate_and_season.jpg", 
       scatterplot_attendance_home_win_rate_and_season, width = 8, height = 5)
ggsave("figs/Question1/boxplot_home_attendance.jpg", boxplot_home_attendance, width = 8, height = 5)

################

# Analysis and answer to the question: 
# ¿Does the return of spectators in the 2021/22 season
# post-pandemic influence home team results?

################

home_matches_with_crowd <- home_matches %>% 
  group_by(season, with_crowd)

# Table that show the number of matches and win rate for each season.
home_match_results <- home_matches_with_crowd %>% 
  summarise(
    n_matches     = n(),
    win_rate      = mean(home_win)
  ) %>% 
  ungroup()


View(home_match_results)

# Get the suspended dates in no covid seasons 
suspended_dates <- df_transformed %>%
  filter(
    season %in% c(2022, 2023, 2024, 2025),
    is.na(attendance) | attendance == 0
  ) %>%
  distinct(date) %>%    
  arrange(date) %>%       
  pull(date)               

print(suspended_dates)


# Table which contains quantity of matches and win rates.
overall_performance <- df_cleaned %>%    
  filter(home_match)  %>%   
  group_by(with_crowd) %>%  
  summarise(
    n_matches = n(),
    win_rate  = mean(home_win),
    .groups   = "drop"
  )

print(overall_performance)


#####################################

# Para gráficar esta pregunta fue necesario pasarle todo el código anterior al 
# ChatGPT y pasarle el siguiente prompt.
# "Crea un gráfico de barras con ggplot2 que compare la tasa de victorias 
# locales(win_rate) en casa para partidos ‘Sin público’ y ‘Con público’ 
# usando el dataframe. 
# Etiqueta cada barra con el porcentaje correspondiente encima"

#####################################

bar_result_question1 <- ggplot(overall_performance, aes(
  x    = factor(with_crowd, levels = c(FALSE, TRUE), labels = c("No Crowd", "With Crowd")),
  y    = win_rate,
  fill = with_crowd
)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 0.1)),
            vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Home Win Rate Comparison: No Crowd vs With Crowd",
    x     = "",
    y     = "Home Win Rate"
  ) +
  theme_minimal(base_size = 14)

# Save plot
ggsave("figs/Question1/bar_result_question1.jpg", bar_result_question1, width = 8, height = 5)



