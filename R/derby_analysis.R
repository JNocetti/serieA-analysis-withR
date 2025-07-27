# Load libraries
library(tidyverse)

# Read the dataset
df <- read_csv("data/matches_serie_A.csv")


# Select only the columns needed for the analysis
df <- df %>%
  select(Team, Opponent, Result, Attendance, Poss, xG, xGA)

# Remove rows with missing values in any key column
df <- df %>%
  filter(
    !is.na(Result),
    !is.na(Attendance),
    !is.na(Poss),
    !is.na(xG),
    !is.na(xGA)
)

# Remove duplicate rows
df <- df %>%
  distinct()


# i. Frequency table of a categorical variable (Result)
result_abs <- df %>% count(Result, name = "Absolute")
result_rel <- df %>%
  count(Result) %>%
  mutate(Relative = n / sum(n)) %>%
  select(Result, Relative)
result_freq_table <- left_join(result_abs, result_rel, by = "Result")
print(result_freq_table)

# ii. Descriptive statistics of Attendance
attendance_stats <- df %>%
  summarise(
    Mean    = mean(Attendance),
    Median  = median(Attendance),
    Std_dev = sd(Attendance)
  )
print(attendance_stats)

# iii. ggplot2 graphs

# 1. Bar plot of Result
bar_result <- ggplot(df, aes(x = Result, fill = Result)) +
  geom_bar() +
  labs(title = "Match Results", x = "Result", y = "Count") +
  theme_classic()

# 2. Scatter plot: xG vs xGA + correlation
cor_xg <- cor(df$xG, df$xGA)
scatter_xg <- ggplot(df, aes(x = xG, y = xGA)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  labs(title = paste("xG vs xGA (cor =", round(cor_xg, 2), ")"),
       x = "Expected Goals (xG)", y = "Expected Goals Against (xGA)") +
  theme_classic()

# 3. Boxplot: Attendance by Result
box_attendance <- ggplot(df, aes(x = Result, y = Attendance, fill = Result)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Attendance by Match Result", x = "Result", y = "Attendance") +
  theme_classic()

# Save plots
ggsave("figs/Question2/bar_result.png", bar_result, width = 6, height = 5)
ggsave("figs/Question2/scatter_xg.png", scatter_xg, width = 6, height = 5)
ggsave("figs/Question2/box_attendance_by_result.png", box_attendance, width = 6, height = 5)


################

# Analysis and answer to the question: 
# Do the Milan–Inter, Roma–Lazio, and Torino–Juventus derbies 
# attract more audience and are more balanced than the others?

################

# Flag the derbies
df <- df %>%
  mutate(
    is_derby =
      (Team == "Milan"    & Opponent == "Inter")   |
      (Team == "Inter"    & Opponent == "Milan")   |
      (Team == "Roma"     & Opponent == "Lazio")   |
      (Team == "Lazio"    & Opponent == "Roma")    |
      (Team == "Torino"   & Opponent == "Juventus")|
      (Team == "Juventus"& Opponent == "Torino")
)

# Compute parity metrics
df <- df %>%
  mutate(
    poss_balance = abs(Poss - 50),   # distance from 50% possession
    expected_goals_diff = abs(xG - xGA),    # difference in expected goals
    is_draw = (Result == "D")   # TRUE if match ended in a draw
)

# Calculate overall, derby, and non-derby statistics
overall_stats <- df %>%
  summarise(
    avg_attendance = mean(Attendance),
    mean_poss_balance = mean(poss_balance),
    mean_expected_goals_diff = mean(expected_goals_diff),
    draw_ratio = mean(is_draw)
)

derby_stats <- df %>%
  filter(is_derby) %>%
  summarise(
    avg_attendance = mean(Attendance),
    mean_poss_balance = mean(poss_balance),
    expected_goals_diff = mean(expected_goals_diff),
    draw_ratio = mean(is_draw)
)

non_derby_stats <- df %>%
  filter(!is_derby) %>%
  summarise(
    avg_attendance = mean(Attendance),
    mean_poss_balance = mean(poss_balance),
    expected_goals_diff = mean(expected_goals_diff),
    draw_ratio = mean(is_draw)
)

# Print the results
cat("Overall statistics \n");    print(overall_stats)
cat("Derby statistics \n");      print(derby_stats)
cat("Non-Derby statistics \n");  print(non_derby_stats)

#####################################

# Para hacer esta pregunta fue necesario pasarle todo el código anterior.
# Necesito que me grafiques como boxplot el público (Attendance), 
# la posesion (poss_balance), goles esperados (xg_diff) y los empates (draw_ratio)
# entre los derbis y los no-derbis

#####################################


# Build a small summary tibble for plotting
draw_df <- tibble(
  group = c("Non-Derby", "Derby"),
  draw_ratio = c(non_derby_stats$draw_ratio, derby_stats$draw_ratio) * 100
)

# Attendance
attendance_plot <- ggplot(df, aes(x = is_derby, y = Attendance, fill = is_derby)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Derby match?", y = "Attendance", title = "Attendance: Derby vs Non-Derby")

# Possession balance
poss_plot <- ggplot(df, aes(x = is_derby, y = poss_balance, fill = is_derby)) +
  geom_boxplot(show.legend = FALSE, na.rm = TRUE) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Derby match?", y = "|Poss − 50|", title = "Possession Imbalance")

# expected goals difference
xg_plot <- ggplot(df, aes(x = is_derby, y = expected_goals_diff, fill = is_derby)) +
  geom_boxplot(show.legend = FALSE, na.rm = TRUE) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = "Derby match?", y = "|xG − xGA|", title = "Expected Goals Difference")

# Create the bar chart
draw_plot <- ggplot(draw_df, aes(x = group, y = draw_ratio, fill = group)) +
  geom_col(show.legend = FALSE, na.rm = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", draw_ratio)), 
            vjust = -0.5) +
  labs(
    x     = "Match type",
    y     = "Draw ratio (%)",
    title = "Draw Ratio: Derby vs Non-Derby"
  ) +
  ylim(0, max(draw_df$draw_ratio) + 5)

# Save plots to figs/Question2
ggsave("figs/Question2/attendance_boxplot.png", attendance_plot, width = 8, height = 5)
ggsave("figs/Question2/poss_balance_boxplot.png", poss_plot,      width = 8, height = 5)
ggsave("figs/Question2/expected_goals_diff_boxplot.png",       xg_plot,      width = 8, height = 5)
ggsave("figs/Question2/draw_ratio_comparison.png", draw_plot, width = 6, height = 5)
