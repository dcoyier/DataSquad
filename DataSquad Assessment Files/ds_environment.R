# Dashiell Coyier
# Last ran 1/20/2026
# DataSquad environment ratings bar chart, alumni

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

make_environment_graph <- function(csv_path, font_scale = 1.2) {
  raw_data <- read_csv(csv_path, col_types = cols(.default = "c"))
  clean_data <- raw_data %>% slice(-(1:2)) # drop metadata rows

  yes_cols <- c("A10_1_4", "A10_2_4", "A10_3_4", "A10_4_4", "A10_5_4")
  somewhat_cols <- c("A10_1_2", "A10_2_2", "A10_3_2", "A10_4_2", "A10_5_2")
  not_really_cols <- c("A10_1_1", "A10_2_1", "A10_3_1", "A10_4_1", "A10_5_1")

  percentages <- vapply(seq_along(yes_cols), function(i) {
    yes_count <- sum(clean_data[[yes_cols[i]]] == "Yes", na.rm = TRUE)
    somewhat_count <- sum(clean_data[[somewhat_cols[i]]] == "Somewhat", na.rm = TRUE)
    not_really_count <- sum(clean_data[[not_really_cols[i]]] == "Not Really", na.rm = TRUE)

    total_count <- yes_count + somewhat_count + not_really_count
    yes_count / total_count
  }, numeric(1))

  env_df <- data.frame(
    question = c(
      "I felt comfortable",
      "I felt encouraged",
      "I felt included",
      "I felt supported",
      "I felt valued"
    ),
    percentage = percentages
  )

  env_df$question <- reorder(env_df$question, env_df$percentage)

  env_theme <- theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "#e9e9e9ff"),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(size = 21 * font_scale, face = "bold", color = "#535353ff"),
      axis.text.x = element_text(face = "bold", color = "#535353ff", size = 18 * font_scale),
      plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 25 * font_scale),
      axis.text.y = element_text(size = 19 * font_scale, hjust = 1, face = "bold", color = "#535353ff"),
      axis.title.y = element_blank()
    )

  ggplot(env_df, aes(x = percentage, y = question)) +
    geom_col(fill = "#97bb96ff", width = 0.7) +
    geom_text(
      aes(label = scales::percent(round(percentage, 2))),
      hjust = 1, vjust = 0.5, nudge_x = -0.02,
      color = "black", size = 6 * font_scale, fontface = "bold"
    ) +
    geom_vline(xintercept = 0, color = "black", linewidth = 1.1) +
    scale_x_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    xlab("\nPercentage of 'Yes' Responses for Alumni") +
    ggtitle("Was the DataSquad a welcoming environment?\n") +
    env_theme
}

csv_path <- "/Users/dashiellcoyier/Desktop/DataSquad/Final-visuals/DS Assessment - Alumni Database - 17 Nov 25 - PLackie - DataSquad Alumni Website Update and Assessment - 25SU_November 17, 2025_17.26.csv"
p_env <- make_environment_graph(csv_path)

print(p_env)
ggsave(
  filename = "ds_environment.png",
  plot = p_env,
  width = 12,
  height = 5.6,
  units = "in",
  dpi = 300,
  bg = "white"
)
