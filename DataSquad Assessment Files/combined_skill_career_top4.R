# Dashiell Coyier
# Last ran 1/20/2026
# 3-question combined skill bar chart, alumni
# Order: top 4 for future development | influence post-college | development on DataSquad

library(tidyverse)
library(patchwork)

# --- 1. Load Data ---
csv_path <- "/Users/dashiellcoyier/Desktop/DataSquad/Final-visuals/DS Assessment - Alumni Database - 17 Nov 25 - PLackie - DataSquad Alumni Website Update and Assessment - 25SU_November 17, 2025_17.26.csv"
raw_data <- read_csv(csv_path, col_types = cols(.default = "c"))

skill_names <- raw_data[1, paste0("A4_", 1:8)] |>
  unlist(use.names = FALSE) |>
  str_remove("Skills Influence - ") |>
  recode(
    "Technical Documentation, Knowledge Sharing" = "Technical Documentation,\nKnowledge Sharing",
    "Database Design, Cloud Systems" = "Database Design,\nCloud Systems",
    "Project Management, Planning" = "Project Management,\nPlanning",
    "Coding, Software Engineering" = "Software Engineering,\nCoding"
  )

clean_data <- raw_data |> slice(-(1:2)) # remove metadata rows

# --- 2. Data Cleaning & Transformation ---
calc_dev_score <- function(x) {
  case_when(
    x == "Yes, a Lot!" ~ 3,
    x == "Somewhat" ~ 2,
    x == "A Little" ~ 1,
    x == "No Improvement" ~ 0,
    x == "N/A" ~ 0,
    TRUE ~ NA_real_
  )
}

calc_inf_score <- function(x) {
  case_when(
    x == "Very Influential" ~ 2,
    x == "Tangentially Influential" ~ 1,
    x == "Not Applicable" ~ 0,
    TRUE ~ NA_real_
  )
}

top4_cols <- paste0("A5_0_GROUP_", 1:8)

complete_responses <- clean_data |>
  mutate(
    non_empty_count = rowSums(!is.na(across(all_of(top4_cols))) & across(all_of(top4_cols)) != "")
  ) |>
  filter(non_empty_count == 4)

skills_df <- map_dfr(1:8, \(i) {
  col_dev <- paste0("A3_", i)
  col_inf <- paste0("A4_", i)
  col_top <- paste0("A5_0_GROUP_", i)

  tibble(
    skill = skill_names[i],
    dev = mean(calc_dev_score(clean_data[[col_dev]]), na.rm = TRUE),
    inf = mean(calc_inf_score(clean_data[[col_inf]]), na.rm = TRUE),
    freq = sum(!is.na(complete_responses[[col_top]]) & complete_responses[[col_top]] != "") /
      nrow(complete_responses)
  )
}) |>
  mutate(skill = fct_reorder(skill, freq))

# --- 3. Plotting ---
common_theme <- theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#e9e9e9ff"),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 18.2, face = "bold", color = "#535353ff"),
    axis.text.x = element_text(face = "bold", color = "#535353ff", size = 15.6),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 20.8),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

p_freq <- ggplot(skills_df, aes(x = freq, y = skill)) +
  geom_col(fill = "#b29882ff", width = 0.7) +
  geom_text(
    aes(label = scales::percent(round(freq, 4))),
    hjust = 1, vjust = 0.5, nudge_x = -0.02,
    color = "black", size = 6, fontface = "bold"
  ) +
  geom_vline(xintercept = 0, color = "black", size = 1.1) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 0.85),
    breaks = seq(0, 0.80, by = 0.2)
  ) +
  xlab("\nPercent Frequency in Top 4") +
  ggtitle(
    "Drawing from your post-college\nexperience, please choose the top 4\nskill areas we should focus on for\nDataSquad member development."
  ) +
  common_theme +
  theme(axis.text.y = element_text(size = 15.6, hjust = 1, face = "bold", color = "#535353ff"))

p_inf <- ggplot(skills_df, aes(x = inf, y = skill)) +
  geom_col(fill = "#b29882ff", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.2f", inf)),
    hjust = 1, vjust = 0.5, nudge_x = -0.05,
    color = "black", size = 6, fontface = "bold"
  ) +
  geom_vline(xintercept = 0, color = "black", size = 1.1) +
  scale_x_continuous(limits = c(0, 2)) +
  xlab(
    "\nAverage Rating:   \"Not Applicable\" = 0, \n\"Tangentially Influential\" = 1, \n\"Very Influential\" = 2"
  ) +
  ggtitle("How influential have these same skills\nbeen to your post-college success?") +
  common_theme

p_dev <- ggplot(skills_df, aes(x = dev, y = skill)) +
  geom_col(fill = "#b29882ff", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.2f", dev)),
    hjust = 1, vjust = 0.5, nudge_x = -0.05,
    color = "black", size = 6, fontface = "bold"
  ) +
  geom_vline(xintercept = 0, color = "black", size = 1.1) +
  scale_x_continuous(limits = c(0, 3)) +
  xlab(
    "\nAverage Rating:  \"No Improvement\" = 0, \n\"A Little\" = 1, \"Somewhat\" = 2, \"Yes a Lot!\" = 3"
  ) +
  ggtitle(
    "Can you attribute any positive change\nin your skills in the following areas\ndue to your work on the DataSquad?"
  ) +
  common_theme

combined_plot <- p_freq + p_inf + p_dev + plot_layout(ncol = 3)
print(combined_plot)

ggsave(
  filename = "combined_skill_career_top4.png",
  plot = combined_plot,
  width = 20,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)
