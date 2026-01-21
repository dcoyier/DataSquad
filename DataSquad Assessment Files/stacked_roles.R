# Dashiell Coyier
# Last ran 1/20/2026
# Stacked bar chart for roles on the DataSquad, alumni

library(tidyverse)
library(scales)

csv_path <- "/Users/dashiellcoyier/Desktop/DataSquad/Final-visuals/DS Assessment - Alumni Database - 17 Nov 25 - PLackie - DataSquad Alumni Website Update and Assessment - 25SU_November 17, 2025_17.26.csv"
if (!file.exists(csv_path)) stop("CSV file not found. Please check path.")

raw_data <- read_csv(csv_path, col_types = cols(.default = "c"))

data_clean <- raw_data %>%
  filter(
    A1 %in% c(
      "I'll help with the assessment",
      "I'll just update the website information about me"
    )
  ) %>%
  mutate(
    combined_roles = paste(A2_1, A2_2, A2_3, A2_4, A2_5, sep = " | "),
    is_programmer = if_else(str_detect(combined_roles, "Programmer|Data Scientist"), 1L, 0L),
    is_analyst = if_else(str_detect(combined_roles, "Data Analyst"), 1L, 0L),
    is_writer = if_else(str_detect(combined_roles, "Technical Writer"), 1L, 0L),
    is_pm = if_else(str_detect(combined_roles, "Project Management"), 1L, 0L),
    is_other = if_else(str_detect(combined_roles, "Other"), 1L, 0L),
    role_count = is_programmer + is_analyst + is_writer + is_pm + is_other
  )

n_active_alumni <- data_clean %>%
  filter(role_count > 0) %>%
  nrow()

count_role <- function(flag_col) {
  single <- sum(data_clean[[flag_col]] == 1L & data_clean$role_count == 1L)
  multi <- sum(data_clean[[flag_col]] == 1L & data_clean$role_count >= 2L)
  c(single_role = single, multiple_roles = multi)
}

role_stats <- tibble(
  role = c("Programmer", "Data Analyst", "Technical Writer", "Project Manager", "Other"),
  flag = c("is_programmer", "is_analyst", "is_writer", "is_pm", "is_other")
) %>%
  rowwise() %>%
  mutate(
    counts = list(count_role(flag)),
    single_role = counts[["single_role"]],
    multiple_roles = counts[["multiple_roles"]]
  ) %>%
  ungroup() %>%
  select(-flag, -counts) %>%
  mutate(
    total_pct = (single_role + multiple_roles) / n_active_alumni * 100,
    role = fct_reorder(role, total_pct)
  )

plot_data <- role_stats %>%
  select(role, single_role, multiple_roles, total_pct) %>%
  pivot_longer(
    cols = c(single_role, multiple_roles),
    names_to = "role_type",
    values_to = "count"
  ) %>%
  mutate(
    pct = count / n_active_alumni * 100,
    role_type = factor(role_type, levels = c("multiple_roles", "single_role"))
  )

common_theme <- theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#e9e9e9ff"),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 25.2, face = "bold", color = "#535353ff"),
    axis.text.x = element_text(size = 21.6, face = "bold", color = "#535353ff"),
    plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 22.8, hjust = 1, face = "bold", color = "#535353ff"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_text(size = 16.8, face = "bold"),
    legend.text = element_text(size = 24),
    legend.margin = margin(t = 5, r = 10, b = 5, l = 10),
    legend.box.margin = margin(t = 15, r = 0, b = 0, l = 0),
    legend.background = element_rect(
      color = "black",
      fill = "white",
      linewidth = 1.3,
      linetype = "solid"
    )
  )

stacked_plot <- ggplot(plot_data, aes(y = role, x = pct, fill = role_type)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = if_else(pct > 4, sprintf("%.0f%%", pct), "")),
    position = position_stack(vjust = 1, reverse = TRUE),
    hjust = 1.2,
    fontface = "bold",
    size = 7.2,
    show.legend = FALSE,
    color = "black"
  ) +
  geom_text(
    data = role_stats,
    aes(y = role, x = total_pct, label = sprintf("Total: %.0f%%", total_pct)),
    inherit.aes = FALSE,
    hjust = -0.2,
    vjust = 0.5,
    size = 6.6,
    color = "#333333",
    fontface = "bold"
  ) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1.1) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = scales::label_percent(scale = 1)
  ) +
  xlab("\nPercentage of Alumni") +
  ggtitle("What best describes your role(s) within\nDataSquad?\n") +
  scale_fill_manual(
    name = "",
    values = c(single_role = "#e6b6b6ff", multiple_roles = "#d28686ff"),
    labels = c(single_role = "Only Role", multiple_roles = "One of Multiple Roles")
  ) +
  common_theme

print(stacked_plot)
ggsave(
  filename = "stacked_roles.png",
  plot = stacked_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)
