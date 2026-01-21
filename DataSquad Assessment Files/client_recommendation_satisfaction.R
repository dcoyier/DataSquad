# Dashiell Coyier
# Last ran 1/20/2026
# Likelihood to recommend DataSquad and satisfaction ratings for clients, weighted grid/dot plot

library(tidyverse)
library(scales)
library(patchwork)

make_client_feedback_plot <- function(
  data_path = "/Users/dashiellcoyier/Desktop/DataSquad/Final-visuals/DSAssessment-Client Feedback 17 October '25_October 21, 2025_16.00 - PLackie - Client Feedback November 10, 2025_10.16 2.csv"
) {
  fs <- 1.3
  min_dot <- 5
  max_dot <- 29

  client_feedback <- read_csv(data_path, skip = 2, na = c("", "NA"))

  prep_recommendation <- function(df) {
    df %>%
      select(raw_score = `How likely are you to recommend working with the DataSquad to a colleague?`) %>%
      filter(!is.na(raw_score)) %>%
      mutate(score = as.numeric(raw_score)) %>%
      count(score, name = "frequency") %>%
      complete(score = 0:10, fill = list(frequency = 0)) %>%
      mutate(freq_sq = frequency^2)
  }

  prep_satisfaction <- function(df) {
    df %>%
      select(
        `Quality of Work` = contains("Satisfaction - Quality of the Work"),
        `Speed of Delivery` = contains("Satisfaction - Speed of Delivery"),
        `Responsiveness` = contains("Satisfaction - Communication & Responsiveness"),
        `Ease of Process` = contains("Satisfaction - Ease of Process")
      ) %>%
      pivot_longer(everything(), names_to = "category", values_to = "rating") %>%
      filter(!is.na(rating)) %>%
      mutate(rating = as.numeric(rating)) %>%
      group_by(category) %>%
      count(rating, name = "frequency") %>%
      complete(rating = 0:5, fill = list(frequency = 0)) %>%
      ungroup() %>%
      mutate(freq_sq = frequency^2)
  }

  rec_prep <- prep_recommendation(client_feedback)
  sat_prep <- prep_satisfaction(client_feedback)

  max_sq_global <- max(c(rec_prep$freq_sq, sat_prep$freq_sq), na.rm = TRUE)

  calc_text_size <- function(val_sq, frequency) {
    # Size text relative to dot size; reduce text for high frequencies.
    dot_size_proxy <- scales::rescale(
      val_sq,
      to = c(min_dot, max_dot),
      from = c(1, max_sq_global)
    )
    base_font_size <- dot_size_proxy * 0.85
    ifelse(frequency >= 10, base_font_size * 0.65, base_font_size)
  }

  rec_data <- rec_prep %>% mutate(txt_size = calc_text_size(freq_sq, frequency))
  sat_data <- sat_prep %>% mutate(txt_size = calc_text_size(freq_sq, frequency))

  orig_max <- sqrt(max_sq_global)
  orig_breaks <- scales::pretty_breaks(n = 5)(c(1, orig_max))
  orig_breaks <- orig_breaks[orig_breaks > 0]
  squared_breaks <- orig_breaks^2

  shared_size_scale <- scale_size_continuous(
    limits = c(1, max_sq_global),
    range = c(min_dot, max_dot),
    breaks = squared_breaks,
    labels = orig_breaks,
    name = "Frequency",
    guide = guide_legend(reverse = TRUE)
  )

  shared_color_scale <- scale_color_gradient(
    low = "#B0B0C0",
    high = "#323044",
    limits = c(1, max_sq_global),
    breaks = squared_breaks,
    labels = orig_breaks,
    name = "Frequency",
    guide = guide_legend(reverse = TRUE)
  )

  common_theme <- theme_minimal(base_size = 14 * fs) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 20 * fs,
        margin = margin(l = 10, b = 150, t = 50),
        hjust = 0.5
      ),
      plot.title.position = "plot",
      plot.margin = margin(t = 100, r = 20, b = 50, l = 20),
      axis.title.y = element_text(size = 14 * fs),
      axis.text.x = element_text(size = 8 * fs),
      axis.text.y = element_text(size = 12 * fs),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )

  p1 <- ggplot(rec_data, aes(x = "Recommendation\nLikelihood", y = score)) +
    geom_point(aes(size = freq_sq, color = freq_sq)) +
    geom_text(
      aes(label = frequency, size = I(txt_size)),
      color = "white", fontface = "bold", show.legend = FALSE
    ) +
    scale_y_continuous(
      limits = c(-0.5, 10.5),
      breaks = 0:10,
      labels = 0:10,
      expand = c(0, 0)
    ) +
    shared_size_scale +
    shared_color_scale +
    coord_cartesian(clip = "off") + # prevent large dots from being clipped
    labs(
      title = "How likely are you to\nrecommend working\n with the DataSquad\nto a colleague?",
      x = NULL,
      y = "Likelihood Score (0-10 scale)\n"
    ) +
    common_theme

  p2 <- ggplot(sat_data, aes(x = category, y = rating)) +
    geom_point(aes(size = freq_sq, color = freq_sq)) +
    geom_text(
      aes(label = frequency, size = I(txt_size)),
      color = "white", fontface = "bold", show.legend = FALSE
    ) +
    scale_y_continuous(
      limits = c(-0.25, 5.25),
      breaks = 0:5,
      labels = 0:5,
      expand = c(0, 0)
    ) +
    shared_size_scale +
    shared_color_scale +
    coord_cartesian(clip = "off") + # prevent large dots from being clipped
    labs(
      title = "Please rate the following\naspects of your experience:",
      x = NULL,
      y = "\nSatisfaction Rating (0-5 scale)\n"
    ) +
    common_theme

  combined_plot <- (p1 + p2) +
    plot_layout(ncol = 2, widths = c(1, 2.5), guides = "collect") &
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        size = 22 * fs, face = "bold", color = "#535353ff"
      ),
      axis.text.y = element_text(size = 20 * fs, face = "bold", color = "#535353ff"),
      axis.title.y = element_text(face = "bold", size = 24 * fs, color = "#535353ff"),
      plot.title = element_text(face = "bold", size = 30 * fs, margin = margin(b = 70), hjust = 0.5),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 23 * fs),
      legend.title = element_text(size = 20 * fs),
      legend.box.background = element_rect(color = "black", linewidth = 2),
      panel.spacing = unit(3, "lines"),
      plot.margin = margin(40, 30, 20, 20)
    )

  combined_plot
}

# print(make_client_feedback_plot())
ggsave("client_recommendation_satisfaction.png", make_client_feedback_plot(), width = 16, height = 16, dpi = 300)
