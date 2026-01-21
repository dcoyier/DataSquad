# Dashiell Coyier
# Last ran 1/20/2026
# Themes from qualitative analysis double bar chart, alumni

library(ggplot2)
library(dplyr)
library(stringr)

make_qualitative_themes_plot <- function(csv_path, font_scale = 1.4) {
  if (!file.exists(csv_path)) {
    stop("CSV file not found. Please check the path.")
  }

  raw_df <- read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )

  df <- raw_df[-c(1, 2), , drop = FALSE]
  col_names <- names(df)
  category_cols <- col_names[1:15]

  label_lookup <- as.character(unlist(raw_df[1, category_cols], use.names = FALSE))
  names(label_lookup) <- category_cols
  stopifnot(all(!is.na(label_lookup)))

  df[category_cols] <- lapply(df[category_cols], \(x) as.logical(as.character(x)))
  counts <- vapply(df[category_cols], \(x) sum(x, na.rm = TRUE), numeric(1))

  build_group_df <- function(main_cat, sub_cats, group_name) {
    temp <- data.frame(
      OriginalID = sub_cats,
      Count = unname(counts[sub_cats]),
      Type = "Sub",
      RawLabel = unname(label_lookup[sub_cats]),
      Group = group_name,
      MainTotal = unname(counts[main_cat]),
      stringsAsFactors = FALSE
    )

    temp <- temp[order(temp$Count, decreasing = TRUE), ]
    temp$DisplayLabel <- temp$RawLabel
    temp$UniqueOrder <- seq_len(nrow(temp))
    temp
  }

  all_data <- rbind(
    build_group_df("Cat1", c("Cat1.1", "Cat1.2", "Cat1.3", "Cat1.4", "Cat1.5"), "Communication"),
    build_group_df("Cat2", c("Cat2.1", "Cat2.2", "Cat2.3", "Cat2.4"), "Real Problems"),
    build_group_df("Cat3", c("Cat3.1", "Cat3.2", "Cat3.3"), "Foundational\nExperiences")
  )

  all_data$DisplayLabel <- dplyr::recode(
    all_data$DisplayLabel,
    "Confidence / Empowerment / Accomplishment" = "Empowerment",
    "Career Relevance / Job Prep" = "Career Relevance",
    "Teamwork / Peers" = "Teamwork",
    "Across Experience Levels" = "Experience Levels",
    "Practical Data Experiences / Struggle" = "Practical Data",
    "Mentorship / Personalization" = "Mentorship"
  )

  group_totals <- unique(all_data[, c("Group", "MainTotal")])
  group_order <- group_totals$Group[order(group_totals$MainTotal, decreasing = TRUE)]
  all_data$Group <- factor(all_data$Group, levels = group_order)

  all_data$WrappedLabel <- stringr::str_wrap(all_data$DisplayLabel, width = 20)
  all_data <- all_data[order(all_data$Group, all_data$UniqueOrder), ]
  all_data$WrappedLabel <- factor(all_data$WrappedLabel, levels = unique(all_data$WrappedLabel))

  ggplot(all_data, aes(x = WrappedLabel, y = Count)) +
    geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = MainTotal, fill = "Main Category"),
      alpha = 1
    ) +
    geom_hline(yintercept = -0.025, color = "black", linewidth = 0.5) +
    geom_text(
      aes(
        x = -Inf, y = MainTotal,
        label = paste("\n ", gsub("\n", "\n  ", Group))
      ),
      hjust = 0, vjust = 1, color = "black",
      size = 5.8 * font_scale, fontface = "bold.italic", check_overlap = TRUE
    ) +
    geom_text(
      aes(
        x = Inf, y = MainTotal,
        label = paste("\n", MainTotal, " ")
      ),
      hjust = 1.2, vjust = 0.78, color = "black",
      size = 7.5 * font_scale, fontface = "bold", check_overlap = TRUE
    ) +
    geom_col(aes(fill = "Subcategory"), width = 0.7) +
    geom_text(aes(label = Count), vjust = 2, size = 7.5 * font_scale, fontface = "bold") +
    facet_grid(~Group, scales = "free_x", space = "free_x") +
    scale_fill_manual(
      name = NULL,
      values = c("Main Category" = "#a9e5bdff", "Subcategory" = "#66b685ff"),
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    scale_y_continuous(limits = c(0, 18), breaks = c(0, 6, 12, 18)) +
    labs(
      x = NULL,
      y = "Count",
      title = "How did your DataSquad experience help you develop\nnew skills beyond your Carleton coursework?",
      subtitle = "Which experiences or moments from your time on\nthe DataSquad have stayed with you?"
    ) +
    theme_classic(base_size = 17 * font_scale) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 20 * font_scale, face = "bold", color = "#535353ff"),
      axis.text.y = element_text(size = 21 * font_scale, face = "bold", color = "#535353ff"),
      axis.title.y = element_text(face = "bold", size = 25 * font_scale, color = "#535353ff"),
      plot.title = element_text(
        face = "bold",
        size = 27 * font_scale,
        hjust = 0.5,
        margin = margin(b = 25)
      ),
      plot.subtitle = element_text(
        face = "bold",
        size = 27 * font_scale,
        color = "black",
        hjust = 0.5,
        margin = margin(b = 20),
        lineheight = 1.0
      ),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 20 * font_scale),
      legend.key.size = grid::unit(1, "cm"),
      legend.box.background = element_rect(color = "black", linewidth = 3),
      panel.spacing = grid::unit(3, "lines"),
      plot.margin = margin(20, 30, 20, 20)
    )
}

csv_path <- "/Users/dashiellcoyier/Desktop/DataSquad/Final-visuals/Oct 21, 2025-Qualtrics+openended variable creation-Data DataSquad Alumni - Harmonized qualitative data Oct21.csv"
p <- make_qualitative_themes_plot(csv_path)
print(p)

ggsave("qualitative_themes.png", plot = p, width = 16, height = 14, dpi = 300)
