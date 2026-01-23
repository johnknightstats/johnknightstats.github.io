#!/usr/bin/env Rscript

# ------------------------------------------------------------
# ucl_8th_place_points_probs_bar.R
#
# Reads:  data/ucl_sim_8th_place_points_probs.csv
# Writes: outputs/ucl_8th_place_points_probs.png
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

# -----------------------------
# Paths
# -----------------------------
IN_CSV  <- "data/ucl_sim_8th_place_points_probs.csv"
OUT_DIR <- "outputs"
OUT_PNG <- file.path(OUT_DIR, "ucl_8th_place_points_probs.png")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Load + prep
# -----------------------------
df <- read_csv(IN_CSV, show_col_types = FALSE)

required_cols <- c("points_8th", "prob")
missing <- setdiff(required_cols, names(df))
if (length(missing) > 0) {
  stop("Missing required columns: ", paste(missing, collapse = ", "))
}

plot_df <- df %>%
  select(points_8th, prob) %>%
  arrange(desc(prob)) %>%
  mutate(
    points_8th = factor(points_8th, levels = rev(points_8th)),
    label = percent(prob, accuracy = 0.1)
  )

x_max <- max(plot_df$prob, na.rm = TRUE)
x_lim <- x_max * 1.12

# -----------------------------
# Plot
# -----------------------------
p <- ggplot(plot_df, aes(x = prob, y = points_8th)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3.6
  ) +
  scale_x_continuous(
    limits = c(0, x_lim),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Probability of points total for 8th-placed team in CL league phase",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.5),
    axis.line.y = element_line(colour = "black", linewidth = 0.5)
  )


# -----------------------------
# Save
# -----------------------------
ggsave(
  filename = OUT_PNG,
  plot = p,
  width = 7,
  height = 4,
  dpi = 300
)
