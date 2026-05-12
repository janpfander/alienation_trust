library(ggplot2)
library(dplyr)
library(MetBrewer)

# ── Theme ─────────────────────────────────────────────────────────────────────
# Copied from parent project (trust_climate_scientists/R/functions/plots.R)

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = rel(1)),
    plot.subtitle      = element_text(face = "plain", size = rel(0.9), color = "grey70"),
    axis.title         = element_text(face = "bold", size = rel(0.85)),
    axis.title.x       = element_text(hjust = 0, margin = ggplot2::margin(t = 10)),
    axis.title.y       = element_text(hjust = 1, margin = ggplot2::margin(r = 10)),
    axis.text          = element_text(size = rel(0.8)),
    axis.ticks         = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(linewidth = 0.25, colour = "grey90"),
    panel.spacing      = unit(1, "lines"),
    strip.text         = element_text(face = "bold", size = rel(0.9), hjust = 0),
    strip.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "top",
    legend.justification = "left",
    legend.title       = element_text(face = "bold", size = rel(0.8)),
    legend.text        = element_text(size = rel(0.8)),
    legend.key.size    = unit(0.7, "line"),
    legend.key         = element_blank(),
    legend.margin      = ggplot2::margin(t = -5, b = 0, l = 0, r = 0)
  )

# ── Color palette ─────────────────────────────────────────────────────────────

juarez_colors <- met.brewer("Juarez", n = 10)

# ── Plot functions ─────────────────────────────────────────────────────────────

# Coefficient / forest plot.
#
# Expects a tidy results tibble with columns:
#   estimate, conf.low, conf.high, p_adjusted, and a label column (label_col).
# Points and error bars are colored by alienation dimension using the Juarez
# palette; non-significant results (p_adjusted >= sig_threshold) are grayed out.
#
# Arguments:
#   results_df     tidy tibble from run_ols_model + p_adjusted
#   label_col      column name to use as y-axis labels
#   title, x_label optional plot labels
#   sig_threshold  significance cutoff for color (default 0.05)
plot_coefficients <- function(results_df,
                              label_col   = "predictor",
                              title       = NULL,
                              x_label     = "Standardized coefficient",
                              sig_threshold = 0.05) {
  n_rows  <- nrow(results_df)
  colors  <- juarez_colors[seq_len(min(n_rows, 10))]

  results_df |>
    mutate(
      label     = .data[[label_col]],
      sig       = p_adjusted < sig_threshold,
      row_color = colors[seq_len(n_rows)]
    ) |>
    ggplot(aes(x = estimate, y = reorder(label, estimate))) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.4) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high,
          color = ifelse(sig, row_color, "grey70")),
      height = 0, linewidth = 0.6
    ) +
    geom_point(
      aes(color = ifelse(sig, row_color, "grey70")),
      size = 3
    ) +
    scale_color_identity(guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = x_label, y = NULL, title = title) +
    plot_theme
}

# Interaction plot.
#
# Shows predicted outcome across the range of the moderator, separately for
# low (-1 SD) and high (+1 SD) values of the predictor. Fits an OLS model
# internally on standardized variables.
#
# Arguments:
#   data                  data frame
#   outcome, predictor, moderator   column names (character)
#   pred_label, mod_label, out_label  axis/legend labels (default: column names)
plot_interaction <- function(data,
                              outcome,
                              predictor,
                              moderator,
                              pred_label = predictor,
                              mod_label  = moderator,
                              out_label  = outcome) {
  df <- data |>
    select(all_of(c(outcome, predictor, moderator))) |>
    drop_na() |>
    mutate(across(where(is.numeric), \(x) (x - mean(x)) / sd(x)))

  formula <- as.formula(paste0(outcome, " ~ ", predictor, " * ", moderator))
  fit     <- lm(formula, data = df)

  mod_seq <- seq(min(df[[moderator]]), max(df[[moderator]]), length.out = 50)

  grid_df <- expand.grid(c(-1, 1), mod_seq)
  names(grid_df) <- c(predictor, moderator)
  grid_df <- as_tibble(grid_df) |>
    mutate(
      .fitted    = predict(fit, newdata = pick(all_of(c(predictor, moderator)))),
      pred_group = factor(
        .data[[predictor]],
        levels = c(-1, 1),
        labels = c(paste0("Low ", pred_label, " (-1 SD)"),
                   paste0("High ", pred_label, " (+1 SD)"))
      )
    )

  ggplot(grid_df, aes(x = .data[[moderator]], y = .fitted, color = pred_group)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c(juarez_colors[1], juarez_colors[10]),
      name   = pred_label
    ) +
    labs(
      x = paste0(mod_label, " (standardized)"),
      y = paste0(out_label, " (standardized)")
    ) +
    plot_theme
}
