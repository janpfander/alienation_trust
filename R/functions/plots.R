library(ggplot2)
library(dplyr)
library(MetBrewer)

# ── Palette ────────────────────────────────────────────────────────────────────
# 4 well-spaced colors from the Juarez palette (warm rust → cool navy).
# All plot functions draw from plot_palette so colors stay coherent across figures.

plot_palette <- met.brewer("Juarez", n = 10)[c(1, 4, 7, 10)]
col_ns       <- "grey75"   # non-significant points / error bars

# ── Theme ─────────────────────────────────────────────────────────────────────

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title           = element_text(face = "bold", size = rel(1)),
    plot.subtitle        = element_text(face = "plain", size = rel(0.9), color = "grey70"),
    axis.title           = element_text(face = "bold", size = rel(0.85)),
    axis.title.x         = element_text(hjust = 0, margin = ggplot2::margin(t = 10)),
    axis.title.y         = element_text(hjust = 1, margin = ggplot2::margin(r = 10)),
    axis.text            = element_text(size = rel(0.8)),
    axis.ticks           = element_blank(),
    panel.grid.minor     = element_blank(),
    panel.grid.major     = element_line(linewidth = 0.25, colour = "grey90"),
    panel.spacing        = unit(1, "lines"),
    strip.text           = element_text(face = "bold", size = rel(0.9), hjust = 0),
    strip.background     = element_rect(fill = "white", colour = NA),
    legend.position      = "top",
    legend.justification = "left",
    legend.title         = element_text(face = "bold", size = rel(0.8)),
    legend.text          = element_text(size = rel(0.8)),
    legend.key.size      = unit(0.7, "line"),
    legend.key           = element_blank(),
    legend.margin        = ggplot2::margin(t = -5, b = 0, l = 0, r = 0)
  )

# ── Plot functions ─────────────────────────────────────────────────────────────

# Coefficient / forest plot.
#
# Expects a tidy results tibble with columns:
#   estimate, conf.low, conf.high, p_adjusted, and a label column (label_col).
# Points and error bars are drawn in plot_palette colors; non-significant results
# (p_adjusted >= sig_threshold) fall back to col_ns.
#
# Arguments:
#   results_df     tidy tibble from run_ols_model + p_adjusted
#   label_col      column name to use as y-axis labels
#   title          optional plot title
#   x_label        x-axis label (default: "Standardized coefficient")
#   sig_threshold  significance cutoff for color (default 0.05)
plot_coefficients <- function(results_df,
                              label_col     = "predictor",
                              title         = NULL,
                              x_label       = "Standardized coefficient",
                              sig_threshold = 0.05) {
  n_rows <- nrow(results_df)
  cols   <- plot_palette[seq_len(min(n_rows, length(plot_palette)))]

  results_df |>
    mutate(
      .label = .data[[label_col]],
      .color = if_else(p_adjusted < sig_threshold, cols[seq_len(n_rows)], col_ns)
    ) |>
    ggplot(aes(x = estimate, y = reorder(.label, estimate), color = .color)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0, linewidth = 0.6) +
    geom_point(size = 3) +
    scale_color_identity(guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = x_label, y = NULL, title = title) +
    plot_theme
}

# Interaction plot.
#
# Shows predicted outcome across the range of the moderator for low (-1 SD) and
# high (+1 SD) values of the predictor. Fits an OLS model internally on
# standardized variables. Line colors are taken from the ends of plot_palette.
#
# Arguments:
#   data                        data frame
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

  mod_seq  <- seq(min(df[[moderator]]), max(df[[moderator]]), length.out = 50)
  grid_df  <- expand.grid(c(-1, 1), mod_seq) |>
    setNames(c(predictor, moderator)) |>
    as_tibble() |>
    mutate(
      .fitted    = predict(fit, newdata = pick(all_of(c(predictor, moderator)))),
      pred_group = factor(
        .data[[predictor]],
        levels = c(-1, 1),
        labels = c(paste0("Low ", pred_label, " (−1 SD)"),
                   paste0("High ", pred_label, " (+1 SD)"))
      )
    )

  ggplot(grid_df, aes(x = .data[[moderator]], y = .fitted, color = pred_group)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c(plot_palette[1], plot_palette[length(plot_palette)]),
      name   = pred_label
    ) +
    labs(
      x = paste0(mod_label, " (standardized)"),
      y = paste0(out_label, " (standardized)")
    ) +
    plot_theme
}
