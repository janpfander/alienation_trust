library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)
library(here)

# Standardize a numeric vector to mean 0, SD 1
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Round all numeric columns in a data frame
rounded_numbers <- function(df, digits = 3) {
  df |> mutate(across(where(is.numeric), \(x) round(x, digits)))
}

# OLS regression with HC2 heteroskedasticity-robust standard errors.
#
# Returns a one-row tibble with the focal predictor's coefficient, so that
# results from multiple predictors can be combined with map_dfr().
# BH correction is NOT applied here — call p.adjust() on the combined results.
#
# Arguments:
#   data                 data frame
#   outcome              character: name of the outcome variable
#   predictor            character: name of the focal predictor
#   covariates           character vector of covariate names (default: NULL)
#   standardize_predictors logical: z-score all numeric vars before fitting (default: TRUE)
run_ols_model <- function(data,
                          outcome,
                          predictor,
                          covariates = NULL,
                          standardize_predictors = TRUE) {
  vars <- c(outcome, predictor, covariates)
  df   <- data |> select(all_of(vars)) |> drop_na()

  if (standardize_predictors) {
    df <- df |> mutate(across(where(is.numeric), standardize))
  }

  rhs     <- paste(c(predictor, covariates), collapse = " + ")
  formula <- as.formula(paste(outcome, "~", rhs))
  fit     <- lm(formula, data = df)
  se_hc2  <- vcovHC(fit, type = "HC2")

  tidy(coeftest(fit, vcov = se_hc2)) |>
    filter(str_starts(term, predictor) & term != "(Intercept)") |>
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      outcome   = outcome,
      predictor = predictor,
      n         = nrow(df)
    )
}

# OLS interaction model: outcome ~ predictor * moderator [+ covariates].
#
# Returns ALL terms so the caller can filter to the interaction row and/or
# inspect main effects. BH correction applied by caller.
#
# Arguments:
#   data, outcome, predictor, covariates — same as run_ols_model
#   moderator            character: name of the moderating variable
run_interaction_model <- function(data,
                                  outcome,
                                  predictor,
                                  moderator,
                                  covariates = NULL,
                                  standardize_predictors = TRUE) {
  vars <- c(outcome, predictor, moderator, covariates)
  df   <- data |> select(all_of(vars)) |> drop_na()

  if (standardize_predictors) {
    df <- df |> mutate(across(where(is.numeric), standardize))
  }

  rhs     <- paste(
    c(paste0(predictor, " * ", moderator), covariates),
    collapse = " + "
  )
  formula <- as.formula(paste(outcome, "~", rhs))
  fit     <- lm(formula, data = df)
  se_hc2  <- vcovHC(fit, type = "HC2")

  tidy(coeftest(fit, vcov = se_hc2)) |>
    mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      outcome   = outcome,
      predictor = predictor,
      moderator = moderator,
      n         = nrow(df)
    )
}
