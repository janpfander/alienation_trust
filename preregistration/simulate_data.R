############################################################
# Simulate data for preregistration
# Alienation and Trust in Climate Scientists
# Mirrors structure of main megastudy simulation script.
# Pure random data — NO programmed effects or correlations.
############################################################

library(tidyverse)
library(here)

set.seed(42)

############################################################
# Global parameters
############################################################

N <- 500   # small demo sample for preregistration

# Two conditions: control group used for robustness checks
conditions <- c("control", "intervention")

############################################################
# Helper functions (identical to main megastudy script)
############################################################

# Slider: bounded [0, 100]
r_slider <- function(n) pmin(pmax(rnorm(n, 50, 30), 0), 100)

# Likert: integer scale from `lo` to `hi`
r_likert <- function(n, lo = 1, hi = 7) sample(lo:hi, n, replace = TRUE)

# Generic categorical draw
r_cat <- function(n, levels) factor(sample(levels, n, replace = TRUE), levels = levels)

############################################################
# Core dataset
############################################################

dat <- tibble(
  id        = 1:N,
  condition = factor(
    sample(conditions, N, replace = TRUE, prob = c(0.5, 0.5)),
    levels = conditions
  )
)

############################################################
# Demographics
############################################################

dat <- dat |>
  mutate(

    # Gender [Male; Female; Other]
    gender = r_cat(N, c("Male", "Female", "Other")),

    # Year of birth (text box) -> age derived
    year_birth = round(pmin(pmax(rnorm(N, 1979, 18), 1934), 2006)),
    age        = 2025L - year_birth,

    # Race/ethnicity
    race = r_cat(N, c(
      "White / Caucasian",
      "Black / African American",
      "Hispanic / Latino",
      "Asian / Asian American",
      "Other"
    )),

    # Education — 6 levels
    education = r_cat(N, c(
      "Less than high school",
      "High school diploma / GED",
      "Some college or Associate's degree",
      "Bachelor's degree",
      "Master's degree / Professional degree",
      "Doctorate degree / Ph.D."
    )),

    # Income — 5 brackets
    income = r_cat(N, c(
      "Less than $30,000",
      "$30,000 to $55,999",
      "$56,000 to $99,999",
      "$100,000 to $167,999",
      "$168,000 or more"
    )),

    # Household size
    household_size = r_cat(N, c("1", "2", "3", "4", "5", "6 or more")),

    # Social class
    social_class = r_cat(N, c(
      "Lower class",
      "Working class",
      "Middle class",
      "Upper class"
    )),

    # Urban / rural
    urban_rural = r_cat(N, c(
      "A large city",
      "A suburb near a large city",
      "A small city or town",
      "A rural area"
    )),

    # Zip code
    zip_code = sprintf("%05d", sample(10000:99999, N, replace = TRUE))
  )

############################################################
# Partisan identity
############################################################

dat <- dat |>
  mutate(
    party = r_cat(N, c("Republican", "Democrat", "Independent", "Other")),
    party_importance = if_else(
      party %in% c("Republican", "Democrat"),
      r_slider(N),
      NA_real_
    )
  )

############################################################
# Religion
############################################################

dat <- dat |>
  mutate(
    religion = r_cat(N, c(
      "I am not religious",
      "Protestant",
      "Catholic",
      "Orthodox Christian",
      "Mormon",
      "Muslim",
      "Jewish",
      "Hindu",
      "Buddhist",
      "Other religion"
    )),
    born_again = if_else(
      religion %in% c("Protestant", "Catholic", "Orthodox Christian", "Mormon"),
      r_cat(N, c("Yes", "No")),
      NA_character_
    ),
    born_again = factor(born_again, levels = c("Yes", "No")),
    religiosity = if_else(
      religion != "I am not religious",
      r_slider(N),
      NA_real_
    )
  )

############################################################
# Need for epistemic autonomy (6 items, 1-7 Likert)
# Item 6 is reverse-scored
############################################################

epist_auton_items <- map_dfc(1:6, ~ r_likert(N, 1, 7)) |>
  set_names(paste0("epist_auton_", 1:6))

dat <- bind_cols(dat, epist_auton_items) |>
  mutate(
    epist_auton_6r   = 8L - epist_auton_6,
    epist_auton_mean = rowMeans(cbind(
      epist_auton_1, epist_auton_2, epist_auton_3,
      epist_auton_4, epist_auton_5, epist_auton_6r
    ))
  )

############################################################
# Pre-treatment measures
############################################################

dat <- dat |>
  mutate(
    belief_pre = r_slider(N),
    trust_pre  = r_slider(N)   # PRIMARY outcome for this project
  )

############################################################
# Alienation from climate science
# Variable names match main megastudy naming convention
############################################################

# Institutional alienation (2 items, 1-7 Likert)
alien_inst_items <- map_dfc(1:2, ~ r_likert(N)) |>
  set_names(c("alien_inst_1", "alien_inst_2"))

# Social alienation (2 items, 1-7 Likert)
alien_social_items <- map_dfc(1:2, ~ r_likert(N)) |>
  set_names(c("alien_social_1", "alien_social_2"))

# Spatial alienation (2 items, 1-7 Likert)
alien_spatial_items <- map_dfc(1:2, ~ r_likert(N)) |>
  set_names(c("alien_spatial_1", "alien_spatial_2"))

# Informational alienation (6 items, 5-level frequency scale)
# Items ask about frequency of exposure; reverse-coded so higher = less exposure = more alienation
alien_info_levels <- c("Never", "Rarely", "Occasionally", "Frequently", "Very frequently")

alien_info_items <- map_dfc(1:6, ~ r_cat(N, alien_info_levels)) |>
  set_names(paste0("alien_info_", 1:6))

dat <- bind_cols(dat, alien_inst_items, alien_social_items,
                 alien_spatial_items, alien_info_items) |>
  mutate(
    alien_inst_mean   = rowMeans(cbind(alien_inst_1, alien_inst_2)),
    alien_social_mean = rowMeans(cbind(alien_social_1, alien_social_2)),
    alien_spatial_mean = rowMeans(cbind(alien_spatial_1, alien_spatial_2)),
    # Recode to numeric (1-5) then reverse so higher = more alienation
    across(starts_with("alien_info_"),
           ~ 6L - as.integer(factor(.x, levels = alien_info_levels)),
           .names = "{.col}_r"),
    alien_info_mean = rowMeans(pick(ends_with("_r") & starts_with("alien_info_")))
  )

############################################################
# Geographic distance to nearest research university
# (unique to this secondary project; not in main megastudy)
# Derived from zip code in real data; simulated as lognormal here
############################################################

dat <- dat |>
  mutate(dist_inst_km = exp(rnorm(N, mean = 3.5, sd = 1.2)))

############################################################
# Primary outcome: Multidimensional trust (12 items, 0-100)
# NOTE: post-treatment in megastudy; used here as ROBUSTNESS CHECK
# for control group only
############################################################

trust_competence_items  <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_competence_",  1:3))
trust_integrity_items   <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_integrity_",   1:3))
trust_benevolence_items <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_benevolence_", 1:3))
trust_openness_items    <- map_dfc(1:3, ~ r_slider(N)) |> set_names(paste0("trust_openness_",    1:3))

dat <- bind_cols(dat, trust_competence_items, trust_integrity_items,
                 trust_benevolence_items, trust_openness_items) |>
  mutate(
    trust_competence       = rowMeans(trust_competence_items),
    trust_integrity        = rowMeans(trust_integrity_items),
    trust_benevolence      = rowMeans(trust_benevolence_items),
    trust_openness         = rowMeans(trust_openness_items),
    trust_multidimensional = rowMeans(cbind(
      trust_competence, trust_integrity, trust_benevolence, trust_openness
    )),
    trust_post = r_slider(N)
  )

############################################################
# Policy role of scientists (4 items, 0-100)
############################################################

policy_role_items <- map_dfc(1:4, ~ r_slider(N)) |>
  set_names(paste0("policy_role_", 1:4))

dat <- bind_cols(dat, policy_role_items) |>
  mutate(policy_role_mean = rowMeans(policy_role_items))

############################################################
# Save
############################################################

dir.create(here("data/simulation"), recursive = TRUE, showWarnings = FALSE)
saveRDS(dat, here("data/simulation/simulated_data.rds"))

message("Simulated data saved: ", nrow(dat), " rows, ", ncol(dat), " columns.")
