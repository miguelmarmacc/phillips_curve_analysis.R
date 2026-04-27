# ------------------------------------------------------------
# THE PHILLIPS CURVE: INFLATION & UNEMPLOYMENT IN THE U.S.
# A Historical Analysis Across Macroeconomic Eras
# ------------------------------------------------------------
# Data:  UNRATE.csv   — BLS U-3 Unemployment Rate (FRED)
#        CPIAUCSL.csv — CPI All Urban Consumers (FRED)
# Author: Miguel Martinez Maccise
# March 15, 2026
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(broom)
library(scales)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

unemployment_raw <- read_csv("UNRATE.csv")
cpi_raw          <- read_csv("CPIAUCSL.csv")

# ------------------------------------------------------------
# 2. CLEAN & MERGE
# ------------------------------------------------------------

unemployment_clean <- unemployment_raw %>%
  rename(date = observation_date, unemployment = UNRATE) %>%
  mutate(date = as.Date(date), unemployment = as.numeric(unemployment)) %>%
  select(date, unemployment)

cpi_clean <- cpi_raw %>%
  rename(date = observation_date, cpi = CPIAUCSL) %>%
  mutate(date = as.Date(date), cpi = as.numeric(cpi)) %>%
  select(date, cpi)

df <- unemployment_clean %>%
  left_join(cpi_clean, by = "date") %>%
  arrange(date)

# ------------------------------------------------------------
# 3. COMPUTE YEAR-OVER-YEAR INFLATION
# ------------------------------------------------------------
# CPI change vs. same month one year prior (12-month lag).
# This is the standard BLS/Fed definition of the inflation
# rate and avoids the noise of compounding single-month CPI
# changes. The first 12 observations are dropped by drop_na().
# ------------------------------------------------------------

df <- df %>%
  mutate(inflation = ((cpi / lag(cpi, 12)) - 1) * 100) %>%
  drop_na()

# ------------------------------------------------------------
# 4. DEFINING MACROECONOMIC ERAS
# ------------------------------------------------------------
#  Post-War Boom    1950-1969  Classic Phillips Curve holds
#  Stagflation      1970-1983  Oil shocks shift curve upward
#  Great Moderation 1984-2007  Fed anchors expectations; curve flattens
#  Modern Era       2008-      GFC and COVID dominate the data
# ------------------------------------------------------------

df <- df %>%
  mutate(
    era = case_when(
      year(date) <= 1969 ~ "Post-War Boom (1950-69)",
      year(date) <= 1983 ~ "Stagflation (1970-83)",
      year(date) <= 2007 ~ "Great Moderation (1984-2007)",
      TRUE               ~ "Modern Era (2008-)"
    ),
    era = factor(era, levels = c(
      "Post-War Boom (1950-69)",
      "Stagflation (1970-83)",
      "Great Moderation (1984-2007)",
      "Modern Era (2008-)"
    ))
  )

# ------------------------------------------------------------
# 5. ADD LAGGED UNEMPLOYMENT (within era)
# ------------------------------------------------------------
# lag() is computed within each era to prevent pairing the
# last observation of one era with the first of the next,
# which would contaminate the regression across structural breaks.
# ------------------------------------------------------------

df <- df %>%
  group_by(era) %>%
  mutate(unemployment_lag = lag(unemployment)) %>%
  ungroup() %>%
  drop_na()

# ------------------------------------------------------------
# 6. REGRESSION MODELS
# ------------------------------------------------------------

# --- 6a. Full-sample OLS ---
model_full <- lm(inflation ~ unemployment, data = df)
summary(model_full)

# --- 6b. Full-sample with lagged unemployment ---
model_lag <- lm(inflation ~ unemployment_lag, data = df)
summary(model_lag)

# --- 6c. Era-interacted model ---
# Formally tests whether the slope differs across eras
model_era <- lm(inflation ~ unemployment * era, data = df)
summary(model_era)
tidy(model_era) %>% print(n = 20)

# --- 6d. Per-era slopes ---
era_models <- df %>%
  group_by(era) %>%
  group_modify(~ tidy(lm(inflation ~ unemployment, data = .x)))

era_models %>%
  filter(term == "unemployment") %>%
  select(era, estimate, std.error, p.value) %>%
  print()

# ------------------------------------------------------------
# 7. PLOT A — Full-Sample Phillips Curve
# ------------------------------------------------------------

ggplot(df, aes(x = unemployment, y = inflation)) +
  geom_point(size = 1.8, alpha = 0.4, color = "#2C7FB8") +
  geom_smooth(method = "lm", se = TRUE, color = "#D7301F", linewidth = 1.2) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "The Phillips Curve: U.S. Inflation vs. Unemployment",
    subtitle = "Monthly data, 1950-2026 | OLS regression with 95% CI",
    x        = "Unemployment Rate (%)",
    y        = "Year-over-Year Inflation (%)",
    caption  = "Sources: BLS (FRED: UNRATE), BLS (FRED: CPIAUCSL)"
  ) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40"),
    plot.caption     = element_text(color = "grey50", size = 9),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 8. PLOT B — Era Breakdown (Faceted)
# ------------------------------------------------------------

ggplot(df, aes(x = unemployment, y = inflation, color = era)) +
  geom_point(size = 1.5, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "#D7301F", linewidth = 1) +
  facet_wrap(~era, scales = "free") +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  scale_color_manual(values = c(
    "Post-War Boom (1950-69)"      = "#2C7FB8",
    "Stagflation (1970-83)"        = "#D7301F",
    "Great Moderation (1984-2007)" = "#31A354",
    "Modern Era (2008-)"           = "#756BB1"
  )) +
  theme_minimal(base_size = 12) +
  labs(
    title    = "The Phillips Curve Across Macroeconomic Eras",
    subtitle = "Per-era OLS fits show how the inflation-unemployment tradeoff shifted across regimes",
    x        = "Unemployment Rate (%)",
    y        = "Year-over-Year Inflation (%)",
    caption  = "Sources: BLS (FRED: UNRATE), BLS (FRED: CPIAUCSL)"
  ) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40"),
    plot.caption     = element_text(color = "grey50", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

# ------------------------------------------------------------
# 9. PLOT C — Time Series: Inflation & Unemployment Together
# ------------------------------------------------------------

df_long <- df %>%
  select(date, unemployment, inflation) %>%
  pivot_longer(
    cols      = c(unemployment, inflation),
    names_to  = "series",
    values_to = "value"
  ) %>%
  mutate(series = recode(series,
    "unemployment" = "Unemployment Rate",
    "inflation"    = "Year-over-Year Inflation"
  ))

ggplot(df_long, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  scale_color_manual(values = c(
    "Unemployment Rate"        = "#2C7FB8",
    "Year-over-Year Inflation" = "#D7301F"
  )) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "U.S. Inflation & Unemployment, 1950-2026",
    subtitle = "Stagflation era (1970-83) shows inflation elevated across all unemployment levels",
    x        = NULL,
    y        = "Rate (%)",
    color    = NULL,
    caption  = "Sources: BLS (FRED: UNRATE), BLS (FRED: CPIAUCSL)"
  ) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40"),
    plot.caption     = element_text(color = "grey50", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  )

# ============================================================
# 10. PLOT D — Modern Era Sub-Period Breakdown
# ------------------------------------------------------------
# The aggregate Modern Era slope is steep and negative, but
# this is driven by two distinct supply-side shocks rather
# than a stable structural Phillips Curve relationship:
#   - Post-GFC (2008-15): high unemployment, near-zero inflation
#   - COVID & Recovery (2020-): low unemployment, high inflation
# The Pre-COVID period (2016-19) is nearly flat, consistent
# with the Phillips Curve flattening documented in the
# Great Moderation literature.
# ============================================================

df_modern <- df %>%
  filter(era == "Modern Era (2008-)") %>%
  mutate(
    period = case_when(
      year(date) >= 2020 ~ "COVID & Recovery (2020-)",
      year(date) <= 2015 ~ "Post-GFC (2008-15)",
      TRUE               ~ "Pre-COVID (2016-19)"
    ),
    period = factor(period, levels = c(
      "Post-GFC (2008-15)",
      "Pre-COVID (2016-19)",
      "COVID & Recovery (2020-)"
    ))
  )

ggplot(df_modern, aes(x = unemployment, y = inflation, color = period)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = c(
    "Post-GFC (2008-15)"       = "#2C7FB8",
    "Pre-COVID (2016-19)"      = "#31A354",
    "COVID & Recovery (2020-)" = "#D7301F"
  )) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Modern Era: Sub-Period Breakdown",
    subtitle = "The aggregate slope is driven by supply shocks, not a stable Phillips Curve tradeoff",
    x        = "Unemployment Rate (%)",
    y        = "Year-over-Year Inflation (%)",
    color    = NULL,
    caption  = "Sources: BLS (FRED: UNRATE), BLS (FRED: CPIAUCSL)"
  ) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey40"),
    plot.caption     = element_text(color = "grey50", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  )
