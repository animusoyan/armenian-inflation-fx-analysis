library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# ==========================
# Load data and month factor
# ==========================

data_all <- read_csv("data/data_all_cpi_fx.csv", show_col_types = FALSE)

data_all <- data_all %>%
  mutate(
    month_factor = factor(
      month_num,
      levels = 1:12,
      labels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sept","Oct","Nov","Dec")
    )
  )

# ==========================
# Summaries
# ==========================

summary(data_all$infl_pct)
summary(select(data_all, dlog_usd, dlog_eur, dlog_rub))

# ==========================
# EDA: time plots and outliers
# ==========================

# --- Time series plots ---

# Inflation time series (Figure: fig_infl_timeseries.pdf)
p_infl <- ggplot(data_all, aes(x = date, y = infl_pct)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.4, color = "#1f78b4") +
  labs(
    x = "Date",
    y = "Monthly inflation (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(face = "bold")
  )

ggsave("fig_infl_timeseries.pdf", p_infl, width = 6.5, height = 3)


# FX time series (USD/EUR/RUB in one plot, Figure: fig_fx_timeseries.pdf)
fx_long <- data_all %>%
  select(date, dlog_usd, dlog_eur, dlog_rub) %>%
  pivot_longer(
    cols = c(dlog_usd, dlog_eur, dlog_rub),
    names_to = "series",
    values_to = "dlog_fx"
  ) %>%
  mutate(
    series = factor(
      series,
      levels = c("dlog_usd", "dlog_eur", "dlog_rub"),
      labels = c("USD/AMD", "EUR/AMD", "RUB/AMD")
    )
  )

p_fx <- ggplot(fx_long, aes(x = date, y = dlog_fx, color = series)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.4) +
  labs(
    x = "Date",
    y = "Monthly FX log change (%)",
    color = "FX pair"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("fig_fx_timeseries.pdf", p_fx, width = 6.5, height = 3)


# --- Boxplots ---
boxplot(data_all$infl_pct, main = "Inflation")
boxplot(data_all$dlog_usd, main = "USD")
boxplot(data_all$dlog_eur, main = "EUR")
boxplot(data_all$dlog_rub, main = "RUB")

# Top spikes 
data_all %>%
  arrange(desc(abs(infl_pct))) %>%
  select(date, infl_pct) %>%
  head(10)

data_all %>%
  arrange(desc(abs(dlog_rub))) %>%
  select(date, dlog_rub) %>%
  head(10)


# ==========================
# Monthly models
# ==========================

# baseline: inflation on lag + seasonality
model_base <- lm(infl_pct ~ infl_lag + month_factor, data = data_all)
summary(model_base)

# FX-augmented
model_fx <- lm(infl_pct ~ infl_lag + month_factor +
                 dlog_usd + dlog_eur + dlog_rub,
               data = data_all)
summary(model_fx)

# F-test for FX block
anova(model_base, model_fx)

# ==========================
# Predicting next-month inflation
# ==========================

data_next <- data_all %>%
  mutate(
    infl_next = dplyr::lead(infl_pct, 1)
  ) %>%
  filter(!is.na(infl_next))

# baseline: infl_{t+1} on infl_t + seasonality
model_base_next <- lm(infl_next ~ infl_pct + month_factor, data = data_next)
summary(model_base_next)

# FX-augmented: add current FX changes
model_fx_next <- lm(infl_next ~ infl_pct + month_factor +
                      dlog_usd + dlog_eur + dlog_rub,
                    data = data_next)
summary(model_fx_next)

# F-test for FX block
anova(model_base_next, model_fx_next)

# ==========================
# 12-month (year-on-year) inflation and FX changes
# ==========================

sum_lags <- function(x, k) {
  out <- x
  for (i in 1:k) {
    out <- out + dplyr::lag(x, i)
  }
  out
}

data_12 <- data_all %>%
  arrange(date) %>%
  mutate(
    infl12 = sum_lags(infl_pct, 11),
    usd12  = sum_lags(dlog_usd, 11),
    eur12  = sum_lags(dlog_eur, 11),
    rub12  = sum_lags(dlog_rub, 11)
  ) %>%
  filter(!is.na(infl12))

# baseline: 12-month inflation on seasonality
model12_base <- lm(infl12 ~ month_factor, data = data_12)
summary(model12_base)

# FX-augmented: 12-month FX changes
model12_fx <- lm(infl12 ~ month_factor + usd12 + eur12 + rub12,
                 data = data_12)
summary(model12_fx)

# F-test for 12-month FX block
anova(model12_base, model12_fx)

# ==========================
# 12-month inflation with lag and FX
# ==========================

data_12_lag <- data_12 %>%
  arrange(date) %>%
  mutate(
    infl12_lag = dplyr::lag(infl12)
  ) %>%
  filter(!is.na(infl12_lag))

# baseline: infl12 on infl12_lag + seasonality
model12lag_base <- lm(infl12 ~ infl12_lag + month_factor, data = data_12_lag)
summary(model12lag_base)

# FX-augmented
model12lag_fx <- lm(infl12 ~ infl12_lag + month_factor +
                      usd12 + eur12 + rub12,
                    data = data_12_lag)
summary(model12lag_fx)

# F-test for FX block given infl12_lag
anova(model12lag_base, model12lag_fx)

# add fitted values and residuals from the final 12-month model
data_12_lag <- data_12_lag %>%
  mutate(
    fitted12 = fitted(model12lag_fx),
    resid12  = resid(model12lag_fx)
  )


# ==========================
# Diagnostics for final 12-month model 
# ==========================

# Residuals vs fitted (fig_resid_vs_fitted.pdf)
p_resid_fit <- ggplot(data_12_lag, aes(x = fitted12, y = resid12)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_point(alpha = 0.5, size = 0.7) +
  labs(
    x = "Fitted 12-month inflation",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("fig_resid_vs_fitted.pdf", p_resid_fit, width = 6, height = 3)


# Residuals over time (fig_resid_time.pdf)
p_resid_time <- ggplot(data_12_lag, aes(x = date, y = resid12)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_line(linewidth = 0.4, color = "#1f78b4") +
  labs(
    x = "Date",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("fig_resid_time.pdf", p_resid_time, width = 6.5, height = 3)


# QQ-plot of residuals (fig_resid_qq.pdf)
qq <- qqnorm(data_12_lag$resid12, plot.it = FALSE)
df_qq <- data.frame(
  theoretical = qq$x,
  sample = qq$y
)

p_resid_qq <- ggplot(df_qq, aes(x = theoretical, y = sample)) +
  geom_abline(slope = 1, intercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.5, size = 0.7) +
  labs(
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("fig_resid_qq.pdf", p_resid_qq, width = 6, height = 3)


# Histogram of residuals (fig_resid_hist.pdf)
p_resid_hist <- ggplot(data_12_lag, aes(x = resid12)) +
  geom_histogram(bins = 30, color = "white", fill = "#1f78b4") +
  labs(
    x = "Residuals",
    y = "Count"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("fig_resid_hist.pdf", p_resid_hist, width = 6, height = 3)


# ==========================
# Influence check (Cook's distance) 
# ==========================

cook <- cooks.distance(model12lag_fx)
df_cook <- data.frame(
  index = seq_along(cook),
  cook = cook
)

p_cook <- ggplot(df_cook, aes(x = index, y = cook)) +
  geom_segment(aes(xend = index, yend = 0)) +
  geom_hline(yintercept = 4 / length(cook),
             linetype = "dashed",
             color = "red",
             linewidth = 0.4) +
  labs(
    x = "Observation index",
    y = "Cook's distance"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("fig_cooks_distance.pdf", p_cook, width = 6.5, height = 3.2)

# Top influential points (for discussion)
data_12_lag %>%
  mutate(cook = cook) %>%
  arrange(desc(cook)) %>%
  select(date, infl12, usd12, eur12, rub12, cook) %>%
  head(10)


p_infl
p_fx
p_resid_fit
p_resid_time
p_resid_qq
p_resid_hist
p_cook

getwd()
list.files()




