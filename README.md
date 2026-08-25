# Exchange Rate Movements and Armenian Inflation

A statistical analysis of the relationship between Armenian consumer price inflation and AMD exchange-rate movements against the US dollar, euro, and Russian ruble.

This project was completed as a three-person academic project for the **CS108 Statistics** course at the **American University of Armenia**.

## Project Overview

The project investigates whether movements in the Armenian dram exchange rate help explain or predict inflation in Armenia.

We analyze monthly Armenian CPI data together with AMD/USD, AMD/EUR, and AMD/RUB exchange rates and study the relationship at both short and medium horizons.

The analysis focuses on three questions:

1. Do monthly exchange-rate changes help explain current monthly inflation after controlling for past inflation and seasonality?
2. Do current exchange-rate movements improve one-month-ahead inflation predictions?
3. Do cumulative 12-month exchange-rate movements help explain 12-month inflation after accounting for inflation persistence?

## Data

The analysis uses:

* Armenian Consumer Price Index data from **ArmStatBank**
* Official AMD exchange rates from the **Central Bank of Armenia**
* AMD/USD
* AMD/EUR
* AMD/RUB

The raw CPI and exchange-rate data are transformed into monthly variables and merged into a single analysis dataset.

## Methodology

The project uses statistical and regression techniques including:

* Multiple linear regression
* Lagged inflation variables
* Monthly seasonality controls
* Monthly exchange-rate log changes
* 12-month cumulative variables
* t-tests for individual coefficients
* F-tests for groups of exchange-rate variables
* Residual diagnostics
* QQ plots and residual distributions
* Cook's distance for influence analysis

All analysis was performed in **R**.

## Main Findings

### Monthly inflation

Monthly exchange-rate movements are jointly significant in explaining current inflation, but their additional explanatory contribution is relatively small.

### One-month-ahead prediction

Current exchange-rate movements do not materially improve the prediction of next-month inflation after controlling for current inflation and seasonality.

### 12-month inflation

The strongest relationship appears at the 12-month horizon.

Cumulative exchange-rate movements are significantly associated with 12-month inflation, and the AMD/RUB exchange-rate movement remains significant even after controlling for strong inflation persistence.

## Regression Diagnostics

The final 12-month model was evaluated using:

* residuals versus fitted values
* residuals over time
* QQ plots
* residual histograms
* Cook's distance

The diagnostics indicate some deviations during crisis periods but do not suggest severe violations of the linear-model assumptions.

## Repository Structure

```text
data/           Raw and processed CPI and exchange-rate data
scripts/        Data preparation and statistical analysis in R
figures/        Main visualizations and regression diagnostics
report/         Full written project report
presentation/   Project presentation
```

## Technologies

* R
* dplyr
* readr
* tidyr
* ggplot2
* lubridate
* Multiple Linear Regression
* Statistical Hypothesis Testing
* Regression Diagnostics

## Authors

* A. Pambukyan
* T. Petrosyan
* A. Musoyan

American University of Armenia
CS108 Statistics
2025
