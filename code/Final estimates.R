#Final estimations for thesis
#The OLS estimates and diagnostics reported in the thesis are estimated in R
#The main system GMM results and robustness are estimated in STATA
#All estimates are based on the dataset created in "Data_cleaning.R", "Variable creation, full dataset.R" and "Creation of preferred dataset.R"

#Upload fullpanel_agg

#Run packages:
library(readr)
library(readxl)
library(readODS)
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(stringr)
library(sandwich)
library(lmtest)
library(car)
library(stats)
library(plm)
library(fixest)

#asign dataframes:
 
pdata_agg <- pdata.frame(fullpanel_agg, index = c("district_code", "year"))


fullpanel_agg2 <- fullpanel_agg %>% #Including the tourism growth variable as a robustness check.
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(d_log_tourism_intensity_ratio = log_tourism_intensity_ratio - dplyr::lag(log_tourism_intensity_ratio, 1),
         d_log_tourism_intensity = log_tourism_intensity - dplyr::lag(log_tourism_intensity, 1)) %>%
  ungroup()
pdata_agg2 <- pdata.frame(fullpanel_agg2, index = c("district_code", "year")) #Includes differenced tourism

#Include the lagged variables for dynamic estimates in R (STATA does this directly)

fullpanel_agglag <- fullpanel_agg %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(lag_year = dplyr::lag(year),
    laglog_tourism_intensity =if_else(year - lag_year == 1, dplyr::lag(log_tourism_intensity), NA_real_),
    laglog_tourism_intensity_ratio =if_else(year - lag_year == 1,dplyr::lag(log_tourism_intensity_ratio), NA_real_),
    laglog_housing_affordability_index = if_else(year - lag_year == 1, dplyr::lag(log_housing_affordability_index), NA_real_),
    laglog_housing_affordability_flats = if_else(year - lag_year == 1, dplyr::lag(log_housing_affordability_flats), NA_real_),
    laglogprice_house =if_else(year - lag_year == 1, dplyr::lag(logprice_house), NA_real_),
    laglogprice_flats =if_else(year - lag_year == 1, dplyr::lag(logprice_flats), NA_real_)) %>%
  ungroup() %>%
  select(-lag_year)
pdata_agglag <- pdata.frame(fullpanel_agglag,index = c("district_code", "year") )


pdim(pdata_agg)
range(as.integer(as.character(index(pdata_agg)$year)))

#Test size of persistence following Roodman 09

#Upper limit: Year FE only
FE_upper <- feols(log_housing_affordability_index ~ laglog_housing_affordability_index + log_tourism_intensity + 
                    log_density + unemployment_rate + log_dwellings_pc + new_dwellings_share + migration_share | year,
                  data = fullpanel_agglag,
                  cluster = ~district_code)
summary(FE_upper)

#Lower limit: Year and district FE (Least Square Dummy Variables)
FE_lower <- feols(log_housing_affordability_index ~ laglog_housing_affordability_index + log_tourism_intensity + 
                    log_density + unemployment_rate + log_dwellings_pc + new_dwellings_share + migration_share | year + district_code,
                  data = fullpanel_agglag,
                  cluster = ~district_code)
summary(FE_lower)
# The lagged dep.var should lie within the 0.069-0.92 range, the size is equal for using current or lagged tourism

#Diagnostic tests, based on the PLM-package that includes the relevant tests:
#District only FE model:
model_DE_plm <- plm(
  log_housing_affordability_index ~ log_tourism_intensity + log_density + log_dwellings_pc + migration_share + unemployment_rate + new_dwellings_share ,
  data = pdata_agg,
  model = "within",
  effect = "individual") #district FE
summary(model_DE_plm)

#Two-way FE model
model_FE_plm <- plm(
  log_housing_affordability_index ~ log_tourism_intensity + log_density + log_dwellings_pc + new_dwellings_share + migration_share + unemployment_rate ,
  data = pdata_agg,
  model = "within",
  effect = "twoway") #Year and district FE (Two-way)
summary(model_FE_plm)
# Diagnostics
bptest(model_DE_plm)                 # heteroskedasticity
pbgtest(model_DE_plm)                # serial correlation
pwartest(model_DE_plm)               # Wooldridge-type serial correlation
pcdtest(model_DE_plm, test = "cd")   # cross-sectional dependence

bptest(model_FE_plm)                 # heteroskedasticity
pbgtest(model_FE_plm)                # serial correlation
pwartest(model_FE_plm)               # Wooldridge-type serial correlation
pcdtest(model_FE_plm, test = "cd")   # cross-sectional dependence

# First static estimates of the static benchmark model (Table 2):
# Based on the feols package that gives cleaner tables and correct district-clustered robust standard errors.

feols_1 <- feols(log_housing_affordability_index ~ laglog_tourism_intensity + 
                   log_density + unemployment_rate + log_dwellings_pc + new_dwellings_share +
                   migration_share + mountain_dummy + lake_dummy + stadt_dummy |
                   year,
                 data = fullpanel_agglag,
                 cluster = ~district_code)
summary(feols_1)

feols_2 <- update(feols_1, .~. | district_code) #adding district FE drops the geo-dummies
summary(feols_2)

feols_3 <- update(feols_1, .~. | district_code + year)
summary(feols_3)

# Mundlak correlated random effects (wooldridge 2019):
# Ubalanced panel -> include the district-specific averages of the year dummies.

# Variables used in the Mundlak specification
mundlak_vars <- c(
  "log_tourism_intensity",
  "log_density",
  "unemployment_rate",
  "log_dwellings_pc",
  "new_dwellings_share",
  "migration_share")
cre_sample <- fullpanel_agg %>% 
  filter(
    !is.na(log_housing_affordability_index),
    if_all(all_of(mundlak_vars), ~ !is.na(.)))
# Year dummies
for (yr in 2016:2024) {cre_sample[[paste0("yr_", yr)]] <-
    as.integer(cre_sample$year == yr)}

year_dummies <- paste0("yr_", 2016:2024)
# Calculate sets of district means:
cre_sample <- cre_sample %>%
  group_by(district_code) %>%
  mutate(across( all_of(mundlak_vars),
      ~ mean(.x),
      .names = "mean_{.col}"),
    across(all_of(year_dummies),
      ~ mean(.x),
      .names = "mean_{.col}")) %>%
  ungroup()

#Wooldridge Correlated Random Effects (CRE)
wooldridge_cre <- feols(
  log_housing_affordability_index ~
    log_tourism_intensity + log_density + unemployment_rate + log_dwellings_pc + new_dwellings_share + migration_share +
    mountain_dummy + lake_dummy + stadt_dummy +
    mean_log_tourism_intensity + mean_log_density + mean_unemployment_rate +  # District means of time-varying regressors
    mean_log_dwellings_pc + mean_new_dwellings_share + mean_migration_share +
    mean_yr_2016 + mean_yr_2017 + #District means of year indicators
    mean_yr_2018 + mean_yr_2019 + mean_yr_2020 + mean_yr_2021 + mean_yr_2022 + mean_yr_2023 + mean_yr_2024 |
    year, #year fixed effects
    data = cre_sample,
    cluster = ~district_code)
summary(wooldridge_cre)

wald(wooldridge_cre,keep = "^mean_")

# Between effect function:
between_effect <- function(model, x, mean_x) {
  
  b <- coef(model)
  V <- vcov(model)
  
  est <- b[x] + b[mean_x]
  
  se <- sqrt(
    V[x, x] +
      V[mean_x, mean_x] +
      2 * V[x, mean_x])
  
  t <- est / se
  df <- 92
  p <- 2 * pt(abs(t), df = df, lower.tail = FALSE)
  
  data.frame(
    variable = x,
    estimate = est,
    se = se,
    t = t,
    p = p)}

between_results <- rbind(
  between_effect(
    wooldridge_cre,
    "log_tourism_intensity",
    "mean_log_tourism_intensity"),
  between_effect(
    wooldridge_cre,
    "log_density",
    "mean_log_density"),
  between_effect(
    wooldridge_cre,
    "unemployment_rate",
    "mean_unemployment_rate"),
  between_effect(
    wooldridge_cre,
    "log_dwellings_pc",
    "mean_log_dwellings_pc"),
  between_effect(
    wooldridge_cre,
    "new_dwellings_share",
    "mean_new_dwellings_share"),
  between_effect(
    wooldridge_cre,
    "migration_share",
    "mean_migration_share"))
# Use function to directly calculate the between district relationship: (Table 2)
between_results 

#Dynamic FE (Appendix Table 11)
dynamic_fe1 <- feols(log_housing_affordability_index ~ laglog_housing_affordability_index + 
                          laglog_tourism_intensity + log_density + unemployment_rate + log_dwellings_pc + new_dwellings_share + 
                         + migration_share | year,
                        data = fullpanel_agglag,
                        cluster = ~district_code)
summary(dynamic_fe1) 

dynamic_fe2 <-update(dynamic_fe1, .~. | district_code + year)
summary(dynamic_fe2)

dynamic_fe3 <- update(dynamic_fe2, . ~ . - laglog_tourism_intensity + log_tourism_intensity )
summary(dynamic_fe3)
# Very low and insignificant results. Both for lagged and current tourism.

#Robustness check: Compare lagged tourism to current tourism in static model (not included in final thesis)
lagtourism_fe1 <- feols(log_housing_affordability_index ~ laglog_tourism_intensity + 
                          log_density + unemployment_rate + log_dwellings_pc + new_dwellings_share  +
                          migration_share + mountain_dummy + lake_dummy + stadt_dummy | year ,
                        data = fullpanel_agglag,
                        cluster = ~district_code)
summary(lagtourism_fe1)

lagtourism_fe2 <- update(lagtourism_fe1, .~. | district_code) #adding district FE drops the geo-dummies
summary(lagtourism_fe2)

lagtourism_fe3 <- update(lagtourism_fe2, .~. | district_code + year)
summary(lagtourism_fe3)

#Recreate the STATA specification, as a robustness check of the main results in STATA.

pgmm_m6_stata <- pgmm(
  log_housing_affordability_index ~
    lag(log_housing_affordability_index, 1) + lag(log_tourism_intensity, 1) +
    log_density + unemployment_rate  + log_dwellings_pc  + new_dwellings_share + migration_share  |
    lag(log_housing_affordability_index, 2:9) + lag(log_tourism_intensity, 2:9) +
    lag(log_density, 1:2) + lag(unemployment_rate, 2:9)  + lag(log_dwellings_pc, 1:2) + 
    lag(new_dwellings_share, 2:9) + lag(migration_share, 2:9) , #treating migration_share as exogenous improves the model
  data = pdata_agg, effect = "twoways", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(pgmm_m6_stata, robust = TRUE) # Not correcting the SE (set = FALSE) gives much lower p-values.
summary(pgmm_m6_stata,robust = TRUE,time.dummies = TRUE) 
#Results are very similar to STATA

#Instrument count:
str(pgmm_m6_stata$W, max.level = 2) # Shows 60 instruments and 17 stacked equations per district.

#Control for log delta tourism intensity
pgmm_m6_deltatourism <- pgmm(
  log_housing_affordability_index ~
    lag(log_housing_affordability_index, 1) + d_log_tourism_intensity +
    log_density + unemployment_rate  + log_dwellings_pc  + new_dwellings_share + migration_share  |
    lag(log_housing_affordability_index, 2:9) + lag(d_log_tourism_intensity, 2:9) +
    lag(log_density, 1:2) + lag(unemployment_rate, 2:9)  + lag(log_dwellings_pc, 1:2) + 
    lag(new_dwellings_share, 2:9) + lag(migration_share, 2:9) , 
  data = pdata_agg2, effect = "twoways", model = "twosteps", transformation = "ld", collapse = TRUE)
summary(pgmm_m6_deltatourism, robust = TRUE) #this gives positive measure
#These results are included in the tourism robustness 


#Create graph for tourism intensity per region, from 1974-2024 :

overnights_longterm <- tourism_nights_from_1974 %>%
  mutate(
    nuts_code = str_extract(district_raw, "AT\\d{2}"),
    district = str_squish(str_remove(district_raw, "\\s*<AT\\d{2}>$")),
    district_code = case_match(
      nuts_code,
      "AT11" ~ 1L,  # Burgenland
      "AT21" ~ 2L,  # Carinthia
      "AT12" ~ 3L,  # Lower Austria
      "AT31" ~ 4L,  # Upper Austria
      "AT32" ~ 5L,  # Salzburg
      "AT22" ~ 6L,  # Styria
      "AT33" ~ 7L,  # Tyrol
      "AT34" ~ 8L,  # Vorarlberg
      "AT13" ~ 9L,  # Vienna
      .default = NA_integer_
    )
  ) %>%
  filter(value_type == "Nights spent") %>%
  select(year, district, district_code, overnight_stays) %>%
  arrange(district_code, year)

tourism_longterm_plot <- ggplot(
  overnights_longterm,
  aes(x = year, y = overnight_stays / 1e6, colour = district)
) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_x_continuous(
    breaks = seq(1975, max(overnights_longterm$year, na.rm = TRUE), by = 5)
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Tourism development in Austrian federal states",
    subtitle = paste0("Overnight stays, 1974–", max(overnights_longterm$year, na.rm = TRUE)),
    x = NULL,
    y = "Overnight stays (millions)",
    colour = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

tourism_longterm_plot
#Table not included in final thesis
