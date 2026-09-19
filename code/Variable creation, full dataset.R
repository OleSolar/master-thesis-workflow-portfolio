#After cleaning the data, next step is to create the variables for estimaton
#I am basing my variables on related literature, and data from Statistik Austria.

#Set up all datasets in correct format (long)
tourism_yearly <- tourism_yearly_final #merge overnight stays with avg flat prices per m2
tourism_yearly <- tourism_yearly %>% #filter out Bundesland and 2025
  filter(!district_code %in% 1:9,
         district_code !=900,
         year !=2025)
tourism_yearly %>% #check for duplicates = no duplicates
  count(district_code, year) %>%
  filter(n > 1)

price_flats <- average_prices_flats
price_flats <- price_flats %>%
  pivot_longer(
    cols = `2015`:`2024`,
    names_to = "year",
    values_to = "price_flats") %>%
  mutate(year = as.integer(year))

price_house <- average_prices_house
price_house <- price_house %>%
  pivot_longer(
    cols = `2015`:`2024`,
    names_to = "year",
    values_to = "price_house") %>%
  mutate(year = as.integer(year))

price_land <- average_prices_land %>%
  rename(price_land = plot_price)

#Log prices for house and flats
logprices <- price_flats %>%
  left_join(price_house, by = c("year", "district_code", "district")) %>%
  mutate(logprice_flats = log(price_flats),
         logprice_house = log(price_house)) %>%
  select(year, district_code, district, logprice_flats, logprice_house, price_flats, price_house)


#Create country aggregated overnights and population
nights_total <- tourism_yearly %>%
  group_by(year) %>%
  summarise(overnight_stays_total = sum(overnight_stays, na.rm = TRUE),
            .groups = "drop")
population_total <- population %>%
  group_by(year) %>%
  summarise(population_total = sum(population, na.rm = TRUE),
            .groups = "drop")
country_total <- nights_total %>%
  left_join(population_total, by = "year") %>%
  mutate(overnight_share_country = overnight_stays_total/population_total)

#Create final variables for estimation:

#Tourism Intensity (Main Explanatory Variable) 
#Avg DAILY overnight stays per capita, divided by nationwide daily overnight stays per total population. (This is the measure the croatian paper uses)

tourism_panel <- tourism_yearly %>%
  left_join(population, by = c("district_code","year")) %>%
  left_join(country_total, by = "year") %>%
  mutate(tourism_intensity_ratio = (overnight_stays/ population) /
           overnight_share_country) %>%
  mutate(tourism_intensity = overnight_stays / population)
log_tourism <- tourism_panel %>%
  mutate(log_tourism_intensity = log(tourism_intensity),
         log_tourism_intensity_ratio = log(tourism_intensity_ratio),
         log_overnight_stays = log(overnight_stays))
tourism_panel %>% #check for duplicates = no duplicates
  count(district_code, year) %>%
  filter(n > 1)
tourism_panel %>% #check wether totals are matched correctly
  select(year, district_code, overnight_stays_total, population_total) %>%
  arrange(year, district_code) %>%
  head(20)
tourism_panel <- tourism_panel %>%
  select(year, district_code, overnight_stays, tourism_intensity, tourism_intensity_ratio)
log_tourism <- log_tourism %>%
  select(year, district_code, log_tourism_intensity, log_tourism_intensity_ratio, log_overnight_stays, tourism_intensity, tourism_intensity_ratio, overnight_stays)

#Net income per capita
head(income_corrected_2014_24)

income_entity <- income_corrected_2014_24 %>%
  select(year, district, district_code, value_type, value) %>%
  pivot_wider(
    id_cols = c(year, district, district_code),
    names_from = value_type,
    values_from = value) %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(income_pc =`Net income (EUR)` / `Entity count: Net income`,
    log_income_pc = log(income_pc),
    income_growth = income_pc - dplyr::lag(income_pc),
    income_growth_share = income_pc / dplyr::lag(income_pc) - 1) %>%
  ungroup() %>%
  select(year,district,district_code,income_pc,log_income_pc, income_growth,income_growth_share)

#suggests a +0.136 correlation between the change in income and the tourism measure, now its corrected.

#Housing affordability (higher ratio = higher affordability and vice versa)
#Ratio of the avg yearly net income per capita to the median residential house price per square meter.
str(housing_affordability)
str(price_house)
housing_affordability <- income_entity %>% 
  left_join(price_house, by = c("district_code","year")) %>%
  arrange(district_code,year) %>%
  group_by(district_code) %>%
  mutate(housing_affordability_index = income_pc/price_house , #turned housing affordability around!
         lag_housing_affordability_index = dplyr::lag(housing_affordability_index, 1),
         log_housing_affordability_index = log(income_pc/price_house),
         lag_log_housing_affordability_index = dplyr::lag(log_housing_affordability_index, 1))
head(housing_affordability )
#Create additional measure for housing affordability based on appartment prices
housing_affordability <- housing_affordability %>% 
  left_join(price_flats, by = c("district_code","year")) %>%
  arrange(district_code,year) %>%
  group_by(district_code) %>%
  mutate(housing_affordability_flats = income_pc/price_flats , #turn housing affordability around?
         lag_housing_affordability_flats = dplyr::lag(housing_affordability_flats, 1),
         log_housing_affordability_flats = log(income_pc/price_flats), #log of the ratio
         lag_log_housing_affordability_flats = dplyr::lag(log_housing_affordability_flats, 1))

  
#Number of beds per capita (Tourism supply control)
population_vienna <- population %>%
  filter(district_code %in% 901:923) %>%
  group_by(year) %>%
  summarise(district_code = 900,
            district = "Vienna",
    population = sum(population, na.rm = TRUE))
population_agg <- population %>% #consider just use this as normal population if it automatically drops the 900..
  bind_rows(population_vienna)
population_agg <- population_agg %>%
  filter(!district_code %in% 901:923)

beds_hotel <- beds_hotel_district %>% 
  mutate(district_code = if_else(district_code == 901, 900, district_code)) %>%
  left_join(population_agg, by = c("year", "district_code"))
beds_hotel <- beds_hotel %>%
  mutate(beds_hotel_pc = beds_hotel/population,
         log_beds_hotel_pc = log(beds_hotel_pc))
beds_hotel<- beds_hotel %>%
  filter(season == "year") %>%
  select(year, district_code, population, beds_hotel, beds_hotel_pc, log_beds_hotel_pc)

beds_private <- beds_private_district %>% 
  mutate(district_code = if_else(district_code == 901, 900, district_code)) %>%
  mutate(district_code = as.integer(district_code)) %>%
  left_join(population_agg, by = c("year", "district_code"))
beds_private <- beds_private %>%
  mutate(beds_private_pc = beds_private/population,
         log_beds_private_pc = log(beds_private_pc))
beds_private <- beds_private%>%
  filter(season == "year") %>%
  select(year, district_code, beds_private, beds_private_pc, log_beds_private_pc)

beds_other <- beds_other_district %>% 
  mutate(district_code = if_else(district_code == 901, 900, district_code)) %>%
  mutate(district_code = as.integer(district_code))
beds_other <- beds_other %>% #nest step, test other tourism variables.
  left_join(population_agg, by = c("year", "district_code"))
beds_other <- beds_other %>%
  mutate(beds_other_pc = beds_other/population,
         log_beds_other_pc = log(beds_other_pc))
beds_other<- beds_other %>%
  filter(season == "year") %>%
  select(year, district_code, beds_other, beds_other_pc, log_beds_other_pc)

#Total beds per capita each district
beds_total <- beds_hotel %>%
  left_join(beds_private, by = c("year", "district_code")) %>%
  left_join(beds_other, by = c("year", "district_code"))
beds_total <- beds_total %>% #Only 900 dataset
  mutate(beds_total = beds_hotel+beds_private+beds_other,
         beds_total_pc = beds_total/population,
         log_beds = log(beds_total),
         log_beds_pc = log(beds_total_pc)) %>%
  filter(year != 2025)

#Tourism Index, weighted average of the overnight stays and number of beds (tourism pressure)
tourism_index <- tourism_total_year %>% #add 900 district code
  select(year, district_code, total_overnight_stays) %>%
  group_by(year, district_code) 
tourism_index <- tourism_index %>%
  left_join(beds_total %>% select(year, district_code, beds_total),
    by = c("year", "district_code")) %>%
  mutate(tourism_pressure = dplyr::if_else(
      !is.na(beds_total) & beds_total > 0,
      total_overnight_stays / (beds_total*365), #multiply by 365 to get the yearly pressure
      NA_real_)) 
tourism_index <- tourism_index %>% #Only Vienna aggregate dataset
  mutate(log_tourism_pressure = log(tourism_pressure))

#Overnight stays per capita broken down to type of accomodation 
tourism_intensity_type <- tourism_total_year %>%
  left_join(population_agg, by = c("year", "district_code"))
tourism_intensity_type <- tourism_intensity_type %>%
  mutate(overnights_hotel_pc = overnights_hotel/population,
         overnights_private_pc = overnights_private/population,
         overnights_other_pc = overnights_other/population)
tourism_intensity_type <- tourism_intensity_type %>%
  mutate(logovernights_hotel_pc = log(overnights_hotel_pc),
         logovernights_private_pc = log(overnights_private_pc),
         logovernights_other_pc = log(overnights_other_pc))

#Population density
density <- district_size_km2 %>%
  filter(district_code !=900) %>%
  left_join(population, by = c("district_code"))
density <- density %>%
  mutate(density = population / area_km2,
         log_density = log(density))

#Population growth (inc. births and deaths)
population_final <- population %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(population_growth = c(NA, diff(population))) %>% #use diff command instead of lag
  ungroup()
population_final <- population_final %>% 
  mutate(log_population = log(population),
         population_growth_share = population_growth/population)

#Share of dwellings per capita (supply measure) 
dwellings_pc <- building_dwelling_totals_2014_24 %>%
  left_join(population, by = c("district_code", "year"))
dwellings_pc <- dwellings_pc %>%
  mutate(dwellings_pc = whg / population,
         log_dwellings_pc = log(dwellings_pc))

#New dwellings share of total dwellings (in %) from yearly dwellings data (more reliable than new dwellings data)
#NB! Don't take logs of shares and negative values.
new_dwellings <- new_dwellings_total %>%
  left_join(building_dwelling_totals_2014_24 %>%
      select(year, district_code, whg,geb),
    by = c("year", "district_code")) %>%
  mutate(new_dwellings_share = new_dwellings / whg) %>%
  select(year,district,district_code,geb,whg,new_dwellings,new_dwellings_share) %>%
  arrange(district_code, year)

#Share of secondary homes in % (nurnws and hwsnws) this seem to differ greatly from Wien to others
building_shares <- building_dwelling_totals_2014_24 %>%
  mutate(secondhome_share = ((nurnws) / geb), #use nur nws or hwsnws (both first and second home residents)
         firsthome_share = (nurhws/geb),
         firstsecond_share = (hwsnws/geb),
         empty_share = (ohnews/geb),
         dwellings_per_building = (whg/geb))

#Migration (in %)
migration_share <- migration_final %>%
  left_join(population, by = c("district_code", "year"))
migration_share <- migration_share %>%
  mutate(migration_share = (total_net_migration / population)) %>%
  select(year, district_code, total_net_migration, migration_share)

#Share of overnight stays per type of accomodation (in %) (either: hotel, private or other)
overnight_shares <- tourism_total_year %>%
  mutate(overnight_private_share = overnights_private / total_overnight_stays,
         overnight_hotel_share = overnights_hotel / total_overnight_stays,
         overnight_other_share = overnights_other / total_overnight_stays)

#Unemployment rate , add vienna districts for flat market
unemployment_vienna_2024 <- unemployment_vienna_2024 %>%
  mutate(year = 2024)

unemployment_rate_24full <- bind_rows(Unemployment_2024,unemployment_vienna_2024) 
unemployment_rate_full <- bind_rows(unemployment_rate,unemployment_rate_24full) #add vienna unempolyment for 2024
unemployment_rate_filtered <- unemployment_rate_full %>%
  filter(year >= 2015) # not 100% sure about the 24 values using the same scale
  

#Lending rates and CPI to pick up yearly shocks (when not using FE)
lending_rate <- lending_rates_2014_25 %>%
  filter(!year %in% c(2014,2025)) %>%
  select(year,lending_rates_housing)
cpi <- cpi_2013_25 %>%
  filter(value_type == "TOTAL INDEX") %>%
  select(year, cpi_yearly_average) %>%
  filter(!year %in% c(2013,2014,2025))


#Robustness check are carried out in STATA using the xtabond2-package and the corresponding sub-samples for each robustness is also done in STATA.

#Create a full dataset, including all districts of Vienna.
#Panel data levels (check logs later)
panel_data <- price_flats %>%
  left_join(price_house, by = c("year", "district_code", "district")) %>%
  left_join(tourism_panel %>%
              select(year, district_code, overnight_stays, tourism_intensity, tourism_intensity_ratio),
            by = c("year", "district_code")) %>%
  left_join(income_entity %>%
              select(year, district_code, income_pc, income_growth, income_growth_share),
            by = c("year", "district_code")) %>%
  left_join(housing_affordability %>%
              select(year, district_code, housing_affordability_index, housing_affordability_flats),
            by = c("year", "district_code")) %>%
  mutate(high_tourism_q75 = if_else(tourism_intensity > cutoff_75, 1, 0)) %>%
  left_join(unemployment_rate_filtered %>% select(year, district_code, unemployment_rate, employment_rate),
            by = c("year", "district_code")) %>%
  left_join(density %>%
              select(year, district_code, density),
            by = c("year", "district_code")) %>%
  left_join(population_final %>%
              select(year, district_code, population, population_growth, population_growth_share),
            by = c("year", "district_code")) %>%
  left_join(migration_share %>% #check both net migration and delta population
              select(year, district_code, migration_share),
            by = c("year", "district_code")) %>%
  left_join(dwellings_pc %>%
              select(year, district_code, dwellings_pc), 
            by = c("year", "district_code")) %>%
  left_join(building_shares %>%
              select(year, district_code, secondhome_share, firsthome_share, firstsecond_share, empty_share, dwellings_per_building), 
            by = c("year", "district_code")) %>%  
  left_join(new_dwellings %>%
              select(year, district_code, new_dwellings_share),
            by = c("year", "district_code")) %>% #Squared tourism share
  mutate(tourism_sq = tourism_intensity_ratio^2)


#Create dummies for OLS spesification
panel_data <- panel_data %>%
  mutate(stadt_dummy = if_else(str_detect(district, fixed("(Stadt)", ignore_case = TRUE)) |
                                 district_code %in% 901:923, 1, 0)) %>%
  left_join(geo_dummies, by = "district_code") %>%
  mutate(KIM_V = ifelse(year %in% c(2022,2023), 1, 0)) %>%
  left_join(lending_rate %>% select(year, lending_rates_housing), by = c("year")) %>%
  left_join(cpi %>% select(year, cpi_yearly_average), by = c("year"))
panel_data <- panel_data %>% #Covid interaction term
  mutate(covid = ifelse(year %in% c(2020, 2021), 1, 0))
panel_data <- panel_data %>% #Vienna interaction term
  mutate(vienna = ifelse(district_code %in% c(901:923), 1, 0))

# Change shares to percent, for easier interpreation
panel_data_100 <- panel_data %>%
  mutate(unemployment_rate = unemployment_rate*100,
         employment_rate = employment_rate*100,
         population_growth_share = population_growth_share*100,
         secondhome_share = secondhome_share*100,
         firsthome_share = firsthome_share*100,
         firstsecond_share = firstsecond_share*100,
         empty_share = empty_share*100,
         new_dwellings_share = new_dwellings_share*100,
         income_growth_share = income_growth_share*100,
         migration_share = migration_share*100)

#Log panel, keep shares (0-1) in levels as logs can be problematic for small values.
log_panel <- panel_data_100 %>%
  mutate(
    logprice_flats = log(price_flats),
    logprice_house = log(price_house),
    log_tourism_intensity = log(tourism_intensity),
    log_tourism_intensity_ratio = log(tourism_intensity_ratio),
    log_income_pc = log(income_pc),
    log_density = log(density),
    log_dwellings_pc = log(dwellings_pc),
    log_housing_affordability_index = log(housing_affordability_index),
    log_housing_affordability_flats = log(housing_affordability_flats),
    log_dwellings_building = log(dwellings_per_building))

write.csv(log_panel, "Full vienna panel, flats.csv")
write_dta(log_panel, "Full Vienna panel, flats.dta")

#Descriptive statistics, create a summary table of variables in Stargazer (Latex).
  summary_data <- panel_data %>%
    select(
      housing_affordability_index,
      housing_affordability_flats,
      price_flats,
      price_house,
      income_pc,
      income_growth,
      tourism_intensity,
      tourism_intensity_ratio,
      overnight_stays,
      population,
      population_growth,
      migration_share,
      density,
      dwellings_pc,
      new_dwellings_share,
      firsthome_share,
      secondhome_share,
      empty_share,
      dwellings_per_building,
      unemployment_rate,
      lending_rates_housing,
      cpi_yearly_average,
    ) %>%
    as.data.frame()
  
   stargazer( summary_data,
    type = "latex",
    title = "Summary Statistics",
    digits = 3,
    summary.stat = c("n", "mean", "sd", "min", "median", "max"),
    out = "summary_statistics.tex")
  
summary(panel_data$tourism_intensity)
quantile(panel_data$tourism_intensity, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE) #na.rm removes the Na and NaN before computing the quantiles
panel_data %>%
  group_by(district_code) %>%
  summarise(mean_tourism_intensity = mean(tourism_intensity, na.rm = TRUE)) %>%
  summarise(
    min = min(mean_tourism_intensity, na.rm = TRUE),
    p25 = quantile(mean_tourism_intensity, 0.25, na.rm = TRUE),
    median = median(mean_tourism_intensity, na.rm = TRUE),
    p75 = quantile(mean_tourism_intensity, 0.75, na.rm = TRUE),
    p90 = quantile(mean_tourism_intensity, 0.90, na.rm = TRUE),
    max = max(mean_tourism_intensity, na.rm = TRUE))

log_panel %>%
  group_by(district_code) %>%
  summarise(mean_tourism_intensity = mean(log_tourism_intensity, na.rm = TRUE)) %>%
  summarise(
    min = min(mean_tourism_intensity, na.rm = TRUE),
    p25 = quantile(mean_tourism_intensity, 0.25, na.rm = TRUE),
    median = median(mean_tourism_intensity, na.rm = TRUE),
    p75 = quantile(mean_tourism_intensity, 0.75, na.rm = TRUE),
    p90 = quantile(mean_tourism_intensity, 0.90, na.rm = TRUE),
    max = max(mean_tourism_intensity, na.rm = TRUE))


# Summary statistic of log_panel
summary_log_panel <- log_panel %>%
  select(
    log_housing_affordability_index,
    log_housing_affordability_flats,
    logprice_flats,
    logprice_house,
    log_tourism_intensity,
    log_tourism_intensity_ratio,
    log_tourism_sq,
    log_overnight_stays,
    log_income_pc,
    log_density,
    log_population,
    log_dwellings_pc,
  ) %>%
  as.data.frame()

stargazer( summary_log_panel,
           type = "latex",
           title = "Summary Statistics Log",
           digits = 3,
           summary.stat = c("n", "mean", "sd", "min", "median", "max"),
           out = "summary_statistics_log.tex")
#Summary of logs are not included in the thesis


#Dummy variables
log_panel <- log_panel %>% #stadt dummies
  mutate(stadt_dummy = if_else(str_detect(district, fixed("(Stadt)", ignore_case = TRUE)) |
                                 district_code %in% 901:923, 1, 0))
log_panel <- log_panel %>% #geo dummies
  left_join(geo_dummies, by = "district_code")
log_panel <- log_panel %>%
  left_join(lending_rate %>%
              select(year, lending_rates_housing),
              by = c("year"))
log_panel <- log_panel %>%
  left_join(cpi %>%
              select(year, cpi_yearly_average),
            by = c("year"))
log_panel <- log_panel %>% #Covid interaction term
  mutate(covid = ifelse(year %in% c(2020, 2021), 1, 0))
log_panel <- log_panel %>% #Vienna interaction term
  mutate(vienna = ifelse(district_code %in% c(901:923), 1, 0))
log_panel <- log_panel %>% #KIM V and financial restrictions in 2022
  mutate(KIM_V = ifelse(year %in% c(2022,2023), 1, 0)) #Implemented in aug 22

#Heterogeneity analysis carried out in STATA for each robustness check

#Create histograms:

# Price flats
plot_price_flats <- ggplot(panel_data, aes(x = price_flats)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Flat Prices",
    x = "Flat prices",
    y = "Frequency"
  ) +
  theme_minimal()

# Housing affordability index
plot_hai_flats <- ggplot(panel_data, aes(x = housing_affordability_index)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Housing Affordability Index",
    x = "Housing affordability index",
    y = "Frequency"
  ) +
  theme_minimal()
plot_hai_house <- ggplot(panel_data, aes(x = housing_affordability_house)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Housing Affordability Index (house prices)",
    x = "Housing affordability house",
    y = "Frequency"
  ) +
  theme_minimal()
# Tourism intensity
ggplot(panel_data, aes(x = tourism_intensity)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Tourism Intensity",
    x = "Tourism intensity",
    y = "Frequency"
  ) +
  theme_minimal()
# Log flat prices
plot_logprice_flats <- ggplot(log_panel, aes(x = logprice_flats)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Log Flat Prices",
    x = "Log flat prices",
    y = "Frequency"
  ) +
  theme_minimal()

# Log housing affordability
plot_loghai_flats <- ggplot(log_panel, aes(x = log_housing_affordability)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Log Housing Affordability",
    x = "Log housing affordability",
    y = "Frequency"
  ) +
  theme_minimal()
# Log housing affordability
plot_loghai_house <- ggplot(log_panel, aes(x = log_housing_affordability_house)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Log Housing Affordability House",
    x = "Log housing affordability",
    y = "Frequency"
  ) +
  theme_minimal()

# Log tourism intensity
ggplot(log_panel, aes(x = log_tourism_intensity)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Log Tourism Intensity",
    x = "Log tourism intensity",
    y = "Frequency"
  ) +
  theme_minimal()
#Relative Tourism Intensity (share)
plot_tourism_ratio <- ggplot(log_panel, aes(x = tourism_intensity_ratio)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Tourism Intensity Share",
    x = "Tourism intensity share",
    y = "Frequency"
  ) +
  theme_minimal()
plot_logtourism_ratio <- ggplot(log_panel, aes(x = log_tourism_intensity_ratio)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Log Tourism Intensity Share",
    x = "Log Tourism intensity share",
    y = "Frequency"
  ) +
  theme_minimal()
# Level house prices
plot_price_house <- ggplot(panel_data, aes(x = price_house)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Level House Prices",
    x = "Level house prices",
    y = "Frequency"
  ) +
  theme_minimal()

# Log house prices
plot_logprice_house <- ggplot(log_panel, aes(x = logprice_house)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Log House Prices",
    x = "Log house prices",
    y = "Frequency"
  ) +
  theme_minimal()

)
