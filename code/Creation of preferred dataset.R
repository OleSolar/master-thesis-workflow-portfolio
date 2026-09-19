# Preferred dataset, based on the full dataset from "Data_cleaning.R" and "Variable creation, Full Dataset.R"
# In this dataset Vienna is treated as one political district, and the aggregated values is official Statistik Austria data.

#Function to aggregate in case data contains all Vienna districts.
to_vienna900 <- function(x) {
  dplyr::if_else(as.integer(x) %in% 901:923, 900L, as.integer(x))}

#Already aggregated variables, from base dataset.
#population (population_agg)
#nights_total 
#population_final (population_final_agg)
population_agg <- population_agg %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(population_growth = c(NA, diff(population))) %>% #use diff command instead of lag
  ungroup()
population_agg <- population_agg %>% 
  mutate(log_population = log(population),
         population_growth_share = population_growth/population)

tourism_yearly_agg <- tourism_yearly_final %>%
  filter(!district_code %in% 901:923,
         year != 2025) %>%
  rename(total_overnight_stays = overnight_stays)

tourism_panel_agg <- tourism_yearly_agg %>%
  left_join(population_agg, by = c("year", "district_code")) %>%
  left_join(country_total, by = "year") %>%
  mutate(
    tourism_intensity = total_overnight_stays / population,
    tourism_intensity_ratio = tourism_intensity / overnight_share_country
  ) %>%
  select(year, district_code, total_overnight_stays, tourism_intensity,
         tourism_intensity_ratio)
tourism_panel_agg <- tourism_panel_agg %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(d_tourism_intensity_ratio = tourism_intensity_ratio - dplyr::lag(tourism_intensity_ratio, 1),
         d_tourism_intensity = tourism_intensity - dplyr::lag(tourism_intensity,1)) %>%
  ungroup()


#(Aggregating makes Vienna quite average on tourism intensity)

income_entity_agg <- net_income_final %>%
  filter(!district_code %in% 901:923, year != 2025) %>%
  group_by(year, district_code, district, value_type) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(year, district, district_code),
    names_from = value_type,
    values_from = value) %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(
    income_pc = `Net income (EUR)` / `Entity count: Net income`,
    log_income_pc =log(income_pc),
    income_growth =income_pc - dplyr::lag(income_pc),
    income_growth_share = income_pc / dplyr::lag(income_pc) - 1) %>%
  ungroup() %>%
  filter(year >= 2015)

#Income per total residents instead of tax payers
income_pc_agg <- net_income_final %>%
  filter(value_type == "Net income (EUR)", !district_code %in% 901:923, year != 2025)
income_pc_agg <- income_pc_agg %>%
  group_by(year, district_code, district, value_type) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(year, district, district_code),
    names_from = value_type,
    values_from = value) %>%
  select(-district)
income_pc_agg <- income_pc_agg %>%
  left_join(population_agg, by = c("district_code", "year")) %>%
  mutate(income_pc_agg = `Net income (EUR)`/population,
         log_income_pc_agg = log(income_pc_agg))
income_growth_agg <- income_pc_agg %>%
  arrange(district_code,year) %>%
  group_by(district_code) %>%
  mutate(income_growth_agg = income_pc_agg - dplyr::lag(income_pc_agg, 1),
         income_growth_share = income_growth_agg/dplyr::lag(income_pc_agg, 1),
         log_income_growth = log(income_growth_agg),) %>%
  ungroup() %>%
  filter(!year %in% c(2013, 2014))
income_pc_agg <- income_pc_agg %>%
  filter(!year %in% c(2013, 2014))

buildings_agg <- building_dwelling_totals_2014_24 %>%
  mutate(district_code = to_vienna900(district_code)) %>%
  group_by(year, district_code) %>%
  summarise(
    whg = sum(whg, na.rm = TRUE),
    geb = sum(geb, na.rm = TRUE),
    nurnws = sum(nurnws, na.rm = TRUE),
    hwsnws = sum(hwsnws, na.rm = TRUE),
    nurhws = sum(nurhws, na.rm = TRUE),
    ohnews = sum(ohnews, na.rm = TRUE),
    .groups = "drop")

dwellings_pc_agg <- buildings_agg %>%
  left_join(population_agg %>% select(year, district_code, population),
            by = c("year", "district_code")) %>%
  mutate(dwellings_pc = whg / population,
    log_dwellings_pc = log(dwellings_pc)) %>%
  select(year, district_code, dwellings_pc, log_dwellings_pc)


vienna_new_dwellings <- new_dwellings_region %>%
  filter(bundesland == "Vienna", year %in% 2014:2024) %>%
  transmute(year, district = "Vienna", district_code = 900L, new_dwellings)
new_dwellings_agg <- new_dwellings %>%
  filter(!district_code %in% c(900L, 901:923)) %>%
  select(year, district, district_code, new_dwellings) %>%
  bind_rows(vienna_new_dwellings) %>%
  left_join(
    buildings_agg %>% select(year, district_code, whg),
    by = c("year", "district_code"),
    relationship = "one-to-one"
  ) %>%
  mutate(new_dwellings_share = new_dwellings / whg) %>%
  arrange(district_code, year)


building_shares_agg <- buildings_agg %>%
  mutate(secondhome_share = nurnws / geb,
    firsthome_share = nurhws / geb,
    firstsecond_share = hwsnws / geb,
    empty_share = ohnews / geb,
    dwellings_per_building = whg / geb
  ) %>%
  select(year, district_code, secondhome_share, firsthome_share, firstsecond_share, empty_share, dwellings_per_building)

density_agg <- district_size_km2 %>%
  filter(!district_code %in% c(901:923)) %>%
  left_join(population_agg, by = c("district_code"))
density_agg <- density_agg %>%
  mutate(density = population / area_km2,
         log_density = log(density))


migration_agg <- migration_final %>%
  mutate(district_code = to_vienna900(district_code)) %>%
group_by(year, district_code) %>%
  summarise(total_net_migration = sum(total_net_migration, na.rm = TRUE),
    .groups = "drop")
migration_agg <- migration_agg %>%
  left_join(population_agg %>% select(year, district_code, population),
            by = c("year", "district_code"))
migration_agg <- migration_agg %>%
  mutate(migration_share = (total_net_migration / population)) %>%
  select(year, district_code, total_net_migration, migration_share)


#Adding it manually rather than taking means 
price_flats_agg <- price_flats %>% mutate( district_code = to_vienna900(district_code), district = if_else(district_code == 900L, "Vienna", district)) %>%group_by(year, district_code, district) %>%  summarise(   price_flats = median(price_flats, na.rm = TRUE),   .groups = "drop")
price_house_agg <- price_house %>% mutate(district_code = to_vienna900(district_code),district = if_else(district_code == 900L, "Vienna", district)) %>% group_by(year, district_code, district) %>% summarise(  price_house = median(price_house, na.rm = TRUE),.groups = "drop")
price_land_agg <- price_land %>% mutate(district_code = to_vienna900(district_code),district = if_else(district_code == 900L, "Vienna", district)) %>% group_by(year, district_code, district) %>% summarise(  price_land = median(price_land, na.rm = TRUE),.groups = "drop")

vienna_prices_manual <- tibble::tribble( #based on the special evaluation from Statistik Austria
  ~year, ~district_code, ~price_flats_manual, ~price_house_manual, ~price_land_manual,
  2015, 900L, 3333, 3575, 485,
  2016, 900L, 3623, 3806, 459,
  2017, 900L, 3828, 4015, 554,
  2018, 900L, 4009, 4200, 708,
  2019, 900L, 4216, 4569, 508,
  2020, 900L, 4521, 4830, 607,
  2021, 900L, 5000, 5424, 678,
  2022, 900L, 5495, 5707, 927,
  2023, 900L, 4993, 5502, 1018,
  2024, 900L, 4941, 5492, 786)

price_flats_agg <- price_flats_agg %>%
  left_join(
    vienna_prices_manual %>% select(year, district_code, price_flats_manual),
    by = c("year", "district_code")) %>%
  mutate(
    price_flats = if_else(
      district_code == 900L & !is.na(price_flats_manual), #copy the manual price over to price column
      price_flats_manual,
      price_flats
    )
  ) %>%
  select(-price_flats_manual)

price_house_agg <- price_house_agg %>%
  left_join(
    vienna_prices_manual %>% select(year, district_code, price_house_manual),
    by = c("year", "district_code")
  ) %>%
  mutate(
    price_house = if_else(
      district_code == 900L & !is.na(price_house_manual),
      price_house_manual, 
      price_house
    )
  ) %>%
  select(-price_house_manual)

price_land_agg <- price_land_agg %>%
  left_join(vienna_prices_manual %>%
              select(year, district_code, price_land_manual),
            by = c("year", "district_code")) %>%
  mutate(
    price_land = if_else(
      district_code == 900L & !is.na(price_land_manual),
      price_land_manual,
      price_land)) %>%
  select(-price_land_manual)

#Set up panel data for the aggregated Vienna dataset:
panel_data_agg <- price_flats_agg %>%
  left_join(price_house_agg, by = c("year", "district_code", "district")) %>%
  left_join(price_land_agg, by = c("year", "district_code", "district")) %>%
  left_join(
    tourism_panel_agg %>% #alternative is the tourism_total_year
      select(year, district_code, total_overnight_stays, tourism_intensity, tourism_intensity_ratio, d_tourism_intensity, d_tourism_intensity_ratio),
    by = c("year", "district_code")
  ) %>%
  left_join(
    income_entity_agg %>%
      select(year, district_code, income_pc, log_income_pc, income_growth, income_growth_share),
    by = c("year", "district_code")
  ) %>%
  left_join(
    density_agg %>%
      select(year, district_code, density),
    by = c("year", "district_code")
  ) %>%
  left_join(
    migration_agg%>%
      select(year, district_code, total_net_migration, migration_share),
    by = c("year", "district_code")
    )%>%
  left_join(
    dwellings_pc_agg %>%
      select(year, district_code, dwellings_pc),
    by = c("year", "district_code")
  ) %>%
  left_join(
    new_dwellings_agg %>%
      select(year, district_code, new_dwellings_share),
    by = c("year", "district_code")
  ) %>%
  left_join(
    building_shares_agg %>%
      select(year, district_code, secondhome_share, firsthome_share, firstsecond_share, empty_share, dwellings_per_building),
    by = c("year", "district_code")
  ) %>%
  left_join(
    population_agg %>%
      select(year, district_code, population, population_growth, population_growth_share),
    by = c("year", "district_code")
  ) %>%
  left_join(
    beds_total %>%
      select(year, district_code, beds_total_pc, beds_hotel_pc, beds_private_pc, beds_other_pc),
    by = c("year", "district_code")
  ) %>%
  left_join(
    tourism_index %>%
      select(year, district_code, tourism_pressure),
    by = c("year", "district_code")
  ) %>%
  left_join(
    tourism_intensity_type %>%
      select(year, district_code, overnights_hotel_pc, overnights_private_pc, overnights_other_pc),
    by = c("year", "district_code")
  ) %>%
  left_join(
    overnight_shares %>%
      select(year, district_code, overnight_hotel_share, overnight_private_share, overnight_other_share),
    by = c("year", "district_code")
  ) %>%
  left_join(unemployment_rate_agg %>% 
      select(year, district_code, unemployment_rate, employment_rate)) %>%
  mutate(
    housing_affordability_index = income_pc / price_house,
    housing_affordability_flats = income_pc / price_flats,
    housing_affordability_land = income_pc / price_land)
  

#Add dummies
panel_data_agg <- panel_data_agg %>%
  mutate(stadt_dummy = if_else(str_detect(district, fixed("(Stadt)", ignore_case = TRUE)) |
                                 district_code == 900, 1, 0)) %>%
  left_join(geo_dummies, by = "district_code") %>%
  mutate(covid = ifelse(year %in% c(2020, 2021), 1, 0)) #Covid dummy
panel_data_agg <- panel_data_agg %>% 
  left_join(
    lending_rate %>%
      select(year, lending_rates_housing),
    by = c("year")) %>% 
  left_join(
    cpi %>%
      select(year, cpi_yearly_average),
    by = c("year")) %>%
  mutate(KIM_V = ifelse(year %in% c(2022,2023), 1, 0))

#Turn shares into percent values for better comparison
panel_data_agg_100 <- panel_data_agg %>%
  mutate(unemployment_rate = unemployment_rate*100,
        employment_rate = employment_rate*100,
        population_growth_share = population_growth_share*100,
        secondhome_share = secondhome_share*100,
        firsthome_share = firsthome_share*100,
        firstsecond_share = firstsecond_share*100,
        empty_share = empty_share*100,
        new_dwellings_share = new_dwellings_share*100,
        income_growth_share = income_growth_share*100,
        migration_share = migration_share*100,
        overnight_hotel_share = overnight_hotel_share*100,
        overnight_private_share = overnight_private_share*100,
        overnight_other_share = overnight_other_share*100,
        tourism_pressure = tourism_pressure*100)

#Create log panel for the aggregated Vienna data

fullpanel_agg <- panel_data_agg_100 %>%
  mutate(
    logprice_flats = log(price_flats),
    logprice_house = log(price_house),
    logprice_land = log(price_land),
    log_tourism_intensity = log(tourism_intensity),
    log_tourism_intensity_ratio = log(tourism_intensity_ratio),
    log_income_pc_agg = log(income_pc),
    log_density = log(density),
    log_dwellings_pc = log(dwellings_pc),
    log_housing_affordability_index = log(housing_affordability_index),
    log_housing_affordability_flats = log(housing_affordability_flats),
    log_housing_affordability_land = log(housing_affordability_land),
    log_tourism_pressure = log(tourism_pressure),
    log_beds_total_pc = log(beds_total_pc),
    log_beds_hotel_pc = log(beds_hotel_pc),
    log_beds_private_pc = log(beds_private_pc),
    log_beds_other_pc = log(beds_other_pc),
    log_overnights_hotel_pc = log(overnights_hotel_pc),
    log_overnights_private_pc = log(overnights_private_pc),
    log_overnights_other_pc = log(overnights_other_pc),
    log_dwellings_buildings = log(dwellings_per_building)) 

#Run fullpanel_agg from here to create the subsequent 

fullpanel_agg2 <- fullpanel_agg %>% #Including the tourism growth variable as a robustness check
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(d_log_tourism_intensity_ratio = log_tourism_intensity_ratio - dplyr::lag(log_tourism_intensity_ratio, 1),
        d_log_tourism_intensity = log_tourism_intensity - dplyr::lag(log_tourism_intensity, 1)) %>%
  ungroup()


#Descriptive statistics, create a summary table of variables in Stargazer (Latex).
summary_data <- fullpanel_agg %>%
  select(
    housing_affordability_index,
    housing_affordability_flats,
    housing_affordability_land,
    price_flats,
    price_house,
    price_land,
    income_pc,
    income_growth,
    tourism_intensity,
    tourism_intensity_ratio,
    total_overnight_stays,
    tourism_pressure,
    population,
    population_growth,
    total_net_migration,
    migration_share,
    unemployment_rate,
    density,
    dwellings_pc,
    new_dwellings_share,
    firsthome_share,
    secondhome_share,
    empty_share,
    dwellings_per_building,
    lending_rates_housing,
    cpi_yearly_average,
    overnights_hotel_pc,
    overnight_hotel_share,
    beds_hotel_pc,
    overnights_private_pc,
    overnight_private_share,
    beds_private_pc,
    overnights_other_pc,
    overnight_other_share,
    beds_other_pc
  ) %>%
  as.data.frame()

#Create lagged variables for the dynamic FE,
fullpanel_agglag <- fullpanel_agg %>%
  arrange(district_code, year) %>%
  group_by(district_code) %>%
  mutate(
    lag_year = dplyr::lag(year),
    
    laglog_tourism_intensity =
      if_else(year - lag_year == 1,
              dplyr::lag(log_tourism_intensity),
              NA_real_),
    
    laglog_tourism_intensity_ratio =
      if_else(year - lag_year == 1,
              dplyr::lag(log_tourism_intensity_ratio),
              NA_real_),
    
    laglog_housing_affordability_index =
      if_else(year - lag_year == 1,
              dplyr::lag(log_housing_affordability_index),
              NA_real_),
    
    laglog_housing_affordability_flats =
      if_else(year - lag_year == 1,
              dplyr::lag(log_housing_affordability_flats),
              NA_real_),
    
    laglogprice_house =
      if_else(year - lag_year == 1,
              dplyr::lag(logprice_house),
              NA_real_),
    
    laglogprice_flats =
      if_else(year - lag_year == 1,
              dplyr::lag(logprice_flats),
              NA_real_)
  ) %>%
  ungroup() %>%
  select(-lag_year)

pdata_lagg <- pdata.frame( #add lags to the dataframe
  fullpanel_agglag,
  index = c("district_code", "year"))


stargazer( summary_data,
           type = "latex",
           title = "Summary Statistics",
           digits = 3,
           summary.stat = c("n", "mean", "sd", "min", "median", "max"),
           out = "summary_statistics_agg.tex")

fullpanel_agg %>% #now its balanced
  summarise(
    n_total = sum(!is.na(total_overnight_stays)),
    n_hotel_share = sum(!is.na(overnight_hotel_share)),
    n_private_share = sum(!is.na(overnight_private_share)),
    n_other_share = sum(!is.na(overnight_other_share)),
    n_tourism_intensity = sum(!is.na(tourism_intensity)))



write_dta(fullpanel_agg, "fullpanel_agg.dta")
write_csv(fullpanel_agg, "fullpanel_agg.csv")
write_dta(fullpanel_agg2, "fullpanel_agg2.dta")





