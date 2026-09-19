
library(readr)
library(readxl)
library(readODS)
library(data.table)
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(tikzDevice)
library(Hmisc)
library(tidyr)
library(stringr)
library(rmarkdown)
library(sandwich)
library(lmtest)
library(car)
library(stats)
library(ivreg)
library(plm)
library(xtable)
library(kableExtra)
library(fixest)
library(purrr)
library(knitr)
library(haven)
library(urca)
library(sf) #geodata



getwd()
setwd("/Users/cosimasolar/Documents/Uni/Master Thesis/Dataset")
write_csv(fullpanel_agg, "fullpanel_agg.csv")
write_csv(unemployment_rate_agg, "unemployment_clean.csv")

#Create key variables for data analysis

#0 Base Data
#0.1 Population (clean)
population <- population_inc_vienna
  
population <- population %>% #separate district name and code
  mutate( 
    district_code = str_extract(`Politischer Bezirk / Wiener Gemeindebezirk`, "(?<=<)\\d+(?=>)"), 
    district = str_remove(`Politischer Bezirk / Wiener Gemeindebezirk`, " <\\d+>"))
population <- population %>%
  mutate(
    population = as.numeric(Number),    # convert to numeric
    district_code = as.integer(district_code),
    year = as.integer(`Time section`)
  )

sum(is.na(population)) #control for NA values
population <- population %>%   #remove "not classifiable and NA values
  filter(district_code != "0", !is.na(population))
population <- population %>% #fix issue with Bruck_Murz having the wrong code in statcube
  mutate(
    district_code = case_when(
      str_detect(district, "Bruck-M�rzzuschlag") ~ 621,
      str_detect(district, "Murtal") ~ 620,
      TRUE ~ district_code
    ))

population_filtered <- population %>% #filtered data is ready to be used in analysis
  select(year, district, district_code, population)

write_csv(population_filtered, "population")

#1. Tourism Variables

#1.1 Tourism intensity, share of overnight stays per capita
#Balanced panel collected from "Gemeindedata"
tourism_yearly <- Overnightstay_District_2015_2024
head(tourism_yearly)

tourism_yearly_final <- tourism_yearly %>%
  pivot_longer(
    cols = -c(district, district_code),
    names_to = "year",
    values_to = "overnight_stays"
  ) %>%
  mutate(
    year = as.integer(year)
  )

write_csv(tourism_yearly_final, "tourism_yearly_final")
#Remember to compare with the data from Statistik.at!

#1.2 Tourism by type of accomodation and season. Summer (May-Oct). Winter (Nov-Apr)
#Unbalanced Panel, some missing values (privacy) and 
#########I have to redo the seasonal datacleaning to make a summer and winter panel!!#######
hotel <- hotels_tourism_stats_2014_2025_UKA
head(hotel)
unique(hotel$District) #check first column
hotel <- hotels_tourism_stats_2014_2025_UKA

hotel_filtered <- hotel %>%
  mutate(
    district_code = as.integer(str_extract(District, "(?<=<)\\d+(?=>)")),
    district = str_remove(District, "\\s*<\\d+>$")
  ) %>%
  filter(!is.na(district_code)) %>%
  select(district, district_code, everything(), -District, -any_of("...26"))

overnightstays_hotels <- hotel_filtered %>%
  pivot_longer(cols = matches("^(Winter-half|Summer-half)"),
               names_to = "period", values_to = "overnight_stays",
               values_transform = list(overnight_stays = as.character)) %>%
  mutate(overnight_stays = readr::parse_number(str_trim(overnight_stays),
                                          na = c("", "-", "—", "..", "...", ":", "NA")),
    accommodation_type = "Hotels",
    season = if_else(str_detect(period, "Winter"), "winter", "summer"),
    year = as.integer(str_extract(period, "\\d{4}")))

private <- private_tourism_stats_2014_2025_UKA
private_filtered <- private %>%
  mutate(
    district_code = as.integer(str_extract(District, "(?<=<)\\d+(?=>)")),
    district = str_remove(District, "\\s*<\\d+>$")
  ) %>%
  filter(!is.na(district_code)) %>%
  select(district, district_code, everything(), -District, -any_of("...26"))

overnightstays_private <- private_filtered %>%
  pivot_longer(cols = matches("^(Winter-half|Summer-half)"),
               names_to = "period", values_to = "overnight_stays",
               values_transform = list(overnight_stays = as.character)) %>%
  mutate(overnight_stays = readr::parse_number(str_trim(overnight_stays),
                                          na = c("", "-", "—", "..", "...", ":", "NA")),
    accommodation_type = "Private Accommodation",
    season = if_else(str_detect(period, "Winter"), "winter", "summer"),
    year = as.integer(str_extract(period, "\\d{4}")))

other <- other_tourism_stats_2014_2025_UKA
other_filtered <- other %>%
  mutate(
    district_code = as.integer(str_extract(District, "(?<=<)\\d+(?=>)")),
    district = str_remove(District, "\\s*<\\d+>$")) %>%
  filter(!is.na(district_code)) %>%
  select(district, district_code, everything(), -District, -any_of("...26"))

overnightstays_other <- other_filtered %>%
  pivot_longer(cols = matches("^(Winter-half|Summer-half)"),
               names_to = "period", values_to = "overnight_stays",
               values_transform = list(overnight_stays = as.character)) %>%
  mutate(overnight_stays = readr::parse_number(str_trim(overnight_stays),
                                          na = c("", "-", "—", "..", "...", ":", "NA")),
    accommodation_type = "Other Accommodation",
    season = if_else(str_detect(period, "Winter"), "winter", "summer"),
    year = as.integer(str_extract(period, "\\d{4}")))

#Create yearly panel 
overnights_hotel <- overnightstays_hotels %>%
  group_by(district, district_code, year) %>%
  summarise(
    overnights_hotel = if_else(any(is.na(overnight_stays)),
                               NA_real_, sum(overnight_stays)),
    .groups = "drop")

overnights_private <- overnightstays_private %>%
  group_by(district, district_code, year) %>%
  summarise(
    overnights_private = if_else(any(is.na(overnight_stays)),
                                 NA_real_, sum(overnight_stays)),
    .groups = "drop")

overnights_other <- overnightstays_other %>%
  group_by(district, district_code, year) %>%
  summarise(
    overnights_other = if_else(any(is.na(overnight_stays)),
                               NA_real_, sum(overnight_stays)),
    .groups = "drop")

tourism_total_year <- overnights_hotel %>%
  left_join(overnights_private,
            by = c("district", "district_code", "year")) %>%
  left_join(overnights_other,
            by = c("district", "district_code", "year")) %>%
  mutate(district_code = if_else(district_code == 901L, 900L, district_code),
    district = if_else(district_code == 900L, "Vienna", district),
    total_overnight_stays = if_else(
      is.na(overnights_hotel) |
        is.na(overnights_private) |
        is.na(overnights_other),
      NA_real_,
      overnights_hotel + overnights_private + overnights_other)) %>%
  filter(year != 2025) %>%
  arrange(district_code, year)
write_csv(tourism_total_year, "tourism_type_final.csv")

#Create seasonal panel as well
tourism_all <- bind_rows(
  overnightstays_hotels %>%
    select(district, district_code, period, year, season,
           accommodation_type, overnight_stays),
  overnightstays_private %>%
    select(district, district_code, period, year, season,
           accommodation_type, overnight_stays),
  overnightstays_other %>%
    select(district, district_code, period, year, season,
           accommodation_type, overnight_stays)) %>%
  mutate(
    district_code = if_else(district_code == 901L, 900L, district_code),
    district = if_else(district_code == 900L, "Vienna", district),
    season = factor(season, levels = c("winter", "summer")),
    accommodation_type = factor(accommodation_type,
                                levels = c("Hotels", "Private Accommodation",
                                           "Other Accommodation"))) %>%
  arrange(district_code, year, season, accommodation_type)

#Create seasonal panel
tourism_total_season <- tourism_all %>%
  group_by(district_code, year, season) %>%
  summarise(
    district = first(district),
    total_overnight_stays = if_else(
      n_distinct(accommodation_type) == 3L && !any(is.na(overnight_stays)),
      sum(overnight_stays), NA_real_),.groups = "drop") %>%
  filter(year != 2025)

#Check
tourism_total_year %>%
  summarise(
    observations = n(),
    districts = n_distinct(district_code),
    years = n_distinct(year),
    missing = sum(is.na(total_overnight_stays)),
    negative = sum(total_overnight_stays < 0, na.rm = TRUE))

write.csv(tourism_season) #seave winter and summer data?

#Save files
write_csv(tourism_total_year, "tourism_type_final.csv")
write_csv(tourism_total_season, "tourism_season")

#Old
write_csv(tourism_season_final, "tourism_season_final") 

write_csv(overnightstays_hotels, "overnightstays_hotels")
write_csv(overnightstays_private, "overnightstays_private")
write_csv(overnightstays_other, "overnightstays_other")

#1.3 (Control) Overnight stays share in private accommodation and share of hotels
overnight_type_accommodation <- tourism_all %>%
  group_by(district_code, year, accommodation_type) %>%
  summarise(overnight_stays = sum(overnight_stays),
    .groups = "drop")
overnight_type_accommodation <- overnight_type_accommodation %>%
  group_by(district_code, year) %>%
  filter(
    n_distinct(accommodation_type) == 3,      # all 3 types present
    all(overnight_stays > 0)                  # no zeros
  ) %>%
  ungroup()
overnight_type_accommodation <- overnight_type_accommodation %>%
  group_by(district_code, year) %>%
  mutate(total_overnights = sum(overnight_stays, na.rm = TRUE)) %>%
  ungroup()
overnight_type_accommodation <- overnight_type_accommodation %>%
  mutate(share = (overnight_stays/total_overnights))

overnight_private <- overnight_type_accommodation %>%
  filter(accommodation_type == "Private Accommodation") %>% 
  select(year, district_code, overnight_stays_private = overnight_stays, overnight_private_share = share)
overnight_hotels <- overnight_type_accommodation %>%
  filter(accommodation_type == "Hotels") %>%
  select(year, district_code, overnight_stays_hotel = overnight_stays, overnight_hotel_share = share)
overnight_others <- overnight_type_accommodation %>%
  filter(accommodation_type == "Other Accommodation") %>%
  select(year, district_code,overnight_stays_other = overnight_stays, overnight_other_share = share, total_overnights)
overnight_shares <- overnight_private %>%
  left_join(overnight_hotels, by = c("year", "district_code")) %>%
  left_join(overnight_others, by = c("year", "district_code"))
write_csv(overnight_shares, "overnight_stays_by_accommodation")

overnight_shares %>%
  filter(!is.na(total_overnight_stays)) %>%
  summarise(
    max_difference = max(abs(
      total_overnight_stays -
        overnights_hotel -
        overnights_private -
        overnights_other
    ))
  )

#1.4 Control Number of beds (supply)
beds_hotel <- beds_district_hotels %>%
  mutate(district_code = str_extract(District, "(?<=<)\\d+(?=>)"),
    district = str_remove(District, "\\s*<\\d+>$")) %>%
  select(district, district_code, everything(), -District)
beds_hotel <- beds_hotel %>%
  pivot_longer(cols = matches("(Winter season|Summer season|Tourism period)"),
    names_to = "period",
    values_to = "beds_hotel") %>%
  mutate(accommodation_type = "Hotels",
         season = case_when(
           str_detect(period, "Winter") ~ "winter",
           str_detect(period, "Summer") ~ "summer",
           TRUE ~ "year"),
         year = str_extract(period, "\\d{4}") %>% as.integer()) %>%
  select(year, district_code, season, beds_hotel)

beds_private <- beds_district_private %>%
  mutate(district_code = str_extract(District, "(?<=<)\\d+(?=>)"),
         district = str_remove(District, "\\s*<\\d+>$")) %>%
  select(district, district_code, everything(), -District)
beds_private <- beds_private %>%
  pivot_longer(cols = matches("(Winter season|Summer season|Tourism period)"),
               names_to = "period",
               values_to = "beds_private") %>%
  mutate(accommodation_type = "Private Accommodation",
         season = case_when(
           str_detect(period, "Winter") ~ "winter",
           str_detect(period, "Summer") ~ "summer",
           TRUE ~ "year"),
         year = str_extract(period, "\\d{4}") %>% as.integer()) %>%
  select(year, district_code, season, beds_private)

beds_other <- beds_district_other %>%
  mutate(district_code = str_extract(District, "(?<=<)\\d+(?=>)"),
         district = str_remove(District, "\\s*<\\d+>$")) %>%
  select(district, district_code, everything(), -District)
beds_other <- beds_other %>%
  pivot_longer(cols = matches("(Winter season|Summer season|Tourism period)"),
               names_to = "period",
               values_to = "beds_other") %>%
  mutate(accommodation_type = "Other Accommodation",
    season = case_when(
      str_detect(period, "Winter") ~ "winter",
      str_detect(period, "Summer") ~ "summer",
      TRUE ~ "year"),
    year = str_extract(period, "\\d{4}") %>% as.integer()) %>%
  select(year, district_code, season, beds_other)


        
write_csv(beds_hotel, "beds_hotel_district")
write_csv(beds_private, "beds_private_district")
write_csv(beds_other, "beds_other_district")


#2 Housing affordability 
#2.1 Net Income

income1 <- netincome_2014_2022_
names(income1) <- c("year_raw", "value_type", "district_raw", "value", "value_shifted", "extra" )
income_fixed <- income1 %>% #fix problem with Vienna district names in csv.
  mutate(
    year_raw = str_remove_all(year_raw, '"'),
    value_type = str_remove_all(value_type, '"'),
    district_raw = str_remove_all(district_raw, '"'),
    value = str_remove_all(value, '"'),
    value_shifted = str_remove_all(value_shifted, '"'),
    
    wien_split = str_detect(district_raw, "^Wien\\s+\\d+\\.$"),
    
    district_full = case_when(
      wien_split ~ paste(district_raw, value, sep = ", "),
      TRUE ~ district_raw
    ),
    
    value = case_when(
      wien_split ~ as.numeric(value_shifted),
      TRUE ~ as.numeric(value)
    ),
    
    district_code = str_extract(district_full, "(?<=<)\\d+(?=>)"),
    district = str_remove(district_full, "\\s*<\\d+>$"),
    year = as.integer(year_raw),
    district_code = as.integer(district_code),
    district_code = if_else(district == "Wien", 900L, district_code),
  ) %>%
  select(year, value_type, district, district_code, value)

income_fixed <- income_fixed %>%
  filter(!is.na(value)) %>%
  filter(!str_to_lower(district) %in% c("unknown", "abroad")) %>%
  select(year, value_type, district, district_code, value)
#In the 23-24 data statistik austria reports the following districts as a group, 
#to separate them I rely on the 2022 data and transfer the share over to the 23-24 data.
grouped_district_names <- c("Eisenstadt", "Amstetten (Bezirk) + Waidhofen/Ybbs")

income2 <- netincome_2022_2024
names(income2) <- c("year_raw", "value_type", "district_raw", "value", "value_shifted", "extra" )
income2_fixed <- income2 %>% #fix problem with Vienna district names in csv.
  mutate(
    year_raw = str_remove_all(year_raw, '"'),
    value_type = str_remove_all(value_type, '"'),
    district_raw = str_remove_all(district_raw, '"'),
    value = str_remove_all(value, '"'),
    value_shifted = str_remove_all(value_shifted, '"'),
    
    wien_split = str_detect(district_raw, "^Wien\\s+\\d+\\.$"),
    
    district_full = case_when(
      wien_split ~ paste(district_raw, value, sep = ", "),
      TRUE ~ district_raw
    ),
    
    value = case_when(
      wien_split ~ as.numeric(value_shifted),
      TRUE ~ as.numeric(value)
    ),
    
    district_code = str_extract(district_full, "(?<=<)\\d+(?=>)"),
    district = str_remove(district_full, "\\s*<\\d+>$"),
    year = as.integer(year_raw),
    district_code = as.integer(district_code),
    district_code = if_else(district == "Wien", 900L, district_code),
  ) %>%
  select(year, value_type, district, district_code, value)

income2_fixed <- income2_fixed %>%
  filter(!is.na(value)) %>%
  filter(!str_to_lower(district) %in% c("unknown", "abroad")) %>%
  select(year, value_type, district, district_code, value)

#Join income dataset:
income_final <- bind_rows(
  income_fixed,
  income2_fixed %>% filter(year != 2022)
) %>%
  arrange(district_code, value_type, year)


#Create income weiights for the 5 districts that are merged in the full data, based on the most recent (2022)
income2 <- netincome_district_2022 #use to create shares for #101,102,103, and 303,305 that is merged togehter.
names(income2) <- c("year_raw", "value_type", "district_raw", "value", "value_shifted", "extra" )
income2_fixed <- income2 %>%
  mutate(
  year_raw = str_remove_all(year_raw, '"'),
  value_type = str_remove_all(value_type, '"'),
  district_raw = str_remove_all(district_raw, '"'),
  value = str_remove_all(value, '"'),
  value_shifted = str_remove_all(value_shifted, '"'),
  wien_split = str_detect(district_raw, "^Wien\\s+\\d+\\.$"),
  district_full = case_when(
    wien_split ~ paste(district_raw, value, sep = ", "),
    TRUE ~ district_raw),
  value = case_when(wien_split ~ as.numeric(value_shifted),
    TRUE ~ as.numeric(value)),district_code = str_extract(district_full, "(?<=<)\\d+(?=>)"),
  district = str_remove(district_full, "\\s*<\\d+>$"),
  year = as.integer(year_raw),
  district_code = as.integer(district_code)) %>%
  select(year, value_type, district, district_code, value)
income2_fixed <- income2_fixed %>%
  filter(!is.na(district_code)) #remvoe NA districts
income2_fixed <- income2_fixed %>%
  filter(!is.na(value)) #should I also remove NA values

# Create 2022 allocation weights from the last year with separate districts
income_2022_weights <- income2_fixed %>%
  mutate(
    district_code = as.integer(district_code),
    value = as.numeric(value),
    district_group = case_when(
      district_code %in% c(101, 102, 103) ~
        "Eisenstadt",
      district_code %in% c(303, 305) ~
        "Amstetten (Bezirk) + Waidhofen/Ybbs",
      TRUE ~ NA_character_)) %>%
  filter(year == 2022,!is.na(district_group),value_type %in% c("Net income (EUR)", "Entity count: Net income" )) %>%
  group_by(district_group, value_type) %>%
  mutate(
    share_2022 = value / sum(value)) %>%
  ungroup() %>%
  select(district_group,value_type,district,district_code,share_2022)

#Apply weihts to merged districts
income3_grouped <- income_final %>%
  filter(is.na(district_code),district %in% grouped_district_names) %>%
  rename(district_group = district,grouped_total = value) %>%
  select(
    year,
    value_type,
    district_group,
    grouped_total)

#Apply the weights from 22 to the 23-24 data.
income3_allocated <- income3_grouped %>% 
  left_join(
    income_2022_weights,
    by = c("district_group", "value_type"),
    relationship = "many-to-many"
  ) %>%
  group_by(year, value_type, district_group) %>%
  arrange(district_code, .by_group = TRUE) %>%
  mutate(
    allocated_raw = grouped_total * share_2022,
    value = round(allocated_raw),
    value = if_else(
      row_number() == n(),
      value + grouped_total - sum(value),
      value),
    income_estimated = TRUE) %>%
  ungroup()

income3_observed <- income_final %>% 
  filter(!is.na(district_code)) %>% #remove the grouped districts
  mutate(district_code = as.integer(district_code), income_estimated = FALSE)
income3_allocated_final <- income3_allocated %>%
  select(year,value_type,district,district_code,value,income_estimated)

net_income_final <- bind_rows(
  income3_observed,
  income3_allocated_final) %>%
  arrange(year, district_code, value_type)
income3_corrected %>%
  count(year, district_code, value_type) %>%
  filter(n > 1)


write.csv(net_income_final, "income_corrected_2014-24.csv")

#2.2.1 Flat prices, most solid measure. Can also use housing prices from the standariezed version
prices_flats <- average_housingprices_district_2015_2024
prices_flats <- prices_flats %>% 
  filter(!is.na(district))

write_csv(prices_flats, "average_prices_flats")  

#2.2.2 House prices, control.
prices_house <- houseaverage_houseprices_district_2015_2024
prices_house <- prices_house %>%
  filter(!is.na(district))
write_csv(prices_house, "average_prices_house")

#2.2.3 Land prices, more variation
prices_land <- SpecialEvaluation1 %>%
  pivot_longer(
    cols = `2015`:`2024`,
    names_to = "year",
    values_to = "plot_price"
  ) %>%
  mutate(
    year = as.integer(year),
    plot_price = as.numeric(plot_price)
  ) %>%
  filter(!is.na(district)) %>%
  arrange(district_code, year)
write_csv(prices_land, "average_prices_land")

#3 Control variables
#3.1 Size of district
district_size <- austria_districts_km2
write_csv(district_size, "district_size_km2")

#3.2 Unemployment
unemployment <- unemployment_database
names(unemployment) <- c("district_raw", "status_raw", "value_type_raw", "year_raw", "value", "extra", "extra2")

unemployment <- unemployment %>%
  mutate(
    district_raw   = str_remove_all(district_raw, '"'),
    status     = str_remove_all(status_raw, '"'),
    value_type = str_remove_all(value_type_raw, '"'),
    year       = str_remove_all(year_raw, '"'),
    extra          = str_remove_all(extra, '"')
  )
unemployment <- unemployment %>% #separate district name and code
  mutate( 
    district_code = str_extract(district_raw, "(?<=<)\\d+(?=>)"), 
    district = str_remove(district_raw, " <\\d+>"))

sum(is.na(unemployment)) #control for NA values
population_filtered <- population %>%   #remove "not classifiable and NA values
  filter(district_code != "0", !is.na(population))
sum(is.na(population_filtered)) 

unemployment_filtered <- unemployment %>% #filtered data is ready to be used in analysis
  select(year, district, district_code, status, value_type, value)
unemployment_filtered <- unemployment_filtered %>%
  mutate(
    year = as.integer(year),
    district_code = as.integer(district_code)
  )
unemployment_filtered <- unemployment_filtered %>% 
  mutate(
  district_code = if_else(nchar(as.character(district_code)) == 5, 900L, district_code),
  district = if_else(district_code == 900L, "Vienna", district)
) %>%
  group_by(year, district_code, district, status, value_type) %>%
  summarise(
    across(
      where(is.numeric) & !any_of(c("year")),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  arrange(year, district_code)


# Create Unemployment rate
unemployment_wide <- unemployment_filtered %>% 
  pivot_wider(
    names_from = status,
    values_from = value
  )
unemployment_wide <- unemployment_wide %>% #calc unemployment rate
  mutate(
    unemployment_rate = Unemployed / Total
  )
unemployment_wide <- unemployment_wide %>%
  mutate(
    employment_rate = Employed / Total
  )

unemployment_rate <- unemployment_wide %>%
  select(
    district,
    district_code,
    year,
    unemployment_rate,
    employment_rate
  )
write_csv(unemployment_rate, "unemployment_rates_vienna_agg")

# Add 2024 to the Vienna aggregated panel, new data has become available (09.06.2026)
unemployment_rate_agg <- unemployment_rates_vienna_agg %>%
  filter(year %in% 2015:2023)
names(Unemployment_2024) <- as.character(Unemployment_2024[1,])
Unemployment_2024 <- Unemployment_2024[-1,]
Unemployment_2024 <- Unemployment_2024 %>%
  select(year, district_code, unemployment_rate, employment_rate) %>%
  mutate(
    year = as.integer(year),
    district_code = as.integer(district_code),
    unemployment_rate = parse_number(as.character(unemployment_rate)),
    employment_rate = parse_number(as.character(employment_rate))) %>%
  filter(nchar(as.character(district_code)) <= 3) %>%
  mutate(district_code = ifelse(district_code == 9, 900, district_code)) %>%
  filter(nchar(as.character(district_code)) >= 3) %>%
  filter(!(district_code %in% 901:923)) %>%
  mutate(unemployment_rate = unemployment_rate / 100,
    employment_rate = employment_rate / 100)
  
unemployment_rate_agg <- bind_rows(unemployment_rate_agg, Unemployment_2024)


#3.3 CPI total, and cpi housing
cpi <- cpi_total_13_25
cpi <- cpi %>%
  mutate(
    year = as.integer(year))
write_csv(cpi, "cpi_2013-25")

#3.4 Housing Lending Rates
lending_rates <- OeNB_lendingrates_2014_2025
lending_rates <- lending_rates %>%
  mutate(year = as.integer(year))
write_csv(lending_rates, "lending_rates_2014-25")


#3.5 Migration rates (from inside Austria, and abroad) #no district data for Vienna
int_migration_net <- international_netmigration_2014_2024
int_migration_net <- int_migration_net %>% #separate district name and code
  mutate( 
    district_code = str_extract(District, "(?<=<)\\d+(?=>)"), 
    district = str_remove(District, " <\\d+>"))
head(int_migration_net)

int_migration_net <- int_migration_net %>%
  mutate(
    Number = na_if(Number, "-"),   # replace "-" with NA
    net_migration = as.numeric(Number),    # convert to numeric
    district_code = as.numeric(district_code)
  )
int_migration_net <- int_migration_net %>%
  filter(!is.na(district_code), district_code != 0 & district_code !=900)
int_migration_net <- int_migration_net %>%
  mutate(year = as.integer(`Time section`))
int_migration_net_filtered <- int_migration_net %>%
  select(year,district,district_code, net_migration)

int_migration_net_wiendistricts <- int_netmigration_viennadist #add districts in vienna
int_migration_net_wiendistricts <- int_migration_net_wiendistricts %>% #separate district name and code
  mutate( 
    district_code = str_extract(`Administrative district`, "(?<=<)\\d+(?=>)"), 
    district = str_remove(`Administrative district`, " <\\d+>"))
head(int_migration_net_wiendistricts)  
int_migration_net_wiendistricts <- int_migration_net_wiendistricts %>%
  mutate(
    net_migration = as.numeric(Number),    # convert to numeric
    district_code = as.numeric(district_code)
  )
int_migration_net_wienfiltered <- int_migration_net_wiendistricts %>%
  select(year,district,district_code, net_migration)

int_migration_final <- bind_rows(
  int_migration_net,
  int_migration_net_wienfiltered
)
int_migration_final <- int_migration_final %>%
  select(year, district, district_code, net_migration)

outmigration_origin <- inmigrationorigin_district_2014_2024
outmigration_origin <- outmigration_origin %>% #separate district name and code
  mutate( 
    district_code = str_extract(`Administrative district of place of origin`, "(?<=<)\\d+(?=>)"), 
    district = str_remove(`Administrative district of place of origin`, " <\\d+>"))
head(outmigration_origin)
outmigration_origin <- outmigration_origin %>%
  mutate(
    out_migration = as.numeric(Number),    # convert to numeric
    district_code = as.numeric(district_code),
    year = as.integer(`Time section`)
  )
outmigration_origin <- outmigration_origin %>% #Some districts has been removed over the years, like 324
  filter(!is.na(out_migration)) #district_code != 0 & district_code !=602) #Remove districts with missing values/old districts
outmigration_origin <- outmigration_origin %>% #fix issue with Bruck_Murz having the wrong code in statcube
  mutate(
    district_code = case_when(
      str_detect(district, "Bruck-M�rzzuschlag") ~ 621,
      str_detect(district, "Murtal") ~ 620,
      TRUE ~ district_code
    )
  )
outmigration_origin <- outmigration_origin %>%
  select(year, district, district_code, out_migration)

inmigration_destination <- internal_migrationdest_district_2014_2024 #add vienna districts!
inmigration_destination <- inmigration_destination %>%
  mutate( 
  district_code = str_extract(`Administrative district of destination`, "(?<=<)\\d+(?=>)"), 
  district = str_remove(`Administrative district of destination`, " <\\d+>"))
head(inmigration_destination)
inmigration_destination <- inmigration_destination %>%
  mutate(
    in_migration = as.numeric(Number),    # convert to numeric
    district_code = as.numeric(district_code),
    year = as.integer(`Time section`)
  )

inmigration_destination <- inmigration_destination %>%
  filter(!is.na(in_migration), district_code != 0 & district_code !=602) #Remove old districts
inmigration_destination <- inmigration_destination %>% #fix issue with Bruck_Murz having the wrong code in statcube
  mutate(
    district_code = case_when(
      str_detect(district, "Bruck-M�rzzuschlag") ~ 621,
      str_detect(district, "Murtal") ~ 620,
      TRUE ~ district_code
    )
)
inmigration_destination <- inmigration_destination %>%
  select(year, district, district_code, in_migration)

inmigration_destination %>%
  count(year, district_code) %>%
  filter(n > 1)

outmigration_origin %>%
  count(year, district_code) %>%
  filter(n > 1)

net_national_migration <- inmigration_destination %>%
  select(year, district, district_code, in_migration) %>%
  full_join(
    outmigration_origin %>%
      select(year, district_code, out_migration),
    by = c("year", "district_code")
  ) %>%
  mutate(
    net_national_migration = in_migration - out_migration
  )
write_csv(net_national_migration, "net_national_migration")  

#add international migration
migration_final <- net_national_migration %>%
  left_join(
    int_migration_final %>%
      select(year, district_code, net_migration),
    by = c("year", "district_code")
  )

migration_final <- migration_final %>%
  mutate(
    total_net_migration = net_national_migration + net_migration
  )
write_csv(migration_final,"migration_final")
#migration_final <- migration_final %>% #code to compute migration rate for analysis.
#left_join(population, by = c("district_code", "year")) %>%
# mutate(
#   total_net_migration_rate = total_net_migration / population)

#3.6 New Dwellings only have data from 2014-2021!!
new_dwellings <- newconst_dwellings_district_2014_2021
new_dwellings_wien <- newconst_dwellings_vienna_2014_2021
head(new_dwellings_wien)
new_dwellings_wien <- new_dwellings_wien %>%
  mutate(
    district_code = str_extract(Commune, "(?<=<)\\d{3}") %>% as.integer(),
    district = str_remove(Commune, "\\s*<\\d+>") %>% str_remove_all('"'),
    Number = as.numeric(Number),
    year = as.integer(Jahr))

new_dwellings <- new_dwellings %>%
  mutate( 
    district_code = str_extract(`Political district`, "(?<=<)\\d+(?=>)"), 
    district = str_remove(`Political district`, " <\\d+>")) %>%
  mutate(
    Number = as.numeric(Number),    # convert to numeric
    district_code = as.integer(district_code),
    year = as.integer(Jahr)
  )
new_dwellings <- new_dwellings %>% #insert Vienna districts
  bind_rows(new_dwellings_wien)  

new_dwellings <- new_dwellings %>% #fix issue with Bruck_Murz having the wrong code in statcube
  mutate(
    district_code = case_when(
      str_detect(district, "Bruck-M�rzzuschlag") ~ 621,
      str_detect(district, "Murtal") ~ 620,
      TRUE ~ district_code
    ))
new_dwellings_clean <- new_dwellings %>%
  filter(str_detect(`Type of construction measure (level +1)`, "New construction"),
         district_code!=900) %>%
  transmute(
    year,
    district,
    district_code,
    new_dwellings = Number
  )

write_csv(new_dwellings_clean, "new_dwellings_2014-2021")
#Rememeber to create share of new dwellings

#Create estimate for district new buildings from 22-24

new_dwellings_22 <- new_dwellings_2014_2021 %>%
  mutate(
    district_code = str_pad(as.character(district_code), 3, pad = "0"),
    bundesland_code = substr(district_code, 1, 1)
  )
bundesland_lookup <- tibble::tribble(
  ~bundesland_code, ~bundesland,
  "1", "Burgenland",
  "2", "Carinthia",
  "3", "Lower Austria",
  "4", "Upper Austria",
  "5", "Salzburg",
  "6", "Styria",
  "7", "Tyrol",
  "8", "Vorarlberg",
  "9", "Vienna"
)

new_dwellings_22 <- new_dwellings_22 %>%
  left_join(bundesland_lookup, by = "bundesland_code")
district_shares <- new_dwellings_22 %>% #compute district shares form know data
  group_by(bundesland, year) %>%
  mutate(
    share = new_dwellings / sum(new_dwellings, na.rm = TRUE)
  ) %>%
  ungroup()
write_csv(district_shares, "region share of new dwellings per district")

avg_shares <- district_shares %>% #method 1, avg shares over time
  group_by(district_code, bundesland) %>%
  summarise(
    avg_share = mean(share, na.rm = TRUE),
    .groups = "drop"
  )

#get 2024 data
head(newconst_dwellings_region_2014_2025)
df <- newconst_dwellings_region_2014_2025 %>% #fixs variable names
  mutate(across(where(is.character), ~ str_replace_all(.x, "[\"']", "")))
df <- df %>% #remove " column
  select(-all_of('"'))
df <- df %>% #fix column names
  rename_with(~ str_replace_all(.x, "[\"']", "")) %>%
  rename_with(~ str_trim(.x))

new_dwellings_region <- df %>%
  filter(str_detect(`Type of construction measure (level +1)`, "New construction of entire buildings"))

new_dwellings_region <- new_dwellings_region %>%
  mutate(Number = as.numeric(Number),    # convert to numeric
  year = as.integer(Jahr),
  bundesland = str_remove(Bundesland, " <.*>$")
)
new_dwellings_region <- new_dwellings_region %>%
  filter(str_detect(`Type of construction measure (level +1)`, "New construction of entire buildings")) %>%
  transmute(
    year,
    bundesland,
    new_dwellings = Number)
head(new_dwellings_region) 
region_2022_2024 <- new_dwellings_region %>%
  filter(year >= 2022)

#merge the estimates with full data
new_dwellings_estimates22_24 <- region_2022_2024 %>%
  left_join(avg_shares, by = "bundesland")
new_dwellings_estimates22_24 <- new_dwellings_estimates22_24 %>%
  mutate(new_dwellings_est = avg_share * new_dwellings) %>%
  mutate(district_code = as.integer(district_code))
new_dwellings_estimates22_24 <- new_dwellings_estimates22_24 %>%
  select(year, district_code, new_dwellings = new_dwellings_est)
new_dwellings_final <- new_dwellings_2014_2021 %>%
  filter(year <= 2021) %>%
  bind_rows(new_dwellings_estimates22_24) %>%
  arrange(district_code, year)
head(new_dwellings_final)

district_lookup <- new_dwellings_2014_2021 %>%
  select(district_code, district) %>%
  distinct()
new_dwellings_final <- new_dwellings_final %>%
  left_join(district_lookup, by = "district_code", suffix = c("", "_lookup")) %>%
  mutate(
    district = ifelse(is.na(district), district_lookup, district)
  ) %>%
  select(-district_lookup)
write_csv(new_dwellings_final, "new_dwellings_total")

#3.7 Total Dwellings 
dwellings14 <- X2014_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings15 <- X2015_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings16 <- X2016_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings17 <- X2017_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings18 <- X2018_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings19 <- X2019_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings20 <- X2020_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings21 <- X2021_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings22 <- X2022_daten_gwr_v_geb_polbez_de %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings23 <- X2023_daten_gwr_v_geb_polbez_en %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
dwellings24 <- X2024_daten_gwr_v_geb_polbez_en %>%
  select(id, name, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)


dwellings14 <- dwellings14 %>% mutate(year = 2014)
dwellings15 <- dwellings15 %>% mutate(year = 2015)
dwellings16 <- dwellings16 %>% mutate(year = 2016)
dwellings17 <- dwellings17 %>% mutate(year = 2017)
dwellings18 <- dwellings18 %>% mutate(year = 2018)
dwellings19 <- dwellings19 %>% mutate(year = 2019)
dwellings20 <- dwellings20 %>% mutate(year = 2020)
dwellings21 <- dwellings21 %>% mutate(year = 2021)
dwellings22 <- dwellings22 %>% mutate(year = 2022)
dwellings23 <- dwellings23 %>% mutate(year = 2023)
dwellings24 <- dwellings24 %>% mutate(year = 2024)

dwellings <- bind_rows(
  dwellings14,
  dwellings15,
  dwellings16,
  dwellings17,
  dwellings18,
  dwellings19,
  dwellings20,
  dwellings21,
  dwellings22,
  dwellings23,
  dwellings24
)

dwellings <- dwellings %>% 
  mutate(
    components_sum = nurhws + nurnws + hwsnws + ohnews,
    check_equal = components_sum == geb
  ) #check that the number are correct, which they are.

dwellings <- dwellings %>%
  rename(district_code = id,
         district = name) %>%
  mutate(district_code = as.integer(district_code),
         year = as.integer(year))

dwellings <- dwellings %>% 
  mutate(share_nurhws = nurhws / geb,
         share_nurnws = nurnws / geb,
         share_hwsnws = hwsnws / geb,
         share_ohnews = ohnews / geb
  )
building_shares <- dwellings %>%
  select(year, district, district_code, share_nurhws, share_nurnws, share_hwsnws, share_ohnews)
building_dwelling_totals <- dwellings %>%
  select(year, district, district_code, whg, wgeb, geb, geb_hotel, nurhws, nurnws, hwsnws, ohnews)
  
write_csv(building_shares, "building_shares_2014-24")
write_csv(building_dwelling_totals, "building_dwelling_totals_2014-24")

#4.1 Fix consistency issues, maybe not neccessary, controlled for this and does not change much
#Wien-umgebung was split into four districts in NÖ.
weights_2017 <- population %>%
  filter(year == 2017, district_code %in% c(307, 312, 319, 321)) %>%
  mutate(weight = population / sum(population)) %>%
  select(new_district_code = district_code, weight)
split_codes <- c(307,312,319,321)
 <- panel_data %>%
  filter(district_code == 324, year %in% c(2015, 2016)) %>%
  crossing(weights_2017) %>%
  mutate(
    district_code = new_district_code
  ) %>%
  select(-new_district_code)

panel_data_split <- panel_data %>%
  filter(district_code == 324, year %in% c(2015, 2016)) %>%
  crossing(weights_2017) %>%
  mutate(
    district_code = district_code.y,
    housing_affordability_index = housing_affordability_index * weight,
    tourism_intensity = tourism_intensity * weight)

# 4.2 Measurement break in income data, new measurement method in 23.



#Create Geo dummies for OLS spesification
geo_dummies <- tribble(
  ~district_code, ~mountain_dummy, ~lake_dummy,
  
  # Burgenland
  101, 0, 0,   # Eisenstadt(Stadt)
  102, 0, 1,   # Rust(Stadt) (Neusiedler See)
  103, 0, 1,   # Eisenstadt-Umgebung (Neusiedler See area)
  104, 0, 0,
  105, 0, 0,
  106, 0, 0,
  107, 0, 1,   # Neusiedl am See
  108, 0, 0,
  109, 0, 0,
  
  # Kärnten
  201, 1, 0,   # Klagenfurt Stadt
  202, 1, 0,   # Villach Stadt (Faaker See / Ossiacher See)
  203, 1, 1,   # Hermagor (Pressegger See / alpine)
  204, 1, 1,   # Klagenfurt Land (Wörthersee)
  205, 1, 0,   # St. Veit an der Glan 
  206, 1, 1,   # Spittal an der Drau (Millstätter See, Weißensee)
  207, 1, 1,   # Villach Land (Faaker See, Ossiacher See, Wörthersee part)
  208, 1, 1,   # Völkermarkt (Klopeiner See, small)
  209, 1, 0,
  210, 1, 1,   # Feldkirchen (Ossiacher See / Millstätter See side nearby)
  
  # Niederösterreich
  301, 0, 0,
  302, 0, 0,
  303, 0, 0,
  304, 0, 0,
  305, 0, 0,   #Amstetten (Goes into the mountains)
  306, 0, 0,
  307, 0, 0,
  308, 0, 0,
  309, 0, 0,
  310, 0, 0,
  311, 0, 0,
  312, 0, 0,
  313, 0, 0,   # Krems - Land (Wachau)
  314, 1, 0,   # Lilienfeld
  315, 0, 0,   # Melk
  316, 0, 0,
  317, 0, 0,
  318, 1, 0,   # Neunkirchen (Schneeberg / Rax)
  319, 0, 0,
  320, 1, 0,   # Scheibbs
  321, 0, 0,
  322, 0, 0,
  323, 1, 0,   # Wiener Neustadt-Land
  325, 0, 0,
  
  # Oberösterreich
  401, 0, 0,
  402, 0, 0,
  403, 0, 0,
  404, 0, 1,   # Braunau (Mattsee partly)
  405, 0, 0,
  406, 0, 0,   
  407, 1, 1,   # Gmunden (Salzkammergut / Traunsee)
  408, 0, 0,
  409, 1, 0,   # Kirchdorf (Hinterstoder / Kalkalpen)
  410, 0, 0,
  411, 0, 0,
  412, 0, 0,
  413, 0, 0,
  414, 0, 0,
  415, 1, 0,   # Steyr-Land (Kalkalpen)
  416, 0, 0,
  417, 0, 1,   # Vöcklabruck (Attersee, Mondsee side)
  418, 0, 0,
  
  # Salzburg
  501, 0, 0,   # Salzburg Stadt
  502, 1, 0,   # Hallein
  503, 1, 1,   # Salzburg-Umgebung (Fuschlsee/Wallersee area)
  504, 1, 0,   # St. Johann im Pongau
  505, 1, 0,   # Tamsweg
  506, 1, 1,   # Zell am See
  
  # Steiermark
  601, 0, 0,
  603, 0, 0,
  606, 0, 0,
  610, 0, 0,
  611, 1, 0,   # Leoben (Eisenerz)
  612, 1, 1,   # Liezen (Ausseerland / alpine lakes)
  614, 1, 0,   # Murau
  616, 0, 0,   # Voitsberg / alpine edge
  617, 0, 0,
  620, 1, 0,   # Murtal
  621, 1, 0,   # Bruck-Mürzzuschlag
  622, 0, 1,   # Hartberg-Fürstenfeld (Stubenbergsee; optional)
  623, 0, 0,
  
  # Tirol
  701, 1, 0, 
  702, 1, 0,
  703, 1, 0,
  704, 1, 0,
  705, 1, 0,
  706, 1, 0,
  707, 1, 0,
  708, 1, 0,
  709, 1, 0,
  
  # Vorarlberg
  801, 1, 0,
  802, 1, 1,   # Bregenz (Lake Constance)
  803, 1, 0,
  804, 1, 0,
  
  # Wien
  900, 0, 0,   # Vienna aggregate if you created it
  901, 0, 0,
  902, 0, 0,
  903, 0, 0,
  904, 0, 0,
  905, 0, 0,
  906, 0, 0,
  907, 0, 0,
  908, 0, 0,
  909, 0, 0,
  910, 0, 0,
  911, 0, 0,
  912, 0, 0,
  913, 0, 0,
  914, 0, 0,
  915, 0, 0,
  916, 0, 0,
  917, 0, 0,
  918, 0, 0,
  919, 0, 0,
  920, 0, 0,
  921, 0, 0,
  922, 0, 0,
  923, 0, 0
)
