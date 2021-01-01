easypackages::libraries("tidyverse", "jsonify", "janitor", "zoo", "scales", "htmltools", "DT", "here", "sparkline", "rmarkdown", "RcppRoll", "stringi")

pop_county <- read_csv("population/county.csv")
pop_zip <- read_csv("population/zip.csv")

md_hospit_county_today <- read_csv("data/md_hospit_county_today.csv")

md_api <- function(api_url) {
	suppressWarnings(from_json(api_url)[["features"]]$attributes) %>%
		clean_names() %>%
		select(-objectid)
}

md_api_pivot <- function(df, first_day, geography = "name", na_to_zero = T) {
	df <- df %>%
		pivot_longer(cols = -1) %>%
		group_by_at(geography) %>%
		mutate(date = seq.Date(first_day, first_day + dplyr::n() - 1, by = "day")) %>%
		ungroup()

	if (na_to_zero)
		df <- mutate(df, value = replace_na(value, 0))

	df
}

md_indicator_combine <- function(cases, deaths, prob_deaths, join_key, uk_text = "unknown") {
	inner_join(cases, deaths, by = c("date", join_key)) %>%
		inner_join(prob_deaths, by = c("date", join_key)) %>%
		filter(date == max(date), str_detect(!!rlang::sym(join_key), uk_text, negate = T)) %>%
		select(-date) %>%
		pivot_longer(cols = -!!rlang::sym(join_key), names_to = "variable") %>%
		mutate(variable = recode(variable, "cases" = "Confirmed cases", "deaths" = "Confirmed deaths", "prob_deaths" = "Probable deaths"))
}

md_fips <- read_csv("md_fips.csv")

md_counties_cases <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_CasesByCounty/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("3/15/2020", "%m/%d/%y")) %>%
	select(date, county = name, cases = value) %>%
	inner_join(md_fips, by = "county") %>%
	group_by(county) %>%
	mutate(new_cases = pmax(cases - lag(cases), 0)) %>%
	ungroup() %>%
	inner_join(pop_county, by = c("county", "fips")) %>%
	mutate(per_100k = cases/population)

md_counties_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ConfirmedDeathsByCounty/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/3/2020", "%m/%d/%y")) %>%
	select(date, county = name, deaths = value) %>%
	inner_join(md_fips, by = "county") %>%
	group_by(county) %>%
	mutate(new_deaths = pmax(deaths - lag(deaths), 0)) %>%
	ungroup()

md_counties_prob_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ProbableDeathsByCounty/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/13/2020", "%m/%d/%y")) %>%
	select(date, county = name, prob_deaths = value) %>%
	inner_join(md_fips, by = "county") %>%
	group_by(county) %>%
	mutate(new_prob_deaths = pmax(prob_deaths - lag(prob_deaths), 0)) %>%
	ungroup()

md_counties <- inner_join(md_counties_cases, md_counties_deaths, by = c("county", "date", "fips")) %>%
	inner_join(md_counties_prob_deaths, by = c("county", "date", "fips")) %>%
	select(county, fips, date, cases, deaths, prob_deaths, new_cases, new_deaths, new_prob_deaths) %>%
	inner_join(pop_county) %>%
	mutate(cases_per_100k = cases/population*100000,
				 deaths_per_100k = deaths/population*100000,
				 prob_deaths_per_100k = prob_deaths/population*100000)

md_counties_today <- filter(md_counties, date == max(date))

md_zips <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_MASTER_ZIP_CODE_CASES/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("2020-04-11"), "zip_code") %>%
	select(date, zip = zip_code, cases = value) %>%
	group_by(zip) %>%
	mutate(new_cases = pmax(cases - lag(cases), 0)) %>%
	ungroup() %>%
	inner_join(pop_zip) %>%
	mutate(cases_per_100k = cases/population*100000)

md_zips_today <- filter(md_zips, date == max(date))

md_age_cases <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_CasesByAgeDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("3/29/2020", "%m/%d/%y")) %>%
	select(date, age_range = name, cases = value)

md_age_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ConfirmedDeathsByAgeDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/8/2020", "%m/%d/%y")) %>%
	select(date, age_range = name, deaths = value)

md_age_prob_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ProbableDeathsByAgeDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/13/2020", "%m/%d/%y")) %>%
	select(date, age_range = name, prob_deaths = value)

age_data <- md_indicator_combine(md_age_cases, md_age_deaths, md_age_prob_deaths, "age_range") %>%
	mutate(age = str_replace_all(str_sub(age_range, 5), c("_to_" = "-", "plus" = "+"))) %>%
	select(age, variable, value) %>%
	pivot_wider(names_from = variable, values_from = value) %>%
	select(-`Probable deaths`)

md_sex_cases <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_CasesByGenderDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("3/12/2020", "%m/%d/%y")) %>%
	select(date, sex = name, cases = value)

md_sex_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ConfirmedDeathsByGenderDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/8/2020", "%m/%d/%y")) %>%
	select(date, sex = name, deaths = value)

md_sex_prob_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ProbableDeathsByGenderDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/13/2020", "%m/%d/%y")) %>%
	select(date, sex = name, prob_deaths = value)

sex_data <- md_indicator_combine(md_sex_cases, md_sex_deaths, md_sex_prob_deaths, "sex") %>%
	mutate(sex = tools::toTitleCase(sex)) %>%
	select(sex, variable, value) %>%
	pivot_wider(names_from = variable, values_from = value) %>%
	select(-`Probable deaths`)

md_race_cases <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_CasesByRaceAndEthnicityDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/6/2020", "%m/%d/%y"), na_to_zero = F) %>%
	select(date, race = name, cases = value)

md_race_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ConfirmedDeathsByRaceAndEthnicityDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/6/2020", "%m/%d/%y"), na_to_zero = F) %>%
	select(date, race = name, deaths = value)

md_race_prob_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_ProbableDeathsByRaceAndEthnicityDistribution/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("4/13/2020", "%m/%d/%y"), na_to_zero = F) %>%
	select(date, race = name, prob_deaths = value)

race_data <- md_indicator_combine(md_race_cases, md_race_deaths, md_race_prob_deaths, "race", uk_text = "not_available") %>%
	mutate(race = recode(race, "african_american" = "Black", "asian" = "Asian", "white" = "White", "hispanic" = "Hispanic", "other" = "Other")) %>%
	pivot_wider(names_from = variable, values_from = value) %>%
	select(-`Probable deaths`)

md_hospit <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalCurrentlyHospitalizedAcuteAndICU/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	setNames(c("date", "Acute", "ICU", "Total")) %>%
	mutate(date = seq.Date(as.Date("2020-03-26"), as.Date("2020-03-26") + n() - 1, by = "day")) %>%
	filter(!is.na(Acute)) %>%
	pivot_longer(cols = -date)

hospit_data <- md_hospit %>%
	pivot_wider(names_from = date, values_from = value)

md_negatives <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_NumberOfPersonsTestedNegative/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	filter(!is.na(negative_tests)) %>%
	mutate(date = seq.Date(as.Date("2020-03-26"), as.Date("2020-03-26") + n() - 1, by = "day")) %>%
	select(date, negatives = negative_tests)

md_isolation <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalNumberReleasedFromIsolation/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	mutate(date = seq.Date(as.Date("2020-03-27"), as.Date("2020-03-27") + n() - 1, by = "day"))

md_volume <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TestingVolume/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	mutate(date = seq.Date(as.Date("2020-03-24"), as.Date("2020-03-24") + n() - 1, by = "day"))

md_ever_hospit <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalHospitalizations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	mutate(date = seq.Date(as.Date("2020-03-13"), as.Date("2020-03-13") + n() - 1, by = "day"))

md_statewide_cases <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalCasesStatewide/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	select(date, cases = count) %>%
	mutate(date = seq.Date(as.Date("2020-03-04"), as.Date("2020-03-04") + n() - 1, by = "day"),
				 new_cases = cases - lag(cases))

md_statewide_deaths <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalConfirmedDeathsStatewide/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	select(date, deaths = count) %>%
	mutate(date = seq.Date(as.Date("2020-03-18"), as.Date("2020-03-18") + n() - 1, by = "day"),
				 new_deaths = deaths - lag(deaths))

md_statewide <- inner_join(md_statewide_cases, md_statewide_deaths, by = "date")

md_population_tested_county <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalPopulationTestedByCounty/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("2020-06-15"), "county") %>%
	mutate(date = as.Date(ifelse(str_sub(name, end = 2) == "d_", str_sub(gsub("[^0-9_]", "", name), start = 2), gsub("[^0-9_]", "", name)), "%m_%d_%y"),
				 county = gsub(" ", "_", gsub("[.']", "", tolower(county)))) %>%  # woah... I actually know how to use gsub??
	select(-name) %>%
	inner_join(md_fips, by = "county")

md_population_tested_county_today <- filter(md_population_tested_county, date == max(date)) %>%
	inner_join(pop_county) %>%
	mutate(tests_per_100k = value/population*100000)

md_daily_volume_tested_county <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_DailyTestingVolumeByCounty/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("2020-06-15"), "county") %>%
	rowwise() %>%
	mutate(date = str_split(name, "_") %>% unlist() %>% tail(3) %>% str_replace_all(., "[^[:digit:]]", "") %>% paste(., collapse = "_") %>% as.Date(., "%m_%d_%Y"),
				 county = gsub(" ", "_", gsub("[.']", "", tolower(county)))) %>%  # woah... I actually know how to use gsub??
	ungroup() %>%
	select(-name) %>%
	inner_join(md_fips, by = "county")

md_total_volume_tested_county <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_TotalTestingVolumeByCounty/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	md_api_pivot(as.Date("2020-06-15"), "county") %>%
	rowwise() %>%
	mutate(date = str_split(name, "_") %>% unlist() %>% tail(3) %>% str_replace_all(., "[^[:digit:]]", "") %>% paste(., collapse = "_") %>% as.Date(., "%m_%d_%Y"),
				 county = gsub(" ", "_", gsub("[.']", "", tolower(county)))) %>%  # woah... I actually know how to use gsub??
	ungroup() %>%
	select(-name) %>%
	inner_join(md_fips, by = "county")

md_statewide_pos_rate <- md_volume %>%
	mutate(rolling_new_cases = roll_sum(number_of_positives, 7, align = "right", fill = NA),
				 rolling_new_tests = roll_sum(number_of_tests, 7, align = "right", fill = NA),
				 rolling_posi_rate = round(rolling_new_cases/rolling_new_tests*100, 2)) %>%
	filter(!is.na(rolling_new_cases)) %>%
	select(date, rolling_posi_rate)

counties_proper_names <- data.frame(County = c("Allegany", "Anne Arundel", "Baltimore County", "Baltimore City", "Calvert", "Caroline", "Carroll", "Cecil", "Charles", "Dorchester", "Frederick", "Garrett", "Harford", "Howard", "Kent", "Montgomery", "Prince George's", "Queen Anne's", "Somerset", "St. Mary's", "Talbot", "Washington", "Wicomico", "Worcester"), county = md_counties_today$county)

md_counties_pos <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_PosPercentByJursidiction/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	mutate(report_date = seq.Date(as.Date("2020-03-30"), as.Date("2020-03-30") + dplyr::n() - 1, by = "day")) %>%
	mutate_at(vars(2:ncol(.)), ~ as.numeric(.)) %>%
	rename_at(vars(2:ncol(.)), ~ stri_replace_last_fixed(stri_replace_last_fixed(., "_", ""), "_", " ")) %>%
	pivot_longer(cols = -report_date, names_to = c("county", "type"), names_sep = " ", values_to = "percent") %>%
	mutate(percent = percent/100) %>%
	pivot_wider(names_from = type, values_from = percent) %>%
	inner_join(md_fips, by = "county") %>%
	select(date = report_date, county, fips, daily = percentpositive, average = rollingavg)

md_counties_today_table <- inner_join(md_counties_today, counties_proper_names, by = "county") %>%
	inner_join(md_population_tested_county_today, by = "county") %>%
	mutate(Deaths = deaths + prob_deaths,
				 deltas = NA) %>%
	select(County, Cases = cases, Deaths, Tests = value, deltas)

md_counties_trend_table <- inner_join(md_counties_cases, md_counties_deaths, by = c("county", "date", "fips")) %>%
	inner_join(md_counties_prob_deaths, by = c("county", "date", "fips")) %>%
	select(county, date, new_cases) %>%
  group_by(county) %>%
	mutate(rolling_avg = rollmeanr(new_cases, 7, fill = NA)) %>%
	filter(date > max(date) - 14) %>%
  ungroup() %>%
	inner_join(counties_proper_names, by = "county") %>%
	select(County, date, new_cases, rolling_avg)

md_vaccinated <- md_api("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MD_COVID19_VaccinationMetrics/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json") %>%
	pivot_wider(names_from = metric)

card_values <- data.frame(
	cases = c(slice(md_statewide_cases, n()) %>% pull(cases) %>% comma(), slice(md_statewide_cases, n()) %>% pull(new_cases) %>% comma() %>% paste0("+", .)),
	deaths = c(slice(md_statewide_deaths, n()) %>% pull(deaths) %>% comma(), slice(md_statewide_deaths, n()) %>% pull(new_deaths) %>% comma() %>% paste0("+", .)),
	hospit = c(filter(md_hospit, date == max(date), name == "Total") %>% pull(value) %>% comma(), filter(md_hospit, name == "Total") %>% mutate(new_hospit = value - lag(value)) %>% slice(n()) %>% pull(new_hospit)),
	volume = c(filter(md_total_volume_tested_county, date == max(date)) %>% pull(value) %>% sum() %>% comma(), slice(md_volume, n()) %>% pull(number_of_tests) %>% comma() %>% paste0("+", .)),
	positivity = c(slice(md_statewide_pos_rate, n()) %>% pull(rolling_posi_rate) %>% paste0(., "%"), mutate(md_statewide_pos_rate, delta_pos = round(rolling_posi_rate - lag(rolling_posi_rate), 2)) %>% slice(n()) %>% pull(delta_pos) %>% paste0(., "%")),
	vaccinated = c(md_vaccinated$TotalVaccinated %>% comma(), md_vaccinated$TotalDelta %>% comma() %>% paste0("+", .)),
	last_updated = c(gsub(" 0", " ", format(max(md_statewide_cases$date), "%B %d")), gsub(" 0", " ", format(max(md_statewide_cases$date), "%B %d"))),
	last_updated_hospit = c(paste(gsub(" 0", " ", format(max(md_hospit_county_today$collection_week), "%B %d")), gsub(" 0", " ", format(max(md_hospit_county_today$collection_week) + 6, "%B %d")), sep = " to "), paste(gsub(" 0", " ", format(max(md_hospit_county_today$collection_week), "%B %d")), gsub(" 0", " ", format(max(md_hospit_county_today$collection_week) + 6, "%B %d")), sep = " to ")),
	stringsAsFactors = F
)

card_values$hospit[2] <- ifelse(card_values$hospit[2] < 0, card_values$hospit[2], paste0("+", card_values$hospit[2]))
card_values$positivity[2] <- ifelse(card_values$positivity[2] < 0, card_values$positivity[2], paste0("+", card_values$positivity[2]))

save_dfs <- function(df)
	write_csv(get(df), paste0("data/", df, ".csv"))

dfs <- c("md_counties_cases", "md_counties_deaths", "md_counties_prob_deaths", "md_counties", "md_counties_today", "md_zips", "md_zips_today", "age_data", "sex_data", "race_data", "hospit_data", "md_negatives", "md_isolation", "md_volume", "md_ever_hospit", "md_statewide", "md_population_tested_county", "md_population_tested_county_today", "md_daily_volume_tested_county", "md_total_volume_tested_county", "md_counties_pos", "md_counties_today_table", "md_counties_trend_table", "card_values")

lapply(dfs, save_dfs)

render(here("website", "Data Table.RMD"), params = list(indic = "Cases"), output_file = here("website", "cases_table.html"))
render(here("website", "Data Table.RMD"), params = list(indic = "Deaths"), output_file = here("website", "deaths_table.html"))
render(here("website", "Data Table.RMD"), params = list(indic = "Tests"), output_file = here("website", "tests_table.html"))
