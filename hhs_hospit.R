easypackages::libraries("tidyverse", "jsonify", "janitor", "zoo", "scales", "htmltools", "DT", "here", "sparkline", "rmarkdown", "RcppRoll", "stringi")

md_fips <- read_csv("md_fips.csv") %>%
	mutate(fips = as.character(fips)) %>%
	filter(!is.na(fips))

# IMPORTANT: NOT UPLOADING THE FILE TO GITHUB SINCE IT'S TOO BIG
national_hospit <- read_csv("/Users/Prayag/Downloads/reported_hospital_capacity_admissions_facility_level_weekly_average_timeseries_20210221.csv")

md_hospit_granular <- filter(national_hospit, state == "MD") %>%
	mutate(collection_week = as.Date(collection_week))

md_hospit_granular_today <- filter(md_hospit_granular, collection_week == max(collection_week))

md_hospit_county <- mutate_all(md_hospit_granular, ~na_if(., -999999)) %>%
	filter(!(xor(is.na(all_adult_hospital_inpatient_beds_7_day_avg), is.na(all_adult_hospital_inpatient_bed_occupied_7_day_avg))),
				 !(xor(is.na(total_staffed_adult_icu_beds_7_day_avg), is.na(staffed_adult_icu_bed_occupancy_7_day_avg)))) %>%
	group_by(collection_week, fips_code) %>%
	summarize(inpatient_beds = sum(all_adult_hospital_inpatient_beds_7_day_avg, na.rm = T),
						inpatient_beds_occupied = sum(all_adult_hospital_inpatient_bed_occupied_7_day_avg, na.rm = T),
						icu_beds = sum(total_staffed_adult_icu_beds_7_day_avg, na.rm = T),
						icu_beds_occupied = sum(staffed_adult_icu_bed_occupancy_7_day_avg, na.rm = T)) %>%
	ungroup() %>%
	mutate(inpatient_percent_occupied = inpatient_beds_occupied/inpatient_beds,
				 icu_percent_occupied = icu_beds_occupied/icu_beds,
				 fips = fips_code) %>%
	mutate_at(vars(inpatient_percent_occupied, icu_percent_occupied), ~ replace(., is.nan(.), NA)) %>%
	group_by(collection_week, fips_code) %>%
	full_join(md_fips, by = "fips") %>%
	ungroup() %>%
	complete(fips, collection_week) %>%
	inner_join(md_fips, by = "fips") %>%
	filter(!is.na(collection_week)) %>%
	arrange(collection_week, fips) %>%
	select(county = county.y, fips, collection_week, inpatient_beds, inpatient_beds_occupied, inpatient_percent_occupied, icu_beds, icu_beds_occupied, icu_percent_occupied)

write_csv(md_hospit_county, "data/md_hospit_county.csv")
write_csv(filter(md_hospit_county, collection_week == max(collection_week)), "data/md_hospit_county_today.csv")
