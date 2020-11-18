easypackages::libraries("tidyverse", "RcppRoll")
source("website/blog/silver_chips.R")

cases <- read_csv("data/md_counties.csv") %>%
	select(county, fips, date, new_cases)
tests <- read_csv("data/md_daily_volume_tested_county.csv") %>%
	rename(new_tests = value)

counties_proper_names <- data.frame(County = c("Allegany", "Anne Arundel", "Baltimore County", "Baltimore City", "Calvert", "Caroline", "Carroll", "Cecil", "Charles", "Dorchester", "Frederick", "Garrett", "Harford", "Howard", "Kent", "Montgomery", "Prince George's", "Queen Anne's", "Somerset", "St. Mary's", "Talbot", "Washington", "Wicomico", "Worcester"), county = cases$county)

df <- inner_join(cases, tests, by = c("county", "fips", "date")) %>%
	group_by(county, fips) %>%
	mutate(rolling_new_cases = roll_sum(new_cases, 7, align = "right", fill = NA),
				 rolling_new_tests = roll_sum(new_tests, 7, align = "right", fill = NA),
				 rolling_posi_rate = new_cases/new_tests) %>%
	ungroup() %>%
	filter(!is.na(rolling_new_cases)) %>%
	inner_join(counties_proper_names, by = "county") %>%
	select(county = County, date, rolling_posi_rate)

p <- ggplot(filter(df, date > max(date) - 14), aes(x = date, y = rolling_posi_rate)) +
	facet_wrap(~ county, ncol = 6) +
	geom_line() +
	theme_silver_chips_minimal() +
	labs(title = "Rolling seven-day positivity rate", x = "Date", y = "Seven-day moving average") +
	scale_x_date(date_labels = "%m/%d") +
	scale_y_continuous(labels = scales::percent_format(accuracy = 2))

print(p)
