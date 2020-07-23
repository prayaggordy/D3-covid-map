library(tidyverse)
library(tidycensus)

census_api_key("d6b1d5d489450347f14ec629d49e1b792a9b5cae")

md_population_county <- get_acs(geography = "county", variables = c(population = "B01001_001"), state = "MD", year = 2018) %>%
	select(fips = GEOID, county = NAME, population = estimate) %>%
	mutate(fips = as.numeric(fips)) %>%
	inner_join(md_fips, by = "fips") %>%
	select(fips, county = county.y, population)

write_csv(md_population_county, "population/county.csv")

md_population_zip <- get_acs(geography = "zcta", variables = c(population = "B01001_001"), year = 2018) %>%
	select(zip = GEOID, population = estimate)

write_csv(md_population_zip, "population/zip.csv")