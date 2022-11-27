# https://data-downloads.evictionlab.org/#estimating-eviction-prevalance-across-us/

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
evdir <- here::here("data", "evictions")

evstmo_path <- path(evdir, "allstates_monthly_2020_2021.csv")
evsitesmo_path <- path(evdir, "all_sites_monthly_2020_2021.csv")

# county_eviction_estimates_2000_2018.csv
evcnty_path <- path(evdir, "county_eviction_estimates_2000_2018.csv")

# get data ----------------------------------------------------------------
## County evictions 2000-2018
evcnty <- read_csv(evcnty_path)
glimpse(evcnty)
summary(evcnty)
count(evcnty, state)
evcnty2 <- evcnty |> 
  mutate(stabbr=factor(state, 
                       levels=c(state.name, "District Of Columbia"),
                       labels=c(state.abb, "DC")),
         across(starts_with("ind_"), as.logical))
glimpse(evcnty2)
count(evcnty2, stabbr, state)
saveRDS(evcnty2, path(evdir, "evict_counties.rds"))


# create a ny subset ------------------------------------------------------
evict <- readRDS(path(evdir, "evict_counties.rds"))

evict2 <- evict |> 
  filter(stabbr=="NY") |> 
  select(-state, -FIPS_state) |> 
  select(stabbr, geoid=FIPS_county, county, year, everything())
summary(evict2)

evict2 |> filter(ind_hht_observed) |> pull(county) |> unique()

glimpse(evict2)

rates <- evict2 |> 
  mutate(evictrate=filings_estimate / renting_hh,
         threatrate=hh_threat_estimate / renting_hh,
         evlower=filings_ci_95_lower / renting_hh,
         evupper=filings_ci_95_upper / renting_hh,
         threatlower=hh_threat_95_lower / renting_hh,
         threatupper=hh_threat_95_upper / renting_hh) |> 
  select(geoid, county, year, renting_hh, 
         evictrate, evlower, evupper,
         threatrate, threatlower, threatupper)
rates |> 
  filter(year==2018, str_detect_any(county, c("Bronx", "Fulton")))


# DATA BELOW HERE DO NOT HAVE WHAT WE NEED --------------------------------

## monthly state data - does not have what we want ----
evstmo <- read_csv(evstmo_path,
               col_types = cols(month = col_date(format = "%m/%Y"), 
                                last_updated = col_date(format = "%Y-%m-%d"))) |> 
  rename(date=month)
glimpse(evstmo)
summary(evstmo)
count(evstmo, date) # Jan 2020 - Aug 2022
count(evstmo, state) # does NOT have NY

## monthly sites data - does not have what we want ----
evsitesmo <- read_csv(evsitesmo_path,
                   col_types = cols(month = col_date(format = "%m/%Y"), 
                                    last_updated = col_date(format = "%Y-%m-%d"))) |> 
  rename(date=month)
count(evsitesmo, city)


