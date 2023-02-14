# https://evictionlab.org/
# https://data-downloads.evictionlab.org/#estimating-eviction-prevalance-across-us/


# citations ---------------------------------------------------------------

# Any works citing Eviction Lab data from the directories /data-for-analysis or
# /estimating-eviction-prevalance-across-us/ should use the following citation:

# Gromis, Ashley, Ian Fellows, James R. Hendrickson, Lavar Edmonds, Lillian
# Leung, Adam Porton, and Matthew Desmond. Estimating Eviction Prevalence across
# the United States. Princeton University Eviction Lab.
# https://data-downloads.evictionlab.org/#estimating-eviction-prevalance-across-us/.
# Deposited May 13, 2022.

# Any works citing Eviction Tracking System data from the directory /ets
# should use the following citation: 

# Peter Hepburn, Renee Louis, and Matthew Desmond. Eviction Tracking System:
# Version 1.0. Princeton: Princeton University, 2020. www.evictionlab.org.

# Any works citing Eviction Lab data from the directory /legacy-data should use the following citation:

#   Desmond, Matthew, Ashley Gromis, Lavar Edmonds, James Hendrickson, Katie
#   Krywokulski, Lillian Leung, and Adam Porton. "Eviction lab national
#   database: Version 1.0." https://data-downloads.evictionlab.org/#legacy-data

# Any works citing Eviction Lab data from the directory
# /demographics-of-eviction should use the following citation:

#   Hepburn, Peter, Renee Louis, and Matthew Desmond. "Racial and gender
#   disparities among evicted Americans." Sociological Science 7 (2020):
#   649-662. https://sociologicalscience.com/articles-v7-27-649/

# Any works citing Eviction Lab data from the directory /eviction-from-public-housing should use the following citation:

#   Gromis, A., Hendrickson, J. R., & Desmond, M. (2022). "Eviction from public
#   housing in the United States." Cities, 127, 103749.
#   https://doi.org/10.1016/j.cities.2022.103749


# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))


# local directories ---------------------------------------------------------------
evdir <- here::here("data", "evictions")
xdir <- here::here("data", "xwalks")


# get geo crosswalks to NY regions ----------------------------------------
# acs_geocodes.rds

xwalk <- readRDS(path(xdir, "acs_geocodes.rds"))
cntyxwalk <- xwalk |> 
  filter(nygeotype=="county") |> 
  select(geoid, shortname, rgn_num, rgn_code, rgn_osc, pop)




# urls --------------------------------------------------------------------
uspropurl <- "https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/us_proprietary_2000_2018.csv"
copropurl <- "https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/county_proprietary_2000_2018.csv"
placepropurl <- "https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/place_proprietary_2000_2018.csv"


usurl <- "https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/national_eviction_estimates_2000_2018.csv"
sturl <- "https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/state_eviction_estimates_2000_2018.csv"
courl <- "https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/county_eviction_estimates_2000_2018.csv"

urls <- c(uspropurl, usurl, sturl, courl, copropurl, placepropurl)

# download and save data --------------------------------------------------
f <- function(url){
  fname <- path_file(url)
  print(fname)
  download.file(url, destfile = path(evdir, fname), mode="wb")
}
f(uspropurl)

purrr::walk(urls, f)


# get data ----------------------------------------------------------------

# varnames:
# us  id,name,parent_location,year,type,filings,filing_rate,threatened,threatened_rate,judgements,judgement_rate
# st state,FIPS_state,year,renting_hh,filings_estimate,filings_ci_95_lower,filings_ci_95_upper,ind_filings_court_issued,ind_filings_court_issued_LT,hh_threat_estimate,hh_threat_95_lower,hh_threat_95_upper
# co state,county,FIPS_state,FIPS_county,year,renting_hh,filings_estimate,filings_ci_95_lower,filings_ci_95_upper,ind_filings_court_issued,ind_filings_court_issued_LT,hh_threat_estimate,hh_threat_95_lower,hh_threat_95_upper,ind_hht_observed
# coprop id,name,parent_location,year,type,filings,filing_rate,threatened,threatened_rate,judgements,judgement_rate
# placeprop id,name,parent_location,year,type,filings,filing_rate,threatened,threatened_rate,judgements,judgement_rate

# key variables

urls
url <- urls[1]
url <- urls[2]
library(bdata)

f3 <- function(url=NULL){
  fname <- path_file(url)
  print(fname)
  
  nygeotype <- case_when(str_starts(fname, "state_") ~ "state",
                         str_starts(fname, "county_") ~ "county",
                         str_starts(fname, "national_") ~ "nation",
                         str_starts(fname, "us_") ~ "nation",
                         str_starts(fname, "place_") ~ "place")
  
  esttype <- ifelse(str_detect(fname, "proprietary"), "proprietary", "model")
  
  print(nygeotype); print(esttype)
  
  fpath <- path(evdir, fname)
  df <- read_csv(fpath, col_types = cols(.default = col_character()))
  df |> 
    mutate(nygeotype=nygeotype,
           esttype=esttype,
           year=as.integer(year))
}

usdf <- f3(usurl) |>
  mutate(stabbr="US", stfips="00", areaname="United States") |> 
  select(stabbr, stfips, areaname, nygeotype, esttype, year, everything())

stdf <- f3(url=sturl) |> 
  rename(areaname=state, 
         stfips=FIPS_state) |> 
  left_join(stcodes |> select(stfips, stabbr), by = join_by(stfips)) |>
  select(stabbr, stfips, areaname, nygeotype, esttype, year, everything())

codf <- f3(url=courl) |> 
  rename(areaname=county, 
         stfips=FIPS_state) |> 
  mutate(cntyfips=str_sub(FIPS_county, 3, 5)) |> 
  left_join(stcodes |> select(stfips, stabbr), by = join_by(stfips)) |>  
  select(-FIPS_county, -state) |> 
  select(stabbr, stfips, cntyfips, areaname, nygeotype, esttype, year, everything())

placedf1 <- f3(url=placepropurl) |> 
  rename(areaname=name) |> 
  mutate(stfips=str_sub(id, 1, 2))
count(placedf1, stfips, parent_location)

placedf <- placedf1 |> 
  left_join(stcodes |> select(stfips, stabbr), by = join_by(stfips)) |>  
  select(-parent_location, -id) |> 
  mutate(cntyfips=NA_character_) |>  # maybe later fill this in
  select(stabbr, stfips, cntyfips, areaname, nygeotype, esttype, type, year, everything())
  
ns(usdf)
ns(stdf)
ns(codf)
ns(placedf)

# idvars <- expression(c(stabbr, stfips, cntyfips, areaname, nygeotype, esttype, year))
idvars <- c("stabbr", "stfips", "cntyfips", "areaname", "nygeotype", "esttype", "year")
evict <- bind_rows(usdf, stdf, codf) |> 
  select(any_of(idvars), everything()) |> 
  mutate(across(.cols=-any_of(idvars), as.numeric),
         geoid=ifelse(nygeotype=="county",
                      paste0(stfips, cntyfips),
                      NA_character_)) |> 
  left_join(cntyxwalk, by = join_by(geoid)) |> 
  mutate(shortname=case_when(is.na(shortname) ~ areaname,
                             nygeotype=="state" & stabbr=="NY" ~ "New York State",
                             nygeotype=="nation" ~ "United States",
                             TRUE ~ shortname),
         rgn_osc=case_when(nygeotype=="state" & stabbr=="NY" ~ "New York State",
                           nygeotype=="nation" ~ "United States",
                           TRUE ~ rgn_osc))
  
glimpse(evict)
summary(evict)

saveRDS(evict, path(evdir, "evict.rds"))


# explore -----------------------------------------------------------------
evict <- readRDS(path(evdir, "evict.rds"))
glimpse(evict)

df <- evict |> 
  filter(nygeotype %in% c("nation", "state")) |> 
  mutate(frate=filings_estimate / renting_hh)

df |> 
  filter(year==2018) |> 
  select(stabbr, nygeotype, frate) |> 
  arrange(desc(frate))

df <- evict |> 
  filter(nygeotype %in% c("nation", "state", "county"),
         stabbr %in% c("US", "NY")) |> 
  mutate(frate=filings_estimate / renting_hh)

df |> 
  filter(year==2018) |> 
  select(stabbr, areaname, shortname, nygeotype, frate, rgn_osc) |> 
  arrange(desc(frate))



tmp <- df |> 
  filter(stabbr=="NY", year==2018) |> 
  mutate(frate=filings_estimate / renting_hh)



# OLD ----
df <- map_dfr(urls, f2)
glimpse(df)
count(df, src)
count(df, state)

tmp <- count(df, src, state)
tmp |> 
  filter(is.na(state))

# state NA for us, county, place proprietary
# county only avail for county nonprop
# id in proprietary looks like 5-character fips
count(df, county, src) |> 
  mutate(src=str_extract_before_first(src, "2000")) |> 
  pivot_wider(names_from = src, values_from = n)

count(df, year, src) |> 
  mutate(src=str_extract_before_first(src, "2000")) |> 
  pivot_wider(names_from = src, values_from = n)

# finish and save the long file
df2 <- df |> 
  mutate(nygeotype=case_when(str_starts(src, "state_") ~ "state",
                             str_starts(src, "county_") ~ "county",
                             str_starts(src, "us_") ~ "nation",
                             str_starts(src, "place_") ~ "place"),
         esttype=ifelse(str_detect(src, "proprietary"), "proprietary", "model"),
         value=as.numeric(value)) |> 
  filter(!is.na(value))
saveRDS(df2, path(evdir, "evict_long.rds"))


# explore -----------------------------------------------------------------
evict <- readRDS(path(evdir, "evict_long.rds"))
glimpse(evict)

df <- evict |> 
  filter(nygeotype %in% c("nation", "state"))
count(df, type)
count(df, vname)
count(df, name)
count(df, state)
count(df, county)


# OLD below here ----------------------------------------------------------

# local locations
evstmo_path <- path(evdir, "allstates_monthly_2020_2021.csv")
evsitesmo_path <- path(evdir, "all_sites_monthly_2020_2021.csv")

# county_eviction_estimates_2000_2018.csv
evcnty_path <- path(evdir, "county_eviction_estimates_2000_2018.csv")

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


