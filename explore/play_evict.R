

# notes -------------------------------------------------------------------

# renting_hh	numeric	number of renting households
# filings_estimate	numeric	estimated number of filings
# filings_ci_95_lower	numeric	lower bound of 95% credible interval for filings
# filings_ci_95_upper	numeric	upper bound of 95% credible interval for filings
# ind_filings_court_issued	binary	indicator: number of filings observed in court-issued data
# ind_filings_court_issued_LT	binary	indicator: court-issued data are landlord-tenant cases
# hh_threat_estimate	numeric	estimated number of households threatened with eviction
# hh_threat_95_lower	numeric	lower bound of 95% credible interval for households threatened with eviction
# hh_threat_95_upper	numeric	upper bound of 95% credible interval for households threatened with eviction
# ind_hht_observed	binary	indicator: number of households threated with eviction observed in data



# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
evdir <- here::here("data", "evictions")


# explore data ------------------------------------------------------------

evict <- readRDS(path(evdir, "evict_counties.rds"))
glimpse(evict)

evict |> 
  filter(stabbr=="NY", year==2018) |> 
  mutate(evrate=filings_estimate / renting_hh,
         ci95_low=evrate * filings_ci_95_lower / filings_estimate,
         ci95_high=evrate * filings_ci_95_upper / filings_estimate) |> 
  arrange(desc(evrate)) |> 
  select(county, filings_estimate, renting_hh, evrate, ci95_low, ci95_high)
  
