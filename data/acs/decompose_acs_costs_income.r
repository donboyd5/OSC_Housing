# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
# census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
# census_api_key(census_apikey, install=TRUE)

# vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT", year = 2019)

# these libraries are key
# library(censusapi)
# library(tidycensus)


# constants ---------------------------------------------------------------
dacs <- here::here("data", "acs")
dxwalks <- here::here("data", "xwalks")

nyc_counties <- read_delim(
  "geoid;borough
36005;Bronx County, New York
36047;Kings County, New York
36061;New York County, New York
36081;Queens County, New York
36085;Richmond County, New York
", delim=";", col_types = cols(.default = col_character()))
nyc_counties


# identify data ----
# https://censusreporter.org/data/table/?table=B25104&geo_ids=16000US3601000&primary_geo_id=16000US3601000#valueType|estimate
# table b25104 has housing costs by size

# B25118 tenure by income
# https://censusreporter.org/tables/B25118/

# B25119 median income by tenure
# https://censusreporter.org/tables/B25119/

# Table B25105: Median Monthly Housing Costs (Dollars)

# B25064: Median Gross Rent (Dollars)

# Table B25088: Median Selected Monthly Owner Costs (Dollars) by Mortgage Status

# get data ----

v2019 <- load_variables(2019, "acs5", cache = TRUE)

v2019 |> 
  filter(str_detect(name, "B25105"))

v2019 |> 
  filter(str_detect(name, "B25119"))

tabs <- c("B25119", # income by tenure
          "B25064", # gross rent
          "B25088", # owner costs by mortgage
          "B25105" # overall costs
          )

vars1 <- v2019 |> 
  filter(str_detect_any(name, tabs))
vars2 <- vars1 |> 
  mutate(tenure=case_when(
    str_detect_any(label, c("Own", "own")) ~ "owner",
    str_detect_any(label, c("Rent", "rent")) ~ "renter",
    TRUE ~ "all"),
    label=str_remove(label, "Estimate!!"),
    vname=case_when(str_detect(label, "gross rent") ~ "cost_renter",
                    str_detect(label, "with a mort") ~ "ocosts_mort",
                    str_detect(label, "without a mort") ~ "ocosts_nomort",
                    str_detect(label, "owner") &
                      str_detect(label, "!!Total") ~ "ocosts_all",
                    str_detect(label, "Median monthly housing costs") ~ "totcosts_all",
                    str_detect(label, "Median household income") &
                      tenure=="all" ~ "mhhi_all",
                    str_detect(label, "Median household income") &
                      tenure=="owner" ~ "mhhi_owner",
                    str_detect(label, "Median household income") &
                      tenure=="renter" ~ "mhhi_renter",
                    TRUE ~ "ERROR")) |> 
  select(variable=name, tenure, vname, label)
vars2

f <- function(table){
  state <- get_acs(geography = "state", 
                   table = table, 
                   year=2019,
                   survey="acs5",
                   state="NY",
                   geometry = FALSE,
                   keep_geo_vars = TRUE,
                   cache_table = TRUE) |> 
    mutate(geotype="state")
  county <- get_acs(geography = "county", 
                    table = table, 
                    year=2019,
                    survey="acs5",
                    state="NY",
                    geometry = FALSE,
                    keep_geo_vars = TRUE,
                    cache_table = TRUE) |> 
    mutate(geotype="county")
  bind_rows(state, county)
}


df1 <- map_dfr(tabs, f)

count(df1, variable)

df2 <- df1 |> 
  left_join(vars2, by = join_by(variable))

df3 <- df2 |> 
  lcnames() |> 
  select(geoid, name, geotype, variable, vname, estimate) |> 
  mutate(index=estimate / estimate[geoid=="36"],
         name=str_remove(name, " County, New York"),
         name=case_when(geotype=="state" & name=="New York" ~ "New York State",
                        geotype=="county" & name=="New York" ~ "Manhattan",
                        name=="Kings" ~ "Brooklyn",
                        name=="Richmond" ~ "Staten Island",
                        TRUE ~ name),
         .by = c(variable))
saveRDS(df3, here::here("data", "acs", "cost_vs_income.rds"))



# get data ----------------------------------------------------------------

costinc <- readRDS(here::here("data", "acs", "cost_vs_income.rds"))

costinc |> 
  filter(name=="Albany")

costinc |> 
  filter(name=="New York State")


costinc |> 
  filter(name=="Washington")

capt1 <- "Red dots have median costs as % of median income above statewide average; green dots are below"
capt2 <- "Large dots have greater cost burden than small dots"
capt3 <- "Source: ACS 5-year estimates, ending 2019"
capt <- paste0(capt1, "\n", capt2, "\n", capt3)

prent <- costinc |> 
  select(-variable, -estimate) |> 
  pivot_wider(names_from = vname, values_from = index) |>
  mutate(cb=cost_renter / mhhi_renter,
         cbval=ifelse(cb < 1, "low", "high")) |> 
  mutate(across(c(mhhi_renter, cost_renter), ~.x - 1)) |> 
  filter(geoid != "36") |> 
  ggplot(aes(x=mhhi_renter, y=cost_renter)) +
  # geom_point(colour="blue", aes(size=cb)) +
  geom_point(aes(size=cb, colour=cbval)) +
  scale_colour_manual(values=c("red", "darkgreen")) +
  geom_text(aes(label=name), colour="black", size=1.5, nudge_y=.015) +
  scale_y_continuous(name="Median renter costs % above or below statewide average",
                     breaks=seq(-1, 1, .1),
                     labels = scales::label_percent(accuracy=1)) +
  scale_x_continuous(name="Median income % above or below statewide average",
                     breaks=seq(-1, 1, .1),
                     labels = scales::label_percent(accuracy=1)) + 
  scale_size_continuous(range = c(0.1, 1.5)) +
  geom_abline(slope=1, intercept=0, linewidth=.35, linetype="dotted", colour="grey5") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  legend_none +
  labs(caption=capt) +
  caption_left +
  ggtitle("Renter housing costs and median household income relative to the statewide average")
prent  

ggsave(here::here("report", "results", "cost_vs_income.jpg"), prent, width=10, height=6, scale=1.25)

ggsave(here::here("report", "results", "cost_vs_income.jpg"), prent, width=7.5, height=4.5, scale=1.25)



# additional views --------------------------------------------------------


ptot <- costinc |> 
  select(-variable, -estimate) |> 
  pivot_wider(names_from = vname, values_from = index) |> 
  ggplot(aes(x=mhhi_all, y=totcosts_all)) +
  geom_point(colour="blue", size=1.25) +
  geom_text(aes(label=name), colour="blue", size=2) +
  geom_abline(slope=1, intercept=0) +
  theme_bw()
ptot  



# prent + scale_size_continuous(range = c(0.5, 2))

# get a rent table

df3 |> 
  filter(vname %in% c("mhhi_renter", "grent_all")) |> 
  select(geoid, name, vname, index) |> 
  pivot_wider(names_from = vname, values_from = index) |> 
  rename(cost_renter=grent_all) |> 
  mutate(cb=cost_renter / mhhi_renter) |> 
  arrange(desc(cb)) |> 
  mutate(across(-c(geoid, name), ~ .x - 1)) |> 
  select(geoid, name, cb, cost_renter, mhhi_renter)




# OLD BELOW HERE ----


stincome <- get_acs(geography = "state", 
                              table = "B25118", 
                              year=2019,
                              survey="acs5",
                              state="NY",
                              geometry = FALSE,
                              keep_geo_vars = TRUE,
                              cache_table = TRUE) |> 
  mutate(geotype="state")

coincome <- get_acs(geography = "county", 
                    table = "B25118", 
                    year=2019,
                    survey="acs5",
                    state="NY",
                    geometry = FALSE,
                    keep_geo_vars = TRUE,
                    cache_table = TRUE) |> 
  mutate(geotype="county")

inc1 <- bind_rows(stincome |> filter(GEOID=="36"),
                  coincome)

vars <- v2019 |> 
  filter(str_detect(name, "B25118")) |> 
  select(variable=name, label) |> 
  mutate(tenure=case_when(str_detect(label, "Renter") ~ "renter",
                          str_detect(label, "Owner") ~ "owner",
                          TRUE ~ "all"))
                          
                          ),
         label=str_remove(label, "Estimate!!"))

inc2 <- inc1 |> 
  left_join(),
            by = join_by(variable))
  


inc2 <- inc1 |> 
  select(-moe) |> 
  pivot_wider(names_from=variable,
              values_from=estimate)









states <- get_acs(geography = "state", 
                  table = table, 
                  year=year,
                  survey="acs5",
                  state=NULL,
                  geometry = geom,
                  keep_geo_vars = TRUE,
                  cache_table = TRUE) |> 
  mutate(geotype="state")

# just get NY counties
counties <- get_acs(geography = "county", 
                    table = table, 
                    year=year,
                    survey="acs5",
                    state="NY",
                    geometry = geom,
                    keep_geo_vars = TRUE,
                    cache_table = TRUE) |> 
  mutate(geotype="county")




