

# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
# census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
# census_api_key(census_apikey, install=TRUE)

# vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT", year = 2019)


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


# get crosswalks and auxilliary data --------------------------------------
# files created below or elsewhere
tabshells <- readRDS(path(dacs, "tabshells_2019.rds"))
acscodes <- readRDS(path(dxwalks, "acs_geocodes.rds"))  


# explore table shells ----------------------------------------------------

tabs <- tabshells |> 
  select(table, tabname, universe) |> 
  distinct()
tabs

tmp <- vtab("B06008")

tname <- "B04007"
tname <- "B11001"
vtab(tname)


# function to get and save an acs 5-year table, all areas of interest ----------------------------------------------------

data(acs5_geography)
count(acs5_geography, geography)

fall <- function(table, year=2019, geom=FALSE){
  print(table)
  # default is acs 5 year, ending 2020
  nation <- get_acs(geography = "us", 
                    table = table, 
                    year=year,
                    survey="acs5",
                    state=NULL,
                    geometry = geom,
                    keep_geo_vars = TRUE,  # TRUE only works if we set geometry=TRUE
                    cache_table = TRUE) |> 
    mutate(geotype="nation")
  
  # always get all states
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
  
  # just NY county subdivisions
  cousubs <- get_acs(geography = "county subdivision", 
                    table = table, 
                    year=year,
                    survey="acs5",
                    state="NY",
                    geometry = geom,
                    keep_geo_vars = TRUE,
                    cache_table = TRUE) |> 
    mutate(geotype="cousub")
  
  # just NY places
  places <- get_acs(geography = "place", 
                   table = table, 
                   year=year,
                   survey="acs5",
                   state="NY",
                   geometry = geom,
                   keep_geo_vars = TRUE,
                   cache_table = TRUE) |> 
    mutate(geotype="place")
  
  df <- bind_rows(nation, states, counties, cousubs, places) |> 
    lcnames() |> 
    mutate(table=!!table, endyear=year, survey="acs5")# |> 
    # this keeps all variables
    #select(geoid, geotype, name, endyear, table, variable, estimate, moe, survey, everything())
  saveRDS(df, here::here("data", "acs", paste0(table, "_", year, ".rds")))
  print(nrow(df))
  df
}

# tables of interest ------------------------------------------------------
# here are tables PRB used:
# PRB Household gross rent data are from table B25070. Homeowner data are from table B25091

# based on year-ending 2019 5-year ACS

# people
# B01003	TOTAL POPULATION
# B02001	RACE
# B05001	NATIVITY AND CITIZENSHIP STATUS IN THE UNITED STATES
# B17019  POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY TENURE

# units
# B25001	HOUSING UNITS
# B25002	OCCUPANCY STATUS
# B25003	TENURE
# B25008	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE
# B25014	TENURE BY OCCUPANTS PER ROOM
# B25015	TENURE BY AGE OF HOUSEHOLDER BY OCCUPANTS PER ROOM
# B25016	TENURE BY PLUMBING FACILITIES BY OCCUPANTS PER ROOM
# B25048	PLUMBING FACILITIES FOR OCCUPIED HOUSING UNITS
# B25049	TENURE BY PLUMBING FACILITIES
# B25052	KITCHEN FACILITIES FOR OCCUPIED HOUSING UNITS
# B25053	TENURE BY KITCHEN FACILITIES

# renters
# B25070	GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25071	MEDIAN GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS (DOLLARS)
# B25072	AGE OF HOUSEHOLDER BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25074	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS

# owners
# B25091	MORTGAGE STATUS BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25093	AGE OF HOUSEHOLDER BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25095	HOUSEHOLD INCOME BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25101	MORTGAGE STATUS BY MONTHLY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25106	TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS


tabs |> 
  filter(str_detect(tabname, "PERCENTAGE"), str_sub(table, 1, 1)=="B")

tname <- "B25101"
vtab(tname)


# ONETIME: get tables of interest -----------------------------------------

gettabs <- c("B01003", "B02001", "B05001", "B17019", "B25001", "B25002", 
             "B25003", "B25008", "B25014", "B25015", "B25016", "B25048", 
             "B25049", "B25052", "B25053", "B25070", "B25071", "B25072", 
             "B25074", "B25091", "B25093", "B25095", "B25101", "B25106")

df <- map_dfr(gettabs, fall)
saveRDS(df, here::here("data", "acs", "acs5yr_osc.rds"))


# ONETIME clean and collapse tables of interest -----------------------------------
# toi <- readRDS(here::here("data", "acs", "acs5yr_osc.rds"))

## get tables of interest ----
foi <- dir_info(dacs, glob="*.rds") |> 
  select(path) |> 
  mutate(path=path_file(path)) |> 
  filter(str_sub(path, 1, 1)=="B",
         str_detect(path, "_2019"))
foi
foi$path

f <- function(fname){
  print(fname)
  readRDS(path(dacs, fname))
}
# f(foi$path[1])  
toi1 <- map_dfr(foi$path, f)
glimpse(toi1)
count(toi1, table)
count(toi1, geotype)



## put stabbr and nygeotype on file, add NY regions, save ---------
glimpse(acscodes)
count(toi1, geotype)
count(acscodes, nygeotype, geotype)
names(acscodes)
intersect(names(acscodes), names(toi1))

# check for dups within geoid of acscodes -- none
count(acscodes, geoid, geotype) |> filter(n>1) 
# check for dups within geoid of toi1 -- none
toi1 |> 
  select(geoid, geotype) |> 
  distinct() |> 
  count(geoid, geotype) |> 
  filter(n>1) 
  

toi2 <- toi1 |> 
  left_join(acscodes |> 
              select(-c(survey, endyear)),
            by=c("geoid", "geotype"))
glimpse(toi2)
count(toi2, nygeotype, geotype)
tmp <- count(toi2, geoid, geotype, nygeotype, name, shortname, fullname)
tmp |> filter(name != fullname)

tmp <- toi1 |> 
  filter(geotype %in% c("county")) |> 
  select(geoid, geotype, name) |> 
  distinct()

toi3 <- toi2 |> 
  select(-name) |> 
  relocate(geotype, nygeotype, shortname, fullname, .after=geoid)
glimpse(toi3)  


# collapse to regions of NY ----
count(toi3 |> filter(nygeotype=="county",
                     !geoid %in% nyc_counties$geoid),
      geoid, fullname) # upstate counties
count(toi3 |> filter(nygeotype=="city", geoid=="3651000"),
      geoid, fullname)

nonnyc <- expression(nygeotype=="county" & !geoid %in% nyc_counties$geoid)
nyc <- expression(nygeotype=="city" & geoid=="3651000")
keep <- expression(eval(nonnyc) | eval(nyc))

nyregions <- toi3 |> 
  filter(eval(keep)) |> 
  # do NOT include geotype, names etc. in grouping variables
  # ONLY include estimate as a summed variable (not moe)
  group_by(stabbr, stname, statefp, statens,
           rgn_num, rgn_code, rgn_osc, 
           table, variable, endyear, survey) |> 
  summarise(estimate=sum(estimate, na.rm=TRUE),
            pop=sum(pop, na.rm=TRUE),
            .groups="drop") |> 
  mutate(nygeotype="region")
summary(nyregions)
count(nyregions, stabbr, stname, rgn_num, rgn_code, rgn_osc)

# cities within regions
rgn_cities <- toi3 |> 
  filter(nygeotype=="city") |> 
  # do NOT include geotype, names etc. in grouping variables
  # ONLY include estimate as a summed variable (not moe)
  group_by(stabbr, stname, statefp, statens,
           rgn_num, rgn_code, rgn_osc, 
           table, variable, endyear, survey) |> 
  summarise(estimate=sum(estimate, na.rm=TRUE),
            pop=sum(pop, na.rm=TRUE),
            .groups="drop") |> 
  mutate(nygeotype="rgn_cities")
count(rgn_cities, stabbr, stname, rgn_num, rgn_code, rgn_osc)

rgn_villages <- toi3 |> 
  filter(nygeotype=="village") |> 
  # do NOT include geotype, names etc. in grouping variables
  # ONLY include estimate as a summed variable (not moe)
  group_by(stabbr, stname, statefp, statens,
           rgn_num, rgn_code, rgn_osc, 
           table, variable, endyear, survey) |> 
  summarise(estimate=sum(estimate, na.rm=TRUE),
            pop=sum(pop, na.rm=TRUE),
            .groups="drop") |> 
  mutate(nygeotype="rgn_villages")
count(rgn_villages, stabbr, stname, rgn_num, rgn_code, rgn_osc)

# rest of region
rgn_xcityvill <- bind_rows(nyregions, rgn_cities, rgn_villages) |>
  # get differences for both pop and estimate
  pivot_longer(cols = c(pop, estimate)) |> 
  pivot_wider(names_from = nygeotype, values_fill = 0) |> 
  mutate(rgn_xcityvill=region - rgn_cities - rgn_villages,
         nygeotype="rgn_xcityvill") |>  
  select(-c(region, rgn_cities, rgn_villages)) |> 
  pivot_wider(values_from = rgn_xcityvill)
  

stack <- bind_rows(toi3, nyregions, rgn_cities, rgn_villages, rgn_xcityvill)
glimpse(stack)  
summary(stack)
ns(stack)
count(stack, nygeotype)
saveRDS(stack, here::here("data", "acs", "acs5yr_tabdata.rds"))

acs <- readRDS(here::here("data", "acs", "acs5yr_tabdata.rds"))



vtab("B25071")
# doesn't work for B25071_001 Median gross rent as a percentage of household income
vtab("B25070")
# B25070_010 50.0 percent or more
rgn_xcityvill |> 
  filter(variable=="B25070_010") 




# NEW SECTION get selected time series ------------------------------------------------

fts <- function(year=2019, table){
  print(year)
  print(table)
  # default is acs 5 year, ending 2020
  nation <- get_acs(geography = "us", 
                    table = table, 
                    year=year,
                    survey="acs1",
                    state=NULL,
                    geometry = FALSE,
                    keep_geo_vars = FALSE,  # TRUE only works if we set geometry=TRUE
                    cache_table = TRUE) |> 
    mutate(geotype="nation")
  
  # always get all states
  states <- get_acs(geography = "state", 
                    table = table, 
                    year=year,
                    survey="acs1",
                    state=NULL,
                    geometry = FALSE,
                    keep_geo_vars = FALSE,
                    cache_table = TRUE) |> 
    mutate(geotype="state")
  
  # just get NY counties
  counties <- get_acs(geography = "county", 
                      table = table, 
                      year=year,
                      survey="acs1",
                      state="NY",
                      geometry = FALSE,
                      cache_table = TRUE) |> 
    mutate(geotype="county")

  df <- bind_rows(nation, states, counties) |> 
    lcnames() |> 
    mutate(table=!!table, endyear=year, survey="acs1") |> 
    select(geoid, geotype, name, endyear, table, variable, estimate, moe, survey)
  # saveRDS(df, here::here("data", "acs", paste0(table, "_", year, ".rds")))
  print(nrow(df))
  df
}

# PRB Household gross rent data are from table B25070. Homeowner data are from table B25091
vars |> 
  filter(table=="B25091") |> 
  select(-tabname)

# df <- fts(2006, "B25070")

table <- "B25091"
df2 <- map_dfr(2006:2019, fts, table)
glimpse(df2)
saveRDS(df2, here::here("data", "acs", paste0(table, "_timeseries.rds")))

count(df2, endyear)
count(df2, variable)


# rent burden
vars |> 
  filter(table=="B25070") |> 
  select(-tabname)

b25070ts <- readRDS(here::here("data", "acs", "B25070_timeseries.rds"))
rentcb <- b25070ts |>
  select(geoid, geotype, name, endyear, variable, estimate) |> 
  pivot_wider(names_from = variable, values_from = estimate) |> 
  mutate(rentcb30=(B25070_007 + B25070_008 + B25070_009 + B25070_010) / (B25070_001 - B25070_011),
         rentcb30b=(B25070_007 + B25070_008 + B25070_009 + B25070_010) / B25070_001)

rentcb |> 
  select(geoid, geotype, name, endyear, rentcb30, rentcb30b) |> 
  filter(geotype=="state", geoid=="36") 

rentcb |> 
  select(geoid, geotype, name, endyear, rentcb30) |> 
  filter(geotype=="state", geoid=="36") |> 
  ggplot(aes(endyear, rentcb30)) +
  geom_line() +
  geom_point()

# owner with mortgage cost burden
vars |> 
  filter(table=="B25091") |> 
  select(-tabname)
# 8-11 / 2-12
b25091ts <- readRDS(here::here("data", "acs", "B25091_timeseries.rds"))
glimpse(b25091ts)
ownmcb <- b25091ts |>
  select(geoid, geotype, name, endyear, variable, estimate) |> 
  pivot_wider(names_from = variable, values_from = estimate) |> 
  mutate(ownmcb30=(B25091_008 + B25091_009 + B25091_010 + B25091_011) / (B25091_002 - B25091_012))
glimpse(ownmcb)

ownmcb |> 
  select(geoid, geotype, name, endyear, ownmcb30) |> 
  filter(geotype=="state", geoid=="36") |> 
  ggplot(aes(endyear, ownmcb30)) +
  geom_line() +
  geom_point()

cb <- bind_rows(rentcb |> 
                  select(geoid, geotype, name, endyear, value=rentcb30) |> 
                  mutate(tenure="renter"),
                ownmcb |> 
                  select(geoid, geotype, name, endyear, value=ownmcb30) |> 
                  mutate(tenure="owner"))

cb |> 
  filter(geotype=="state", geoid=="36") |> 
  ggplot(aes(endyear, value, colour=tenure)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0, NA))

cb |> 
  filter((geotype=="nation") |
           (geotype=="state" & geoid=="36")) |> 
  ggplot(aes(endyear, value, colour=geotype)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0, NA)) +
  facet_wrap(~tenure)
  

# OLDER below here ----

# check NY subtypes -------------------------------------------------------
df <- fall("B01003", geom=TRUE) # get a table with only one variable, for exploring names

names <- df |> 
  select(geotype, geoid, name) |> 
  distinct()

names |> filter(geotype=="county") |> arrange(name) # 62 counties (incl NYC)
# 36005 Bronx County, New York
# 36047 Kings County, New York
# 36061 New York County, New York
# 36081 Queens County, New York
# 36085 Richmond County, New York

names |> 
  filter(geotype == "cousub") |> # about 1023 cousubs
  filter(str_detect(name, "city")) |> arrange(name)
# 61 cities in cousubs -- 2 Genevas (one is not real), no NYC, no Sherrill

names |> 
  filter(type == "place") |> # about 1023 cousubs
  filter(str_detect(name, "city")) |> arrange(name)
# 62 cities in places - includes Sherrill, 1 Geneva, 1 NYC -- so maybe only use places for cities

df |> 
  filter(str_detect(name, "New York city"))

df |> 
  filter(!str_detect(name, "town")) |> 
  filter(str_detect(name, "borough") |
           geoid %in% c("36005", "36047", "36061", "36081", "36085")) |>
  arrange(estimate)
# good, boroughs and NY counties match

# 36005 Bronx County, New York
# 36047 Kings County, New York
# 36061 New York County, New York
# 36081 Queens County, New York
# 36085 Richmond County, New York

df |> 
  filter(str_detect(name, "Geneva city"))

df |> 
  filter(str_detect(name, "Sherrill city"))

df |> 
  filter(str_detect(name, "New York city"))

# what are the county subdivisions that aren't cities?
names |> 
  filter(type == "cousub") |> # about 1023 cousubs
  filter(!str_detect(name, "city")) |> 
  mutate(subtype=case_when(str_detect(name, "town") ~ "town",
                           str_detect(name, "Reservation") ~ "reservation",
                           str_detect(name, "borough") ~ "borough",
                           str_detect(name, "not defined") ~ "undefined",
                           TRUE ~ "other")) |> 
  # filter(subtype=="other") # Chautauqua Lake UT
  count(subtype)
# 933 towns

# what are the places that aren't cities?
names |> 
  filter(type == "place") |> 
  filter(!str_detect(name, "city")) |> # about 1033 places that aren't cities
  mutate(subtype=case_when(str_detect(name, "village") ~ "village",
                           str_detect(name, "CDP") ~ "CDP",
                           TRUE ~ "other")) |> 
  count(subtype)
# 538 villages
# 595 CDPs


# create geotypes xwalk ---------------------------------------------------
cnty_xwalk <- read_excel(here::here("data", "xwalks", "nycounty_xwalk_data.xlsx"),
                         sheet="region_codes",
                         col_types="text")

xwalk
names2 <- names |> 
  mutate(nygeotype=case_when(
    geotype %in% c("nation", "state", "county") ~ geotype,
    
    # cousubs
    geotype=="cousub" & str_detect(name, "town") ~ "town",
    geotype=="cousub" & str_detect(name, "Reservation") ~ "reservation",
    geotype=="cousub" & str_detect(name, "borough") ~ "borough",
    geotype=="cousub" & str_detect(name, "city") ~ "city_cousub",
    geotype=="cousub" & str_detect(name, "not defined") ~ "undefined_cousub",
    
    # caution: places don't have a county assigned to them
    geotype=="place" & str_detect(name, "city") ~ "city",
    geotype=="place" & str_detect(name, "village") ~ "village",
    geotype=="place" & str_detect(name, "CDP") ~ "CDP",
    
    TRUE ~ "other")) # Chautauqua Lake UT, Chautauqua County, New York

count(names2, nygeotype, geotype)

names2 |> filter(nygeotype=="undefined_cousub")
names2 |> filter(nygeotype=="other")

