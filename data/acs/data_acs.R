


# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
# census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
# census_api_key(census_apikey, install=TRUE)

# vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT", year = 2019)


# constants ---------------------------------------------------------------
dacs <- here::here("data", "acs")

nyc_counties <- read_delim(
"geoid;borough
36005;Bronx County, New York
36047;Kings County, New York
36061;New York County, New York
36081;Queens County, New York
36085;Richmond County, New York
", delim=";", col_types = cols(.default = col_character()))
nyc_counties


# ONETIME get save table shells ---------------------------------------------------
urlbase <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2019/"
url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2019/B25072.xlsx"
# read_excel(path=url) # cannot read from url

library(httr)
library(XML)

flist <- readHTMLTable(content(GET(urlbase), "text"))[[1]] |> 
  select(-1) |> 
  as_tibble() |> 
  lcnames() |> 
  filter(!is.na(name), name != "Parent Directory") |> 
  select(name, modified=`last modified`, size)
glimpse(flist)
flist

urllist <- flist |> 
  filter(str_sub(name, 1, 1)=="B") |> 
  mutate(name=paste0(urlbase, name)) |> 
  pull(name)

f <- function(url){
  print(url)
  # read_excel won't read directly from url so write to temporary file
  httr::GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_excel(tf, sheet=1, col_types="text")
}

df1 <- map_dfr(urllist, f) # get ALL of the table shells for 2019
glimpse(df1)

df2 <- df1 |> 
  setNames(c("table", "line", "description")) |>
  group_by(table) |> 
  mutate(tabname=description[1],
         universe=description[2]) |> 
  filter(row_number() > 2) |> 
  mutate(group=ifelse(str_sub(description, -1, -1)==":",
                      str_sub(description, 1, -2) |> str_to_lower(),
                      NA_character_),
         nline=as.integer(line)) |> 
  fill(group, .direction="down") |> 
  ungroup() |> 
  filter(nline > 0) |> # get rid of the oddballs that have noninteger values??
  mutate(vname=paste0(table, "_", str_pad(line, width=3, side="left", pad="0")),
         universe=str_remove(universe, "Universe: ")) |> 
  select(table, line=nline, vname, description, group, tabname, universe)

df2 |> 
  filter(is.na(group))

saveRDS(df2, path(dacs, "tabshells_2019.rds"))



# explore table shells ----------------------------------------------------
tabshells <- readRDS(path(dacs, "tabshells_2019.rds"))

tabs <- tabshells |> 
  select(table, tabname, universe) |> 
  distinct()

vtab <- function(table) {
  # view table
  tab <- tabshells |> filter(table==!!table)
  list(tab=tab |> select(-tabname, -universe),
       table=table,
       tabname=tab$tabname[1],
       universe=tab$universe[1]
       )
}

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
cnty_xwalk <- read_excel(here::here("data", "crosswalks", "nycounty_xwalk_data.xlsx"),
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
vars |> 
  filter(table==tname) |> 
  select(-tabname)


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

## construct NY region data  ----
# xwalk <- readRDS(here::here("data", "crosswalks", "nycounty_xwalk.rds"))



## put stabbr and nygeotype on file, add NY regions, save ---------
count(toi1, geotype)
toi2 <- toi1 |> 
  mutate(nygeotype=case_when(geotype %in% c("nation", "state", "county") ~ geotype,
                             geotype=="cousub" & str_detect(name, "town") ~ "town",
                             geotype=="cousub" & str_detect(name, "Reservation") ~ "reservation",
                             geotype=="cousub" & str_detect(name, "borough") ~ "borough",
                             geotype=="cousub" & str_detect(name, "not defined") ~ "undefined_cousub",
                             geotype=="place" & str_detect(name, "city") ~ "city",
                             geotype=="place" & str_detect(name, "village") ~ "village",
                             TRUE ~ "other"))
count(toi2, nygeotype, geotype)


tmp <- toi1 |> 
  filter(geotype %in% c("county")
  select(geoid, geotype, name) |> 
  distinct()


# get regions of NY






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
  

df2 <- df1 |> 
  # as_tibble() |> 
  setNames(c("table", "line", "description")) |>
  # mutate(across(c(table, description), str_trim)) |> 
  group_by(table) |> 
  mutate(tabname=description[1],
         universe=description[2],
         group=ifelse(str_sub(description, -1, -1)==":",
                      str_sub(description, 1, -2) |> str_to_lower(),
                      NA_character_)) |> 
  fill(group, .direction="down") |> 
  ungroup() |> 
  filter(!is.na(line)) |> 
  mutate(vname=ifelse(line==round(line), # is value of line an integer?
                      paste0(table, "_", str_pad(line, width=3, side="left", pad="0")),
                      NA_character_)) |> 
  relocate(vname, .after=table) |> 
  relocate(group, .after=description)