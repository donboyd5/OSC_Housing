
# TODO: ----
# when nygeotype=undefined_cousub
#   I have not bothered to get the county code and county name, would not be hard
#   there are 9 of these records
#   shortname is missing for these records, although fullname is not
# nygeotype=CDP
#   I have not bothered to get the county code and county name, probably hard

# Overview ----

# Goal:
#   create geocodes xwalk that has:
#     multiple codes for each geographic area of interest
#     state abbreviations
#     county codes and county names for lower-level geographies
#     NY geotypes - nation, state, county, city, town, village, CDP, others

# General approach: start with a full set of Census codes in the ACS, and extend it

#  Background: Census places (e.g., NY cities and villages) do not have codes
#  that indicate the county they are in because under Census geography rules
#  they may cross county borders

#  We want to assign a dominant county to each Census place in NY
#  NYS has a file that has this information but only has gnis codes
#  the census code placens is a gnis code

#  Method: We can get dominant county by a circuitous method:
#    TIGER/Line Shapefiles
#    see https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf
#      p.3-52+ 


# create a data frame with geocodes, names, and dominant county for places we care about

# str_detect(shortname, " UT") ~ "unorganized",  # what is this?? Chautauqua Lake UT unorganized territory

# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
# census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
# census_api_key(census_apikey, install=TRUE)

# constants ---------------------------------------------------------------
dacs <- here::here("data", "acs")
dxwalks <- here::here("data", "xwalks")

# function to get Census names and ids for a one-variable ACS table ------------------------------

get_names <- function(table, year=2019, geom=FALSE){
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
    mutate(table=!!table, endyear=year, survey="acs5")
  print(nrow(df))
  df
}

# get the base ACS names and codes -------------------------------------------------------
## get names data with population ----
df1 <- get_names("B01003", geom=TRUE) # get a table with only one variable, for exploring names
glimpse(df1)
df1 |> filter(name.x != name.y)

## drop geometry ----
df2 <- df1 |> 
  as_tibble() |> 
  mutate(geometry=NULL) |>  # this gets rid of geometry; select(-geometry) does not work
  rename(shortname=name.x,
         fullname=name.y,
         pop=estimate) |> 
  select(-c(variable, moe, table))
glimpse(df2)
tmp <- count(df2, shortname, fullname)
saveRDS(df2, path(dxwalks, "acs_geocodes.rds"))

# prepare to enhance the ACS codes ----
acscodes1 <- readRDS(path(dxwalks, "acs_geocodes.rds"))

# put state abbreviation on file, with help from fips_codes in tidycensus ----
data(fips_codes) # tidycensus
glimpse(fips_codes)
count(fips_codes, state, state_code, state_name) # 57 -- no US

stdf <- fips_codes |> 
  select(stabbr=state, statefp=state_code, stname=state_name) |> 
  distinct()

acscodes2 <- acscodes1 |> 
  left_join(stdf, by = "statefp") |> 
  mutate(stabbr=case_when(geoid=="1" ~ "US",
                          is.na(stabbr) & 
                            str_ends(fullname, ", New York", negate = FALSE) ~ "NY",
                          TRUE ~ stabbr),
         stname=case_when(is.na(stname) & stabbr=="US" ~ "United States",
                          is.na(stname) & stabbr=="NY" ~ "New York",
                          TRUE ~ stname))
# at this point, places don't have a county assigned to them

count(acscodes2, stabbr, stusps, statefp, stname) # we no longer need stusps


# define nygeotypes -------------------------------------------------------
acscodes3 <- acscodes2 |> 
  select(-stusps) |> 
  mutate(nygeotype=case_when(
    geotype %in% c("nation", "state", "county") ~ geotype,
    
    # cousubs
    geotype=="cousub" & str_detect(fullname, "town") ~ "town",
    geotype=="cousub" & str_detect(fullname, "Reservation") ~ "reservation",
    geotype=="cousub" & str_detect(fullname, "borough") ~ "borough",
    geotype=="cousub" & str_detect(fullname, "city") ~ "city_cousub",
    geotype=="cousub" & str_detect(fullname, "not defined") ~ "undefined_cousub",
    # is Chautauqua Lake UT, Chautauqua County, New York - unorganized territory
    geotype=="cousub" & str_detect(fullname, "Chautauqua Lake UT") ~ "unorganized_cousub",
  
    # caution: places don't have a county assigned to them
    geotype=="place" & str_detect(fullname, "city") ~ "city",
    geotype=="place" & str_detect(fullname, "village") ~ "village",
    geotype=="place" & str_detect(fullname, "CDP") ~ "CDP",
    
    TRUE ~ "other")) 

count(acscodes3, geotype, nygeotype)

# check for availability of county information
count(acscodes3 |> filter(geotype %in% "place"), countyfp) # no countyfp
count(acscodes3 |> filter(geotype %in% "cousub"), countyfp)
count(acscodes3 |> filter(geotype %in% "county"), countyfp)
# we have countyfp for all but 9 cousub and all county recs, but none for place recs

# look at how NYC is coded
check <- acscodes3 |> filter(str_detect(fullname, "New York city"))
# geoid 3651000, countyfp missing, placens 02395220

# put dominant county on file ----
# match placens (gnis code) to a NYS data file that has gnis and dominant county
count(acscodes3, placens) |> ht()
# here is the placens code for the city of Albany: 00978659 
# the codes are 8 characters long, zero-padded on the left

## ONETIME: download NYS data with dominant county county fips and name ----
# https://data.ny.gov/Government-Finance/New-York-State-Locality-Hierarchy-with-Websites/55k6-h6qq
# https://data.ny.gov/Government-Finance/New-York-State-Locality-Hierarchy-with-Websites/55k6-h6qq/data
nyfn <- "New_York_State_Locality_Hierarchy_with_Websites.csv"
url <- "https://data.ny.gov/api/views/55k6-h6qq/rows.csv?accessType=DOWNLOAD&sorting=true"
download.file(url, path(dxwalk, nyfn), mode="wb")

## get NY codes data and put dominant county code on the file ----
df1 <- read_csv(path(dxwalks, nyfn), col_types = cols(.default = col_character()))
glimpse(df1)

df2 <- df1 |> 
  rename(swis=`SWIS Code`, type=`Type Code`, typef=Type, 
         county=`County Name`, city=`City Name`, town=`Town Name`, village=`Village Name`,
         county2=`2nd County`, web=Website, muni=Municipality, 
         gnis=`GNIS ID`, stfips=`State FIPS`, cntycode=`County Code`, cntyfips=`County FIPS`)

count(df2, type, typef)
count(df2, gnis) |> ht()
# NY gnis codes are 6 characters, no zero padding

# create placens codes that are 8 characters long, zero-padded on the left
str_pad("978659", width=8, side="left", pad="0")

domco1 <- df2 |> 
  filter(muni != "New York City") |> # not coded the way I want, we'll do NYC by hand
  filter(typef %in% c("City", "Village")) |> 
  mutate(placens=str_pad(gnis, width=8, side="left", pad="0")) |> 
  select(typef, placens, muni, cntycode, county)

acscodes4 <- acscodes3 |> 
  left_join(domco1, by="placens")

check <- acscodes4 |> filter(nygeotype %in% c("city", "village"))
# we did not match (geoid, fullname, placens):
#   3651000 New York city, New York 02395220 (intentional)
#   3646085 Mastic Beach village, New York 02390131
#   3664749 Salamanca city, New York 00979450

domco1 |> filter(str_detect(muni, "Mastic"))
# Village 02680279 Mastic Beach 103      Suffolk
domco1 |> filter(str_detect(muni, "Salamanca"))
# City  00979451 Salamanca 009      Cattaraugus

# make sure the to-be-adjusted placens codes aren't elsewhere in the acs data
acscodes4 |> filter(placens %in% c("02680279", "00979451"))

# create revised domco that adjusts these 2
domco2 <- domco1 |> 
  mutate(placens=case_when(placens=="02680279" ~ "02390131",
                           placens=="00979451" ~ "00979450",
                           TRUE ~ placens))

acscodes5 <- acscodes3 |> # start with the previous good acscodes
  left_join(domco2, by="placens")
check <- acscodes5 |> filter(nygeotype %in% c("city", "village"))  
count(check, nygeotype, typef)

## put county names of dominant county on the file, for all records ----
counties <- acscodes5 |> 
  filter(nygeotype=="county") |> 
  select(geoid, countyfp, stabbr, shortname, fullname, pop)

acscodes6 <- acscodes5 |> 
  mutate(countyfp=ifelse(is.na(countyfp), cntycode, countyfp)) |> 
  left_join(counties |> 
              select(stabbr, countyfp, countyname=shortname),
            by=c("stabbr", "countyfp")) |> 
  # handle NYC manually
  mutate(countyfp=case_when(is.na(cntycode) & 
                              geotype=="place" & 
                              geoid=="3651000" ~ "51000",
                            TRUE ~ countyfp),
         countyname=case_when(is.na(cntycode) & 
                              geotype=="place" & 
                              geoid=="3651000" ~ "New York City",
                            TRUE ~ countyname),
         stcntyfips=ifelse(!is.na(countyfp),
                           paste0(statefp, countyfp),
                           NA_character_)) |> 
  relocate(muni, .after = fullname) # make it easier to compare names
count(acscodes6, countyname, county)

# examine selected records
check <- acscodes6 |> filter(countyname=="Albany")
check <- acscodes6 |> filter(str_detect(fullname, "New York city"))
check <- acscodes6 |> filter(nygeotype=="city")
check <- acscodes6 |> filter(nygeotype=="village")

acscodes6 |> 
  filter(is.na(countyname)) |> 
  count(geotype, nygeotype)
# everything looks ok - I haven't bothered with counties for Census Designated Places

# drop unneeded NY codes variables, put vars in desired order, and save
# we don't need cntycode, county, typef from the NY file
acscodes7 <- acscodes6 |>
  select(affgeoid, geoid, geotype, nygeotype, shortname, fullname,
         stabbr, stname, statefp, statens, 
         stcntyfips, countyfp, countyns, countyname,
         cousubfp, cousubns, 
         placefp, placens, 
         lsad, 
         pop, aland, awater,
         survey, endyear)

# saveRDS(acscodes7, path(dxwalks, "acs_geocodes.rds"))  

tmp <- acscodes7 |> filter(str_detect(fullname, "New York city"))
tmp <- acscodes7 |> filter(str_detect(fullname, "New York city"))


# final step - put NY region codes on the acscodes file -----------------------------------
# acscodes7 <- readRDS(path(dxwalks, "acs_geocodes.rds"))  
rgns <- read_excel(path(dxwalks, "nycounty_xwalk.xlsx"), sheet="region_codes")

glimpse(acscodes7)
glimpse(rgns)

acscodes8 <- acscodes7 |> 
  left_join(rgns |> 
              select(stcntyfips=geoid,
                     tmpcounty=county, 
                     rgn_num, rgn_code, rgn_osc), 
            by = "stcntyfips")
count(acscodes8, rgn_num, rgn_code, rgn_osc, stcntyfips, countyname, tmpcounty)
# all good
glimpse(acscodes8)

acscodes9 <- acscodes8 |> 
  select(-tmpcounty) |> 
  relocate(rgn_num, rgn_code, rgn_osc, .after=lsad)
glimpse(acscodes9)

saveRDS(acscodes9, path(dxwalks, "acs_geocodes.rds"))  


# explore -----------------------------------------------------------------

acscodes <- readRDS(path(dxwalks, "acs_geocodes.rds"))  
glimpse(acscodes)

