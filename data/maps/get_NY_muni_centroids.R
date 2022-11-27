
# get centroids so that we can put labels on maps

# CAUTION: Where is Sherrill?? Find it

# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))
# source(path(path_home_r(), "R_projects", "api_keys.r"))
# census_api_key(census_apikey, install = TRUE)


# get basic info for NY counties, cities, towns, villages, school  --------
state <- get_acs(geography = "state", state = "NY", year = 2020,
                 keep_geo_vars=TRUE,
                 # B01001_001 is total population
                 variables = c("B01001_001"), output = "wide",geometry = TRUE)

county <- get_acs(geography = "county", state = "NY", year = 2020,
                  keep_geo_vars=TRUE,
                  # B01001_001 is total population
                  variables = c("B01001_001"), output = "wide",geometry = TRUE)

cousub <- get_acs(geography = "county subdivision", state = "NY", year = 2020,
                  keep_geo_vars=TRUE,
                  # B01001_001 is total population
                  variables = c("B01001_001"), output = "wide",geometry = TRUE)

place <- get_acs(geography = "place", state = "NY", year = 2020,
                 keep_geo_vars=TRUE,
                 # B01001_001 is total population
                 variables = c("B01001_001"), output = "wide",geometry = TRUE)

elsd <- get_acs(geography = "school district (elementary)", state = "NY", year = 2020,
                keep_geo_vars=TRUE,
                # B01001_001 is total population
                variables = c("B01001_001"), output = "wide",geometry = TRUE)

secsd <- get_acs(geography = "school district (secondary)", state = "NY", year = 2020,
                 keep_geo_vars=TRUE,
                 # B01001_001 is total population
                 variables = c("B01001_001"), output = "wide",geometry = TRUE)

usd <- get_acs(geography = "school district (unified)", state = "NY", year = 2020,
               keep_geo_vars=TRUE,
               # B01001_001 is total population
               variables = c("B01001_001"), output = "wide",geometry = TRUE)


# checks ------------------------------------------------------------------
elsd |> filter(GEOID=="9500000US3611460")


# combine geography files -------------------------------------------------

# cousub and place has overlap for cities that we must get rid of

nyareas1 <- bind_rows(
  state |> mutate(atype="state"),
  county |> mutate(atype="county"),
  cousub |> mutate(atype="cousub"),
  place |> mutate(atype="place"),
  elsd |> mutate(atype="elsd"),
  secsd |> mutate(atype="secsd"),
  usd |> mutate(atype="usd"))
saveRDS(nyareas1, here::here("data", "acs", "nyareas_raw.rds"))



# clean data --------------------------------------------------------------
nyareas1 <- readRDS(here::here("data", "acs", "nyareas_raw.rds"))
names(nyareas1)

nyareas1 |>
  st_drop_geometry() |> 
  filter(str_detect(NAME.x, coll("Sherrill", ignore_case = TRUE)) |
           str_detect(NAME.y, coll("Sherrill", ignore_case = TRUE)) |
           str_detect(NAMELSAD, coll("Sherrill", ignore_case = TRUE)))

 
summary(nyareas1)
tmp1 <- nyareas1 |> 
  st_drop_geometry()
count(tmp1, atype)
summary(tmp1)

length(unique(tmp$GEOID))
tmp2 <- tmp1 |> 
  group_by(GEOID) |> 
  mutate(n=n()) |> 
  ungroup() |> 
  filter(n>1) |> 
  arrange(GEOID)
count(tmp2, atype) # there is some overlap between place and school district geoids

tmp3 <- tmp1 |> 
  filter(!str_detect(atype, "sd"))
nrow(tmp3)
length(unique(tmp3$GEOID))
# ok, our only problem with non-unique geoids is school districts overlapping places
# They do have different AFFGEOIDs, however, so keep that

tmp4 <- tmp1 |> 
  group_by(area)


# clean combined geography file -------------------------------------------------
nyareas2 <- nyareas1 |>
  as_tibble() |> 
  lcnames() |> 
  # st_drop_geometry() |> 
  mutate(area=ifelse(!is.na(namelsad), namelsad, name.y)) |> 
  # put the county name and acs pop on each record, where available
  left_join(county |> 
              st_drop_geometry() |> 
              lcnames() |> 
              select(countyfp, county=name.x),
            by="countyfp") |> 
  filter(geoid != "3699999", !str_detect(area, "County subdivisions not defined")) |> 
  mutate(shortname=case_when(atype=="county" & str_detect(area, " County") ~ str_remove(area, " County"),
                             atype=="cousub" & str_detect(area, " town") ~ str_remove(area, " town"),
                             atype=="cousub" & str_detect(area, " city") ~ str_remove(area, " city"),
                             atype=="cousub" & str_detect(area, " Reservation") ~ str_remove(area, " Reservation"),
                             atype=="cousub" & str_detect(area, " borough") ~ str_remove(area, " borough"),
                             atype=="place" & str_detect(area, " city") ~ str_remove(area, " city"),
                             atype=="place" & str_detect(area, " village") ~ str_remove(area, " village"),
                             atype=="place" & str_detect(area, " CDP") ~ str_remove(area, " CDP"),
                             TRUE ~ area)) |> 
  select(stabbr=stusps, atype, affgeoid, geoid, area, shortname, countyfp, county,
         pop5yr=b01001_001e, pop5yrmoe=b01001_001m, aland, geometry)
# note that there are a few areas with zero pop and zero land area

tmp <- nyareas2 |> filter(area == shortname) 


tmp <- nyareas2 |> select(atype, affgeoid, geoid, area)

# check the data out, removing geometry
tmp <- nyareas2 |> 
  st_drop_geometry()
summary(tmp)
count(tmp, stabbr)
count(tmp, atype)
count(tmp, countyfp, county)
count(tmp, area) |> filter(n>1)
tmp |> 
  filter(area=="Albany city")
count(tmp, nchar(geoid))

tmp2 <- tmp |> 
  group_by(area, pop5yr) |> 
  mutate(n=n()) |> 
  ungroup() |> 
  filter(n>1) |> 
  arrange(area)

nyareas3 <- nyareas2 |> 
  mutate(centroids(geometry)) |> 
  rename(loncenter=X, latcenter=Y)


# save versions with and without geometry -------------------------------------------
# all data so far, with geometry, in the maps folder
saveRDS(nyareas3, here::here("data", "maps", "nycentroids_with_geometry.rds"))


# investigate duplicates --------------------------------------------------
df <- readRDS(here::here("data", "maps", "nycentroids_with_geometry.rds"))
df
count(df, atype)
glimpse(df)

dups <- df |> 
  st_drop_geometry() |> 
  group_by(area, pop5yr) |> 
  mutate(n=n()) |> 
  ungroup() |> 
  filter(n>1) |> 
  arrange(area)
count(dups, atype)

# we have 60 duplicates - each is a cousub and a place
# they are the 60 cities outside NYC
dups |> pull(area) |> unique() |> sort()


# save data subset: just centroids, drop school dists, drop duplicates, save in main data folder -----------------
df <- readRDS(here::here("data", "maps", "nycentroids_with_geometry.rds"))
glimpse(df)

# get base data
df2 <- df |> 
  st_drop_geometry() |> 
  filter(!str_detect(atype, "sd"))
glimpse(df2)


# mark duplicates and drop the place versions of the 60 cities outside nyc
df3 <- df2 |> 
  group_by(area, pop5yr) |> 
  mutate(n=n(), dup=n > 1) |> 
  ungroup() |> 
  filter(!(dup & atype=="place"))
summary(df3)

# note that places do not have counties because they can cross county boundaries
# possibly give them a dominant county?

df4 <- df3 |> 
  mutate(sqmiles=aland * 0.00000038610) |> # convert land area from square meters to square miles
  arrange(atype, area) |> 
  select(stabbr, atype, area, shortname, affgeoid, geoid, county, pop5yr, aland, sqmiles, loncenter, latcenter)
glimpse(df4)

saveRDS(df4, here::here("data", "maps", "nycentroids.rds"))


# examine results ---------------------------------------------------------

nycentroids <- readRDS(here::here("data", "maps", "nycentroids.rds"))
count(nycentroids, atype, affgeoid, geoid, area, county) |> filter(is.na(county))

