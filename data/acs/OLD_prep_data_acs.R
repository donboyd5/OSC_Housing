# str_detect(shortname, " UT") ~ "unorganized",  # what is this?? Chautauqua Lake UT unorganized territory

# notes -------------------------------------------------------------------

# https://walker-data.com/census-r/an-introduction-to-tidycensus.html
# https://www.census.gov/data/developers/data-sets/acs-5year.html
# https://www.census.gov/programs-surveys/acs/data/summary-file.html


# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))

# locations --------------------------------------------------------------
acsdir <- here::here("data", "acs")


# get documentation -------------------------------------------------------
docfn <- "ACS2020_Table_Shells.xlsx"
docpath <- path(acsdir, docfn)

df1 <- read_excel(path=docpath, sheet="2020")
df2 <- df1 |> 
  setNames(c("table", "line", "vname", "stub", "release"))
glimpse(df2)

df3 <- df2 |> 
  filter(if_any(everything(), ~ !is.na(.x))) |> # drop all-na rows
  mutate(row=row_number(),
         tabname=ifelse(!is.na(release), stub, NA_character_),
         universe=ifelse(str_detect(stub, "Universe"), stub, NA_character_),
         universe=str_remove(universe, "Universe:  ")) |> 
  fill(tabname, universe, release, .direction="down") |> 
  filter(!is.na(vname)) |> 
  select(table, tabname, universe, release, line, vname, stub)
saveRDS(df3, here::here("data", "acs_variables.rds"))

tabs <- count(df3, table, tabname)
saveRDS(tabs, here::here("data", "acs", "acs_tables.rds"))



# check documentation -----------------------------------------------------
acs_variables <- readRDS(here::here("data", "acs_variables.rds"))
check <- tabs |> 
  filter(str_detect(tabname, coll("hous", ignore_case = TRUE)))
# B11012 HOUSEHOLDS BY TYPE  
# B25106  TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                               46

check |> 
  filter(str_detect(tabname, coll("cost", ignore_case = TRUE)))


# get and save data all areas ----------------------------------------------------
f <- function(table){
  print(table)
  # default is acs 5 year, ending 2020
  state <- get_acs(geography = "state", 
          table = table, 
          state="NY",
          geometry = FALSE,
          keep_geo_vars = FALSE,  # TRUE only works if we geometry=TRUE
          cache_table = TRUE) |> 
    mutate(type="state")
  
  county <- get_acs(geography = "county", 
          table = table,
          state="NY",
          geometry = FALSE,
          cache_table = TRUE) |> 
    mutate(type="county")
  
  cousub <- get_acs(geography = "county subdivision", 
                    table = table,
                    state="NY",
                    geometry = FALSE,
                    cache_table = TRUE) |> 
    mutate(type="cousub")
  
  place <- get_acs(geography = "place", 
          table = table,
          state="NY",
          geometry = FALSE,
          cache_table = TRUE) |> 
    mutate(type="place")
  
  df <- bind_rows(state, county, cousub, place) |> 
    mutate(table=!!table)
  saveRDS(df, here::here("data", "acs", paste0("tab_", table, ".rds")))
  print(nrow(df))
}

# df <- f("B25003")
# count(df, type)
# count(df, table)

table <- "B25003"

tabvec <- c("B11012", "B25001", "B25002", "B25003", "B25006", "B25007", "B25008", "B25009", 
            "B25070", "B25072", "B25074", "B25093", "B25095", "B25106")

nrecs <- map_int(tabvec, f)


# stack and clean ------
gettab <- function(table){
  print(table)
  fname <- paste0("tab_", table, ".rds")
  df <- readRDS(here::here("data", "acs", fname))
  df
}

df1 <- map_dfr(tabvec, gettab)
count(df1, type)

df2 <- df1 |> 
  lcnames() |> 
  rename(area=name,
         vname=variable) |> 
  left_join(acs_variables |> 
              select(table, vname, line, stub, tabname, universe),
            by=c("vname", "table")) |> 
  select(type, geoid, area, line, vname, estimate, moe, table, tabname, universe, stub)

saveRDS(df2, here::here("data", "acs", "acsdata.rds"))



# play --------------------------------------------------------------------

acsdata <- readRDS(here::here("data", "acs", "acsdata.rds"))
nycentroids <- readRDS(here::here("data", "maps", "nycentroids.rds"))
nygeom <- readRDS(here::here("data", "maps", "nycentroids.rds"))

df1 <- acsdata |> 
  filter(table=="B25003")

df2 <- df1 |> 
  mutate(var=factor(line, levels=1:3, labels=c("total", "owner", "renter"))) |> 
  select(geoid, area, var, estimate) |> 
  pivot_wider(names_from = var, values_from = estimate) |> 
  mutate(ownpct=owner / total)


# f(hhtenuretype)

# B11012	 		HOUSEHOLDS BY TYPE
# B25001	 		HOUSING UNITS
# B25002	 		OCCUPANCY STATUS
# B25003	 		TENURE
# B25006	 		RACE OF HOUSEHOLDER
# B25007	 		TENURE BY AGE OF HOUSEHOLDER
# B25008	 		TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE
# B25009	 		TENURE BY HOUSEHOLD SIZE

# B25070	 		GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25072	 		AGE OF HOUSEHOLDER BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25074	 		HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25093	 		AGE OF HOUSEHOLDER BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS

# B25095	 		HOUSEHOLD INCOME BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# C25095	 		HOUSEHOLD INCOME BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS -- more detail (djb)
# C25095 is generating errors - why??

# B25106			TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS -- [THIS IS KEY]


