

# notes -------------------------------------------------------------------

# https://walker-data.com/census-r/an-introduction-to-tidycensus.html
# https://www.census.gov/data/developers/data-sets/acs-5year.html
# https://www.census.gov/programs-surveys/acs/data/summary-file.html


# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))

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
saveRDS(tabs, here::here("data", "acs_tables.rds"))



# check documentation -----------------------------------------------------


acs_variables <- readRDS(here::here("data", "acs_variables.rds"))
check <- tabs |> 
  filter(str_detect(tabname, coll("hous", ignore_case = TRUE)))
# B11012 HOUSEHOLDS BY TYPE  
# B25106  TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                               46

check |> 
  filter(str_detect(tabname, coll("cost", ignore_case = TRUE)))


# get data ----------------------------------------------------------------

acs_var <- 'B01003_001E'
tot <- get_acs(geography = "county", variables = acs_var, state=c("PA", "VA", "DC","MD"),
               geometry = TRUE)

tot <- get_acs(geography = "county", variables = acs_var, state=c("NY"),
               geometry = TRUE)

acs_tab <- "B11012"
tot <- get_acs(geography = "county", table = acs_tab, state=c("NY"),
               geometry = TRUE, cache_table = TRUE)

tot2 <- tot |> 
  left_join(acs_variables |> 
              select(variable=vname, line, stub),
            by="variable")
count(tot2 |> st_drop_geometry(), variable, line, stub)
# 1 total, 2 married, 5 8 13
acs_variables |> 
  filter(table=="B11012", line %in% c(1, 2, 5, 8, 13))

tot2 |> 
  st_drop_geometry() |> 
  filter(GEOID=="36001", line %in% c(1, 2, 5, 8, 13)) |> 
  select(area=NAME, variable, estimate) |> 
  pivot_wider(names_from=variable, values_from = estimate)

tmp <- acs_variables |> 
  filter(table=="B25106")
# lines:
# 1 total
# 2-23 owner
# 24-46 renter
