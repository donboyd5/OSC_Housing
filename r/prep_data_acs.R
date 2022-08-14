

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




# data to get ------------------------------------------------------------------
acs_variables <- readRDS(here::here("data", "acs_variables.rds"))

f <- function(table){
  print(table)
  get_acs(geography = "county", 
          table = table, state=c("NY"),
          geometry = FALSE,
          cache_table = TRUE)
}
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


tabvec <- c("B11012", "B25001", "B25002", "B25003", "B25006", "B25007", "B25008", "B25009", 
            "B25070", "B25072", "B25074", "B25093", "B25095", "B25106")

df <- map_dfr(tabvec, f)
f("B25095")

f2 <- function(df){
  df2 <- df |> 
    lcnames() |> 
    rename(area=name,
           vname=variable) |> 
    left_join(acs_variables |> 
              select(table, vname, line, stub, tabname, universe),
            by="vname") |> 
    select(geoid, area, line, vname, estimate, moe, table, tabname, universe, stub)
}
df2 <- f2(df)
glimpse(df2)
summary(df2)
count(df2, table, tabname)

saveRDS(df2, here::here("data", "acs",  "acs_selected_table_data.rds"))



