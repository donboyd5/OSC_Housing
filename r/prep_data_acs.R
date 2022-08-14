

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


# enhance the downloaded data ---------------------------------------------
acsdata <- readRDS(here::here("data", "acs",  "acs_selected_table_data.rds"))

hcburden1 <- acsdata |> 
  filter(table=="B25106")
count(hcburden1, stub)

hcburden2 <- hcburden1 |> 
  mutate(tenure=case_when(line==1 ~ "total",
                          line %in% 2:23 ~ "own",
                          line %in% 24:46 ~ "rent",
                          TRUE ~ "ERROR"),
         income=case_when(line %in% c(1, 2, 24) ~ "all",
                          str_detect(stub, coll("$")) ~ stub,
                          str_detect(stub, coll("Zero or negative income")) ~ "noincome",
                          TRUE ~ NA_character_),
         hcpercent=case_when(line %in% c(1, 2, 24) ~ "all",
                             str_detect(stub, "percent") ~ stub,
                             str_detect(stub, coll("$")) ~ "all",
                             str_detect(stub, coll("Zero or negative income")) ~ "noincome",
                             str_detect(stub, coll("No cash rent")) ~ "norent",
                             TRUE ~ NA_character_)) |> 
  fill(income, .direction="down") |> 
  select(geoid, area, line, vname, tenure, income, hcpercent, estimate, moe, stub)
count(hcburden2, tenure)
count(hcburden2, income)
count(hcburden2, hcpercent)
hcburden2 |> 
  filter(is.na(hcpercent))

check <- hcburden2 |> 
  filter(geoid=="36001")
library(writexl)
check |> 
  write_xlsx(here::here("explore", "check.xlsx"))
# write_xlsx(
#   x,
#   path = tempfile(fileext = ".xlsx"),
#   col_names = TRUE,
#   format_headers = TRUE,
#   use_zip64 = FALSE
# )
summary(hcburden2)

hcsummary <- hcburden2 |> 
  # filter(!(income=="all" & tenure!="total")) |> 
  group_by(geoid, area, tenure, hcpercent) |> 
  summarise(estimate=sum(estimate), .groups = "drop") |> 
  mutate(hcpctf=factor(hcpercent,
                       levels=c("norent",
                                "noincome",
                                "Less than 20 percent",
                                "20 to 29 percent",
                                "30 percent or more",
                                "all"),
                       labels=c("norent",
                                "noincome",
                                "plt20",
                                "p2029",
                                "p30p",
                                "all")))

count(hcsummary, hcpctf, hcpercent)
hcsummary2 <- hcsummary |> 
  select(-hcpercent) |> 
  pivot_wider(names_from = hcpctf,
              values_from = estimate)

hcsummary |> filter(geoid=="36001")
hcsummary |> filter(geoid=="36001") |> summarise(estimate=sum(estimate))
hcsummary |> filter(geoid=="36001") |> 
  group_by(tenure) |> 
  summarise(estimate=sum(estimate))

hcsummary |> 
  filter(geoid=="36001") |> 
  group_by(hcpctf) |> 
  summarise(estimate=sum(estimate)) |> 
  mutate(pct=estimate /sum(estimate))

hcmapdata <- hcsummary |> 
  group_by(geoid, area, hcpctf) |> 
  summarise(estimate=sum(estimate), .groups="drop_last") |> 
  mutate(pct=estimate /sum(estimate)) |> 
  ungroup() |> 
  filter(hcpctf=="p30p") |> 
  arrange(desc(pct))

hcsummary <- hcburden2 |> 
  filter(!(income=="all" | hcpercent=="all")) |> 
  group_by(geoid, area, tenure, hcpercent) |> 
  summarise(estimate=sum(estimate), .groups = "drop") |> 
  mutate(hcpctf=factor(hcpercent,
                       levels=c("norent",
                                "noincome",
                                "Less than 20 percent",
                                "20 to 29 percent",
                                "30 percent or more",
                                "all"),
                       labels=c("norent",
                                "noincome",
                                "plt20",
                                "p2029",
                                "p30p",
                                "all")))

hcwide <- hcsummary |> 
  select(geoid, area, tenure, hcpctf, estimate) |> 
  pivot_wider(names_from=c(tenure, hcpctf), values_from = estimate) |> 
  mutate(otot=own_plt20 + own_p2029 + own_p30p + own_noincome,
         rtot=rent_plt20 + rent_p2029 + rent_p30p + rent_noincome + rent_norent,
         htot=otot + rtot,
         opct=own_p30p / otot,
         rpct=rent_p30p / rtot,
         totpct=(own_p30p + rent_p30p) / htot,
         renters_hhpct=rtot / htot,
         owners_hhpct=1 - renters_hhpct)

hcwide2 <- hcwide |> 
  select(geoid, area, otot, rtot, htot, contains("pct"))

hcwide |> 
  select(geoid, area, otot, rtot, htot, contains("pct")) |> 
  arrange(desc(rpct))

# https://ggplot2.tidyverse.org/reference/scale_brewer.html
# Diverging BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# Qualitative Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# Sequential Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd

# https://bookdown.org/alhdzsz/data_viz_ir/maps.html

nycomap(hcwide2, mapvar="owners_hhpct", maptitle="Homeownership percentage by county")
nycomap(hcwide2, mapvar="opct", maptitle="Percentage of homeowners who are cost-burdened")
nycomap(hcwide2, mapvar="rpct", maptitle="Percentage of renters who are cost-burdened")



## households by type and tenure ----
acs_tab <- "B11012"
hcosts <- get_acs(geography = "county", table = acs_tab, state=c("NY"),
                  geometry = TRUE, cache_table = TRUE)
# lines:
# 1 total
# 2-23 owner
# 24-46 renter

hcosts2 <- hcosts |> 
  st_drop_geometry() |> 
  lcnames() |> 
  left_join(acs_variables |> 
              select(variable=vname, line, stub),
            by="variable") |> 
  mutate(tenure=case_when(line==1 ~ "total",
                          line %in% 2:23 ~ "own",
                          line %in% 24:46 ~ "rent",
                          TRUE ~ "ERROR"),
         hhtype=ifelse(str_detect(stub, ":"), str_remove(stub, ":"), NA_character_)) |> 
  fill(hhtype, .direction="down")

count(hcosts2, hhtype)


## burden by type of household ----


library(sf)
library(rnaturalearth)
world <- ne_countries(scale = "small", returnclass = "sf")
ns(world)
unique(world$region_wb)
unique(world$income_grp)
world %>% 
  cbind(st_coordinates(st_centroid(world$geometry))) %>% 
  filter(admin %in% c("Venezuela","Colombia","Guyana","Suriname",
                      "Ecuador","Peru","Chile","Argentina","Brazil",
                      "Paraguay","Uruguay","Bolivia")) %>% 
  mutate(pop_est = pop_est / 1000000) %>% 
  ggplot() + 
  geom_sf(data = world, fill = "gray70", color = NA) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c() +
  coord_sf(
    xlim = c(-85, -35),
    ylim = c(10, -55)) +
  ggrepel::geom_label_repel(aes(X, Y, label = admin), 
                            size = 3,
                            fontface = "bold") +
  labs(fill = "Pop. Est. \n Millions:") +
  theme_void() +
  theme(plot.background = element_rect(fill = "aliceblue", color = NA))
