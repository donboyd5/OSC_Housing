
# Low-Income Housing Tax Credit Qualified Census Tracts must have 50 percent of
# households with incomes below 60 percent of the Area Median Gross Income
# (AMGI) or have a poverty rate of 25 percent or more. Difficult Development
# Areas (DDA) are areas with high land, construction and utility costs relative
# to the area median income and are based on Fair Market Rents, income limits,
# the 2010 census counts, and 5-year American Community Survey (ACS) data.


# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

# locations ---------------------------------------------------------------
dlihtc <- here::here("data", "hud", "lihtc")

# constants ---------------------------------------------------------------
fnz <- path(dlihtc, "lihtcpub.zip")
fn1 <- "LIHTCPUB.CSV"


# get data ----------------------------------------------------------------
# xwalk <- readRDS(here::here("data", "xwalks", "nycounty_xwalk.rds"))
xwalk <- readRDS(here::here("data", "xwalks", "acs_geocodes.rds"))

xwalk |> 
  filter(rgn_num==9) |> 
  select(geoid, nygeotype, shortname)

# get a list of 62 counties
cntycodes <- xwalk |> 
  filter(nygeotype=="county") |> 
  select(geoid, nygeotype, shortname) |> 
  arrange(geoid)


df1 <- read_csv(unz(fnz, fn1), col_types = cols(.default = col_character()))
glimpse(df1)

df2 <- df1 |> 
  lcnames() |> 
  filter(st2010=="36") |> 
  mutate(across(c(latitude, longitude, allocamt:nlm_reason), as.numeric)) |> 
  mutate(geoid=paste0(st2010, cnty2010))
  

df2 |> 
  filter(yr_alloc >= 2010 | yr_pis >= 2010) |> # 1504 projects
  count(yr_alloc, yr_pis) |>
  arrange(yr_alloc, yr_pis) |>
  pivot_wider(names_from = yr_pis, values_from = n, names_sort = TRUE)

cntycodes |> 
  left_join(
    count(df2, st2010, cnty2010) |> 
      mutate(geoid=paste0(st2010, cnty2010)),
    by = join_by(geoid))

df3 <- df2 |> 
  left_join(cntycodes |> select(geoid, shortname),
            by = join_by(geoid))
skim(df3)
saveRDS(df3, fs::path(dlihtc, "lihtc_coded.rds"))

# NY projects since 2010  
df3 |> 
  filter(yr_alloc >= 2010 | yr_pis >= 2010) |> # 1504 projects
  summarise(n=n(),
            across(c(n_units, li_units,
                     n_unitsr, li_unitr), ~sum(.x, na.rm=TRUE)),
            .by=c(geoid, shortname)) |> 
  janitor::adorn_totals() |> 
  mutate(lipct=li_unitr / n_unitsr, lipct2=li_units / n_units) |> 
  arrange(desc(lipct))
  

# all but Hamilton county

  
# OLD BELOW HERE ----

check <- df2 |> 
  filter(is.na(cnty2010)) |> 
  arrange(desc(li_units)) |> 
  select(1:17, n_units, li_units)

# googling sro 1790 Clinton Ave., Bronx suggests NYC19990010 is in bronx

df3 <- df2 |> 
  mutate(cnty2010=case_when(
    hud_id %in% c("NYB20100801", "NYB20100010", "NYC19990010") ~ "005", # Bronx
    hud_id %in% c("NYE00000076", "NYE00000091") ~ "047", # Brooklyn
    hud_id %in% c("NYA19970090") ~ "055", # Monroe
    hud_id %in% c("NYC20120815", "NYC20110803", "NYC20120812", "NYC20120819") ~
      "061", # Manhattan
    hud_id %in% c("NYA00000695") ~ "063", # Niagara
    hud_id %in% c("NYE00000057") ~ "067", # Onondaga
    hud_id %in% c("NYE00000026", "NYA20090110", "NYC20100070") ~ "081", # Queens
    hud_id %in% c("NYA20000255", "NYE00000100") ~ "103", # Suffolk
    is.na(cnty2010) ~ str_sub(fips1990, 3, 5), # county code
    TRUE ~ cnty2010)) |> 
  mutate(stcnty=paste0(st2010, cnty2010)) |>
  left_join(xwalk |> select(stcnty=geoid, county),
            by="stcnty")

check2 <- df3 |> 
  filter(hud_id %in% check$hud_id) |> 
  select(1:3, 4, starts_with("fips"), cnty2010, county, n_units, li_units) |> 
  arrange(desc(li_units))


# save rds ----
saveRDS(df3, path(dlihtc, "lihtc.rds"))


# explore -----------------------------------------------------------------
lihtc <- readRDS(path(dlihtc, "lihtc.rds"))

lihtc |> 
  mutate(n_units=ifelse(li_units > n_units, li_units, n_units)) |> 
  group_by(stcnty, county) |> 
  summarise(across(c(n_units, li_units), ~ sum(.x, na.rm=TRUE)), .groups = "drop") |> 
  mutate(lipct=li_units / n_units) |> 
  arrange(desc(li_units))

lihtc |> 
  mutate(n_units=ifelse(li_units > n_units, li_units, n_units)) |> 
  group_by(stcnty, county) |> 
  summarise(across(c(n_units, li_units), ~ sum(.x, na.rm=TRUE)), .groups = "drop") |> 
  mutate(lipct=li_units / n_units) |> 
  arrange(desc(li_units))

lihtc |> 
  filter(li_units > n_units) |> 
  select(hud_id, project, stcnty, county, li_units, n_units)






