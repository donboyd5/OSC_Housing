# https://data2.nhgis.org/

# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# constants ---------------------------------------------------------------
# dfb <- r"(E:\data\FederalBudget\2022\BUDGET-2022-DB)"

dnhgis <- here::here("data", "nhgis")
fnz <- "nhgis0002_csv.zip"
fpathz <- path(dnhgis, fnz)

# unzip nhgis database ---------------------------------------------------
## 2005-2009 ----
fpath <- path("nhgis0002_csv", "nhgis0002_ds195_20095_county.csv")

df1a <- read_csv(unz(fpathz, fpath), col_types = cols(.default = col_character()))
glimpse(df1a)

df2a <- df1a |>
  lcnames() |>
  filter(row_number() > 1, statea=="36") |>  
  select(year, geoid, year, state, cnty=countya, name=county, 
         hunits=rp7e001, mdnvalue=rr7e001)

df2a

## 2019 ----
fpath <- path("nhgis0002_csv", "nhgis0002_ds243_2019_county.csv")

df1b <- read_csv(unz(fpathz, fpath), col_types = cols(.default = col_character()))
glimpse(df1b)

df2b <- df1b |>
  lcnames() |>
  filter(row_number() > 1, statea=="36") |>  
  select(year, geoid, year, state, cnty=countya, name=county, 
         hunits=alile001, mdnvalue=allje001)

df2b  # only 39

## combine ----

stack <- bind_rows(df2a |> mutate(year="2009"),
                   df2b) |> 
  mutate(across(c(year, hunits, mdnvalue),
                as.numeric),
         name2=str_remove(name, " County"))

# 2019 value vs hunit growth
# drop manhattan and brooklyn?
wide <- stack |> 
  pivot_wider(names_from = year, values_from = c(hunits, mdnvalue)) |> 
  mutate(ugrowth=hunits_2019 / hunits_2009 - 1,
         vgrowth=mdnvalue_2019 / mdnvalue_2009 - 1)

wide |> arrange(desc(mdnvalue_2019)) |> select(-c(geoid, state, name))

wide |> 
  filter(!cnty %in% c("061", "047"), !is.na(hunits_2019)) |> 
  ggplot(aes(ugrowth, mdnvalue_2019)) +
  geom_point() +
  geom_text(aes(label=name2))
  





