# https://data2.nhgis.org/

# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# constants ---------------------------------------------------------------
# dfb <- r"(E:\data\FederalBudget\2022\BUDGET-2022-DB)"

dnhgis <- here::here("data", "nhgis")
fnz <- "nhgis0003_csv.zip"
fpathz <- path(dnhgis, fnz)

# unzip nhgis database ---------------------------------------------------
# files: 
# 2005-2009: nhgis0003_ds195_20095_nation.csv, state, county
# 2010-2014: nhgis0003_ds206_20145_nation.csv, ...
# 2015-2019: nhgis0003_ds244_20195_nation.csv, ...

list.files(path(fpathz, "nhgis0003_csv"), pattern=".csv")
df <- unzip(fpathz, list=TRUE) |> str_subset(".csv")

fpath <- path("nhgis0003_csv", "nhgis0003_ds195_20095_nation.csv")
df1a <- read_csv(unz(fpathz, fpath), col_types = cols(.default = col_character()))

a <- archive(fpathz)
a
filenames <- a |> filter(str_detect(path, "\\.csv")) |> pull(path)
filenames

# read_all_zip <- function(file, ...) {
#   # filenames <- unzip(file, list = TRUE)$Name
#   a <- archive(file)
#   filenames <- a |> filter(str_detect(path, "\\.csv")) |> pull(path)
#   print(filenames)
#   # vroom(purrr::map(filenames, ~ unz(file, .x)), ...)
# }

f <- function(fname){
  print(fname)
  fn <- path_file(fname) |> path_ext_remove()
  df1 <- vroom(unz(fpathz, fname), col_types = cols(.default = col_character()))
  df2 <- df1 |>
    pivot_longer(cols=-c(1:NAME_E, NAME_M)) |>
    mutate(src=fn)
  df2
}
f(filenames[1])

df1 <- map_dfr(filenames, f)
glimpse(df1)

df2 <- df1 |> 
  lcnames() |> 
  rename(stabbr=stusab) |> 
  mutate(geo=case_when(str_detect(src, "nation") ~ "nation",
                       str_detect(src, "state") ~ "state",
                       str_detect(src, "county") ~ "county",
                       TRUE ~ "ERROR"),
         value=as.numeric(value))
glimpse(df2)
count(df2, year)
count(df2, stabbr)
count(df2, geo)
count(df2, src)
count(df2, year, src)

saveRDS(df2, here::here("data", "nhgis", "nhgis.rds"))


# explore -----------------------------------------------------------------

nys <- df2 |> 
  filter(geo=="state", 
         stabbr=="NY") |> 
  select(geo, stabbr, geoid, year, name, value)

# housing units
# RP7E001 2009
# ABGVE001 2014
# ALZJE001 2019
hunits <- c("RP7E001", "ABGVE001", "ALZJE001")
nys |> filter(name %in% hunits) 

vnamesdf <- read_csv(
"vname, code2009, code2014, code2019
hunits, RP7E001, ABGVE001, ALZJE001
occupied, RP8E002, ABGWE002, ALZKE002
vacant, RP8E003, ABGWE003, ALZKE003
owner, RP9E002, ABGXE002, ALZLE002
renter, RP9E003, ABGXE003, ALZLE003
") |> 
  pivot_longer(-vname) |> 
  mutate(endyear=str_sub(name, -4, -1),
         year=paste0(as.integer(endyear) - 4, "-", endyear)) |> 
  select(vname, year, name=value)
vnamesdf

tmp <- count(df2, name)

df2 |> 
  #filter(geo=="nation") |> 
  # filter(geo=="state", stabbr=="NY") |>
  filter(geo=="county", stabbr=="NY") |> 
  filter(name %in% hunits) |> 
  select(geo, stabbr, geoid, name_e, year, name, value)


df1 <- f(filenames[16])
path_file(filenames) |> path_ext_remove()
path_ext_remove(filenames)


df1 <- map_dfr(filenames, f)
head(glimpse(df1))
count(df1, GISJOIN)
count(df1, YEAR)

ns(df1)




## 2005-2009 ----
fpath <- path("nhgis0003_csv", "nhgis0003_ds195_20095_county.csv")

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
  





