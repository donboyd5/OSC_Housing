
# CAUTION: Negative numbers seem to indicate missingness or other things????


# negative numbers --------------------------------------------------------

# see the picture documentation

# Definition of Missing values
# Some cell entries across variables report no data or are suppressed. In such cases one of the following codes will apply to such missing values in the downloadable file
# "NA" = Not applicable
# "-1" = Missing
# "-4" = Suppressed (where the cell entry is less than 11 for reported families)
# "-5" = Non-reporting (where reporting rates--see % Reported--are less than 50%)

# Average size of household (with decimal point and place, e.g., 2.5, with -ve
# sign, decimal point for suppressed and non-reporting values, e.g., -4.0, -5.0)


# links -------------------------------------------------------------------

# https://www.huduser.gov/portal/datasets/assthsg.html#2009-2021_data

# https://www.huduser.gov/portal/datasets/pictures/files/US_2021.xlsx
# https://www.huduser.gov/portal/datasets/pictures/files/US_2010.xlsx
# https://www.huduser.gov/portal/datasets/pictures/files/STATE_2021.xlsx
# https://www.huduser.gov/portal/datasets/pictures/files/COUNTY_2021.xlsx

# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# constants ---------------------------------------------------------------
dpict <- here::here("data", "hud", "picture")
dxwalks <- here::here("data", "xwalks")


# get xwalk data ----------------------------------------------------------

acscodes <- readRDS(path(dxwalks, "acs_geocodes.rds"))  


# ONETIME: download picture data ------------------------------------------
urls <- c("https://www.huduser.gov/portal/datasets/pictures/files/US_2021.xlsx",
          "https://www.huduser.gov/portal/datasets/pictures/files/STATE_2021.xlsx",
          "https://www.huduser.gov/portal/datasets/pictures/files/COUNTY_2021.xlsx")
f <- function(url) {
  print(path_file(url))
  download.file(url,path(dpict, path_file(url)), mode="wb")
}

walk(urls, f)


# read the downloaded data, add codes, save -------------------------------
(files <- path(dpict, path_file(urls)))

f <- function(file){
  print(file)
  read_excel(file, sheet=1, col_types = "text")
}

df1 <- map_dfr(files, f)
glimpse(df1)
count(df1, sumlevel)
count(df1, sumlevel, gsl) # rectype
count(df1, entities)
count(df1, code) # stabbr if US rec, looks like a fips code otherwise
count(df1, fedhse) # all NA
count(df1, program, program_label) 
count(df1, program, program_label, sub_program) # subprogram NA for all
count(df1, name) # 1900 different levels
count(df1, fedhse) # NA for all
count(df1, cbsa) # NA for all
count(df1, place) # NA
count(df1, latitude) # NA
count(df1, longitude) # NA
count(df1, pha_total_units) # NA
count(df1, ha_size) # NA
count(df1, Quarter) # 44561 for all -- 31Dec2021
count(df1, sumlevel, states) # stabbr stname if county rec
count(df1, state) # stabbr if county rec
count(df1, States) # stabbr and name if state rec
count(df1, State) # stabbr if state rec

count(df1, sumlevel, code, state, States, State)

df2 <- df1 |> 
  select(-c(sub_program, fedhse, cbsa, place, latitude, longitude,
            pha_total_units, ha_size, Quarter)) |> 
  mutate(stabbr=case_when(sumlevel=="1" ~ code,
                          sumlevel=="3" ~ State,
                          sumlevel=="9" ~ state,
                          TRUE ~ "ERROR"), 
         nygeotype=case_when(sumlevel=="1" ~ "nation",
                           sumlevel=="3" ~ "state",
                           sumlevel=="9" ~ "county"),
         cofips=str_sub(entities, -1, -6),
         entities=str_sub(entities, 4, -7) |> str_trim(),
         geoname=ifelse(stabbr=="US", "United States", name)) |> 
  select(sumlevel, gsl, nygeotype, stabbr, code, State, state, States, state, cofips,
         entities, geoname,
         everything())
glimpse(df2)
count(df2, sumlevel, gsl, nygeotype)
count(df2, stabbr, rectype, State, States, state, states)
count(df2 |> filter(stabbr=="NY"), nygeotype, code, State, States, state, states)
count(df2, entities, geoname)

# deal with negative numbers (see above) BEFORE converting to numeric
# set all -1, -4, -5 to NA_character
df3 <- df2 |> 
  select(-c(sumlevel, gsl, State, States, state, states, entities, name)) |> 
  rename(fips=code, programf=program_label) |> 
  mutate(fips=ifelse(fips=="US", "1", fips),
         program=as.integer(program)) |> 
  relocate(program, .before=programf) |> 
  mutate(across(-c(nygeotype:programf),
                ~ ifelse(.x %in% c("-1", "-4", "-5"), NA_character_, .x)))

df4 <- df3 |> 
  mutate(across(-c(nygeotype:programf), as.numeric))
glimpse(df4)
count(df4, nygeotype)
count(df4, stabbr)
df4 |> filter(is.na(stabbr))
count(df4, nygeotype)
count(acs2, nygeotype)

glimpse(df4)
glimpse(acscodes)
count(acscodes, nygeotype)

count(acscodes, geoid)
acs2 <- acscodes |> 
  filter(nygeotype %in% c("nation", "state", "county"))

df5 <- df4 |> 
  rename(geoid=fips) |> 
  left_join(acscodes, by = join_by(nygeotype, stabbr, geoid))
glimpse(df5)

saveRDS(df5, path(dpict, "picture.rds"))

# get picture data --------------------------------------------------------

df1 <- read_excel(path(dpict, fn),
                  sheet="County_Extract31DEC2021",
                  col_types = "text")
glimpse(df1)

df1 |> 
  select(name, code, people_total, latitude, longitude, cbsa)
count(df1, latitude, longitude, cbsa, place, fedhse, pha_total_units)
summary(df1)

df2 <- df1 |> 
  select(-c(latitude, longitude, cbsa, place, fedhse, pha_total_units)) |> 
  lcnames() |> 
  mutate(across(-c(quarter:name, code), ~ ifelse(str_sub(.x, 1, 1)=="-", NA_character_, .x)),
         across(-c(quarter:name, code), as.numeric),
         program=as.integer(program),
         stabbr=str_sub(states, 1, 2))

glimpse(df2)
summary(df2)
count(df2, stabbr) # 56: states, DC, GU, PR, VI, XX, NA
count(df2, program, program_label)

dfny <- df2 |> 
  filter(stabbr=="NY") |> 
  mutate(nyc=ifelse(code %in% nycfips, "nyc", "ros"))
saveRDS(dfny, path(dpict, "nypicture.rds"))

count(dfny, code, name) # 63: 57 xnyc, 5 nyc, 1 missing 36XXX

dfny |> 
  group_by(program, program_label) |> 
  summarise(across(c(people_total), ~ sum(.x, na.rm=TRUE)), .groups = "drop")

dfny |> 
  group_by(nyc, program, program_label) |> 
  summarise(across(c(people_total), ~ sum(.x, na.rm=TRUE)), .groups = "drop") |> 
  pivot_wider(names_from=nyc, values_from=people_total, values_fill = 0)

shortlab <- c("all", "pubhousing", "hcv", "modrehab", "projbased", "bmir", "elderly", "disabled")

dfny |> 
  mutate(shortlab=factor(program, levels=c(1:5, 7:9), labels=shortlab),
         shortlab=fct_other(shortlab, 
                            drop=c("modrehab", "bmir"),
                            other_level="other"),
         code=ifelse(nyc=="nyc", "36NYC", code),
         name=ifelse(nyc=="nyc", "New York City", name)) |> 
  group_by(name, shortlab) |> 
  summarise(across(c(people_total), ~ sum(.x, na.rm=TRUE)), .groups = "drop") |> 
  pivot_wider(names_from=shortlab, values_from=people_total, values_fill = 0) |> 
  mutate(diff=all - rowSums(across(-c(all, name))),
         other=other + diff,
         phpct=pubhousing / all) |> 
  select(-diff) |> 
  relocate(phpct, .after = all) |>
  arrange(desc(phpct))


