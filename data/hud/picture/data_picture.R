
# https://www.huduser.gov/portal/datasets/assthsg.html#2009-2021_data

# https://www.huduser.gov/portal/datasets/pictures/files/US_2021.xlsx
# https://www.huduser.gov/portal/datasets/pictures/files/US_2010.xlsx
# https://www.huduser.gov/portal/datasets/pictures/files/STATE_2021.xlsx
# 
# https://www.huduser.gov/portal/datasets/pictures/files/COUNTY_2021.xlsx

# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# constants ---------------------------------------------------------------
dpict <- here::here("data", "hud", "picture")
fn <- "COUNTY_2021.xlsx"

fnames <- "COUNTY_2021.xlsx"


# get picture data --------------------------------------------------------

df1 <- read_excel(path(dpict, fn), col_types = "text")
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


