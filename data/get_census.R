
# get census data useful for allocation, etc.


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))

library(censusapi)



# FRED population ---------------------------------------------------------
# https://fred.stlouisfed.org/series/NYPOP

nypop1 <- fredr(
  series_id = "NYPOP",
  observation_start = as.Date("1980-01-01"),
  observation_end = NULL # as.Date("2000-01-01")
  )

nypop <- nypop1 |> 
  mutate(year=year(date),
         stabbr="NY") |> 
  select(year, pop=value)

nypop |> 
  ggplot(aes(year, pop)) +
  geom_line() +
  geom_point()

nypop_smooth <- nypop |> 
  arrange(year) %>%
  do(cbind(., stldf(.$pop, 4)))

nypop_smooth |> 
  select(year, pop, trend) |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point()

saveRDS(nypop_smooth, here::here("data", "_misc", "nypop_smooth.rds"))


# BAD BELOW HERE ----


# get state population for NY, over time ----------------------------------
# for now, use tidycensus for 2000-2019, and censusapi for 2020, 2021


pop1a <- get_estimates(
  geography="state",
  variables = "POP",
  year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)
pop1a

pop1b <- pop1a |> 
  select(stname=NAME, pop=value, date=DATE) |> 
  mutate(type=case_when(date==1 ~ "cen2010",
                        date==2 ~ "base2010",
                        TRUE ~ "est"),
         year=case_when(date %in% 1:2 ~ 2010,
                        date %in% 3:12 ~ date + 2007,
                        TRUE ~ 9999)) |> 
  select(-date)
pop1b

pop2a <- getCensus(
  name = "pep/population",
  vars = c("NAME", "POP_2020", "POP_2021"), 
  region="state:36",
  vintage = 2021)
pop2a

pop2b <- pop2a |> 
  select(stname=NAME, POP_2020, POP_2021) |> 
  pivot_longer(-stname, values_to = "pop") |> 
  mutate(type="est",
         year=str_sub(name, -4, -1) |> as.integer()) |> 
  select(-name)
pop2b

popst <- bind_rows(pop1b, pop2b) |> 
  select(stname, type, year, pop)
popst

popst |> 
  filter(type=="est") |> 
  ggplot(aes(year, pop)) +
  geom_line() +
  geom_point()


df2 <- df1 |> 
  select(state=NAME, pop=value, date=DATE) |> 
  mutate(name=case_when(date==1 ~ "cen2010",
                        date==2 ~ "base2010",
                        TRUE ~ "est"),
         year=case_when(date %in% 1:2 ~ 2010,
                        date %in% 3:12 ~ date + 2007,
                        TRUE ~ 9999))






# OLD below here ----------------------------------------------------------



listCensusMetadata(
  name="pep/population",
  vintage = NULL,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

# https://www.hrecht.com/censusapi/news/index.html
# https://www.hrecht.com/censusapi/reference/index.html
df <- listCensusApis()
tmp <- df |> filter(str_detect(name, "pep"))
tmp2 <- tmp |> 
  filter(vintage %in% c(2019, 2021))

# 2019 vintage
# https://api.census.gov/data/2019/pep/population/variables.html
dict1 <- listCensusMetadata(
  name = "pep/population",
  vintage = 2019,
  type = "variables",
  include_values = TRUE)

pop1 <- getCensus(
  name = "pep/population",
  vars = c("NAME", "DATE_DESC", "POP"), 
  region="state:36",
  # regionin="state:36",
  # region = "county:*", 
  vintage = 2019)
pop1
# https://api.census.gov/data/pep/population?key=b27cb41e46ffe3488af186dd80c64dce66bd5e87&get=POP&time=2021

pop2 <- getCensus(
  name = "pep/population",
  vars = c("NAME", "POP_2020", "POP_2021"), 
  region="state:36",
  # regionin="state:36",
  # regionin = "state:36",
  # region = "county:*", 
  vintage = 2021)
pop2

popest <- getCensus(
  name = "pep/population",
  vintage = 2019,
  vars = c("POP", "DATE_DESC"),
  region = "state:*")
ht(df)
glimpse(df)
names(df)
count(df, str_sub(title, 1, 80), sort = TRUE) |> head(10)
# unique(df$title)
count(df, str_sub(name, 1, 80), sort = TRUE) |> head(50)
count(df, type)
tsdf <- df |> filter(type=="Timeseries")
aggdf <- df |> filter(type=="Aggregate")


df1 <- get_estimates(
  geography="state",
  # product = "population",
  variables = "POP",
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)
df1

df2 <- get_estimates(
  geography="state",
  # product = "population",
  variables = c("POP_2020", "POP_2021"),
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  year = 2021,
  state = "NY",
  county = NULL,
  # time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = TRUE)
df2

# https://api.census.gov/data/2021/pep/population?get=NAME%2CPOP%2CDENSITY&for=state%3A36



df2 <- df1 |> 
  select(state=NAME, pop=value, date=DATE) |> 
  mutate(name=case_when(date==1 ~ "cen2010",
                        date==2 ~ "base2010",
                        TRUE ~ "est"),
         year=case_when(date %in% 1:2 ~ 2010,
                        date %in% 3:12 ~ date + 2007,
                        TRUE ~ 9999))




# get data ----------------------------------------------------------------

# https://walker-data.com/tidycensus/reference/get_estimates.html
# mappings: 
#   https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html
# DATE_CODE: Estimate Date
# 1 = April 1, 2010 Census population or housing unit count
# 2 = April 1, 2010 population or housing unit estimates base
# 3 = July 1, 2010 population or housing unit estimate
# 4 = July 1, 2011 population or housing unit estimate
# 5 = July 1, 2012 population or housing unit estimate
# 6 = July 1, 2013 population or housing unit estimate
# 7 = July 1, 2014 population or housing unit estimate
# 8 = July 1, 2015 population or housing unit estimate
# 9 = July 1, 2016 population or housing unit estimate
# 10 = July 1, 2017 population or housing unit estimate
# 11 = July 1, 2018 population or housing unit estimate
# 12 = July 1, 2019 population or housing unit estimate

#   https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2021.html
# DATE_CODE: Estimate Date
# 1 = April 1, 2020 Census population or housing unit count
# 2 = April 1, 2020 population or housing unit estimates base
# 3 = July 1, 2020 population or housing unit estimate
# 4 = July 1, 2021 population or housing unit estimate

# population estimates

df0 <- get_estimates(
  geography="state",
  product = "population",
  variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)
df0

df1 <- get_estimates(
  geography="county",
  product = "population",
  variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)
df1

count(df1, NAME) # 62
count(df1, DATE) # 1-12 


df2 <- get_estimates(
  geography="place",
  product = "population",
  variables = NULL,
  # breakdown = NULL,
  # breakdown_labels = FALSE,
  # year = 2019,
  state = "NY",
  county = NULL,
  time_series = TRUE,
  output = "tidy",
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  key = NULL,
  show_call = FALSE)

df2 


count(df2, NAME) # 62
count(df2, DATE) # 1-12
df2 |> filter(str_detect(NAME, coll("New York City", ignore_case = TRUE))) # 3651000

censuspop1 <- bind_rows(df0, df1, df2 |> filter(GEOID=="3651000")) |> 
  select(censusfips=GEOID, censusname=NAME, name=variable, value, date=DATE) |> 
  mutate(name=str_to_lower(name),
         name=case_when(date==1 ~ paste0(name, "_cen2010"),
                        date==2 ~ paste0(name, "_base2010"),
                        TRUE ~ name),
         year=case_when(date %in% 1:2 ~ 2010,
                        date %in% 3:12 ~ date + 2007,
                        TRUE ~ 9999))
glimpse(censuspop1)
count(censuspop1, name)
count(censuspop1, censusname)
summary(censuspop1)

censuspop <- censuspop1 |> 
  select(-date)

saveRDS(censuspop, here::here("data", "census", "censuspop.rds"))

# get_estimates(
#   geography,
#   product = NULL,
#   variables = NULL,
#   breakdown = NULL,
#   breakdown_labels = FALSE,
#   year = 2019,
#   state = NULL,
#   county = NULL,
#   time_series = FALSE,
#   output = "tidy",
#   geometry = FALSE,
#   keep_geo_vars = FALSE,
#   shift_geo = FALSE,
#   key = NULL,
#   show_call = FALSE,
#   ...
# )



library(censusapi)
# https://www.hrecht.com/censusapi/news/index.html
# https://www.hrecht.com/censusapi/reference/index.html
df <- listCensusApis()
ht(df)
glimpse(df)
names(df)
count(df, str_sub(title, 1, 80), sort = TRUE) |> head(10)
# unique(df$title)
count(df, str_sub(name, 1, 80), sort = TRUE) |> head(50)
count(df, type)
tsdf <- df |> filter(type=="Timeseries")
aggdf <- df |> filter(type=="Aggregate")

tmp <- aggdf |> 
  filter(str_detect(title, coll("Population Estimate", ignore_case = TRUE)))

# pep/population https://api.census.gov/data/pep/population/variables.json not found
# https://api.census.gov/data/

listCensusMetadata(
  name="pep/population",
  vintage = NULL,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

listCensusMetadata(
  name="pep/population",
  vintage = NULL,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

df |> filter(str_detect(title, "Resilience"))

listCensusMetadata(
  name = "cre",
  vintage = 2019,
  type = "geographies") # US, state, county, tract

