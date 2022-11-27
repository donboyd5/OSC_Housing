
# https://www.huduser.gov/apps/public/pums/home

# To advance the Obama Administration's Open Government Initiative, PD&R is
# releasing statistical samples of tenant-level data to qualified researchers.
# These Public Use Microdata Sample (PUMS) datasets cover HUD's largest rental
# assistance programs: the Housing Choice Voucher Program, Public Housing,
# Project-based Section 8, and the Section 202/811 Programs. This dataset will
# not identify individual households in the sample. The purpose of the data
# release is to help the research community better understand the
# characteristics of households receiving assistance.

# The dataset will include data on family type, household income, race, gender,
# and other household and geographic characteristics. The sample size (5
# percent) is large enough to be statistically valid and representative of
# states and the nation as a whole while small enough to preserve privacy. Data
# dictionaries provide information on the sampling design and variables.

# The PUMS_PIH_XX datasets contain data for Office of Public and Indian Housing
# programs: Housing Choice Vouchers, and Public Housing.

# The PUMS_MF_XX datasets contain data for Office of Housing multifamily
# programs: Project Based Section 8, Section 202, and Section 811. XX denotes
# data for year 20XX. There are SAS and CSV versions for each dataset.


# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# constants ---------------------------------------------------------------
dpums <- here::here("data", "hud", "pums")


# get pums data --------------------------------------------------------

# integer: h6,mbrs,bdrms
year <- 2013

f <- function(year){
  print(year)
  y2 <- str_sub(year, 3, 4)
  fn1 <- paste0("pums_mf_", y2, ".csv")
  fn2 <- paste0("pums_pih_", y2, ".csv")
  # df1 <- read_csv(path(dpums, fn1), col_types = cols(.default = col_character()))
  # df2 <- read_csv(path(dpums, fn2), col_types = cols(.default = col_character()))
  df1 <- vroom(path(dpums, fn1), col_types = cols(.default = col_character()))
  df2 <- vroom(path(dpums, fn2), col_types = cols(.default = col_character()))
  bind_rows(df1 |> lcnames() |> mutate(office="mf"),
            df2 |> lcnames() |> mutate(office="pih")) |> 
    mutate(year=as.integer(!!year),
           across(c(h6, mbrs, bdrms), as.integer), # I checked and h6 is ok to convert
           weight=as.numeric(weight)) |> 
    relocate(office, year, .before=prog) |> 
    select(-strata)  # not needed
}

df <- f(2021)
glimpse(df)
summary(df)
count(df, year, office)
count(df, h6)
count(df, mbrs)
count(df, bdrms)
f(2013)

df <- map_dfr(c(2009, 2010, 2012:2021), f) # no 2011
problems(df)
memory()
# count(df, h6)
glimpse(df)

saveRDS(df, path(dpums, "pihmf_pums.rds"))


# explore -----------------------------------------------------------------
pihmf <- readRDS(path(dpums, "pihmf_pums.rds"))

ny <- pihmf |> 
  filter(fipst=="36")
count(ny, year)

ny2 <- ny |> 
  filter(is.na(poverty), is.na(ur), is.na(race_eth))
count(ny2, year) # 2009 looks fishy



