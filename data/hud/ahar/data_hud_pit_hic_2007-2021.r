
# https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# locations ---------------------------------------------------------------

dahar <- here::here("data", "hud", "ahar")

# constants ---------------------------------------------------------------


# functions ---------------------------------------------------------------



# PIT initial gathering of data -------------------------------------------
fn <- "2007-2021-PIT-Counts-by-State_djb.xlsx"
df1 <- read_excel(path(dahar, fn), sheet="2020")

year <- 2020

fstate_year <- function(year) {
  # read state data for a year
  print(year)
  sfn <- "2007-2021-PIT-Counts-by-State.xlsx"
  # cfn <- "2007-2021-PIT-Counts-by-CoC.xlsx"
  df1 <- read_excel(path(dahar, sfn), 
                    sheet=as.character(year), 
                    col_names = TRUE,
                    col_types = "text")
  df2 <- df1 |> 
    rename(stabbr=1, ncocs=2) |> 
    pivot_longer(cols=-c(stabbr, ncocs)) |> 
    mutate(year=as.integer(!!year),
           rectype="state",
           ncocs=as.integer(ncocs),
           value=as.numeric(value)) |> 
    separate(name, into = c("vdesc", "varyear"), sep=",") |> 
    mutate(varyear=as.integer(varyear))
  
  if(sum(df2$year) != sum(df2$varyear)) rlang::abort("ERROR in variable name for state.")
  
  df3 <- df2 |> 
    select(stabbr, rectype, year, ncocs, vdesc, value)
  df3
}

year <- 2008

fcoc_year <- function(year) {
  # read coc data for a year
  print(year)
  # sfn <- "2007-2021-PIT-Counts-by-State.xlsx"
  cfn <- "2007-2021-PIT-Counts-by-CoC.xlsx"
  df1 <- read_excel(path(dahar, cfn), 
                    sheet=as.character(year), 
                    col_names = TRUE,
                    col_types = "text")
  
  if(year==2021) {
    df2 <- df1 |> 
      rename(cocnum=1, cocname=2, coctype=3, counttype=4)
    } else{
      df2 <- df1 |>
        rename(cocnum=1, cocname=2) |> 
        mutate(coctype=NA_character_, counttype=NA_character_) |> 
        select(cocnum, cocname, coctype, counttype, everything())
    }
  
  df3 <- df2 |> 
    mutate(stabbr=str_sub(cocnum, 1, 2)) |> 
    pivot_longer(cols=-c(stabbr, cocnum, cocname, coctype, counttype)) |> 
    mutate(year=as.integer(!!year),
           rectype="coc",
           value=as.numeric(value)) |> 
    separate(name, into = c("vdesc", "varyear"), sep=",") |> 
    mutate(varyear=as.integer(varyear))
  
  if(sum(df3$year) != sum(df3$varyear)) rlang::abort("ERROR in variable name for CoC")
  
  df4 <- df3 |> 
    select(stabbr, rectype, year, cocnum, cocname, coctype, counttype, vdesc, value)
  df4
}

df1 <- fstate_year(2008)
glimpse(df1)

df2 <- fcoc_year(2008)
glimpse(df2)

fall <- function(years){
  print("states")
  df1 <- map_dfr(years, fstate_year)
  print("CoCs")
  df2 <- map_dfr(years, fcoc_year)
  df3 <- bind_rows(df1, df2)
  df3 |> 
    mutate(vdesc=str_trim(vdesc)) |> 
    relocate(vdesc, value, .after=counttype)
}

pit_raw <- fall(2007:2021)
saveRDS(pit_raw, path(dahar, "pit_raw.rds"))


# clean pit raw data ------------------------------------------------------
pit1 <- readRDS(path(dahar, "pit_raw.rds"))
glimpse(pit1)
summary(pit1)
count(pit1, year)

tmp <- count(pit1, vdesc, year) |> 
  arrange(year) |> 
  pivot_wider(names_from = year, values_from = n) |> 
  arrange(vdesc)

# key variables:
# almost every year (2021 questionable)
#   Overall Homeless
#   Sheltered Total Homeless
#   Unsheltered Homeless

vmap <- read_csv(
"vdesc, vname
Overall Homeless, hl_total
Sheltered Total Homeless, hl_shelter
Unsheltered Homeless, hl_unshelter
")
vmap

pit1 |> filter(stabbr=="* ")
tmp <- pit1 |> filter(is.na(stabbr))

pit2 <- pit1 |> 
  mutate(stabbr=str_trim(stabbr),
         stabbr=case_when(stabbr=="*" ~ "countnote",
                          stabbr=="a" ~ "footnote",
                          stabbr=="Total" ~ "US",
                          is.na(stabbr) &
                            is.na(cocnum) &
                            cocname=="Total" ~ "US",
                          TRUE ~ stabbr)
           ) |> 
  left_join(vmap, by = join_by(vdesc)) |> 
  left_join(stcodes |> select(stabbr, stname),
            by = join_by(stabbr)) |> 
  mutate(stname=case_when(stabbr=="AS" ~ "American Samoa",
                          stabbr=="GU" ~ "Guam",
                          stabbr=="MP" ~ "Northern Mariana Islands",
                          TRUE ~ stname)
         ) |> 
  relocate(stname, .after=stabbr) |> 
  relocate(vname, .before=value)
glimpse(pit2)
count(pit2, stabbr, stname)

saveRDS(pit2, path(dahar, "pit.rds"))

pit1 <- readRDS(path(dahar, "pit.rds"))

pit2 <- pit1 |> 
  filter(!is.na(vname)) |> 
  left_join(spop.a |> 
              filter(type=="intercensal") |> 
              select(stabbr, year, pop=value),
            by = join_by(stabbr, year))
saveRDS(pit2, path(dahar, "pit_report.rds"))

# quick check
pit2 |> 
  filter(rectype=="state", stabbr=="NY") |> 
  select(stabbr, year, vname, value) |> 
  pivot_wider(names_from = vname) |> 
  mutate(diff=hl_total - hl_shelter - hl_unshelter)


# explore -----------------------------------------------------------------
pit1 <- readRDS(path(dahar, "pit_report.rds"))

pit1 |> 
  filter(rectype=="state", stabbr=="NY") |> 
  filter(year < 2021) |>  # big dropoff
  select(stabbr, year, vname, value) |> 
  ggplot(aes(year, value, colour=vname)) +
  geom_line() +
  geom_point()

sts <- c(state.abb, "DC", "US")
pit1 |> 
  filter(rectype=="state", year==2020) |>
  filter(stabbr %in% sts) |> 
  select(stabbr, stname, year, vname, value) |> 
  pivot_wider(names_from = vname) |> 
  mutate(unpct=hl_unshelter / hl_total) |> 
  arrange(desc(unpct))

big5us <- pit1 |> 
  filter(year==2020, vname=="hl_total", rectype=="state") |> 
  arrange(desc(value)) |> 
  filter(row_number() <= 6) |> 
  select(stabbr, stname, rectype, year, ncocs, vname, value, pop)
big5us

pit1 |> 
  filter(vname=="hl_total", rectype=="state", stabbr %in% big5us$stabbr) |> 
  mutate(ivalue=value / value[year==2007], .by=c(stabbr)) |> 
  ggplot(aes(year, ivalue, colour=stabbr)) +
  geom_line() +
  geom_point() 

sts <- c(state.abb, "DC", "US")

percap <- pit2 |>
  filter(vname=="hl_total", rectype=="state", stabbr %in% sts, year <= 2020) |> 
  select(stabbr, stname, year, hl_total=value) |> 
  left_join(spop.a |> 
              filter(type=="intercensal") |> 
              select(stabbr, year, pop=value),
            by = join_by(stabbr, year)) |> 
  mutate(percap=hl_total / pop)

percap |> 
  filter(year==2020) |> 
  arrange(desc(percap))
  


# OLD ONETIME: downloads ------------------------------------------------------
uhic_coc <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-HIC-Counts-by-CoC.xlsx"
uhic_state <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-HIC-Counts-by-State.xlsx"
upit_coc <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-CoC.xlsx"
upit_state <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-State.xlsx"
# https://www.huduser.gov/portal/sites/default/files/xls/2007-2021-PIT-Counts-by-CoC.xlsx

# https://www.hudexchange.info/resource/3031/pit-and-hic-data-since-2007/

files <- c(uhic_coc, uhic_state, upit_coc, upit_state)

file_info(uhic_coc) |> glimpse()
# path_sanitize(uhic_coc)
basename(uhic_coc)

uhic_coc |> 
  fs_path() |> 
  path_ext_remove()

for(file in files){
  fname <- basename(file)
  print(fname)
  download.file(file, here::here("data", fname), mode="wb")
}

