


# loads -------------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------

# get data ----------------------------------------------------------------

fn <- "HPI_AT_BDL_county.xlsx"
df1 <- read_excel(here::here("data", "fhfa", fn),
           skip=6)
df1

df2 <- df1 |> 
  setNames(c("stabbr", "cntyname", "geoid", "year", "pchya", "hpi", "hpib1990", "hpib2000")) |> 
  mutate(year=as.integer(year), 
         across(c(pchya, starts_with("hpi")), as.numeric),
         cntyname=ifelse(str_detect(cntyname, "St Lawrence"), "St. Lawrence", cntyname)) |> 
  ungroup()

saveRDS(df2, here::here("data", "fhfa", "hpi_county.rds"))


hpi <- readRDS(here::here("data", "fhfa", "hpi_county.rds"))

df2 |> 
  filter(stabbr=="NY") |> 
  group_by(geoid) |> 
  mutate(pch=hpi / hpi[match(year - 1, year)] * 100 - 100)

df2 |> filter(geoid=="36061")

df2 |> 
  filter(stabbr=="NY") |> 
  group_by(geoid) |> 
  mutate(pch5ya=hpi / hpi[match(year - 5, year)] - 1) |> 
  filter(year==2021) |> 
  arrange(desc(pch5ya))



