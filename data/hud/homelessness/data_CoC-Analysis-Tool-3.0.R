
# the only place I could find data on CoCs was hidden in this analysis tool
# https://www.hudexchange.info/resource/5787/coc-analysis-tool-race-and-ethnicity/
# https://www.hudexchange.info/resources/documents/CoC-Analysis-Tool-3.0.xlsb
# I downloaded the file, saved as xlsx, and unhid several sheets
# you can look at the Dashboard sheet to reverse engineer some of the numbers they use


# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

# locations ---------------------------------------------------------------
dhless <- here::here("data", "hud", "homelessness")
fn <- "CoC-Analysis-Tool-3.0.xlsb.xlsx"
coc_sheet <- "CoC ACS2019 Data"
st_sheet <- "State ACS2019 Data"

df1 <- read_excel(path(dhless, fn), sheet=coc_sheet)
glimpse(df1)
ns(df1)

# to get total population
# In the race category, add:
  # asian	Asian/Pacific Islander
  # black	Black
  # native	Native American/Alaskan
  # white	White
  # other	Other/Multi-Racial
# =INDEX(CoCData,MATCH(Selected_CoC,CoCData[cocnum],0),
#                MATCH("tract"&$B54&E$46&E$47&"_"&DASHBOARD!$C54,CoCData[#Headers],0))
# b54: null, e46: null, e47: , c54: 

# NY-500
# tract_white	 564,617 
# tract_black	 113,808 
# tract_native	 3,040 
# tract_asian	 27,405 
# tract_other	 34,471 
#  sum of this is pop

df1 |> 
  filter(cocnum=="NY-500") |> 
  pivot_longer(-c(cocnum, state))

saveRDS(df1, here::here("data", "hud", "homelessness", "cocdata_raw.rds"))

df2 <- df1 |> 
  mutate(pop=tract_white + tract_black + tract_native + tract_asian + tract_other) |> 
  select(cocnum, stabbr=state, pop, 
         tract_white, tract_black, tract_native, tract_asian, tract_other)
summary (df2) # nothing missing

df2 |> 
  filter(stabbr=="NY") |> 
  arrange(desc(pop))

saveRDS(df2, here::here("data", "hud", "homelessness", "cocdata_pop.rds"))



