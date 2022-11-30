
# hcv by tract
# https://hudgis-hud.opendata.arcgis.com/datasets/8d45c34f7f64433586ef6a448d00ca12_0/explore?location=24.773379%2C0.315564%2C2.89&showTable=true



# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

# locations ---------------------------------------------------------------
dhud <- here::here("data", "hud")

# constants ---------------------------------------------------------------
# fnz <- r"(E:\R_projects\OSC_Housing\data\hud\QCT2023CSV.zip)"
fn1 <- "Housing_Choice_Vouchers_by_Tract.csv"
(fpath <- path(dhud, fn1))


# get data ----------------------------------------------------------------
xwalk <- readRDS(here::here("data", "xwalks", "nycounty_xwalk.rds"))

df1 <- read_csv(fpath, col_types = cols(.default = col_character()))
glimpse(df1)

df2 <- df1 |> 
  lcnames() |> 
  filter(state=="36")

count(df2, state, county, sort=TRUE)

