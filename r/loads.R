
# libraries
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))

# data
## map-related data
nycentroids <- readRDS(here::here("data", "maps", "nycentroids.rds"))
# xwalk <- readRDS(here::here("data", "xwalks", "nycounty_xwalk.rds"))
nycos_shape <- readRDS(here::here("data", "maps", "nycos_shape.rds"))

acsdata <- readRDS(here::here("data", "acs", "acsdata.rds"))
jchs <- readRDS(here::here("data", "jchs", "price_income_jchs.rds"))
