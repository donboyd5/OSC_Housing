# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))

# locations --------------------------------------------------------------
# acsdir <- here::here("data", "acs")


# get data ----------------------------------------------------------------

acsdata <- readRDS(here::here("data", "acs", "acsdata.rds"))
nycentroids <- readRDS(here::here("data", "maps", "nycentroids.rds"))
nygeom <- readRDS(here::here("data", "maps", "nycentroids.rds"))
xwalk <- readRDS(here::here("data", "xwalks", "nycounty_xwalk.rds"))


# get owner percentages ----------------------------------------
# df1 <- acsdata |> 
#   filter(table=="B25003")
# 
# ownpct <- df1 |> 
#   mutate(var=factor(line, levels=1:3, labels=c("total", "owner", "renter"))) |> 
#   select(geoid, area, var, estimate) |> 
#   pivot_wider(names_from = var, values_from = estimate) |> 
#   mutate(ownpct=owner / total)
# 
# mapdata <- ownpct |> 
#   select(geoid, area, ownpct) |> 
#   left_join(xwalk, by="geoid")
# names(mapdata)
# count(mapdata, area, costate)
# 
# p <- nycomap(mapdata, mapvar="ownpct", maptitle="Homeownership percentage by county")
# p
# ggsave(here::here("report", "results", "owners_hhpct.png"), p, width = 10, height = 6, scale=1.5)


