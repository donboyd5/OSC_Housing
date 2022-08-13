# https://geocompr.robinlovelace.net/adv-map.html
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://r-spatial.org/

# maps
library(maps) # contains outlines, INCLUDES state.fips and county.fips with crosswalks as_tibble(state.fips)
library(mapdata) # higher resolution outlines
library(mapproj) # map projections collection

# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)

library(choroplethr)

library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# https://walker-data.com/census-r/an-introduction-to-tidycensus.html
library(tidycensus)
