# https://geocompr.robinlovelace.net/adv-map.html
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://r-spatial.org/
# https://cran.r-project.org/web/packages/censusxy/vignettes/censusxy.html

# maps
library(maps) # contains outlines, INCLUDES state.fips and county.fips with crosswalks as_tibble(state.fips)
library(mapdata) # higher resolution outlines
library(mapproj) # map projections collection

# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)

library(choroplethr)
library(viridis)

library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# https://walker-data.com/census-r/an-introduction-to-tidycensus.html
# https://walker-data.com/tidycensus/articles/other-datasets.html
library(tidycensus)
