# libraries ---------------------------------------------------------------

library(tidyverse)
tprint <- 100  # default tibble print is 50
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

# general utilities
library(fs)
library(archive)

# tools
library(vroom)
library(readxl)
library(openxlsx) # useful for reading from urls
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(tidycensus)

# boyd libraries
library(btools)
library(bdata)
library(bggtools)
library(bmaps)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)

# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)
library(gtExtras)



