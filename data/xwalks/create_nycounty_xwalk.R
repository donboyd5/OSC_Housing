
# somehow I lost the code I used to create the NY county xwalk.
# for now, I recreate it by writing it to an Excel file and modifying from there
source(here::here("r", "libraries.r"))
library(writexl)

# ONETIME save the file I created previously ----
# xwalk <- readRDS(here::here("data", "crosswalks", "nycounty_xwalk.rds"))
# saveRDS(xwalk, here::here("data", "crosswalks", "nycounty_xwalk_ORIGINAL.rds"))
# write_xlsx(xwalk, here::here("data", "crosswalks", "nycounty_xwalk_data.xlsx"))

# create a better xwalk ----
xwalk1 <- readRDS(here::here("data", "crosswalks", "nycounty_xwalk_ORIGINAL.rds"))
xwalk2 <- xwalk1 |> 
  mutate(rgn_osc=str_remove(rgn_oscQ, " Region"))
xwalk2
saveRDS(xwalk2, here::here("data", "crosswalks", "nycounty_xwalk.rds"))
