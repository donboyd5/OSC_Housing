
# prepare data needed for the cost chapter

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

# tabshells <- readRDS(here::here("data", "acs", "tabshells_2019.rds"))
tabdict <- readRDS(path(dchas2019, "chasdict.rds"))
glimpse(tabdict)

tmp <- tabdict |> filter(table=="T9", str_detect(vname, "est"))
t9 <- readRDS(path(dchwide, "tab9_enhanced.rds"))
glimpse(t9)
# we need cnty, cntyname maybe shortname also

# get all-races cost burden
# T9_est1 hunits
# T9_est2 owner occupied
# T9_est38 renter occupied
tmp |> 
  filter(str_detect(desc1, "Renter")) |> 
  filter(str_detect(desc3, "greater than")) |> 
  select(vname, desc2, desc3) |> 
  mutate(desc2=str_sub(desc2, 20, 40))
# 5, 6, 10, 11, 15, 16, 
(owncb3050vars <- paste0("T9_est", seq(5, 35, 5)))
(owncb50pvars <- paste0("T9_est", seq(6, 36, 5)))
(rentcb3050vars <- paste0("T9_est", seq(41, 71, 5)))
(rentcb50pvars <- paste0("T9_est", seq(42, 72, 5)))

t9cb <- t9 |> 
  mutate(owncb3050 = rowSums(across(all_of(c(owncb3050vars)))),
         owncb50p = rowSums(across(all_of(c(owncb50pvars)))),
         rentcb3050 = rowSums(across(all_of(c(rentcb3050vars)))),
         rentcb50p = rowSums(across(all_of(c(rentcb50pvars)))),
  ) |> 
  relocate(owncb3050, owncb50p, rentcb3050, rentcb50p, .before=T9_est1)
glimpse(t9cb)

t9cb_full <- t9cb |> 
  select(nygeotype, stabbr, geoid, affgeoid, shortname, fullname, countyfp, countyname,
         T9_est1, T9_est2, T9_est38, owncb3050, owncb50p, rentcb3050, rentcb50p) |> 
  mutate(own_cost30=owncb3050 + owncb50p,
         rent_cost30=rentcb3050 + rentcb50p,
         alltenure_cost30=own_cost30 + rent_cost30,
         own_pct30=own_cost30 / T9_est2,
         rent_pct30=rent_cost30 / T9_est38,
         alltenure_pct30=alltenure_cost30 / T9_est1,
         renter_share=T9_est38 / T9_est1)
count(t9cb_full, nygeotype)
saveRDS(t9cb_full, here::here("report", "results", "t9cb_full.rds"))


# tabdata for all types
# nytype groups: state, region, county, city, town, village
tabdata <- t9cb_full |> 
  filter(nygeotype %in%
           c("nation", "state", "region", "county", "city", "town", "village",
             "rgn_cities", "rgn_villages", "rgn_xcityvill")) |>
  rename(allunits=T9_est1,
         own_pct=own_pct30, rent_pct=rent_pct30, alltenure_pct=alltenure_pct30) |> 
  group_by(nygeotype) |> 
  mutate(ownrank=row_number(desc(own_pct)),
         rentrank=row_number(desc(rent_pct)),
         alltenurerank=row_number(desc(alltenure_pct)),
         shortname=ifelse(nygeotype=="state" & stabbr=="NY",
                          paste0(shortname, " State"),
                          shortname))  |> 
  select(nygeotype, stabbr, shortname, countyname, 
         allunits,
         own_pct, rent_pct, alltenure_pct, 
         ownrank, rentrank, alltenurerank,
         renter_share) |> 
  ungroup() |> 
  arrange(desc(alltenure_pct))

saveRDS(tabdata, here::here("report", "results", "cb_tabdata.rds")) # for appendix