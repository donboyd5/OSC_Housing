

# loads -------------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
chdir <- here::here("data", "chas")
chdir2019 <- path(chdir, "2015-2019")

# get data ----------------------------------------------------------------
# Table 9	 Tenure (2) by Race (7) by Housing Cost Burden (4)	Universe: All occupied housing units

t9 <- readRDS(path(chdir2019, "nychas_t9.rds"))
glimpse(t9)
count(t9, atype, nytype)
count(t9, rgn_num, rgn_code, rgn_osc)

# quick checks ----
# t9_est1	Total	Total: Occupied housing units
# t9_est2	Subtotal	Owner occupied
# t9_est38	Subtotal	Renter occupied

tmp <- t9 |> 
  filter(atype=="state", stabbr=="NY", estmoe=="est", vnum %in% c(1, 2, 38))
tmp |> 
  select(vname, tenure, value, starts_with("desc"))

# create a cost burden data file ----
# t9_est1	Total	Total: Occupied housing units
# t9_est2	Subtotal	Owner occupied
# t9_est38	Subtotal	Renter occupied
# we want all records where desc3 is not missing
# so we can calculate units with high costs

cost1 <- t9 |> 
  filter(dominant) |> 
  filter(estmoe=="est") |>  
  filter(vnum %in% c(1, 2, 38) | !is.na(desc3)) |> 
  select(geoid, stabbr, atype, nytype, shortname, mininame,
         rgn_num, rgn_code, rgn_osc, vnum, vname, value, cnty, cntyname, tenure, desc1:desc5)
glimpse(cost1)
count(cost1, desc1) # need
count(cost1, desc2) # don't need but keep
count(cost1, desc3) 
count(cost1, desc4) # drop
count(cost1, desc5) # drop


## establish cost factor info ----
# use distinct to retain original order
fcost <- cost1 |> 
  select(desc3) |> 
  distinct() |> 
  mutate(order=row_number(),
         vdesc3=c("all", "costle30", "costgt30le50", "costgt50", "costnocomp")) |> 
  select(order, vdesc3, desc3)
fcost

cost2 <- cost1 |> 
  select(-c(desc4, desc5)) |> 
  left_join(fcost |> select(-order), by="desc3") |> 
  mutate(vdesc3=factor(vdesc3, levels=fcost$vdesc3))
count(cost2, vdesc3, desc3)
count(cost2, tenure, vdesc3, desc3)
glimpse(cost2)

# now collapse across race categories because all-race subtotals are not given
cost3 <- cost2 |> 
  group_by(geoid, stabbr, nytype, shortname, mininame,
           cnty, cntyname,
           rgn_num, rgn_code, rgn_osc,
           tenure, vdesc3, desc3) |> 
  summarise(value=sum(value), .groups="drop")

# add region summaries to the data
rgns <- cost3 |> 
  filter(stabbr=="NY", nytype=="county") |> 
  group_by(stabbr, rgn_num, rgn_code, rgn_osc,
           tenure, vdesc3, desc3) |> 
  summarise(value=sum(value), .groups="drop") |> 
  mutate(nytype="region", mininame=rgn_osc)

cost4 <- bind_rows(cost3, rgns)

glimpse(cost4)
count(cost4, nytype)
count(cost4, rgn_num, rgn_code, rgn_osc)
count(cost4 |> filter(nytype=="region"), rgn_num, rgn_code, rgn_osc, mininame)
count(cost4 |> filter(nytype=="state"), stabbr, shortname, mininame)
count(cost4 |> filter(nytype=="city", str_detect(shortname, "New York")), stabbr, shortname, mininame)

saveRDS(cost4, here::here("data", "cost.rds"))

cost <- readRDS(here::here("data", "cost.rds"))


