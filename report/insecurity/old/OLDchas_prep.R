
# for each of several tables, create a wide dataframe that has certain
# guaranteed variables for each table:
#   category name (costburden, crowding, table, etc.)
#   id info (geoid, mininame, cnty, cntyname)
#   tenure (occ, own, rent)
#   bad thing
#   occ_all, own_all, rent_all  (#s of units)
#   own_pct, rent_pct, occ_pct (the "bad" thing as % of tenure total)
#   rent_share, 
# plus selected additional variables that are table-specific

# for information on which CHAS tables contain which data, use the
#   documentation file CHAS data dictionary 15-19.xlsx


# loads -------------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
chdir <- here::here("data", "chas")
chdir2019 <- path(chdir, "2015-2019")


# constants ---------------------------------------------------------------
geovars <- quos(c(geoid, stabbr, nytype, mininame,
                  rgn_num, rgn_code, rgn_osc, cnty, cntyname))

# tenure x race x cost burden  ----
# Table 9	 Tenure (2) by Race (7) by Housing Cost Burden (4)	Universe: All occupied housing units




# tenure x race x cost burden  ----
# Table 9	 Tenure (2) by Race (7) by Housing Cost Burden (4)	Universe: All occupied housing units
# we need to add all-race x cost-detail, and all-cost x race-detail

t9 <- readRDS(path(chdir2019, "nychas_t9.rds"))
glimpse(t9)
count(t9, atype, nytype)
count(t9, rgn_num, rgn_code, rgn_osc)


### quick checks ----
# t9_est1	Total	Total: Occupied housing units
# t9_est2	Subtotal	Owner occupied
# t9_est38	Subtotal	Renter occupied

tmp <- t9 |> 
  filter(atype=="state", stabbr=="NY", estmoe=="est", vnum %in% c(1, 2, 38))
tmp |> 
  select(vname, tenure, value, starts_with("desc"))

count(t9, desc1) # tenure
count(t9, desc2) # race
count(t9, desc3) # problem - housing cost burden
count(t9, desc4) # empty
count(t9, desc5) 

# get the basic desired data 
# we want all records where desc3 is not missing
# so we can calculate units with high costs

cost1 <- t9 |> 
  filter(dominant) |> 
  filter(estmoe=="est") |>  
  # filter(vnum %in% c(1, 2, 38) | !is.na(desc3)) |> 
  select(!!!geovars, tenure, vnum, vname, desc1:desc5, value)
  # select(geoid, stabbr, nytype, mininame,
  #        rgn_num, rgn_code, rgn_osc, cnty, cntyname, tenure, vnum, vname, desc1:desc5, value)
glimpse(cost1)
count(cost1, desc1) # tenure
count(cost1, desc2) # race
count(cost1, desc3) # problem - housing cost burden
count(cost1, desc4) # empty
count(cost1, desc5) # empty

# get distinct georecs for later use ----
georecs <- cost2 |> select(!!!geovars) |> distinct()
# 2,171 unique geographic recs

### establish factor info ----
# use distinct to retain original order
frace <- cost1 |> 
  select(desc2) |> 
  mutate(race=desc2) |> 
  distinct() |> 
  mutate(race_order=row_number(),
         race_var=c("all", "whitexhisp", "blackxhisp", "asianxhisp",
                    "aianxhisp", "pixhisp", "hisp", "other")) |> 
  select(race_order, race_var, race, desc2)
frace

fcost <- cost1 |> 
  select(desc3) |> 
  mutate(cost=desc3) |> 
  distinct() |> 
  mutate(cost_order=row_number(),
         cost_var=c("all", "costle30", "costgt30le50", "costgt50", "costnocomp")) |> 
  select(cost_order, cost_var, cost, desc3)
fcost

## create a bare file with minimal information, for summaries ----
cost2 <- cost1 |>
  left_join(frace, by="desc2") |>
  left_join(fcost, by="desc3") |> 
  select(geoid, mininame, tenure, cost_var, race_var, value)

glimpse(cost2)
count(cost2, tenure, race_var, cost_var) # we have cost-all for each race (rent, own)
count(cost2, tenure, cost_var, race_var) # we do NOT have race-all for each cost

## create all-race records for each non-summary cost category ----
count(cost2, tenure, cost_var, race_var)

racesums <- cost2 |> 
  filter(cost_var != "all") |> 
  group_by(geoid, tenure, cost_var) |> 
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") |> 
  mutate(race_var="all")
glimpse(racesums)
count(racesums, tenure, cost_var, race_var)

# ## create all-cost records for each non-summary race category ----
# count(cost2, tenure, race_var, cost_var)
# 
# costsums <- cost2 |> 
#   filter(race_var != "all") |> 
#   group_by(geoid, tenure, race_var) |> 
#   summarise(value=sum(value), .groups="drop") |> 
#   mutate(cost_var="all")
# glimpse(costsums)
# count(costsums, tenure, race_var, cost_var)

## create combined cost file before creating the "all" totals ----
# cost3 <- bind_rows(cost2, racesums, costsums)
cost3 <- bind_rows(cost2, racesums)
count(cost3, tenure, cost_var, race_var) # we have cost-all for each race, x occ
count(cost3, tenure, race_var, cost_var) # we have race-all for each cost, x occ

## create all-tenure (occ) totals ----
# if not, then we'll need to add renters and owners together in analyses
# get totals for all combinations of race and cost, without tenure
racecostsums <- cost3 |> 
  filter(tenure != "occ") |> 
  filter(!(race_var=="all" & cost_var=="all")) |> # we already have this
  # do not include tenure in the grouping
  group_by(geoid, cost_var, race_var) |>
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") |> 
  mutate(tenure="occ")
glimpse(racecostsums)
count(racecostsums, tenure, cost_var, race_var)
count(racecostsums, tenure, race_var, cost_var)

## create final file before regions, with geoinfo ----
cost4 <- bind_rows(cost3, racecostsums) |> 
  left_join(georecs |> 
              select(!!!geovars),
            by = c("geoid", "mininame"))
sum(is.na(cost4$geoid))
count(cost4, tenure, cost_var, race_var) # we have cost-all for each race, x occ
count(cost4, tenure, race_var, cost_var) # we have race-all for each cost, x occ

## add region summaries to the data ----
# put region info on the file
tmp <- count(cost4, tenure, race_var, cost_var) # 120 combos
rgns <- cost4 |> 
  filter(!is.na(rgn_code)) |> 
  group_by(rgn_num, rgn_code, rgn_osc,
           tenure, cost_var, race_var) |> 
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") |> 
  mutate(geoid="region", nytype="region", mininame=rgn_osc)

comboall <- cost4 |> 
  #filter(stabbr=="CA") |> 
  count(tenure, race_var, cost_var) # only 73 combos, why?
combost <- cost4 |> 
  filter(stabbr=="CA") |> 
  count(tenure, race_var, cost_var) # only 73 combos, why?
tmp <- bind_rows(cost4 |>
                   count(tenure, race_var, cost_var) |> 
                   distinct() |> 
                   mutate(rec="all"),
                 cost4 |>
                   filter(stabbr=="CA") |> 
                   count(tenure, race_var, cost_var) |> 
                   distinct() |> 
                   mutate(rec="ca")) |> 
  group_by(tenure, race_var, cost_var) |> 
  mutate(nrec=n()) |> 
  ungroup() |> 
  filter(nrec==1)

cost4 |> 
  filter(tenure=="occ", race_var=="all", cost_var=="all") |> 
  count(stabbr)
                   



count(rgns, tenure, race_var, cost_var) # 73 combos, 10 of each
count(rgns, tenure, cost_var, race_var) # 

tmp <- rgns |> mutate(data="nyonly") |> 
  bind_rows(rgns2 |> mutate(data="all"))

tmp2 <- tmp |> 
  group_by(tenure, race_var, cost_var) |> 
  mutate(n=n()) |> 
  ungroup()

tmp2 |> 
  filter(tenure=="occ", race_var=="aianxhisp", cost_var=="all")

# NOTE: this only has 73 combos - does not have the racesums or racecostsums 


## create the final file with values ----
# we now will have 10 more geographic records
cost5 <- bind_rows(cost4, rgns) |> 
  left_join(frace, by = "race_var") |> 
  left_join(fcost, by = "cost_var") |> 
  mutate(table="t9",
         tenure=factor(tenure, levels=c("occ", "own", "rent")),
         race_var=fct_reorder(race_var, race_order),
         cost_var=fct_reorder(cost_var, cost_order)) |> 
  select(!!!geovars, table, tenure, starts_with("race"), starts_with("cost"), value)

# final checks
glimpse(cost5)
count(cost2, tenure, race_var, cost_var) # 73 combos
count(cost3, tenure, race_var, cost_var) # 81 combos
count(cost4, tenure, race_var, cost_var) # 120 combos
count(cost4, tenure, race_var, cost_var) |> count(n)

count(cost5, tenure, race_var, cost_var)
count(cost5, tenure, race_var, cost_var) |> count(n)



# OLD ---------------------------------------------------------------------



# put the rest of the geoids on the costsums tibble
costsums2 <- costsums1 |> 
  left_join(cost2 |> select(!!!geovars) |> distinct(), 
            by="geoid")

## create final cost file before adding regions ----
cost4 <- cost3 |> 
  mutate(table="t9",
         tenure=factor(tenure, levels=c("occ", "own", "rent")),
         race_var=fct_reorder(race_var, race_order),
         cost_var=fct_reorder(cost_var, cost_order)) |> 
  select(!!!geovars, table, tenure, starts_with("race"), starts_with("cost"), value)
  
glimpse(cost4)
count(cost4, tenure)
count(cost4, race_var, race_order)
count(cost4, cost_var, cost_order)
count(cost4, tenure, race_var, cost_var)
count(cost4, tenure, cost_var, race_var)





tmp1 <- cost2 |> filter(tenure=="own", cost_var=="costle30", race_var=="whitexhisp")
tmp2 <- racesums |> filter(tenure=="own", cost_var=="costle30", race_var=="all")


tmp1a <- tmp1 |> filter(stabbr=="AL")
tmp2a <- tmp2 |> filter(stabbr=="AL")

cost2 |> 
  filter(nytype=="village", str_detect(mininame, "Cambridge")) |> 
  select(geoid) |>
  distinct()

gi <- "16000US3611825"

cost2 |> filter(geoid==gi, 
                tenure=="own", 
                cost_var=="costle30") |> 
  select(geoid, tenure, cost_var, race_var)

racesums |> filter(geoid==gi, 
                tenure=="own", 
                cost_var=="costle30") |> 
  select(geoid, tenure, cost_var, race_var)



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