

# loads -------------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
chdir <- here::here("data", "CHAS")
chdir2019 <- path(chdir, "2015-2019")

# get data ----------------------------------------------------------------
# Table 3	 Tenure (2) by Housing Unit Problem Severity (7) by Household Income (5)	Universe: All occupied housing units
# desc 2 has  AND lacking complete plumbing or kitchen facilities
# use vnum 1, 2, 45, or lacking...

t3 <- readRDS(path(chdir2019, "nychas_t3.rds"))
glimpse(t3)
count(t3, nytype)
count(t3, rgn_num, rgn_code, rgn_osc)

# quick checks ----
# t9_est1	Total	Total: Occupied housing units
# t9_est2	Subtotal	Owner occupied
# t9_est38	Subtotal	Renter occupied

df1 <- t3 |> 
  filter(estmoe=="est") |> 
  filter(vnum %in% c(1, 2, 45) |
           (str_detect(desc2, "plumbing") & is.na(desc3)))

count(df1, desc1) # tenure
count(df1, desc2) # lacking, or not
count(df1, desc3) # hamfi
count(df1, desc4) # drop
count(df1, desc5) # drop

tmp <- t3 |> 
  filter(atype=="state", stabbr=="NY", estmoe=="est", vnum %in% c(1, 2, 45))
df1 |> 
  filter(nytype=="state", stabbr=="NY") |> 
  select(vname, tenure, value, starts_with("desc"))

# create a plumbing kitchen data file ----

plumb1 <- t3 |> 
  filter(dominant) |> 
  filter(estmoe=="est") |>  
  filter(vnum %in% c(1, 2, 45) |
           (str_detect(desc2, "plumbing") & is.na(desc3))) |> # summary recs for plumbing
  select(geoid, stabbr, atype, nytype, shortname, mininame,
         rgn_num, rgn_code, rgn_osc, vnum, vname, value, cnty, cntyname, tenure, desc1, desc2)
glimpse(plumb1)
count(plumb1, desc1) # need
count(plumb1, desc2) # need
count(plumb1, desc1, desc2)


## establish cost factor info ----
# use distinct to retain original order
fplumb <- plumb1 |> 
  select(desc2) |> 
  distinct() |> 
  mutate(order=row_number(),
         vdesc2=c("all", "lackplumb")) |> 
  select(order, vdesc2, desc2)
fplumb

plumb2 <- plumb1 |>
  left_join(fplumb |> select(-order), by="desc2") |> 
  mutate(vdesc2=factor(vdesc2, levels=fplumb$vdesc2))
count(plumb2, vdesc2, desc2)
count(plumb2, tenure, vdesc2, desc2)
glimpse(plumb2)

# add region summaries to the data
rgns <- plumb2 |> 
  filter(stabbr=="NY", nytype=="county") |> 
  group_by(stabbr, rgn_num, rgn_code, rgn_osc,
           tenure, vdesc2, desc2) |> 
  summarise(value=sum(value), .groups="drop") |> 
  mutate(nytype="region", mininame=rgn_osc)

plumb3 <- bind_rows(plumb2, rgns)

glimpse(plumb3)
count(plumb3, nytype)
count(plumb3, rgn_num, rgn_code, rgn_osc)
count(plumb3 |> filter(nytype=="region"), rgn_num, rgn_code, rgn_osc, mininame)
count(plumb3 |> filter(nytype=="state"), stabbr, shortname, mininame)
count(plumb3 |> filter(nytype=="city", str_detect(shortname, "New York")), stabbr, shortname, mininame)

saveRDS(plumb3, here::here("data", "plumbkitch.rds"))

plumbkitch <- readRDS(here::here("data", "plumbkitch.rds"))
