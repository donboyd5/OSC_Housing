

# loads -------------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
chdir <- here::here("data", "CHAS")
chdir2019 <- path(chdir, "2015-2019")

# get data ----------------------------------------------------------------
# Table 10	 Tenure (2) by Overcrowding (3) by Household Income (5) by Family Status (3)	Universe: All occupied housing units

t10 <- readRDS(path(chdir2019, "nychas_t10.rds"))
glimpse(t10)
count(t10, atype, nytype)
count(t10, rgn_num, rgn_code, rgn_oscQ)

# quick checks ----
# T10	T10_est1	t10_est1	Total	Total: Occupied housing units
# T10	T10_est2	t10_est2	Subtotal	Owner occupied
# T10	T10_est66	t10_est66	Subtotal	Renter occupied

df <- t10 |> 
  filter(atype=="state", stabbr=="NY", estmoe=="est")

tmp <- t10 |> 
  filter(stabbr=="NY") |>
  filter(atype=="cousub") |> 
  filter(estmoe=="est") |>  
  filter(vnum %in% c(1, 2, 66)) |> 
  select(geoid, stabbr, atype, shortname, vname, value, cnty, cntyname) |> 
  pivot_wider(names_from = vname) |> 
  mutate(rpct=t10_est66 / t10_est1) |> 
  arrange(desc(rpct))
tmp

# create a crowding data file ----
# T10	T10_est1	t10_est1	Total	Total: Occupied housing units
# T10	T10_est2	t10_est2	Subtotal	Owner occupied
# T10	T10_est3	t10_est3	Subtotal	Owner occupied	 AND persons per room is less than or equal to 1
# T10	T10_est24	t10_est24	Subtotal	Owner occupied	 AND persons per room is greater than 1 but less than or equal to 1.5
# T10	T10_est45	t10_est45	Subtotal	Owner occupied	 AND persons per room is greater than 1.5
# T10	T10_est66	t10_est66	Subtotal	Renter occupied
# T10	T10_est67	t10_est67	Subtotal	Renter occupied	 AND persons per room is less than or equal to 1
# T10	T10_est88	t10_est88	Subtotal	Renter occupied	 AND persons per room is greater than 1 but less than or equal to 1.5
# T10	T10_est109	t10_est109	Subtotal	Renter occupied	 AND persons per room is greater than 1.5
vnums <- c(1:3, 24, 45, 66, 67, 88, 109)

crowding1 <- t10 |> 
  filter(dominant) |> 
  filter(estmoe=="est") |>  
  filter(vnum %in% vnums) |> 
  select(geoid, stabbr, atype, nytype, shortname, mininame,
         rgn_num, rgn_code, rgn_osc, vnum, vname, value, cnty, cntyname, tenure, desc1:desc5)
tmp <- count(crowding1, tenure, desc1, desc2, desc3, desc4, desc5, vnum, vname)
count(crowding1, desc1, desc2)
count(crowding1, desc2)

## establish crowding factor info ----
fcrowd <- crowding1 |> 
  select(desc2) |> 
  distinct() |> 
  mutate(order=row_number(),
         vdesc2=c("all", "pprle1", "pprgt1le1p5", "pprgt1p5")) |> 
  select(order, vdesc2, desc2)
fcrowd

crowding2 <- crowding1 |> 
  left_join(fcrowd |> select(-order), by="desc2") |> 
  mutate(vdesc2=factor(vdesc2, levels=fcrowd$vdesc2))
count(crowding2, vdesc2, desc2)
count(crowding2, tenure, vdesc2, desc2)
glimpse(crowding2)


# add region summaries to the data
rgns <- crowding2 |> 
  filter(stabbr=="NY", nytype=="county") |> 
  group_by(stabbr, rgn_num, rgn_code, rgn_osc,
           tenure, vdesc2, desc2) |> 
  summarise(value=sum(value), .groups="drop") |> 
  mutate(nytype="region", mininame=rgn_osc)

crowding3 <- bind_rows(crowding2, rgns)


saveRDS(crowding3, here::here("data", "crowding.rds"))

crowding <- readRDS(here::here("data", "crowding.rds"))



## start with crowding as % of occupied ----
idvars <- c("geoid", "stabbr" , "atype", "nytype", "shortname", "mininame", "cnty", "cntyname")
crowd3 <- crowd2 |> 
  # CAUTION - drop the grand total before adding all
  filter(tenure %in% c("own", "rent")) |>
  group_by(across(all_of(idvars)), vdesc2, desc2) |> 
  summarise(value=sum(value), .groups="drop")

crowdwide <- crowd3 |> 
  select(all_of(idvars), vdesc2, value) |> 
  pivot_wider(names_from = vdesc2) |> 
  mutate(crowdgt1_pct=(pprgt1le1p5 + pprgt1p5) / all,
         crowdgt1p5_pct=pprgt1p5 / all)

### state-level crowding ----
crowdwide |> 
  filter(atype=="state") |> 
  select(-cnty, -cntyname, -atype, -nytype) |> 
  arrange(desc(crowdgt1p5_pct)) # NY has 4th-most crowding after HI CA AK

### local-level crowding ----
# Flanders is crowded and poor
# https://www.nytimes.com/2002/10/27/nyregion/drums-along-the-peconic.html
crowdwide |> 
  filter(stabbr=="NY") |> 
  filter(all >= 500) |> 
  arrange(desc(crowdgt1p5_pct)) |> 
  select(-atype, -stabbr, -cnty, -starts_with("ppr"))

crowdwide |> 
  filter(stabbr=="NY", cnty=="115") |> 
  filter(all >= 500) |> 
  arrange(desc(crowdgt1p5_pct)) |> 
  select(-atype, -stabbr, -cnty, -starts_with("ppr"))

crowdwide |> 
  filter(stabbr=="NY", nytype=="county") |> 
  filter(all >= 500) |> 
  arrange(desc(crowdgt1p5_pct)) |> 
  select(-atype, -stabbr, -cnty, -starts_with("ppr"))

crowdwide |> 
  filter(stabbr=="NY", nytype=="city") |> 
  filter(all >= 500) |> 
  arrange(desc(crowdgt1p5_pct)) |> 
  select(-atype, -stabbr, -cnty, -starts_with("ppr"))

crowdwide |> 
  filter(stabbr=="NY", nytype=="village") |> 
  filter(all >= 500) |> 
  arrange(desc(crowdgt1p5_pct)) |> 
  select(-atype, -stabbr, -cnty, -starts_with("ppr"))

crowdwide |> 
  filter(stabbr=="NY", str_detect(shortname, "Cambridge")) |> 
  filter(all >= 500) |> 
  arrange(desc(crowdgt1p5_pct))





# table1 ----
# Table 1	 Tenure (2) by Housing Unit Problems (2) by Household Income (5) by Race (6)	Universe: All occupied housing units
# the descriptions are in alpha order, put them in desired order
forder <- function(df, colname, prefix="lev"){
  # function to put a factor in order based on first variable numbers associated with each level
  df |> 
    select(vnum, desc=.data[[colname]]) |> 
    mutate(descname=colname) |> 
    group_by(desc) |>
    arrange(vnum) |>
    filter(row_number()==1) |>
    ungroup() |>
    arrange(vnum) |>
    mutate(order=row_number(),
           vlev=paste0(prefix, order))
}

t1 <- nychas |> 
  filter(table=="t1")

t1d1 <- forder(t1, "desc1", "tenure")
t1d2 <- forder(t1, "desc2", "problems")
t1d3 <- forder(t1, "desc3", "pcthamfi")
t1d4 <- forder(t1, "desc4", "race")

t1a <- t1 |> 
  mutate(fdesc1=factor(desc1, levels=t1d1$desc, labels=t1d1$vlev),
         fdesc2=factor(desc2, levels=t1d2$desc, labels=t1d2$vlev),
         fdesc3=factor(desc3, levels=t1d3$desc, labels=t1d3$vlev),
         fdesc4=factor(desc4, levels=t1d4$desc, labels=t1d4$vlev))
count(t1a, fdesc1, desc1)  # tenure
count(t1a, fdesc2, desc2) # problems
count(t1a, fdesc3, desc3) # pcthamfi
count(t1a, fdesc4, desc4)
