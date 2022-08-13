source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
chdir <- here::here("data", "CHAS")



doc2 <- doc |> 
  filter(str_detect(desc1, "Owner"),
         str_detect(desc3, "cost burden"),
         ctype=="Subtotal")

df1 <- readRDS(path(chdir, "chas_states.rds")) |> 
  filter(stabbr=="NY", vname %in% doc2$cname) |> 
  left_join(doc2, by=c("vname"="cname"))

df2 <- df1 |> 
  group_by(desc2, desc3) |> 
  summarise(value=sum(value), .groups="drop")

df2 |> 
  group_by(desc3) |> 
  summarise(value=sum(value))

df2 |> 
  summarise(value=sum(value))

# compare to check totals -------------------------------------------------



# T3_est21	Subtotal	Owner occupied	with housing cost burden greater than 50%, none of the needs above	All
# T3_est27	Subtotal	Owner occupied	with housing cost burden greater than 30% but less than or equal to 50%, none of the needs above	All
# T8_est4	Subtotal	Owner occupied	less than or equal to 30% of HAMFI	less than or equal to 30%	All


# Housing Cost Burden Overview 3	Owner	Renter	Total	
# Cost Burden <=30%	2,833,210	1,671,570	4,504,780	
# Cost Burden >30% to <=50%	587,200	726,835	1,314,035	
# Cost Burden >50%	494,425	893,560	1,387,985	
# Cost Burden not available	28,510	81,210	109,720	
# Total	3,943,355	3,373,180	7,316,535	

df1 <- readRDS(path(chdir, "chas_states.rds")) |> 
  filter(stabbr=="NY")

vars <- c("T3_est21", "T3_est27", "T8_est4")
df2 <- df1 |> 
  filter(vname %in% vars)
df2

df2 <- df1 |> 
  filter(str_detect(vname, "T8_est"))
df2

# T8_est7	Subtotal	Owner occupied	less than or equal to 30% of HAMFI	greater than 30% but less than or equal to 50%	All
df2 |> 
  filter(vname=="T8_est7")


# Income by Cost Burden (Owners only)	Cost burden > 30%	Cost burden > 50%	Total	
# Household Income <= 30% HAMFI	236,900	200,440	294,745	
# Household Income >30% to <=50% HAMFI	230,405	133,110	338,710	
# Household Income >50% to <=80% HAMFI	237,485	95,705	516,540	
# Household Income >80% to <=100% HAMFI	127,845	32,015	377,270	
# Household Income >100% HAMFI	248,990	33,155	2,416,090	
# Total	1,081,625	494,425	3,943,355	


# test things out ---------------------------------------------------------
# TODO: create nyc records, regional records
df1 <- readRDS(path(chdir, "chas_states.rds"))
df1 <- readRDS(path(chdir, "chas_nycos.rds"))
glimpse(df1)

vars <- c("T1_est1", "T1_est2", "T1_est4", "T1_est75")
df2 <- df1 |> 
  filter(vname %in% vars) |> 
  select(stabbr, cnty, name, vname, value) |> 
  separate(col = vname, into=c("table", "var")) |> 
  select(-table) |> 
  pivot_wider(names_from=var)

df2 |> 
  mutate(ownocc=est2 / est1,
         rentocc=est75 / est1,
         diff=est1 - est2 -est75) |> 
  arrange(desc(ownocc))



