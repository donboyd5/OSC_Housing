
# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))


# constants ---------------------------------------------------------------
# dfb <- r"(E:\data\FederalBudget\2022\BUDGET-2022-DB)"
dfb <- r"(E:\data\FederalBudget\2023\BUDGET-2023-DB)"
fnz <- "BUDGET-2023-DB.zip"

dfbhere <- here::here("data", "fedbud")


# unzip budget database ---------------------------------------------------

# read_excel does not allow reading from a connection so unzip to a temporary directory
tempd <- tempdir()
zpath <- r"(E:\data\FederalBudget\2023\BUDGET-2023-DB.zip)"

# unzip all files - it is also possible to extract only those we want to use
unzip(zipfile=zpath, exdir = tempd)


# get budauth data --------------------------------------------------------
# E:/data/FederalBudget/2023/BUDGET-2023-DB/BUDGET-2023-DB.zip/BUDGET-2023-DB/xls/BUDGET-2023-DB:BUDGET-2023-DB-1.xlsx

fpath <- fs::path(tempd, "BUDGET-2023-DB", "xls", "BUDGET-2023-DB-1.xlsx") # authority path within the archive
df1 <- read_excel(fpath, col_types="text")

glimpse(df1)

budauth1 <- df1 |> 
  rename(agcode=`Agency Code`, agname=`Agency Name`,
         burcode=`Bureau Code`, burname=`Bureau Name`,
         acctcode=`Account Code`, acctname=`Account Name`, 
         treasagcode=`Treasury Agency Code`, 
         subfuncode=`Subfunction Code`, subfun=`Subfunction Title`,
         beacat=`BEA Category`,
         onoffbud=`On- or Off- Budget`, 
         y1976=`1976`, tq=TQ) |> 
  pivot_longer(cols=`1977`:ncol(df1),
               names_to = "fyear",
               values_to = "budauth")

glimpse(budauth1)
summary(budauth1)


count(budauth1, agcode, agname)  # 025 HUD
count(budauth1, burcode, burname)
count(budauth1, acctcode, acctname)
count(budauth1, subfuncode, subfun) # subfun 604 is housing assistance -- also see hist table 3.2

budauth2 <- budauth1 |> 
  mutate(fyear=as.integer(fyear),
         budauth=as.numeric(budauth)) # no missing values
summary(budauth2) # 1977:2027

saveRDS(budauth2, path(dfbhere, "budauth.rds"))


# get outlays data ----------------------------------------------------------------
# E:/data/FederalBudget/2023/BUDGET-2023-DB/BUDGET-2023-DB.zip/BUDGET-2023-DB/xls/BUDGET-2023-DB:BUDGET-2023-DB-1.xlsx

fpath <- fs::path(tempd, "BUDGET-2023-DB", "xls", "BUDGET-2023-DB-2.xlsx") # outlays path within the archive
df1 <- read_excel(fpath, col_types="text")

glimpse(df1)

outlay1 <- df1 |> 
  rename(agcode=`Agency Code`, agname=`Agency Name`,
         burcode=`Bureau Code`, burname=`Bureau Name`,
         acctcode=`Account Code`, acctname=`Account Name`, 
         treasagcode=`Treasury Agency Code`, 
         subfuncode=`Subfunction Code`, subfun=`Subfunction Title`,
         beacat=`BEA Category`, 
         grantnongrant=`Grant/non-grant split`,
         onoffbud=`On- or Off- Budget`, 
         tq=TQ) |> 
  pivot_longer(cols=c(`1962`:`1976`, `1977`:ncol(df1)), # don't pivot the transition quarter
               names_to = "fyear",
               values_to = "outlay")
summary(outlay1)

count(outlay1, agcode, agname)  # 025 HUD
count(outlay1, burcode, burname)
count(outlay1, acctcode, acctname)
count(outlay1, subfuncode, subfun) # subfun 604 is housing assistance -- also see hist table 3.2

outlay2 <- outlay1 |> 
  mutate(fyear=as.integer(fyear),
         outlay=as.numeric(outlay)) # no missing values
summary(outlay2)

saveRDS(outlay2, path(dfbhere, "outlay.rds"))


# examine data ------------------------------------------------------------

## get data ----
budauth <- readRDS(path(dfbhere, "budauth.rds"))
outlay <- readRDS(path(dfbhere, "outlay.rds"))

## important codes ----
# agcode 25 acctcode 0302  Tenant Based Rental Assistance




## explore budget authorizations ----

budauth |> 
  filter(subfuncode=="604") |> 
  group_by(fyear) |> 
  summarise(n=n(), budauth=sum(budauth), .groups="drop") |> 
  filter(fyear %in% 2015:2025)

budauth |> 
  filter(subfuncode=="604") |> 
  group_by(fyear) |> 
  summarise(n=n(), budauth=sum(budauth), .groups="drop") |> 
  filter(fyear %in% 2000:2027) |> 
  ggplot(aes(fyear, budauth)) +
  geom_line() +
  geom_point()

budauth |> 
  filter(subfuncode=="604", fyear==2022) # 89 recs

# by agency
# which agencies?
tmp <- budauth |> 
  filter(subfuncode=="604", budauth!=0, fyear >= 1990) |> 
  group_by(agcode, agname) |> 
  summarise(budauth=sum(budauth) / 1e6, .groups="drop") |> 
  arrange(desc(budauth))

agsnames <- read_csv(
"agcode, sname
025, HUD
015, Treasury
005, USDA
"
)
agsnames


budauth |> 
  filter(subfuncode=="604") |> 
  group_by(agcode, agname, fyear) |> 
  summarise(budauth=sum(budauth) / 1e6)

budauth2 <- budauth |> 
  filter(subfuncode=="604") |> 
  mutate(fagcode=factor(agcode, levels=c(agsnames$agcode), labels=agsnames$sname),
         fagcode=fct_explicit_na(fagcode, na_level = "Other"))

budauth2 |> 
  count(fagcode, agcode, agname)

budauth2 |> 
  group_by(fagcode, fyear) |> 
  summarise(budauth=sum(budauth), .groups = "drop") |> 
  ggplot(aes(fyear, budauth, colour=fagcode)) +
  geom_line() +
  geom_point()
  


## explore outlays ----

outlay |> 
  filter(subfuncode=="604", fyear==2022) # 104 recs

outlay |> 
  filter(subfuncode=="604") |> 
  group_by(fyear) |> 
  summarise(n=n(), outlay=sum(outlay), .groups="drop") |> 
  filter(fyear %in% 2015:2027)

outlay |> 
  filter(subfuncode=="604") |> 
  group_by(fyear) |> 
  summarise(n=n(), outlay=sum(outlay), .groups="drop") |> 
  filter(fyear %in% 2000:2025) |> 
  ggplot(aes(fyear, outlay)) +
  geom_line() +
  geom_point()


tmp <- outlay |> 
  filter(subfuncode=="604", fyear==2022)
count(tmp, agcode, agname)

unique(tmp$acctname)

tmp |> 
  group_by(acctcode, acctname) |> 
  summarise(n=n(), outlay=sum(outlay) / 1e6, .groups="drop") |> 
  arrange(desc(outlay))

tmp |> 
  group_by(onoffbud) |> 
  summarise(n=n(), outlay=sum(outlay) / 1e6, .groups="drop") |> 
  arrange(desc(outlay))

tmp |> 
  group_by(grantnongrant) |> 
  summarise(n=n(), outlay=sum(outlay) / 1e6, .groups="drop") |> 
  arrange(desc(outlay))  
  
t1 <- outlay |> 
  filter(subfuncode=="604", acctcode=="0302", fyear==2022)

treas <- outlay |> 
  filter(subfuncode=="604", agcode=="015", fyear==2022)

hud <- outlay |> 
  filter(agcode=="025", fyear==2022)

hud |> 
  write_csv(here::here("hud.csv"))

housassist <- outlay |> 
  filter(subfuncode=="604", fyear==2022)
housassist |> 
  write_csv(here::here("housassit.csv"))


# categorize hud spending -------------------------------------------------
# Rental Assistance, as defined by HUD, includes Tenant-Based Rental Assistance,
# the Public Housing Fund, Project-Based Rental Assistance, Housing for the
# Elderly, and Housing for Persons with Disabilities.

# all fall in subfunction 604, acct codes:
#  Tenant-Based Rental Assistance (0302)
#  Project-Based Rental Assistance (0303)
#  Public Housing Fund (0481); capital (0304) maybe
#  Housing for the Elderly (0320)  (Section 202)
#  Housing for Persons with Disabilities (0237)  (Section 811)

rentassist <- c("0302", "0303", "0481", "0304", "0320", "0237")

outlay |> 
  filter(subfuncode=="604", agcode=="025", fyear==2022) |>  # housing, HUD
  filter(acctcode %in% rentassist) |> 
  summarise(outlay=sum(outlay) / 1e6)

# create a spreadsheet for coding housing assistance expenditures
hacodes <- outlay |> 
  filter(subfuncode=="604", fyear %in% 1980:2023) |> 
  select(-tq) |> 
  pivot_wider(names_from = fyear, values_from = outlay) |> 
  rowwise() |> 
  mutate(progtype=NA_character_,
         special=NA_character_,  # placeholders
         maxval=max(`1980`:`2023`)) |> 
  ungroup() |> 
  relocate(progtype, special, maxval, .before=`1980`) |> 
  relocate(subfuncode, subfun, .after=`2023`) |> 
  arrange(agcode, agname, burcode, burname, acctcode, acctname)

hacodes |> 
  write_csv(here::here("hacodes.csv"))

# hacodes2 <- outlay |> 
#   filter(subfuncode=="604", fyear %in% 1980:2023) |> 
#   select(-tq) |> 
#   pivot_wider(names_from = fyear, values_from = outlay) |> 
#   mutate(progtype=NA_character_,
#          maxval=pmax(as.character(1980:2023))) |> 
#   relocate(progtype, maxval, .before=`1980`) |> 
#   relocate(subfuncode, subfun, .after=`2023`) |> 
#   arrange(agcode, agname, burcode, burname, acctcode, acctname)


# tax expenditures --------------------------------------------------------



