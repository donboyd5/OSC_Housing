
# https://www.hudexchange.info/resource/5691/system-performance-measures-data-since-fy-2015/

# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

# locations ---------------------------------------------------------------
dhless <- here::here("data", "hud", "homelessness")
fn <- "System-Performance-Measures-Data-Since-FY-2015.xlsx"


# get data ----------------------------------------------------------------

cols1 <- read_excel(path(dhless, fn), 
                  sheet="2021",
                  col_types = "text",
                  range = cell_rows(1:2),
                  col_names = FALSE)
glimpse(cols1)
names(cols1)

vnames <- cols1 |> 
  select(-c(1:5)) |> 
  filter(row_number()==2) |> 
  as.character()
vnames

cats <- cols1 |> 
  select(-c(1:5)) |> 
  filter(row_number()==1) |> 
  as.character()
cats

columns <- tibble(colnum=1:length(vnames) + 5,
                category=cats,
                vname=vnames) |> 
  fill(category)
columns

count(columns, category)


df1 <- read_excel(path(dhless, fn), 
                  sheet="2021",
                  col_names = FALSE,
                  range=cell_rows(c(3, NA)),
                  col_types = "text")
glimpse(df1)
names(df1)[1:6]

df2 <- df1 |> 
  rename(stabbr=1, cocname=2, cocnum=3, cocaward=4, coctype=5) |> 
  pivot_longer(-c(1:3, 5)) |> 
  mutate(colnum=str_remove_all(name, "\\.") |> as.integer())
glimpse(df2)
count(df2, name)

df3 <- df2 |> 
  left_join(columns, by = "colnum") |> 
  mutate(value=as.numeric(value),
         vname=ifelse(name=="cocaward", "cocaward", vname),
         colnum=ifelse(name=="cocaward", 4, colnum),
         category=ifelse(name=="cocaward", "cocaward", category)) |> 
  select(stabbr, cocnum, cocname, colnum, category, vname, value)
glimpse(df3)

tmp <- df3 |> 
  filter(cocnum=="NY-503") |> 
  select(cocnum, colnum, category, vname, value)


# explore -----------------------------------------------------------------
# HIC/PIT Data Collection Notice Page 6 October 2015
# The Project Types included in the HIC are:
# A. Emergency Shelter (ES)
# B. Transitional Housing (TH)
# C. Safe Haven (SH)
# D. Permanent Housing (PH)
# (1) Permanent Supportive Housing (PSH)
# (2) Rapid Re-housing (RRH)
# (3) Other PH (OPH) – consists of PH – Housing with Services (no disability
# required for entry) and PH – Housing Only, as identified in the 2014 HMIS Data
# Standards)

# 1 Bed Coverage                     3
# 2 HMIS DQ                         15
# 3 SPM 1                            4
# 4 SPM 2 (All)                      4
# 5 SPM 2 (Emergency Shelter)        7
# 6 SPM 2 (Permanent Housing)       10
# 7 SPM 2 (Safe Haven)               7
# 8 SPM 2 (Street Outreach)          7
# 9 SPM 2 (Transitional Housing)     7
# 10 SPM 3                            7
# 11 SPM 4                            8
# 12 SPM 5                            6
# 13 SPM 7                            6

# https://files.hudexchange.info/resources/documents/system-performance-measures-in-context.pdf
# Measure 1: Length of Time Persons Remain Homeless
# Measure 2: The Extent to which Persons who Exit Homelessness to Permanent
#   Housing Destinations Return to Homelessness within 6, 12, and 24 months
# Measure 3: Number of Homeless Persons 
# Measure 4: Employment and Income Growth for Homeless Persons in CoC Program-Funded Projects
# Measure 5: Number of Persons who Become Homeless for the First Time

# Measure 6: Homeless Prevention and Housing Placement of Persons Defined by
#   Category 3 of HUD's Homeless Definition in CoC Program-funded Projects
# As of this writing, no HUD-Funded projects are authorized to use the Category
#   3 homeless definition. This article will not cover Measure 6.

# Measure 7: Successful Placement from Street Outreach and Successful Placement in or Retention of Permanent Housing


# how long are people homeless?
cn <- 78

df3 |> 
  filter(stabbr=="NY", colnum==cn) |>
  arrange(desc(value))


df3 |> 
  filter(stabbr=="CA", colnum==cn) |>
  arrange(desc(value))





