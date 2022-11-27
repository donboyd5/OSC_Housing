
# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

# locations ---------------------------------------------------------------
dusa <- here::here("data", "usaspending")



# constants ---------------------------------------------------------------
# fnz <- r"(E:\R_projects\OSC_Housing\data\hud\HUDFY2021Grants_All_PrimeTransactions_2022-11-13_H16M32S20439814.zip)"
# fname <- "All_Assistance_PrimeTransactions_2022-11-13_H16M32S20_1.csv"

fnz <- r"(E:\R_projects\OSC_Housing\data\hud\HUD_All_PrimeTransactions_2022-11-13_H16M56S03306486.zip)"
fn1 <- "All_Assistance_PrimeTransactions_2022-11-13_H16M58S35_1.csv"
fn2 <- "All_Contracts_PrimeTransactions_2022-11-13_H16M56S03_1.csv"

# get data ----------------------------------------------------------------
df1 <- read_csv(unz(fnz, fn1), col_types = cols(.default = col_character()))
glimpse(df1)
count(df1, cfda_number, cfda_title)
count(df1, record_type_code, record_type_description) # all are 2, non-aggeregate
count(df1, action_type_code, action_type_description)
# action_type_code action_type_description     n
# <chr>            <chr>                   <int>
#   1 A                NEW                     20041
# 2 C                REVISION                 7323

tmp <- df1 |> 
  mutate(value=as.numeric(total_obligated_amount)) |> 
  group_by(cfda_number, cfda_title) |> 
  summarise(n=n(), value=sum(value), .groups="drop") |> 
  arrange(desc(value))

head(tmp, 10)
# cfda_number cfda_title                                                                                  n        value
# <chr>       <chr>                                                                                   <int>        <dbl>
# 1 14.850      PUBLIC AND INDIAN HOUSING                                                                3442 11764817077 
# 2 14.195      SECTION 8 HOUSING ASSISTANCE PAYMENTS PROGRAM                                            4874  8937751357 
# 3 14.871      SECTION 8 HOUSING CHOICE VOUCHERS                                                        2544  3259997357 
# 4 14.218      COMMUNITY DEVELOPMENT BLOCK GRANTS/ENTITLEMENT GRANTS                                     164  1370787851 
# 5 14.239      HOME INVESTMENT PARTNERSHIPS PROGRAM                                                      157  1037701768.
# 6 14.231      EMERGENCY SOLUTIONS GRANT PROGRAM                                                          43   859122449.
# 7 14.872      PUBLIC HOUSING CAPITAL FUND                                                                94   792758018.
# 8 14.856      LOWER INCOME HOUSING ASSISTANCE PROGRAM SECTION 8 MODERATE REHABILITATION                  58   683698078 
# 9 14.228      COMMUNITY DEVELOPMENT BLOCK GRANTS/STATE'S PROGRAM AND NON-ENTITLEMENT GRANTS IN HAWAII     8   630288975 
#10 14.275      HOUSING TRUST FUND                                                                          6   307534563 

tmp2 <- df1 |> 
  rename(fipsco=recipient_county_code,
         county=recipient_county_name,
         stabbr=recipient_state_code,
         city=recipient_city_name,
         recip=recipient_name,
         cfda=cfda_number,
         totoblig=total_obligated_amount,
         nonfed=total_non_federal_funding_amount) |> 
  filter(cfda=="14.871") |> 
  mutate(across(c(totoblig, nonfed), as.numeric))


df2 <- df1 |> 
  select(fipsco=recipient_county_code,
         county=recipient_county_name,
         stabbr=recipient_state_code,
         city=recipient_city_name,
         recip=recipient_name,
         cfda=cfda_number, 
         cfda_title,
         actcode=action_type_code, acttype=action_type_description,
         
         totoblig=total_obligated_amount, 
         nonfed=total_non_federal_funding_amount) |> 
  mutate(across(c(totoblig, nonfed), as.numeric)) |> 
  filter(cfda=="14.871", city=="ALBANY")



