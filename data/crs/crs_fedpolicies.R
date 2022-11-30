

# loads -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))
source(here::here("r", "constants.r"))

library(Microsoft365R)

# locations ---------------------------------------------------------------
dhud <- here::here("data", "hud")
onedrive <- r"(C:\Users\donbo\OneDrive\Documents\OSC Housing)"

# constants ---------------------------------------------------------------
# fnz <- r"(E:\R_projects\OSC_Housing\data\hud\HUDFY2021Grants_All_PrimeTransactions_2022-11-13_H16M32S20439814.zip)"
# fname <- "All_Assistance_PrimeTransactions_2022-11-13_H16M32S20_1.csv"

fnz <- r"(E:\R_projects\OSC_Housing\data\hud\QCT2023CSV.zip)"
fn1 <- "QCT2023.csv"

# get data ----------------------------------------------------------------
# xwalk <- readRDS(here::here("data", "xwalks", "nycounty_xwalk.rds"))
fn <- "Boyd OSC Housing Report.xlsx"
fn2 <- r"(C:\Users\donbo\OneDrive\Documents\OSC Housing\Boyd OSC Housing Report.xlsx)"

# sign in to onedrive, then
od <- get_personal_onedrive()
od$list_items()
od$list_files("Documents")
# # folder: CA_BU6_OneDrive
a <- od$list_files("Documents/OSC Housing")
a
# str(a)
odfn <- "Boyd OSC Housing Report.xlsx"
odpath <- "Documents/OSC Housing/Boyd OSC Housing Report.xlsx"
# item <- od$get_item("Documents/CA_BU6_OneDrive/Boyd_CABU6.xlsx")

od$download_file(src=odpath, dest=here::here("data", "crs", "Boyd OSC Housing Report_static.xlsx"), overwrite = TRUE)

# str(item)
# item$get_path()
# od$get_path()
# 
# od$download_file(dest="C:/Users/donbo/Downloads/", odpath)
# 
# 
# url <- "https://1drv.ms/x/s!AM1z6InC8nJgkAU"
# 
# url <- "https://onedrive.live.com/edit.aspx?cid=6072f2c289e873cd"
# url <- "https://1drv.ms/x/s!As1z6InC8nJgkAXlu5PwSkglYhqo?e=BA6ipn"
# url <- "https://onedrive.live.com/edit.aspx?cid=6072f2c289e873cd&page=view&resid=6072F2C289E873CD!2053&parId=6072F2C289E873CD!2045&app=Excel"
# read_excel(url, sheet="retplan_parameters", skip = 1)

# df <- read_excel(path(onedrive, fn), sheet="fedpolicy_crs")
df <- read_excel(here::here("data", "crs", "Boyd OSC Housing Report_static.xlsx"), 
                 sheet="fedpolicy_crs",
                 range="a3:h42")
df
names(df)

df |> 
  select(-totreal) |> 
  pivot_longer(cols=-c(ffy, total)) |> 
  mutate(share=value / total) |> 
  ggplot(aes(ffy, share, colour=name)) +
  geom_line() +
  geom_point()

df |> 
  mutate(ratio=totreal / total,
         across(-c(ffy, totreal, ratio), ~ .x * ratio)) |> 
  pivot_longer(cols=-c(ffy, totreal, ratio)) |> 
  ggplot(aes(ffy, value, colour=name)) +
  geom_line() +
  geom_point()



  
  


