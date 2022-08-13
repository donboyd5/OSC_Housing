
source(here::here("r", "libraries.r"))



# jchs metro tables -------------------------------------------------------

fn <- "Harvard_JCHS_State_Nations_Housing_2022_Appendix_Tables_0.xlsx"

# Ratio of the Median Home Price for Existing Home Sales to the Median Household Income
df <- read_excel(here::here("data", fn), sheet="W-8", range="a5:ag387")
glimpse(df)

df2 <- df |> 
  rename(metro=1) |> 
  pivot_longer(cols=-metro, names_to = "year", values_to = "ratio") |> 
  mutate(year=as.integer(year))
glimpse(df2)


df3 <- df2 |> 
  mutate(us=metro=="United States",
         ny=str_detect(metro, coll("NY"))) |> 
  group_by(year) |> 
  mutate(usratio=ratio[us],
         sort=rank(desc(ratio))) |>  # a ranking that includes the US
  group_by(us, year) |> 
  mutate(diffus=ratio - usratio,
         rank=ifelse(us, NA_real_, rank(desc(ratio))),
         prank=ifelse(us, NA_real_, percent_rank(desc(ratio)))) |> 
  ungroup()

df3 |> filter(ny) |> select(metro) |> distinct()

df3 |> filter(ny | us, year==2021) |> arrange(sort)

c1 <- "Table W-8: Ratio of the Median Home Price for Existing Home Sales to the Median Household Income, JCHS Nation 2022"
c2 <- "https://www.jchs.harvard.edu/sites/default/files/interactive-item/files/Harvard_JCHS_State_Nations_Housing_2022_Appendix_Tables_0.xlsx"
c3 <- paste0(c1, "\n", c2)
c3
comment(df3) <- c3
comment(df3)
saveRDS(df3, here::here("data", "price_income_jchs.rds"))


