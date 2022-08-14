
# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))


# locations --------------------------------------------------------------
acsdir <- here::here("data", "acs")


# get data ----------------------------------------------------------------
acsdata <- readRDS(here::here("data", "acs",  "acs_selected_table_data.rds"))
glimpse(acsdata)

# make a wide file with counties as rows, pcts as columns -----------------
# B25106  TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS       

hcburden1 <- acsdata |> 
  filter(table=="B25106")
count(hcburden1, stub)

hcburden2 <- hcburden1 |> 
  mutate(tenure=case_when(line==1 ~ "total",
                          line %in% 2:23 ~ "own",
                          line %in% 24:46 ~ "rent",
                          TRUE ~ "ERROR"),
         income=case_when(line %in% c(1, 2, 24) ~ "all",
                          str_detect(stub, coll("$")) ~ stub,
                          str_detect(stub, coll("Zero or negative income")) ~ "noincome",
                          TRUE ~ NA_character_),
         hcpercent=case_when(line %in% c(1, 2, 24) ~ "all",
                             str_detect(stub, "percent") ~ stub,
                             str_detect(stub, coll("$")) ~ "all",
                             str_detect(stub, coll("Zero or negative income")) ~ "noincome",
                             str_detect(stub, coll("No cash rent")) ~ "norent",
                             TRUE ~ NA_character_)) |> 
  fill(income, .direction="down") |> 
  select(geoid, area, line, vname, tenure, income, hcpercent, estimate, moe, stub)
count(hcburden2, tenure) # we have more renter records because of the no cash rent category
count(hcburden2, income)
count(hcburden2, hcpercent)

hcsummary <- hcburden2 |> 
  filter(!(income=="all" | hcpercent=="all")) |> 
  group_by(geoid, area, tenure, hcpercent) |> 
  summarise(estimate=sum(estimate), .groups = "drop") |> 
  mutate(hcpctf=factor(hcpercent,
                       levels=c("norent",
                                "noincome",
                                "Less than 20 percent",
                                "20 to 29 percent",
                                "30 percent or more",
                                "all"),
                       labels=c("norent",
                                "noincome",
                                "plt20",
                                "p2029",
                                "p30p",
                                "all")))

hcwide <- hcsummary |> 
  select(geoid, area, tenure, hcpctf, estimate) |> 
  pivot_wider(names_from=c(tenure, hcpctf), values_from = estimate) |> 
  mutate(otot=own_plt20 + own_p2029 + own_p30p + own_noincome,
         rtot=rent_plt20 + rent_p2029 + rent_p30p + rent_noincome + rent_norent,
         htot=otot + rtot,
         opct=own_p30p / otot,
         rpct=rent_p30p / rtot,
         totpct=(own_p30p + rent_p30p) / htot,
         renters_hhpct=rtot / htot,
         owners_hhpct=1 - renters_hhpct)

mapddata <- hcwide |> 
  select(geoid, area, otot, rtot, htot, contains("pct"))




nycomap(hcwide2, mapvar="owners_hhpct", maptitle="Homeownership percentage by county")
nycomap(hcwide2, mapvar="opct", maptitle="Percentage of homeowners who are cost-burdened")
nycomap(hcwide2, mapvar="rpct", maptitle="Percentage of renters who are cost-burdened")


mapddata |> 
  mutate(lab=str_remove(area, " County, New York")) |> 
  ggplot(aes(opct, rpct)) +
  geom_point() +
  geom_text(aes(label=lab), hjust=0, vjust=0)


