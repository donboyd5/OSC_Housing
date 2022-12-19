
source(here::here("r", "libraries.r"))

pop2010 <- get_decennial(geography = "state", state="NY", variables="P001001", year=2010)
pop2020 <- get_decennial(geography = "state", state="NY", variables="P1_001N", year=2020)

df <- bind_rows(pop2010 |> mutate(year=2010), 
                pop2020 |> mutate(year=2020)) |> 
  lcnames() |> 
  select(name, year, value) |> 
  mutate(pch=value / value[year==2010] - 1)
df
