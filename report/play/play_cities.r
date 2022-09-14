

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))
source(path(path_home_r(), "R_projects", "api_keys.r"))


# nycomap(hcwide2, mapvar="owners_hhpct", maptitle="Homeownership percentage by county")


# put city points on a map ------------------------------------------------
nycos_shape <- readRDS(here::here("data", "nycos_shape.rds"))
nycentroids <- readRDS(here::here("data", "nycentroids.rds"))

mdata <- nycos_shape

citiesxnyc <- nycentroids |> 
  filter(atype=="cousub", str_detect(area, "city"), pop5yr > 0) |> 
  mutate(aname=str_remove(area, " city"),
         popden=pop5yr / sqmiles)

citiesxnyc |> write_csv("cities.csv")

mdata |> 
  ggplot() +
  geom_sf() +
  geom_point(aes(x=loncenter, y=latcenter), data=citiesxnyc)

mdata |> 
  ggplot() +
  geom_sf() +
  geom_point(aes(x=loncenter, y=latcenter), size=1, colour="blue", data=citiesxnyc) +
  geom_label_repel(aes(x=loncenter, y=latcenter, label=aname), size=3, data=citiesxnyc) +
  theme_map()

# bubble chart
library(viridis)
mdata |> 
  ggplot() +
  geom_sf() +
  geom_point(aes(x=loncenter, y=latcenter, size=popden, colour=popden), alpha=0.5, stroke=FALSE,
             data=citiesxnyc |> filter(popden < 10e3)) +
  scale_size_continuous(range = c(.1, 10)) +
  scale_colour_viridis(discrete=FALSE, guide="none", option="D") +
  # scale_size_continuous(name="Population (in M)", trans="log", range=c(1,12), breaks=mybreaks) +
  # scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  geom_text_repel(aes(x=loncenter, y=latcenter, label=aname), size=3, data=citiesxnyc) +
  theme_map()



# put the point info on the data not sure if this works ----
mdata <- nycos_shape |> 
  left_join(citiesxnyc |> select(-c(area, aland)), 
            by = c("affgeoid", "geoid"))

mdata |> 
  ggplot() +
  geom_sf() +
  geom_point(aes(x=loncenter, y=latcenter), size=1, colour="blue") +
  geom_label_repel(aes(x=loncenter, y=latcenter, label=aname), size=3, data=citiesxnyc) +
  theme_map()


  

