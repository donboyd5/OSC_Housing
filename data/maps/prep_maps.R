


# libraries ---------------------------------------------------------------


# map
# library(choroplethr)
# library(choroplethrZip)
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_maps.r"))
source(here::here("r", "functions_maps.r"))

# NY counties: Simple feature shape files --------------------------------
nycos_shape1 <- tigris::counties(state = "New York", cb = TRUE)
# add centroids for labels, and adjust so that Westchester label does not overlap
nycos_shape <- nycos_shape1  |> 
  lcnames() |> 
  rename(area=name) |> 
  mutate(centroids(geometry),
         Y=ifelse(area=="Westchester", Y +.05, Y)) |> 
  rename(xlabel=X, ylabel=Y)
nycos_shape
saveRDS(nycos_shape, here::here("data", "nycos_shape.rds"))


# NY metro areas: Simple feature shape files --------------------------------
# Census metro areas
nymetro_shape <- core_based_statistical_areas(cb = TRUE) %>%  # this appears to be what we want
  lcnames() |> 
  rename(area=name) |> 
  filter(str_detect(namelsad, "Metro"), str_detect(area, "NY"))
nymetro_shape
saveRDS(nymetro_shape, here::here("data", "nymetro_shape.rds"))


# New York county crosswalk --------------------------------
# https://en.wikipedia.org/wiki/Category:Regions_of_New_York_(state)
# Regions of New York as defined by the New York State Department of Economic Development
# Regions in Upstate New York:
# 1. Western New York – counties : Niagara, Erie, Chautauqua, Cattaraugus, Allegany
# 2. Finger Lakes – counties : Orleans, Genesee, Wyoming, Monroe, Livingston, Wayne, Ontario, Yates, Seneca
# 3. Southern Tier – counties : Steuben, Schuyler, Chemung, Tompkins, Tioga, Chenango, Broome, Delaware
# 4. Central New York – counties : Cortland, Cayuga, Onondaga, Oswego, Madison
# 5. North Country – counties : St. Lawrence, Lewis, Jefferson, Hamilton, Essex, Clinton, Franklin
# 6. Mohawk Valley – counties : Oneida, Herkimer, Fulton, Montgomery, Otsego, Schoharie
# 7. Capital District – counties : Albany, Columbia, Greene, Warren, Washington, Saratoga, Schenectady, Rensselaer
# Regions in Downstate New York:
# 8. Hudson Valley – counties : Sullivan, Ulster, Dutchess, Orange, Putnam, Rockland, Westchester
# 9. New York City – counties (boroughs) : New York (Manhattan), Bronx (The Bronx), Queens (Queens), Kings (Brooklyn), Richmond (Staten Island)
# 10. Long Island – counties : Nassau, Suffolk

# Borough	County
# Bronx	Bronx County
# Brooklyn	Kings County
# Manhattan	New York County
# Queens	Queens County
# Staten Island	Richmond County

ny1 <- readRDS(here::here("data", "nycos_shape.rds")) |> 
  st_drop_geometry() |> 
  select(geoid, county=area, namelsad) |> 
  distinct()

# Labor market regions:
# download.file("https://data.ny.gov/api/views/imem-myat/rows.csv?accessType=DOWNLOAD&sorting=true",
#               here::here("data", "crosswalks", "Labor_Market_Regions.csv"))

ny2 <- read_csv(here::here("data", "xwalks", "Labor_Market_Regions.csv")) |> 
  setNames(c("rgn_labor", "county"))

nycounty_xwalk1 <- ny1 |> 
  left_join(ny2, by = "county") |> 
  mutate(costate=paste0(namelsad, ", New York"),
         rgn_edc=ifelse(str_detect(rgn_labor, "Hudson"), "Mid-Hudson", rgn_labor),
         # OSC has this note on their map: 
         # https://www.osc.state.ny.us/files/local-government/publications/pdf/osc-economic-regions.pdf
         # Otsego and Hamilton County differ from the Economic Development Regions used by the State’s Labor Department
         # OSC map shows Otsego in Southern Tier (OSC), not Mohawk Valley (EDC), and
         #               Hamilton in Mohawk Valley (OSC), not North Country (EDC)
         rgn_oscQ=ifelse(str_detect(rgn_edc, "Capital"), "Capital District", rgn_edc),
         rgn_oscQ=paste0(rgn_oscQ, " Region"),
         rgn_code=case_when(rgn_labor=="Western New York" ~ "wny",
                           rgn_labor=="Finger Lakes" ~ "flakes",
                           rgn_labor=="Southern Tier" ~ "stier",
                           rgn_labor=="Central New York" ~ "cny",
                           rgn_labor=="North Country" ~ "ncountry",
                           rgn_labor=="Mohawk Valley" ~ "mohawk",
                           rgn_labor=="Capital Region" ~ "capdist",
                           rgn_labor=="Hudson Valley" ~ "hudson",
                           rgn_labor=="New York City" ~ "nyc",
                           rgn_labor=="Long Island" ~ "li",
                           TRUE ~ "ERROR"), 
         rgn_num=case_when(rgn_labor=="Western New York" ~ 1,
                           rgn_labor=="Finger Lakes" ~ 2,
                           rgn_labor=="Southern Tier" ~ 3,
                           rgn_labor=="Central New York" ~ 4,
                           rgn_labor=="North Country" ~ 5,
                           rgn_labor=="Mohawk Valley" ~ 6,
                           rgn_labor=="Capital Region" ~ 7,
                           rgn_labor=="Hudson Valley" ~ 8,
                           rgn_labor=="New York City" ~ 9,
                           rgn_labor=="Long Island" ~ 10,
                           TRUE ~ 0),         
         borough_county=case_when(county=="New York" ~ "Manhattan",
                                  county=="Kings" ~ "Brooklyn",
                                  county=="Richmond" ~ "Staten Island",
                                  TRUE ~ county)
         )
nycounty_xwalk1
# Albany County, New York

count(nycounty_xwalk1, rgn_num, rgn_code, rgn_labor, rgn_edc, rgn_oscQ)
count(nycounty_xwalk1, rgn_num, rgn_code, rgn_labor, rgn_edc, county, borough_county, namelsad, costate)

nycounty_xwalk2 <- nycounty_xwalk1 |> 
  select(geoid, county, borough_county, namelsad, costate, rgn_num, rgn_code, rgn_edc, rgn_labor, rgn_oscQ) |> 
  arrange(geoid)

# verify against EDC table (Wikipedia above) - hoping correct, as EDC does not seem to have a list on their site
nycounty_xwalk2 |> 
  select(rgn_num, rgn_edc, county) |> 
  arrange(rgn_num, rgn_edc, county) |> 
  group_by(rgn_num, rgn_edc) |> 
  mutate(name=paste0("name", row_number())) |> 
  pivot_wider(values_from = county)
# EDC list from Wikipedia
# 1. Western New York – counties : Niagara, Erie, Chautauqua, Cattaraugus, Allegany
# 2. Finger Lakes – counties : Orleans, Genesee, Wyoming, Monroe, Livingston, Wayne, Ontario, Yates, Seneca
# 3. Southern Tier – counties : Steuben, Schuyler, Chemung, Tompkins, Tioga, Chenango, Broome, Delaware
# 4. Central New York – counties : Cortland, Cayuga, Onondaga, Oswego, Madison
# 5. North Country – counties : St. Lawrence, Lewis, Jefferson, Hamilton, Essex, Clinton, Franklin
# 6. Mohawk Valley – counties : Oneida, Herkimer, Fulton, Montgomery, Otsego, Schoharie
# 7. Capital District – counties : Albany, Columbia, Greene, Warren, Washington, Saratoga, Schenectady, Rensselaer
# 8. Hudson Valley – counties : Sullivan, Ulster, Dutchess, Orange, Putnam, Rockland, Westchester
# 9. New York City – counties (boroughs) : New York (Manhattan), Bronx (The Bronx), Queens (Queens), Kings (Brooklyn), Richmond (Staten Island)
# 10. Long Island – counties : Nassau, Suffolk

# data
# 1       1 Western New York Allegany Cattaraugus Chautauqua Erie       Niagara   NA          NA           NA         NA   
# 2       2 Finger Lakes     Genesee  Livingston  Monroe     Ontario    Orleans   Seneca      Wayne        Wyoming    Yates
# 3       3 Southern Tier    Broome   Chemung     Chenango   Delaware   Schuyler  Steuben     Tioga        Tompkins   NA   
# 4       4 Central New York Cayuga   Cortland    Madison    Onondaga   Oswego    NA          NA           NA         NA   
# 5       5 North Country    Clinton  Essex       Franklin   Hamilton   Jefferson Lewis       St. Lawrence NA         NA   
# 6       6 Mohawk Valley    Fulton   Herkimer    Montgomery Oneida     Otsego    Schoharie   NA           NA         NA   
# 7       7 Capital Region   Albany   Columbia    Greene     Rensselaer Saratoga  Schenectady Warren       Washington NA   
# 8       8 Mid-Hudson       Dutchess Orange      Putnam     Rockland   Sullivan  Ulster      Westchester  NA         NA   
# 9       9 New York City    Bronx    Kings       New York   Queens     Richmond  NA          NA           NA         NA   
# 10      10 Long Island      Nassau   Suffolk     NA         NA         NA        NA          NA           NA         NA   

saveRDS(nycounty_xwalk2, here::here("data", "nycounty_xwalk.rds"))


# Crosswalk to JCHS metro area names (nonstandard) ----
nymetro <- readRDS(here::here("data", "nymetro_shape.rds")) |> 
  st_drop_geometry() |> 
  select(geoid, area)

jchs <- readRDS(here::here("data", "price_income_jchs.rds")) |> 
  filter(ny, year==2021) |> 
  select(metro) |> 
  mutate(area=ifelse(metro=="Buffalo-Cheektowaga-Niagara Falls, NY",
                     "Buffalo-Cheektowaga, NY",
                     metro))
jchs

jchs_cbsa_xwalk <- nymetro |> 
  left_join(jchs, by="area")
jchs_cbsa_xwalk
saveRDS(jchs_cbsa_xwalk, here::here("data", "jchs_cbsa_xwalk.rds"))



# notes below here --------------------------------------------------------

jchsnames <- jchs |> 
  filter(ny, year==2021) |> 
  pull(metro) |> 
  sort() 
# send them email



# https://en.wikipedia.org/wiki/New_York_statistical_areas

# http://courtneylee.net/nacis-2019/#15
# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

# group. ggplot2’s functions can take a group argument which controls (amongst other things) whether adjacent points should be connected by lines. If they are in the same group, then they get connected, but if they are in different groups then they don’t. Essentially, having points in different groups means that ggplot “lifts the pen” when going between them.

# ggmap simplifies the process of downloading base maps from Google or Open Street Maps or Stamen Maps to use in the background of your plots. It also sets the axis scales, etc, in a nice way. Once you have gotten your maps, you make a call with ggmap() much as you would with ggplot()

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf

usa <- map_data("usa")

usa_base <- ggplot() +
  geom_polygon(data = usa, 
               aes(x = long, 
                   y = lat, 
                   group = group
               )) +
  coord_fixed(1.3)
usa_base

# Change the fill color and projection
ggplot() +
  geom_polygon(data = usa, 
               aes(x = long, y = lat, group = group), 
               fill = "lightgreen", color = "darkgreen") +
  coord_fixed(1.3) +
  labs(title = "The 48 Contiguous States of the USA") + 
  coord_map("conic", lat0 = 30) +
  theme(panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank())

states <- map_data("state")
calif <- subset(states, region == "california") 
newyork <- subset(states, region == "new york") 
# just the lat long points for the state
# county lines
ca_counties <- subset(map_data("county"), region == "california")

ny_counties <- subset(map_data("county"), region == "new york")

# plot the state itself 
ny_base <- ggplot(data = newyork, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "lightgreen") +
  theme(panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.border = element_blank(), 
        panel.grid = element_blank())

# plot county lines 
ny_base + 
  geom_polygon(data = ny_counties, fill = NA, color = "white") + 
  geom_polygon(color = "black", fill = NA)


# WE DON'T HAVE THIS
# attach our county group definition to the lat/longs
# provided by the `maps` library
cts_macro <-  inner_join(x = counties, y = cty_macro)
ca_base + 
  geom_polygon(data = cts_macro, aes(fill = msaname), 
               color = "white") + 
  geom_polygon(color = "black", fill = NA) + 
  scale_fill_viridis_d(option = "C") +
  labs(fill = "MSA Regions") + 
  guides(fill = FALSE)


# use tidycensus
acs_var <- 'B01003_001E'
tot <- get_acs(geography = "county", variables = acs_var, state=c("PA", "VA", "DC","MD"),
               geometry = TRUE)

tot <- get_acs(geography = "county", variables = acs_var, state=c("NY"),
               geometry = TRUE)


# counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL, ...)
df <- tigris::counties(state = "New York", cb = TRUE)


# tigris
# cb=TRUE limits this to generalized cartographic boundaries rather than detailed
metros1 <- core_based_statistical_areas(cb = TRUE) %>%  # this appears to be what we want
  rename(metro_name = NAME)
metros1
tmp1 <- metros1 |> 
  filter(str_detect(metro_name, "NY")) |> 
  arrange(metro_name)
# tmp |> select(-geometry)
tmp1$metro_name |> sort() # 27 areas

metros2 <- combined_statistical_areas(cb = TRUE) %>%
  select(metro_name = NAME)
metros2
tmp <- metros2 |> 
  filter(str_detect(metro_name, "NY"))
tmp[[1]]  # 7 areas

metros3 <- metro_divisions() %>%
  select(metro_name = NAME)
tmp <- metros3 |> 
  filter(str_detect(metro_name, "NY"))
tmp[[1]] # 2 areas

metros4 <- urban_areas()
tmp <- metros4 |> 
  select(metro_name=NAME10) |> 
  filter(str_detect(metro_name, "NY"))
tmp[[1]] # 127 areas

# urban_areas()

jchsnames |> sort() # 12 areas


# the jchsnames seem to be a subset of the core_based_statistical_areas, except that
# jchs has "Buffalo-Cheektowaga-Niagara Falls, NY" but
# tigris has "Buffalo-Cheektowaga, NY"

# per wikipedia: cbsa https://en.wikipedia.org/wiki/New_York_statistical_areas
# Buffalo-Cheektowaga, NY MSA  has Erie and Niagara counties

# proximity one had a 
# Buffalo-Cheektowaga-Niagara Falls, NY Metropolitan Statistical Area (CBSA 15380)
# tigris calls geoid 15380

tmp1a <- tmp1 |> filter(str_detect(NAMELSAD, "Metro"))
tmp1a$metro_name # 13
# > tmp1a$metro_name
#  [1] "Albany-Schenectady-Troy, NY"           "Binghamton, NY"                       
#  [3] "Buffalo-Cheektowaga, NY"               "Elmira, NY"                           
#  [5] "Glens Falls, NY"                       "Ithaca, NY"                           
#  [7] "Kingston, NY"                          "New York-Newark-Jersey City, NY-NJ-PA"
#  [9] "Poughkeepsie-Newburgh-Middletown, NY"  "Rochester, NY"                        
# [11] "Syracuse, NY"                          "Utica-Rome, NY"                       
# [13] "Watertown-Fort Drum, NY"  
jchsnames # 12
#  [1] "Albany-Schenectady-Troy, NY"           "Binghamton, NY"                       
#  [3] "Buffalo-Cheektowaga-Niagara Falls, NY" "Elmira, NY"                           
#  [5] "Glens Falls, NY"                       "Ithaca, NY"                           
#  [7] "Kingston, NY"                          "New York-Newark-Jersey City, NY-NJ-PA"
#  [9] "Rochester, NY"                         "Syracuse, NY"                         
# [11] "Utica-Rome, NY"                        "Watertown-Fort Drum, NY"  


# there are two difference between the 12 JCHS metro area names and tigris cbsa metro names
# 1) "Poughkeepsie-Newburgh-Middletown, NY" is NOT in the JCHS names
# 2) JCHS calls the "Buffalo-Cheektowaga, NY" CBSA Buffalo-Cheektowaga-Niagara Falls, NY"

# So I think I can use the tigris CBSA metro shape files

# ACS variable names
# https://api.census.gov/data/2015/acs/acs5/groups/

