

# tools -------------------------------------------------------------------


# p + theme(text=element_text(size=20), #change font size of all text
#           axis.text=element_text(size=20), #change font size of axis text
#           axis.title=element_text(size=20), #change font size of axis titles
#           plot.title=element_text(size=20), #change font size of plot title
#           legend.text=element_text(size=20), #change font size of legend text
#           legend.title=element_text(size=20)) #change font size of legend title  



centroids <- function(geometry) {
  # returns a tibble with X, Y centroid points of a geometry column
  xy=st_centroid(geometry)
  as_tibble(st_coordinates(xy))

  # example  
  # nycos_shape |> 
  #   mutate(centroids(geometry),
  #          Y=ifelse(area=="Westchester", Y +.05, Y)) |> 
  #   rename(xlabel=X, ylabel=Y)
}




# https://ggplot2.tidyverse.org/reference/scale_brewer.html
# Diverging BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# Qualitative Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# Sequential Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd


nycomap <- function(data, mapvar, maptitle, direction=1){
  # data should have geoid
  
  nycos_shape <- readRDS(here::here("data", "maps", "nycos_shape.rds"))
  
  df <- data |> 
    select(geoid, value=!!mapvar)
  
  mdata <- nycos_shape |>
    left_join(df,
              by="geoid")
  
  p <- mdata |> 
    ggplot() +
    geom_sf(aes(fill=value)) +
    scale_fill_distiller(palette = "BrBG", 
                         direction=direction, 
                         breaks=seq(0, 1, .1), 
                         labels = scales::percent_format(accuracy = 1)) +
    theme_map() + 
    ggtitle(maptitle) + 
    theme(plot.title=element_text(size=30)) +
    labs(fill="Percentage") +
    geom_label(aes(xlabel, ylabel, label = area), 
               size = 2, fontface = "bold", colour="black",
               label.padding = unit(0.1, "lines"), # default 0.25
               label.size = 0.15,) + # default 0.25
    theme(legend.position = c(0.9, 0.45))
  
  p
}
# nycomap(hcwide2, mapvar="owners_hhpct", maptitle="Homeownership percentage by county")




