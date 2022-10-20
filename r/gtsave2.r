
ftemp <- function(){
  # use a function to create a local environment
  gtl <- readRDS("gtlist.rds")
  # print(gtl)
  gtsave(data=gtl$tab, filename=gtl$fpath, zoom=gtl$zoom,
         expand=gtl$expand, vwidth=gtl$vwidth, vheight=gtl$vheight)
}

ftemp()
