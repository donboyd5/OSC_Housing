
source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
chdir <- here::here("data", "CHAS")

# get data ----------------------------------------------------------------

# States (040)
# Counties (050)
# County Subdivisions (060)
# Places split by County and County Subdivision boundaries (070)
# Census tracts (140)
# Counties split by Place boundaries (155)
# Places (160)
# Consolidated Cities (170)

f <- function(sumlev){
  fname <- paste0("2014thru2018-", sumlev, "-csv.zip")
  zpath <- path(chdir, fname)
  csvfiles <- unzip(zipfile = zpath, list = TRUE) |> 
    filter(str_sub(Name, -3, -1)=="csv") |> 
    pull(Name)
  # print(csvfiles)
  
  getfile <- function(fname){
    print(fname)
    df <- read_csv(unz(zpath, fname),
                   col_types = cols(.default = col_character()))
    ids <- str_subset(names(df), "_", negate=TRUE)
    
    if(sumlev != "040") df <- df |> filter(str_sub(geoid, 8, 9)=="36")
    
    dfl <- df |> 
      pivot_longer(cols=-ids, names_to = "vname") |>  
      mutate(tabname=fname) |> 
      relocate(tabname, .after=source)
    dfl
  }
  
  # getfile(csvfiles[1])
  df <- map_dfr(csvfiles, getfile)
  df <- df |>
    mutate(st=str_sub(geoid, 8, 9),
           stabbr=factor(st, levels=stcodes$stfips, labels=stcodes$stabbr),
           value=as.numeric(value))
  df
}

# States (040)
# Counties (050)
# County Subdivisions (060)
# Places (160)
# Consolidated Cities (170)

chas_states <- f("040")
chas_nycos <- f("050")
chas_nycosubs <- f("060")
chas_nyplaces <- f("160")
chas_concities <- f("170") # zero obs

saveRDS(chas_states, path(chdir, "chas_states.rds"))
saveRDS(chas_nycos, path(chdir, "chas_nycos.rds"))
saveRDS(chas_nycosubs, path(chdir, "chas_nycosubs.rds"))
saveRDS(chas_nyplaces, path(chdir, "chas_nyplaces.rds"))

dim(chas_nycosubs)
dim(chas_nyplaces)


# get documentation -------------------------------------------------------
# CHAS data dictionary 14-18.xlsx
dpath <- path(chdir, "CHAS data dictionary 14-18.xlsx")

doc <- read_excel(dpath, sheet="All Tables", col_types="text") |> 
  setNames(c("cname", "ctype", paste0("desc", 1:5), "cenvars"))
glimpse(doc)
saveRDS(doc, path(chdir, "chas_docs.rds"))



# play below here ---------------------------------------------------------


