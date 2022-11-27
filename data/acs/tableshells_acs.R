
# read about 1200 table shell files from the web, then combine, clean, and save


# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
library(httr)
library(XML)


# constants ---------------------------------------------------------------
dacs <- here::here("data", "acs")


# ONETIME get save table shells from web ---------------------------------------------------
urlbase <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2019/"
url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/2019/B25072.xlsx"
# read_excel(path=url) # cannot read from url


flist <- readHTMLTable(content(GET(urlbase), "text"))[[1]] |> 
  select(-1) |> 
  as_tibble() |> 
  lcnames() |> 
  filter(!is.na(name), name != "Parent Directory") |> 
  select(name, modified=`last modified`, size)
glimpse(flist)
flist

urllist <- flist |> 
  filter(str_sub(name, 1, 1)=="B") |> 
  mutate(name=paste0(urlbase, name)) |> 
  pull(name)
urllist

f <- function(url){
  print(url)
  # read_excel won't read directly from url so write to temporary file
  httr::GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_excel(tf, sheet=1, col_types="text")
}

df1 <- map_dfr(urllist, f) # get ALL of the table shells for 2019
glimpse(df1)

df2 <- df1 |> 
  setNames(c("table", "line", "description")) |>
  group_by(table) |> 
  mutate(tabname=description[1],
         universe=description[2]) |> 
  filter(row_number() > 2) |> 
  mutate(group=ifelse(str_sub(description, -1, -1)==":",
                      str_sub(description, 1, -2) |> str_to_lower(),
                      NA_character_),
         nline=as.integer(line)) |> 
  fill(group, .direction="down") |> 
  ungroup() |> 
  filter(nline > 0) |> # get rid of the oddballs that have noninteger values??
  mutate(vname=paste0(table, "_", str_pad(line, width=3, side="left", pad="0")),
         universe=str_remove(universe, "Universe: ") |> str_trim()) |> 
  select(table, line=nline, vname, description, group, tabname, universe)

df2 |> 
  filter(is.na(group))

saveRDS(df2, path(dacs, "tabshells_2019.rds"))
