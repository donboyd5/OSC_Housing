

# utility functions -------------------------------------------------------
# some wrappers for scale functions
f_pct <- function(num, ...) {scales::label_percent(...)(num)}
f_comma <- function(num, ...) {scales::label_comma(...)(num)}
f_dollar <- function(num, ...) {scales::label_dollar(...)(num)}


# tables, etc. ------------------------------------------------------------



vtab <- function(table) {
  # view details about an acs table
  # assumes that tabshells already has been loaded
  # tabshells <- readRDS(path(dacs, "tabshells_2019.rds"))
  tab <- tabshells |> filter(table==!!table)
  # print(tab)
  list(tab=tab |> select(-tabname, -universe),
       table=table,
       tabname=tab$tabname[1],
       universe=tab$universe[1]
  )
}

f_tabranks <- function(tabdata,
                       stubvar,
                       stubhead,
                       tabtitle,
                       tabsubtitle=NULL,
                       keepcounty=FALSE){
  
  mainvars <- c("allunits",
                "renter_share",
                "own_pct", "rent_pct", "alltenure_pct", 
                "ownrank", "rentrank", "alltenurerank")
  
  if(keepcounty) keepvars <- c("stub", "countyname", mainvars) else
    keepvars <- c("stub", mainvars)
  
  if(keepcounty) cntyhead <- "County" else
    cntyhead <- NULL
  
  tab <- tabdata |>
    rename(stub=all_of(stubvar)) |>
    select(all_of(keepvars)) |>
    arrange(desc(alltenure_pct)) |>
    gt::gt() |>
    sub_missing(columns = everything()) |>
    gt::tab_header(
      title = tt,
      subtitle=tabst
    ) |>
    gt::tab_spanner(columns = c(own_pct, rent_pct, alltenure_pct),
                    label=html("Percent of households<br>with this problem")) |>
    gt::tab_spanner(columns = c(ownrank, rentrank, alltenurerank),
                    label="Rank") |>
    gt::cols_label(stub=stubhead,
                   allunits=html("Number of<br>occupied<br>housing units"),
                   renter_share=html("Renter households<br>as percent of<br>all households"),
                   own_pct="Owners",
                   rent_pct="Renters",
                   alltenure_pct="All",
                   ownrank="Owners",
                   rentrank="Renters",
                   alltenurerank="All") |>
    gt::fmt_percent(columns=c(own_pct, rent_pct, alltenure_pct, renter_share), decimals=1) |>
    gt::fmt_number(columns=c(allunits, ownrank, rentrank, alltenurerank),
                   decimals=0) |>
    gt::tab_source_note(source_note = "Source: HUD 5-year CHAS ending 2019")
  
  if(keepcounty) tab <- tab |> gt::cols_label(countyname=cntyhead)
  tab
}



# gtsave2 <- function(tab, fpath, zoom=2, expand=5, vwidth=992, vheight=744) {
#   gtlist <- list()
#   gtlist$tab <- tab
#   gtlist$fpath <- fpath
#   gtlist$zoom <- zoom
#   gtlist$expand <- expand
#   gtlist$vwidth <- vwidth
#   gtlist$vheight <- vheight
#   
#   saveRDS(gtlist, "gtlist.rds")
#   # print(gtlist)
#   source("r/gtsave2.r") # gtsave2.r must be where it will be found -- creates local env
#   
#   # right after (outside) the code chunk, do the following, where we give the path to the image:
#   # ![](images/tab_topdepts.png){width="100%"}
#   
#   # webshot:
#   #   zoom: zoom factor of 2 will result in twice as many pixels vertically and horizontally
#   
# }
