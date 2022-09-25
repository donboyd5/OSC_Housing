

ftabranks <- function(tabdata,
              stubvar,
              stubhead,
              tabtitle,
              tabsubtitle=NULL,
              keepcounty=FALSE){
  
  mainvars <- c("own_pct", "rent_pct", "occ_pct", 
                "ownrank", "rentrank", "occrank", "renter_share")
  
  if(keepcounty) keepvars <- c("stub", "cntyname", mainvars) else
    keepvars <- c("stub", mainvars)
  
  if(keepcounty) cntyhead <- "County" else
    cntyhead <- NULL
  
  tab <- tabdata |> 
    rename(stub=all_of(stubvar)) |> 
    select(all_of(keepvars)) |>
    arrange(desc(occ_pct)) |> 
    gt() |> 
    sub_missing(columns = everything()) |> 
    tab_header(
      title = tabtitle,
      subtitle = tabsubtitle
    ) |> 
    tab_spanner(columns = c(own_pct, rent_pct, occ_pct),
                label="Percent of households") |> 
    tab_spanner(columns = c(ownrank, rentrank, occrank),
                label="Rank") |> 
    cols_label(stub=stubhead,
               own_pct="Owners",
               rent_pct="Renters",
               occ_pct="All",
               ownrank="Owners",
               rentrank="Renters",
               occrank="All",
               renter_share=html("Renter households as<br>% of all households")) |> 
    fmt_percent(columns=c(own_pct, rent_pct, occ_pct, renter_share), decimals=1) |> 
    fmt_number(columns=c(ownrank, rentrank, occrank),
               decimals=0) |> 
    tab_source_note(source_note = "Source: HUD 5-year CHAS ending 2019")
  
  if(keepcounty) tab <- tab |> cols_label(cntyname=cntyhead)
  tab
}