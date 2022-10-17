

ftabranks <- function(tabdata,
              stubvar,
              stubhead,
              tabtitle,
              tabsubtitle=NULL,
              keepcounty=FALSE){
  
  mainvars <- c("allunits",
                "own_pct", "rent_pct", "alltenure_pct", 
                "ownrank", "rentrank", "alltenurerank",
                "renter_share")
  
  if(keepcounty) keepvars <- c("stub", "cntyname", mainvars) else
    keepvars <- c("stub", mainvars)
  
  if(keepcounty) cntyhead <- "County" else
    cntyhead <- NULL
  
  tab <- tabdata |> 
    rename(stub=all_of(stubvar)) |> 
    select(all_of(keepvars)) |>
    arrange(desc(alltenure_pct)) |> 
    gt() |> 
    sub_missing(columns = everything()) |> 
    tab_header(
      title = tabtitle,
      subtitle = tabsubtitle
    ) |> 
    tab_spanner(columns = c(own_pct, rent_pct, alltenure_pct),
                label=html("Percent of households<br>with this problem")) |> 
    tab_spanner(columns = c(ownrank, rentrank, alltenurerank),
                label="Rank") |> 
    cols_label(stub=stubhead,
               allunits=html("# of<br>occupied units"),
               own_pct="Owners",
               rent_pct="Renters",
               alltenure_pct="All",
               ownrank="Owners",
               rentrank="Renters",
               alltenurerank="All",
               renter_share=html("Renter households as<br>% of all households")) |> 
    fmt_percent(columns=c(own_pct, rent_pct, alltenure_pct, renter_share), decimals=1) |> 
    fmt_number(columns=c(allunits, ownrank, rentrank, alltenurerank),
               decimals=0) |> 
    tab_source_note(source_note = "Source: HUD 5-year CHAS ending 2019")
  
  if(keepcounty) tab <- tab |> cols_label(cntyname=cntyhead)
  tab
}
