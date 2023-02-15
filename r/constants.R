# oscdir <- path_home("Documents", "R_projects", "OSC_Housing", "report", "gtables")
oscdir <- path("e:/", "R_projects", "OSC_Housing", "report", "oscdir")
dchas <- r"(E:\data\housing_insecurity\chas)"
dchas2019 <- r"(E:\data\housing_insecurity\chas\2015-2019)"
dchwide <- r"(E:\data\housing_insecurity\chas\2015-2019\wide_tables)"

legend_none <- theme(legend.position = "None")
legend_notitle <- theme(legend.title = element_blank())
caption_left <- theme(plot.caption = element_text(hjust = 0))

nycfips <- c("36005", "36047", "36061", "36081", "36085")
