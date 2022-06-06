parks <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/renamed_Parks.csv")
rec <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/renamed_Recreation Centers.csv")


df2 <- rec[grepl("Hillside High", rec$school_zones), ]

df <- parks[grepl("Hillside High", parks$school_zone), ]
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val, val)
}

df2$URL <- createLink(df2$URL)

View(df2[c("name","ADDRESS","URL")])
