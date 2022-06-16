library(dplyr)
library(cleaner)
library(ggplot2)

parks <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/renamed_Parks.csv")
rec <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/renamed_Recreation Centers.csv")


df2 <- rec[grepl("Hillside High", rec$school_zones), ]

df <- parks[grepl("Hillside High", parks$school_zone), ]
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val, val)
}

df2$URL <- createLink(df2$URL)

View(df2[c("name","ADDRESS","URL")])


#percentage bbar chart
counts <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/counts data.csv", skip = 1)
counts_grouped <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/counts grouped.csv")

counts <- counts %>% na_replace()
counts_grouped <- counts_grouped %>% na_replace()

counts_grouped$varname <- as.factor(counts_grouped$varname)

#removing all the rows of schoolname 'All' because it's not relevant for comparison
counts_grouped<-counts_grouped[!(counts_grouped$schoolname=="All"),]
View(counts_grouped)

ggplot(counts_grouped, aes(fill=schoolname, y=count, x=varname)) + 
  geom_bar(position="fill", stat="identity")
