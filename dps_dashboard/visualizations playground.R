library(dplyr)
library(cleaner)
library(ggplot2)
library(leaflet)
library(sf)

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
counts <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/counts.csv", skip = 1)
counts_grouped <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/counts grouped.csv")

#counts_grouped$varname <- as.factor(counts_grouped$varname)

#removing all the rows of name 'All' because it's not relevant for comparison
counts_grouped<-counts_grouped[!(counts_grouped$name=="All School"),]
View(counts_grouped)

#stacked % bar chart
ggplot(counts_grouped, aes(fill=name, y=count, x=as.factor(varname))) + 
  geom_bar(position="fill", stat="identity")

#circular bar chart
label_data <- counts_grouped
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

ggplot(counts_grouped, aes(x=as.factor(varname), y=count, fill=name)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=varname, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )




#CHOROPLETH MAP WORKING W GEOJSON


durham_choro <- geojsonio::geojson_read("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/map_data/All.geojson", what = "sp")
durham_choro@data <- merge(durham_choro@data, counts_grouped, by = 'name')
#durham_choro@data$varname <- as.factor(durham_choro@data$varname)
View(durham_choro@data %>% group_by(varname))

grouped_by_varname <- durham_choro@data %>% group_by(varname) %>% group_split()
View(grouped_by_varname[[1]])

# Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=grouped_by_varname[[1]]$count, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
m <- sf::st_as_sf(grouped_by_varname[[1]]) %>% leaflet() %>% 
  addTiles()  %>%  
  addPolygons( fillColor = ~mypalette(count), stroke=FALSE )

m
