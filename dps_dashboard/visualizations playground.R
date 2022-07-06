library(dplyr)
library(cleaner)
library(ggplot2)
library(leaflet)
library(sf)
library(readxl)
library(stringr)

parks <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/renamed_Parks.csv")
rec <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/renamed_Recreation Centers.csv")


df2 <- rec[grepl("Hillside High", rec$school_zones), ]

df <- parks[grepl("Hillside High", parks$school_zone), ]
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">%s</a>',val, val)
}

df2$URL <- createLink(df2$URL)

View(df2[c("name","ADDRESS","URL")])


#percentage bar chart
counts <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/counts.csv", skip = 1)
counts_grouped <- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/spatial_data/counts grouped.csv")
pop<- read.csv("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/school_stats_data/population.csv")

#counts_grouped$varname <- as.factor(counts_grouped$varname)

#removing all the rows of name 'All' because it's not relevant for comparison
counts_grouped<-counts_grouped[!(counts_grouped$name=="All School"),]
counts_grouped$name <- str_replace_all(counts_grouped$name, 'Lakewood Elementary School', 'Lakewood Elementary')
counts_grouped$name <- str_replace_all(counts_grouped$name, 'Lakewood Middle School', 'Lakewood Middle')
counts_grouped$name <- str_replace_all(counts_grouped$name, ' Middle School', '')
counts_grouped$name <- str_replace_all(counts_grouped$name, ' Elementary School', '')
counts_grouped$name <- str_replace_all(counts_grouped$name, ' High School', '')


counts_grouped <- merge(counts_grouped, pop, by = 'name')
counts_grouped <- subset(counts_grouped, select = -id)
counts_grouped$population <- as.numeric(gsub(",", "", counts_grouped$population))
counts_grouped$pop_per_sq_mile <- as.numeric(gsub(",", "", counts_grouped$pop_per_sq_mile))

counts_grouped <- mutate(counts_grouped, normalized_count = count / population)

variables_group_1 <- c("After-School Care Programs", "Childcare Centers_2022", "Community & Cultural Centers", "Recreation Centers",
                       "Community Gardens", "Parks")

counts_grouped_1 <- subset(counts_grouped, counts_grouped$varname %in% variables_group_1)
counts_grouped_2 <- subset(counts_grouped, !counts_grouped$varname %in% variables_group_1)

View(counts_grouped)

#individual bar plots per school
ggplot(counts_grouped, aes(x = as.factor(varname), y = normalized_count)) +
  facet_wrap(~name, ncol=1)

#stacked % bar chart
ggplot(counts_grouped, aes(fill=name, y=normalized_count, x=as.factor(varname))) + 
  geom_bar(position="fill", stat="identity") + xlab("Variable") + ylab("Normalized Percentage of Each Resource") + ggtitle("Population-Normalized Comparison of Resources Across School Districts")

ggplot(counts_grouped, aes(fill=varname, y=normalized_count, x=as.factor(name))) + 
  geom_bar(position="fill", stat="identity")

#CIRCULAR BAR CHART
draw_circular_chart <- function(input_df) {
  label_data <- input_df
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 4
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(label_data$varname), ncol(label_data)) )
  colnames(to_add) <- colnames(label_data)
  to_add$varname <- rep(levels(label_data$varname), each=empty_bar)
  label_data <- rbind(label_data, to_add)
  label_data <- label_data %>% arrange(varname)
  label_data$id <- seq(1, nrow(label_data))
  
  # Get the name and the y position of each label
  plot_data <- label_data
  number_of_bar <- nrow(plot_data)
  angle <- 90 - 360 * (plot_data$id-0.5)/number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  plot_data$hjust <- ifelse( angle < -90, 1, 0)
  plot_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # Make the plot
  ggplot(label_data, aes(x=as.factor(id), y=normalized_count, fill=varname)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
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
    geom_text(data=plot_data, aes(x=id, y=normalized_count+10, label=name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= plot_data$angle, inherit.aes = FALSE )
}

draw_circular_chart(counts_grouped_1)
draw_circular_chart(counts_grouped_2)

#the old racial demographics plot
ggplot(all_race, aes(factor(school), number, fill = race)) + 
  geom_bar(stat="identity", position = "dodge") +   scale_fill_manual(values = c("#1414AB", "#005BAD", "#60A6D4",
                                                                                 "#D1E3F4", "#C6CBCF")) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 1.5)) +
  labs(title = "Racial Demographics of Schools" , x = "School", y = "Students (%)", fill="Race")
ggplotly(p3)

#a metric for comparing community wealth across school districts
#community wealth of school zone x = (importance coefficient for parks * number of parks in school zone x) +(importance coefficient for gardens * number of gardens in school zone x)...
#then compare this community wealth variable across school districts


#CHOROPLETH MAP WORKING W GEOJSON

durham_choro <- geojsonio::geojson_read("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/map_data/All.geojson", what = "sp")
durham_choro@data <- merge(durham_choro@data, counts_grouped, by = 'name')
#durham_choro@data$varname <- as.factor(durham_choro@data$varname)
View(durham_choro@data %>% group_by(varname))

grouped_by_varname <- durham_choro@data %>% group_by(varname) %>% group_split()
View(grouped_by_varname[[1]])

variables <- as.factor(durham_choro@data$varname)
View(variables)

#for each var in variables:
  #subset durham_choro@data
durham_choro@data <- subset(durham_choro@data, durham_choro@data$varname == 'After-School Care Programs')
View(durham_choro@data)

# Create a color palette for the map:
mypalette <- colorNumeric(palette="viridis", domain=durham_choro@data$count, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
m <- sf::st_as_sf(durham_choro@data) %>% leaflet() %>% 
  addTiles()  %>%  
  addPolygons( fillColor = ~mypalette(count), stroke=FALSE )

m

#fixing racial demographics
all_race <- read_excel("C:/Users/poona/Desktop/College Doc Dump/Data+/New Repo/visualizing_DPS/dps_dashboard/data/2021/school_stats_data/all race 1.xlsx")
View(all_race)

cbPalette <- c("#FFC0CB", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(all_race, aes(fill=race, y=number, x=as.factor(school))) + 
  geom_bar(position="fill", stat="identity")+ ggtitle("Racial Demographics") + ylab("Percentage") + xlab("School Name")+
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values=cbPalette) +
  theme(plot.title = element_text(hjust = 0.5))


