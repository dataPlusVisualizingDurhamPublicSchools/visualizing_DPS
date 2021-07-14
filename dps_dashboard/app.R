#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)

#Load School Stats Data
Race_SCHOOL_ONLY <- read_excel("Race SCHOOL ONLY.xlsx")
race <- read_excel("race.xlsx")
race_diff <- read_excel("race diff.xlsx")
poc_per_school <- read_excel("poc per school.xlsx")
funding <- read_excel("funding.xlsx")
all_race <- read_excel("all race.xlsx")
schoolstats <- read.csv("Data + School Info - School Statistics.csv")

#Load Map Data
durham <- geojsonio::geojson_read("Ten Schools.geojson", what = "sp")
cc <- geojsonio::geojson_read("CC Spaulding.geojson", what = "sp")
eastway <- geojsonio::geojson_read("Eastway.geojson", what = "sp")
ek <- geojsonio::geojson_read("EK Powe.geojson", what = "sp")
fayetteville <- geojsonio::geojson_read("Fayetteville St.geojson", what = "sp")
forest <- geojsonio::geojson_read("Forest View.geojson", what = "sp")
hillside <- geojsonio::geojson_read("Hillside.geojson", what = "sp")
jordan <- geojsonio::geojson_read("CEJordan.geojson", what = "sp")
lakewood <- geojsonio::geojson_read("Lakewood.geojson", what = "sp")
parkwood <- geojsonio::geojson_read("Parkwood.geojson", what = "sp")
southwest <- geojsonio::geojson_read("Southwest.geojson", what = "sp")

#Spatial Data
bus <- read.csv("renamed_Bus Stops.csv")
childcare <- read.csv("renamed_Childcare Centers.csv")
cultural <- read.csv("renamed_Community & Cultural Centers.csv")
gardens <- read.csv("renamed_Community Gardens.csv")
grocery <- read.csv("renamed_Grocery Stores.csv") #note what we consider a grocery store, not food desert food apartheid
libraries <- read.csv("renamed_Libraries.csv")
parks <- read.csv("renamed_Parks.csv")
rec <- read.csv("renamed_Recreation Centers.csv")
religious <- read.csv("renamed_Religious Centers.csv")

schoolstats$name <- c("C.C. Spaulding Elementary", "Eastway Elementary",
                      "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                      "Forest View Elementary", "Lakewood Elementary", "Parkwood Elementary",
                      "Southwest Elementary", "Hillside High","Jordan High", "All")

#Icons

iconSet <- iconList(
    parks = makeIcon("https://img.icons8.com/material-rounded/24/000000/deciduous-tree.png", iconWidth=20, iconHeight=20),
    rec = makeIcon("https://img.icons8.com/ios-glyphs/30/000000/children.png", iconWidth=20, iconHeight=20),
    gardens = makeIcon("https://img.icons8.com/doodle/48/000000/flower--v1.png", iconWidth=20, iconHeight=20),
    bus = makeIcon("https://img.icons8.com/material-rounded/24/000000/bus.png", iconWidth=20, iconHeight=20),
    childcare = makeIcon("https://img.icons8.com/material-rounded/24/000000/rocking-horse.png", iconWidth=20, iconHeight=20),
    cultural = makeIcon("https://img.icons8.com/ios-filled/30/000000/crowd.png", iconWidth=20, iconHeight=20),
    grocery = makeIcon("https://img.icons8.com/ios-glyphs/30/000000/grocery-store.png", iconWidth=20, iconHeight=20),
    libraries = makeIcon("https://img.icons8.com/fluent-systems-filled/50/000000/book.png", iconWidth=20, iconHeight=20),
    religious = makeIcon("https://img.icons8.com/ios-filled/50/000000/chapel.png", iconWidth=20, iconHeight=20)
)

sidebar <- {dashboardSidebar(
    sidebarMenu(
        menuItem("Our Ten Community Schools - Home", tabName = "home"),
        menuItem(tabName = "CCS", text = "C.C. Spaulding"),
        menuItem(tabName = "EastWay", text = "Eastway"),
        menuItem(tabName = "EKPowe", text = "E.K. Powe"),
        menuItem(tabName = "FayettevilleSt", text = "Fayetteville St"),
        menuItem(tabName = "ForestView", text = "Forest View"),
        menuItem(tabName = "Lakewood", text = "Lakewood"),
        menuItem(tabName = "Parkwood", text = "Parkwood"),
        menuItem(tabName = "Southeast", text = "Southeast"),
        menuItem(tabName = "Hillside", text = "Hillside"),
        menuItem(tabName = "Jordan", text = "C E Jordan"),
        menuItem("School Statistics", tabName = "statstab"),
        menuItem("Maps", tabName = "mapstab")
    )
)
}

body <- {dashboardBody(
    tabItems(
        #Landing Page
        {tabItem(tabName = "home",
                 br(),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4,
                                 img(src="School Image_nocircle.png", height = 200)),
                          column(width=4,
                                 img(src="School Image_nocircle.png", height = 200)),
                          column(width=4,
                                 img(src="School Image_nocircle.png", height = 200)),
                 ),
                 fluidRow(class = "text-center", width=12,
                          column(width=4, h1("CC Spaulding Elementary")),
                          column(width=4, h1("Eastway Elementary")),
                          column(width=4, h1("E K Powe Elementary"))
                 ),
                 fluidRow(class = "text-center", width=12,
                          column(width=4, h4("Click below to view info on C.C. Spaulding Elementary.")),
                          column(width=4, h4("Click below to view info on Eastway Elementary.")),
                          column(width=4, h4("Click below to view info on E.K. Powe Elementary."))
                 ),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4,
                                 actionButton("button", "View C.C. Spaulding Elementary >>")),
                          column(width=4,
                                 actionButton("button", "View Eastway Elementary >>")),
                          column(width=4,
                                 actionButton("button", "View E.K. Powe Elementary >>"))
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4,
                                 img(src="School Image_nocircle.png", width=200)),
                          column(width=4,
                                 img(src="School Image_nocircle.png", width=200)),
                          column(width=4,
                                 img(src="School Image_nocircle.png", width=200))
                 ),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4, h1("Fayetteville St Elementary")),
                          column(width=4, h1("Forest View Elementary")),
                          column(width=4, h1("Lakewood Elementary"))
                 ),
                 fluidRow(class = "text-center", width=12,
                          column(width=4, h4("Click below to view info on Fayetteville Street Elementary.")),
                          column(width=4, h4("Click below to view info on Forest View Elementary")),
                          column(width=4, h4("Click below to view info on Lakewood Elementary."))
                 ),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4,
                                 actionButton("button", "View Fayetteville Street Elementary >>")),
                          column(width=4,
                                 actionButton("button", "View Forest View Elementary >>")),
                          column(width=4,
                                 actionButton("button", "View Lakewood Elementary >>"))
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4,
                                 img(src="School Image_nocircle.png", width=200)),
                          column(width=4,
                                 img(src="School Image_nocircle.png", width=200)),
                          column(width=4,
                                 img(src="School Image_nocircle.png", width=200))
                 ),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4, h1("Parkwood Elementary")),
                          column(width=4, h1("Southwest Elementary")),
                          column(width=4, h1("Hillside High"))
                 ),
                 fluidRow(class = "text-center", width=12,
                          column(width=4, h4("Click below to view info on Parkwood Elementary.")),
                          column(width=4, h4("Click below to view info on Southwest Elementary.")),
                          column(width=4, h4("Click below to view info on Hillside High."))
                 ),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=4,
                                 actionButton("button", "View Parkwood Elementary >>")),
                          column(width=4,
                                 actionButton("button", "View Southwest Elementary >>")),
                          column(width=4,
                                 actionButton("button", "View Hillside High >>"))
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=12,
                                 img(src="School Image_nocircle.png", width=200))
                 ),
                 fluidRow(class = "text-center", width=12,
                          column(width=12, h1("Jordan High"))
                 ),
                 fluidRow(class = "text-center", width=12,
                          column(width=12, h4("Click below to view info on Jordan High."))
                 ),
                 br(),
                 fluidRow(class = "text-center", width=12,
                          column(width=12,
                                 actionButton("button", "View Jordan High >>"))
                 ),
                 br(),
                 br()
        )},
        
        #School Pages
        tabItem(tabName = "CCS",
                h2("Insert cc Info")
        ),
        tabItem(tabName = "EastWay",
                h2("Insert Eastway Info")
        ),
        tabItem(tabName = "EKPowe",
                h2("Insert EK Powe Info")
        ),
        tabItem(tabName = "FayettevilleSt",
                h2("Insert Fayetteville St Info")
        ),
        tabItem(tabName = "ForestView",
                h2("Insert Forest View Info")
        ),
        tabItem(tabName = "Lakewood",
                h2("Insert Lakewood Info")
        ),
        tabItem(tabName = "Parkwood",
                h2("Insert Parkwood Info")
        ),
        tabItem(tabName = "Southeast",
                h2("Insert Southeast Info")
        ),
        tabItem(tabName = "Hillside",
                h2("Insert Hillside Info")
        ),
        tabItem(tabName = "Jordan",
                h2("Insert CE Jordan Info")
        ),
        
        #School Stats
        {tabItem(tabName = "statstab",
                 fluidRow(
                     #Box Plot Outputs
                     box(title = strong("Charts"), plotlyOutput("barplots",width="auto",height = "auto")
                     ),
                     #Drop Down Widget for Box Plots
                     box(title = strong("Measurements, Context, and Resources"), box(width = 12,
                                                                                                      title = strong("Select a Measurement"),
                                                                                                      selectInput("select", em("Click the drop down menu to select which measurement you would like to view."), 
                                                                                                                  choices = list("Average Class Size","Bachelor Degree Rate","Diversity per District", "Enrollment","ESL Students","Experienced Teacher Ratio",
                                                                                                                                 "Free/Red Lunch","Funding Per Pupil","Homesale Price","Household Income", "In-School Suspensions (ISS)",
                                                                                                                                 "Median Age","POC per School","Race per School", "Racial Demographics", "Racial Differential","School and Zone Racial Breakdown",
                                                                                                                                 "Sidewalk Coverage","Students Per Device","Student-Teacher Ratio, Elementary School","Student-Teacher Ratio, High School", 
                                                                                                                                 "Students With Disabilities")
                                                                                                      )),
                         box(width = 12,
                             title = strong("Context & Resources"),
                             htmlOutput("resources")
                         )
                     ),
                 )
        )},
        
        #Map Data
        tabItem(tabName = "mapstab",
                fluidRow(
                    box(width  = 12,
                        title = "Map",
                        leafletOutput("map"))
                ),
                fluidRow(
                    box(width = 4,
                        title = "Drop Down Select",
                        selectInput("var",
                                    label = "Choose a variable to display",
                                    choices = c("Bus Stops", 
                                                "Childcare Centers", "Community & Cultural Centers", "Gardens",
                                                "Grocery Stores", "Libraries", "Parks", 
                                                "Recreation Centers", "Religious Centers"),
                                    multiple = FALSE),
                        selectInput("zone",
                                    label = "Choose a school zone to display",
                                    choices = c("All", "C.C. Spaulding Elementary", "Eastway Elementary",
                                                "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                                "Forest View Elementary", "Hillside High",
                                                "Jordan High","Lakewood Elementary", 
                                                "Parkwood Elementary", "Southwest Elementary"),
                                    multiple = FALSE)),
                    box(width = 8,
                        title = "Pop Up Description"),
                ),
                fluidRow(
                    box(width = 4,
                        title = "Additional Resources")
                )
        )
    )
)
}

shinyApp(
    ui = dashboardPage(
        dashboardHeader(title = "Visualizing DPS"),
        sidebar,
        body
    ),
    server = function(input, output) { 
        output$barplots <- renderPlotly({
            if(input$select == "Median Age") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
                p <- ggplot(schoolstats_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 35.2), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Median Age of School Assignment Zones", x = "School", y = "Median Age", caption= "line represents Durham County Avg")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Experienced Teacher Ratio"){
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
                p <- ggplot(schoolstats_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = .79), color ='red') +
                    scale_y_continuous(labels = scales::percent) +
                    coord_flip() +
                    theme_minimal() +
                    labs(title = "Experienced Teacher Ratio", x = "School", y = "Ratio")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Free/Red Lunch"){
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = FREE_RED_PERCENT), hjust = 2, color = "black") +
                    scale_y_continuous(labels = scales::percent) +
                    geom_hline(aes(text="Durham County Average", yintercept = .5165), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Students Enrolled in Free and Reduced Lunch", x = "School", y = "Number of Students")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Student-Teacher Ratio, Elementary School") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 20.5), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Elementary School Student-Teacher Ratio", x = "School", y = "Ratio")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Student-Teacher Ratio, High School") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_HS)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_HS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_HS), y=STUDENT_TEACHER_HS)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = STUDENT_TEACHER_HS), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 24), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "High School Student-Teacher Ratio", x = "School", y = "Ratio")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Average Class Size") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = AVG_CLASS_SIZE), hjust = 2, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 19), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Average Class Size", x = "School", y = "Average # of Students")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Students Per Device") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = .8), color ='red') +
                    scale_y_continuous(labels = scales::percent) +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Students Per Device", x = "School", y = "Number of Students")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Funding Per Pupil") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 11672), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    labs(title = "Funding Per Pupil", x = "School", y = "Amount of Funding in USD")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Students With Disabilities") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
                    scale_y_continuous(labels = scales::percent) +
                    coord_flip() +
                    geom_hline(aes(text="Durham County Average", yintercept = .133), color ='red') +
                    theme_minimal() +
                    labs(title = "Percent of Students with Disabilities", x = "School", y = "Percent of Students")
                ggplotly(p, tooltip = c("label"))
            } else if(input$select == "ESL Students") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
                    scale_y_continuous(labels = scales::percent) +
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = .158), color ='red') +
                    labs(title = "Percent of ESL Enrolled Students Per School", x = "School", y = "Percent of Students")
                ggplotly(p, tooltip = c("label"))
            } else if(input$select == "In-School Suspensions (ISS)") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = -.1, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 188.92), color ='red') +
                    coord_flip(y=c(0,900)) +
                    theme_minimal() +
                    labs(title = "In-School Suspensions Per School", x = "School", y = "Students Per 1000")
                ggplotly(p, tooltip = c("text", "yintercept"))
            }else if(input$select == "Enrollment") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT)
                p <-  ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -ENROLLMENT), ENROLLMENT)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = ENROLLMENT), vjust = 0)+
                    labs(title = "Enrollment per School" , x = "School", y = "Enrollment")
                ggplotly(p)
            }else if(input$select == "Household Income") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 58190), color ='red') +
                    geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
                    labs(title = "Median Household Income", y = "Median Household Income ($)", x = "School")
                ggplotly(p)
            }else if(input$select == "Homesale Price") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 278000), color ='red') +
                    geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
                    labs(title = "Median Homesale Price", y = "Median Homesale Price ($)", x = "School")
                ggplotly(p)
            }else if(input$select == "Bachelor Degree Rate") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = .441), color ='red') +
                    geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
                    labs(title = "Bachelor Degree Rate per School Zone", y = "Bachelor Degree Rate", x = "School")
                ggplotly(p)
            }else if(input$select == "Sidewalk Coverage") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
                    geom_hline(aes(text="Durham County Average", yintercept = .35), color ='red') +
                    labs(title = "Sidewalk Coverage per School Zone", y = "Sidewalk Coverage (%)", x = "School")
                ggplotly(p)
            }else if(input$select == "Diversity per District") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(DIVERSITY_DISTRICT)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -DIVERSITY_DISTRICT), DIVERSITY_DISTRICT)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = .51), color ='red') +
                    geom_text(aes(label = DIVERSITY_DISTRICT), vjust = 0)+
                    labs(title = "Diversity per School Zone", y = "Diversity (%)", x = "School")
                ggplotly(p)
            }else if(input$select == "School and Zone Racial Breakdown"){
                p <- ggplot(race, aes(factor(place), number, fill = sorz)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    coord_flip() +
                    theme_minimal() +
                    labs(title = "Racial Composition of Schools vs. School Zones" , x = "School", y = "Percentage of Students of Color", fill=" ")
                ggplotly(p)
            } else if(input$select == "Racial Differential"){
                p <- ggplot(race_diff, aes(reorder(place, -number), number)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = number), vjust = 0)+
                    labs(title = "Difference in % of Students of Color between Schools and Zones" , x = "School", y = "Difference in Students of Color (%)")
                ggplotly(p)
            } else if(input$select == "POC per School") {
                p <- ggplot(poc_per_school, aes(reorder(place, -number), number)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = number), vjust = 0)+
                    labs(title = "Percentage of Students of Color" , x = "School", y = "Students of Color (%)")
                ggplotly(p)
            } else if(input$select == "Funding per Pupil") {
                p <- ggplot(funding, aes(reorder(place, -number), number)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = number), vjust = 0)+
                    labs(title = "Funding per Pupil" , x = "School", y = "Funding per Pupil ($)")
                ggplotly(p)
                
                
            } else if(input$select == "Racial Demographics") {
                p3 <- ggplot(all_race, aes(factor(school), number, fill = race)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    coord_flip() +
                    theme_minimal() +
                    labs(title = "Racial Composition of Schools" , x = "School", y = "Percentage of Students", fill="Race")
                ggplotly(p3)
            } 
            
            else if(input$select == "Race per School") {
                p <- ggplot(all_race, aes(factor(x=school), y=number, fill=race)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    coord_flip() +
                    theme_minimal() +
                    facet_wrap(~school)+
                    labs(title = "Racial Composition", subtitle="Faceted by School" , x = "Race", y = "Percentage of Students (%)")
                ggplotly(p)
            }
        })
        
        output$resources <- renderText({
            if(input$select == "Median Age") {
                paste("Here are some resouces for Median Age.")
            } else if (input$select == "Experienced Teacher Ratio") {
                paste("Research shows teacher experience does not directly correlate to effective teaching. 
        With the evolution of research-based practices, it is important to continue to educate new and 
        returning teachers on the best teaching practices that are scientifically-proven to promote 
        student success. Good professional development workshops are paramount to provide effective, 
        culturally-responsive teaching practices.", "<br>","<br>","Below are articles on Teacher 
              Experience and Professional Development:", "<br>",
                      a("Effects of Hiring and Recommendations for Supporting Experienced Teachers", 
                        href = "https://learningpolicyinstitute.org/product/does-teaching-experience-increase-teacher-effectiveness-review-research"),"<br>",
                      a("Teacher Experience in Relation to Teacher Effectiveness", 
                        href ="https://www.nea.org/advocating-for-change/new-from-nea/does-teaching-experience-matter-lets-count-ways"),"<br>",
                      a("NCDPI Educator Professional Development", 
                        href ="https://www.dpi.nc.gov/educators/professional-development"),"<br>",
                      a("Importance and Resources for Professional Development", 
                        href ="https://www.nea.org/professional-excellence/professional-learning/teachers"))
            } else if (input$select == "Free/Red Lunch") {
                paste("The percent of students receiving free and reduced lunch is a great indicator of 
        socioeconomic statuses. The percentage of students that fall below the poverty line 
        determines if a school is considered Title 1. Title 1 schools are eligible to receive grants 
        through the Every Student Succeeds Act (ESEA). It is important to be cognizant of students’ 
        socioeconomic statuses without being condescending and prejudiced. Socioeconomic status is 
        not a limit, it is a barrier.", "<br>","<br>",
                      "Below are articles on Free/Reduced Lunch and its connection to Title 1 schools:", "<br>",
                      a("Rural Schools and Free/Reduced Lunch", 
                        href = "https://www.nea.org/advocating-for-change/new-from-nea/whos-looking-out-rural-schools"), "<br>",
                      a("NC Community Free/Reduced Lunch Eligibility",
                        href = "https://www.dpi.nc.gov/districts-schools/federal-program-monitoring/title-i-part/community-eligibility-free-and-reduced-price-meals"))
            } else if (input$select == "Student-Teacher Ratio, Elementary School"){
                paste("Research proves smaller student-teacher ratios have a positive effect on student achievement. 
        By allowing for more centralized and one-on-one instruction, smaller student-teacher ratios 
        can increase test scores, lower dropout rates, and increase graduation rates.","<br>","<br>",
                      "Resources on Student-Teacher Ratios:","<br>",
                      a("Infographics and Information on Student-Teacher Ratios",
                        href = "https://www.hunschool.org/resources/student-teacher-ratios"))
            } else if (input$select == "Student-Teacher Ratio, High School"){
                paste("Research proves smaller student-teacher ratios have a positive effect on student achievement. 
        By allowing for more centralized and one-on-one instruction, smaller student-teacher ratios 
        can increase test scores, lower dropout rates, and increase graduation rates.","<br>","<br>",
                      "Resources on Student-Teacher Ratios:","<br>",
                      a("Infographics and Information on Student-Teacher Ratios",
                        href = "https://www.hunschool.org/resources/student-teacher-ratios"))
            } else if (input$select == "Average Class Size"){
                paste("Research proves smaller class sizes are beneficial to student achievement. Smaller classes 
              allow for the teacher to focus less on classroom management and more on centralized learning. 
              Students stated they feel more comfortable in smaller classes as well.","<br>","<br>",
                      "Resources Discussing the Importance of Class Size:","<br>",
                      a("The Benefits of Investing in Smaller Class Sizes",
                        href = "https://www.nea.org/advocating-for-change/new-from-nea/educators-and-parents-reset-class-size-debate"), "<br>",
                      a("State Policy View on Class Size",
                        href = "https://www.brookings.edu/research/class-size-what-research-says-and-what-it-means-for-state-policy/"))
            } else if (input$select == "Students Per Device"){
                paste("Living in a digital age, technology usage in the classroom has increased tremendously, 
        especially because of the COVID-19 pandemic. Although technology is a great resource, students 
        may not have equitable access to these devices. It is important for students and teachers to 
        not only have access to these technological devices, but also understand how to use them. 
        Professional development is necessary to keep educators up to date on any new technology 
        entering the classroom to create the most effective learning environment.","<br>","<br>",
                      "Resources on Access and Technology in the Classroom:", "<br>",
                      a("Durham Public Schools’ Technological Services",
                        href = "https://www.dpsnc.net/site/default.aspx?PageType=3&DomainID=207&ModuleInstanceID=8115&ViewID=6446EE88-D30C-497E-9316-3F8874B3E108&RenderLoc=0&FlexDataID=42210&PageID=4738"), "<br>",
                      a("Equitable Access to Technology",
                        href = "https://digitalpromise.org/2019/04/29/equity-in-schools-access-technology/"))
            } else if (input$select == "Funding Per Pupil"){
                paste("Here are some resouces on school funding.")
            } else if (input$select == "Students With Disabilities"){
                paste("Here are some resouces on students with disabilities.")
            } else if (input$select == "ESL Students"){
                paste("Here are some resouces on ESL students and programs.")
            } else if (input$select == "In-School Suspensions (ISS)"){
                paste("Students of color are more susceptible to harsher punishments in schools.
             Black students are subject to higher disciplinary actions compared to their white peers.
             A reason for this is racial bias leading to the overpolicing of Black students, fueling the school-to-prison pipeline.", "<br>","<br>",
                      "Below are articles on In-School Suspensions and the School-to-Prison Pipeline:", "<br>", 
                      a("Racial Bias in School Discipline", 
                        href = "https://www.pnas.org/content/116/17/8255"), "<br>",
                      a("School-to-Prison Pipeline", 
                        href = "https://www.nea.org/advocating-for-change/new-from-nea/school-prison-pipeline-time-shut-it-down"))
            }else if(input$select == "Enrollment") {
                paste("Here are some resouces for school enrollment numbers.")
            }
            else if (input$select == "School and Zone Racial Breakdown") {
                paste("Here are some resouces for differences in school zone and school racial demographics.")
            }
            else if (input$select == "Racial Differential") {
                paste("Here are some resouces for differences in school zone and school racial demographics.")
            }
            else if (input$select == "POC per School"){
                paste("Here are some resouces for the number of students of color in a school.")
                
            }
            else if (input$select == "Funding per Pupil"){
                paste("Here are some resouces for school funding.")
            }
            else if (input$select == "Racial Demographics"){
                paste("Here are some resouces for racial demographics.")
            }
            else if (input$select == "Race per School"){
                paste("Here are some resouces on racial demographics.")
            }
            else if (input$select == "Household Income"){
                paste("Here are some resouces on household income rates.")
            }
            else if (input$select == "Homesale Price"){
                paste("Here are some resouces on homesale prices..")
            }
            else if (input$select == "Bachelor Degree Rate"){
                paste("Here are some resouces about bachelor degree rates.")
            }
            
            else if (input$select == "Sidewalk Coverage"){
                paste("Here are some resouces about sidewalk coverage.")
            }
            
            else if (input$select == "Diversity per District"){
                paste("Here are some resouces about diversity in school districts.")
            }
            
        })
        
        displayVar <- reactive({
            switch(input$var,
                   "Parks" = parks, 
                   "Recreation Centers" = rec, 
                   "Gardens" = gardens, 
                   "Bus Stops" = bus, 
                   "Childcare Centers" = childcare, 
                   "Community & Cultural Centers" = cultural, 
                   "Grocery Stores" = grocery, 
                   "Libraries" = libraries, 
                   "Religious Centers" = religious)
        })
        
        displayIcon <- reactive({
            switch(input$var,
                   "Parks" = iconSet$parks, 
                   "Recreation Centers" = iconSet$rec, 
                   "Gardens" = iconSet$gardens, 
                   "Bus Stops" = iconSet$bus, 
                   "Childcare Centers" = iconSet$childcare, 
                   "Community & Cultural Centers" = iconSet$cultural, 
                   "Grocery Stores" = iconSet$grocery, 
                   "Libraries" = iconSet$libraries, 
                   "Religious Centers" = iconSet$religious)
        })
        
        displaySchool <- reactive({
            schoolstats %>% filter(name == input$zone)
        })
        
        displayZone <- reactive({
            switch(input$zone,
                   "C.C. Spaulding Elementary" = cc, 
                   "Eastway Elementary" = eastway,
                   "E.K. Powe Elementary" = ek, 
                   "Fayetteville Street Elementary" = fayetteville, 
                   "Forest View Elementary" = forest,
                   "Hillside High" = hillside,
                   "Jordan High" = jordan,
                   "Lakewood Elementary" = lakewood, 
                   "Parkwood Elementary" = parkwood, 
                   "Southwest Elementary" = southwest, 
                   "All" = durham)
        })
        
        displayColor <- reactive({
            switch(input$zone,
                   "C.C. Spaulding Elementary" = "red", 
                   "Eastway Elementary" = "orange",
                   "E.K. Powe Elementary" = "yellow", 
                   "Fayetteville Street Elementary" = "green", 
                   "Forest View Elementary" = "blue",
                   "Hillside High" = "violet",
                   "Jordan High" = "pink",
                   "Lakewood Elementary" = "darkred", 
                   "Parkwood Elementary" = "lightblue", 
                   "Southwest Elementary" = "brown", 
                   "All" = "gray")
        })
        
        output$map <- renderLeaflet({
            leaflet(displayZone()) %>%
                addProviderTiles("CartoDB.Positron") %>%
                addPolygons(data = displayZone(),
                            fillColor = displayColor(),
                            stroke = TRUE,
                            fillOpacity = 0.75,
                            smoothFactor = 1) %>%
                addMarkers(data = displayVar(), lng = ~LONGITUDE, lat= ~LATITUDE, 
                           label = displayVar()$name, icon = displayIcon(), 
                           clusterOptions = markerClusterOptions()) %>%
                addMarkers(data = displaySchool(), lng = ~LONGITUDE, lat = ~LATITUDE, 
                           label = displaySchool()["name"])
        })
        
    }
)


