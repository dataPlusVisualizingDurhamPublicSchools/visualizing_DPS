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
library(slickR)
library(leaflet)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)
library(gotop)

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
schools <- read.csv("schools.csv")

schoolstats$name <- c("C.C. Spaulding Elementary", "Eastway Elementary",
                      "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                      "Forest View Elementary", "Lakewood Elementary", "Parkwood Elementary",
                      "Southwest Elementary", "Hillside High","Jordan High", "All")

#Icons

iconSet <- iconList(
    parks = makeIcon("https://img.icons8.com/windows/32/000000/tree.png", iconWidth=20, iconHeight=20),
    rec = makeIcon("https://img.icons8.com/material-outlined/24/000000/children.png", iconWidth=20, iconHeight=20),
    gardens = makeIcon("https://img.icons8.com/dotty/80/000000/flower.png", iconWidth=20, iconHeight=20),
    bus = makeIcon("https://img.icons8.com/material-outlined/24/000000/bus.png", iconWidth=20, iconHeight=20),
    childcare = makeIcon("https://img.icons8.com/material-outlined/24/000000/rocking-horse.png", iconWidth=20, iconHeight=20),
    cultural = makeIcon("https://img.icons8.com/windows/32/000000/crowd.png", iconWidth=20, iconHeight=20),
    grocery = makeIcon("https://img.icons8.com/ios/50/000000/grocery-store.png", iconWidth=20, iconHeight=20),
    libraries = makeIcon("https://img.icons8.com/windows/32/000000/book--v1.png", iconWidth=20, iconHeight=20),
    religious = makeIcon("https://img.icons8.com/fluent-systems-regular/48/000000/chapel.png", iconWidth=20, iconHeight=20),
    uni = makeIcon("https://img.icons8.com/wired/64/000000/university-campus.png", iconWidth = 40, iconHeight = 40),
    schools = makeIcon("https://img.icons8.com/material-sharp/24/000000/school-building.png", iconWidth = 20, iconHeight = 20)
)

sidebar <- {dashboardSidebar(
    sidebarMenu(
        id = "TabItems",
        menuItem("Home", tabName = "home", icon = icon("fas fa-home")),
        menuItem("Maps", tabName = "mapstab", icon = icon("fas fa-map-marked-alt")),
        menuItem("School Statistics", tabName = "statstab", icon = icon("fas fa-chart-bar"))
    )
)
}

body <- {dashboardBody(
    tabItems(
        #Landing Page
        {tabItem(tabName = "home",
                 fluidRow(
                     class = "text-center",
                     box(h3(strong("Visualizing Durham Public Schools")),
                         width = 12,
                         background = "navy",
                         br(),
                         p("This project is inspired by an inter-institutional Bass Connections team from Duke and
                            North Carolina Central University that is committed to developing more responsible and
                            imaginative ways of partnering with Durham Public Schools. The objective of this project is to provide a 
                            centralized web application that will serve as a tool for
                            those entering Durham Public Schools. Our research aims to inform future pre-service trainings for university students, 
                            support local neighborhood schools in visualizing their communities, 
                            and help varied university offices articulate what “community” actually looks like."),
                         br(),
                         p("Through spatial and school-specific data, along with 
                            contextual resources, we hope to provide a holistic view of Durham Public schools and their communities -
                            highlighting their resources and assets." ),
                         p("Visit the", a("Visualizing DPS and Bass Connections Website",
                             href = "https://bassconnections.duke.edu/project-teams/strengthening-partnerships-between-durham-public-schools-and-local-universities-2021", target="_blank"),
                           "for more information!"))),
                 fluidRow(
                     class = "text-center",
                     box(
                         solidHeader = TRUE,
                         br(),
                         width = 12,
                         valueBox(9, "Geospatial Variables", icon = icon("map"), color = "light-blue", width = 4),
                         valueBox(21, "School-Specific Variables", icon = icon("pencil"), color = "light-blue", width = 4),
                         valueBox(1, "Centralized Web Application", icon = icon("window-restore"), color = "light-blue", width = 4))),
                 fluidRow(
                        class = "text-center",
                        box(title = strong("2 Universities, 10 Public Schools"),
                            solidHeader = TRUE,
                            width = 7,
                            p("The Durham Public Schools District contains 54 public schools: 
                                30 elementary, 9 middle, 2 secondary, 11 high, 1 alternative, and 
                                1 hospital. Our project focuses on the 10 schools that are most 
                                partnered with by Duke University and North Carolina Central 
                                University. These include 8 elementary schools: C.C. Spaulding, 
                                Eastway, E.K. Powe, Fayetteville, Forest View, Lakewood, Parkwood, 
                                Southwest, and 2 high schools: Hillside and Jordan."),
                            br(),
                            p("The shared goal of both universities is to foster equitable partnernships with 
                            Durham Public Schools. Based on previous Bass Connections research, an issue preventing 
                            meaningful engagement between Duke, NCCU, and the public schools is that “many 
                            university students lack an understanding of city and community dynamics”. Additionally, 
                            there is a “lack of student volunteer training with Durham’s context, particularly in 
                            the areas of history, school-specific demographics, and implicit bias and power dynamics 
                            that may manifest in schools."),
                            br(),
                            p(strong("Inspired by the community schools model, our project 
                            explores a way of visualizing schools as the “center of the community that 
                            brings together academics, health and social services, youth and community development 
                            and community engagement under one roof."))),
                        box(width = 5,
                            background = "light-blue",
                            solidHeader = TRUE,
                            leafletOutput("home"))),
                 fluidRow(class = "text-center",
                          box(title = strong("View Our 10 Schools"),
                              width = 12,
                              background = "light-blue",
                              actionButton("viewMap", "View Spatial Data"),
                              actionButton("viewStat", "View School Statistics"))),
                 fluidRow(
                       box(width = 12,
                           solidHeader = TRUE,
                           background = "navy",
                           fluidRow(width = 12,
                                    class = "text-center",
                                    h3(strong("Meet Our Team"))),
                           fluidRow(width = 12,
                                    imageOutput("ally")),
                                #imageOutput("patience"),
                                #imageOutput("rhea"),
                                #imageOutput("drew")),
                           fluidRow(width = 12,
                                    p("thinking about including all of our pictures and short bios")))),
                 fluidRow(class = "text-center",
                     box(width = 5,
                         background = "light-blue",
                         solidHeader = TRUE,
                         title = strong("Our Partners"),
                         box(width = 12,
                             solidHeader = TRUE,
                            p(h4(a("Durham Public Schools", href = "https://www.dpsnc.net/"))),
                            br(),
                            p(a("Duke University", href = "https://duke.edu/")),
                            br(),
                            p(a("Data+", href = "https://bigdata.duke.edu/")),
                            br(),
                            p(a("Bass Connections", href = "https://bassconnections.duke.edu/")))),
                        box(width = 7,
                            solidHeader = TRUE,
                            slickROutput("slickr", width = "auto"),
                            use_gotop()))
        )},
        
        #School Stats
        {tabItem(tabName = "statstab",
                 fluidRow(
                     #Box Plot Outputs
                     box(background = "navy", 
                         solidHeader = TRUE, 
                         title = strong("Charts"), 
                         plotlyOutput("barplots",
                                        width="auto",
                                        height = "auto")
                     ),
                     #Drop Down Widget for Box Plots
                     box(solidHeader = TRUE, 
                         title = strong("Measurements, Context, and Resources"), 
                         box(width = 12,
                            title = strong("Select a Measurement"),
                            selectInput("select", em("Click the drop down menu to select which measurement you would like to view."), 
                            choices = list("Advanced Placement (AP) Course Enrollment", "Average Class Size","Bachelor Degree Rate","CTE Enrollment Rate, High School",#"Diversity per School Zone", 
                                   "Enrollment","ESL Students","Experienced Teacher Ratio",
                                    "Free/Red Lunch","Funding Per Pupil","Graduation Rate","Homesale Price","Household Income", "In-School Suspensions (ISS)",
                                    "Median Age","POC per School",#"Race per School", 
                                    "Racial Demographics", #"Racial Differential",
                                    "School and Zone Racial Breakdown",
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
                        background = "navy",
                        solidHeader = TRUE,
                        title = strong("Interactive Map"),
                        leafletOutput("map"))
                ),
                fluidRow(
                    box(width = 4,
                        solidHeader = TRUE,
                        title = strong("Measurement"),
                        selectInput("var",
                                    label = em("Choose a variable to display"),
                                    choices = c("Bus Stops", 
                                                "Childcare Centers", "Community & Cultural Centers", "Gardens",
                                                "Grocery Stores", "Libraries", "Parks", 
                                                "Recreation Centers", "Religious Centers"),
                                    multiple = FALSE),
                        selectInput("zone",
                                    label = em("Choose a school zone to display"),
                                    choices = c("All", "C.C. Spaulding Elementary", "Eastway Elementary",
                                                "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                                "Forest View Elementary", "Hillside High",
                                                "Jordan High","Lakewood Elementary", 
                                                "Parkwood Elementary", "Southwest Elementary"),
                                    multiple = FALSE)),
                    box(width = 8,
                        solidHeader = TRUE,
                        title = strong("Context")),
                )
                ,
                fluidRow(
                    box(width = 4,
                        solidHeader = TRUE,
                        title = strong("Additional Resources"))
                )
        )
    )
)
}



shinyApp(
    ui = dashboardPage(
        skin = "black",
        dashboardHeader(
            title = "Visualizing DPS", tags$li(class = "dropdown", actionButton("Our Ten Schools - Home", "Home"))),
        sidebar,
        body
    ),
    server = function(input, output, session) { 
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
                    labs(title = "Median Age of School Assignment Zones", x = "School Zone", y = "Median Age", caption= "line represents Durham County Avg")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Experienced Teacher Ratio"){
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
                p <- ggplot(schoolstats_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 79), color ='red') +
                    coord_flip() +
                    theme(plot.title = element_text(hjust = .5)) +
                    theme_minimal() +
                    labs(title = "Experienced Teacher Ratio", x = "School", y = "Ratio")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Free/Red Lunch"){
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = FREE_RED_PERCENT), hjust = 2, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 51.65), color ='red') +
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
            } else if(input$select == "Enrollment") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
                p <-  ggplot(schoolstats_summary[!is.na(schoolstats_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = ENROLLMENT_NA), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Enrollment per School" , x = "School", y = "Enrollment")
                ggplotly(p)
            }else if(input$select == "Students Per Device") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = .8), color ='red') +
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
                    scale_y_continuous(labels=scales::dollar_format()) +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Funding Per Pupil", x = "School", y = "Amount of Funding in USD")
                ggplotly(p, tooltip = c("text", "yintercept"))
            } else if(input$select == "Students With Disabilities") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
                    coord_flip() +
                    geom_hline(aes(text="Durham County Average", yintercept = 13.3), color ='red') +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Percent of Students with Disabilities", x = "School", y = "Percent of Students")
                ggplotly(p, tooltip = c("label"))
            } else if(input$select == "ESL Students") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 15.8), color ='red') +
                    theme(plot.title = element_text(hjust = .5)) +
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
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "In-School Suspensions Per School", x = "School", y = "Students Per 1000")
                ggplotly(p, tooltip = c("text", "yintercept"))
            }else if(input$select == "Household Income") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    scale_y_continuous(labels=scales::dollar_format()) +
                    geom_hline(aes(text="Durham County Average", yintercept = 58190), color ='red') +
                    geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Median Household Income", y = "Median Household Income ($)", x = "School Zone")
                ggplotly(p)
            }else if(input$select == "Homesale Price") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    scale_y_continuous(labels=scales::dollar_format()) +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 278000), color ='red') +
                    geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Median Homesale Price", y = "Median Homesale Price ($)", x = "School Zone")
                ggplotly(p)
            }else if(input$select == "Bachelor Degree Rate") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 44.1), color ='red') +
                    geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Bachelor Degree Rate per School Zone", y = "Bachelor Degree Rate", x = "School Zone")
                ggplotly(p)
            }else if(input$select == "Sidewalk Coverage") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
                    geom_hline(aes(text="Durham County Average", yintercept = 35), color ='red') +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Sidewalk Coverage per School Zone", y = "Sidewalk Coverage (%)", x = "School Zone")
                ggplotly(p)
            }else if(input$select == "Diversity per School Zone") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(DIVERSITY_DISTRICT)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -DIVERSITY_DISTRICT), DIVERSITY_DISTRICT)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 51), color ='red') +
                    geom_text(aes(label = DIVERSITY_DISTRICT), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Diversity per School Zone", y = "Diversity (%)", x = "School Zone")
                ggplotly(p)
            }else if(input$select == "School and Zone Racial Breakdown"){
                p <- ggplot(race, aes(factor(place), number, fill = sorz)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Racial Composition of Schools vs. School Zones" , x = "School/School Zone", y = "Percentage of Students of Color", fill=" ")
                ggplotly(p)
            }
            # } else if(input$select == "Racial Differential"){
            #     p <- ggplot(race_diff, aes(reorder(place, -number), number)) + 
            #         geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
            #         coord_flip() +
            #         theme_minimal() +
            #         geom_text(aes(label = number), vjust = 0)+
            #         theme(plot.title = element_text(hjust = .5)) +
            #         labs(title = "Difference in % of Students of Color between Schools and Zones" , x = "School/School Zones", y = "Difference in Students of Color (%)")
            #     ggplotly(p)
            
            else if(input$select == "POC per School") {
                p <- ggplot(poc_per_school, aes(reorder(place, -number), number)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 80.7), color ='red') +
                    geom_text(aes(label = number), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Percentage of Students of Color" , x = "School", y = "Students of Color (%)")
                ggplotly(p)
            } else if(input$select == "Funding per Pupil") {
                p <- ggplot(funding, aes(reorder(place, -number), number)) + 
                    geom_bar(stat="identity", position = "dodge", fill="powder blue") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_text(aes(label = number), vjust = 0)+
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Funding per Pupil" , x = "School", y = "Funding per Pupil ($)")
                ggplotly(p)
                
                
            } else if(input$select == "Racial Demographics") {
                p3 <- ggplot(all_race, aes(factor(school), number, fill = race)) + 
                    geom_bar(stat="identity", position = "dodge") + 
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Racial Composition of Schools" , x = "School", y = "Percentage of Students", fill="Race")
                ggplotly(p3)
            } 
            
            # else if(input$select == "Race per School") {
            #     p <- ggplot(all_race, aes(factor(x=school), y=number, fill=race)) + 
            #         geom_bar(stat="identity", position = "dodge") + 
            #         coord_flip() +
            #         theme_minimal() +
            #         facet_wrap(~school)+
            #         theme(plot.title = element_text(hjust = .5)) +
            #         labs(title = "Racial Composition", subtitle="Faceted by School" , x = "Race", y = "Percentage of Students (%)")
            #     ggplotly(p)
            # } 
            else if(input$select == "CTE Enrollment Rate, High School") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") + #no Durham County AVG
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average", yintercept = 53), color ='red') +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Percent of Students Enrolled in CTE Courses", x = "School", y = "Percent of Students")
                ggplotly(p)
            } else if(input$select == "Graduation Rate") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(GRADUATION_RATE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$GRADUATION_RATE),], aes(x=reorder(SCHOOL_NAME, -GRADUATION_RATE), y=GRADUATION_RATE)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = GRADUATION_RATE), hjust = -.1, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 83.5), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Graduation Rate", x = "School", y = "Percent of Students")
                ggplotly(p, tooltip = c("text", "yintercept"))
            }else if(input$select == "Advanced Placement (AP) Course Enrollment") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ADV_COURSES_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ADV_COURSES_PERCENT),], aes(x=reorder(SCHOOL_NAME, -ADV_COURSES_PERCENT), y=ADV_COURSES_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "powder blue", color = "white") +
                    geom_text(aes(label = ADV_COURSES_PERCENT), hjust = -.1, color = "black") +
                    geom_hline(aes(text="Durham County Average", yintercept = 9.22), color ='red') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = .5)) +
                    labs(title = "Advanced Placement Course Enrollment", x = "School", y = "Percent of Students")
                ggplotly(p, tooltip = c("text", "yintercept"))
            }
        })
        
        output$resources <- renderText({
            if(input$select == "Advanced Placement (AP) Course Enrollment") {
                paste("Advanced Placement (AP) courses are challenging, collegiate-level courses high school 
                students can take for an opportunity to receive college credit upon scoring a 3 or higher on 
                the standardized assessment. Additionally, it weighs more than an honors course on the high 
                school level. Hillside High offers 26 AP courses while Jordan High offers 19.", "<br>","<br>",
                      "Below is more information about AP courses:", "<br>", 
                      a("NCDPI AP Courses", 
                        href = "https://www.dpi.nc.gov/students-families/enhanced-opportunities/advanced-learning-and-gifted-education/advanced-coursework/advanced-placement"), "<br>",
                      a("DPS AP Courses", 
                        href = "https://www.dpsnc.net/Page/430"),
                      a("College Board",
                        href="https://apstudents.collegeboard.org/course-index-page"))
            }else if (input$select == "Average Class Size"){
                paste("Research proves smaller class sizes are beneficial to student achievement. Smaller classes 
              allow for the teacher to focus less on classroom management and more on centralized learning. 
              Students stated they feel more comfortable in smaller classes as well.","<br>","<br>",
                      "Resources Discussing the Importance of Class Size:","<br>",
                      a("The Benefits of Investing in Smaller Class Sizes",
                        href = "https://www.nea.org/advocating-for-change/new-from-nea/educators-and-parents-reset-class-size-debate"), "<br>",
                      a("State Policy View on Class Size",
                        href = "https://www.brookings.edu/research/class-size-what-research-says-and-what-it-means-for-state-policy/"))
            }
            else if (input$select == "CTE Enrollment Rate, High School"){
                paste("Career and Technical Education (CTE) courses are designed for high school students to receive real-world experience 
                      in the career field they are most interested in. Durham Public Schools started the “3-2-1” initiative in 2019 where 
                      all high school students are required to take 3 CTE courses, participate in 2 career exposure activities, and get an 
                      internship or a job before they graduate. This initiative, as well as all of CTE courses, are created to develop 
                      students’ soft skills, gain real-world experience, and help students decide on their post-graduate plans.", "<br>","<br>",
                    "Below is more information about CTE courses in Durham Public Schools and North Carolina:", "<br>",
                    a("DPS CTE Course Initiative",
                      href = "https://www.dpsnc.net/domain/293"), "<br>",
                    a("NCDPI CTE Course Overview",
                      href = "https://www.dpi.nc.gov/districts-schools/classroom-resources/career-and-technical-education")
                )
                
            }else if (input$select == "Experienced Teacher Ratio") {
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
                paste("The percent of students receiving free and reduced lunch is a strong indicator of 
        socioeconomic statuses. The percentage of students that fall below the poverty line 
        determines if a school is considered ", HTML(paste0(strong("Title I"),tags$sup("1"))), ".", "Title I schools are eligible to receive grants 
        through the Every Student Succeeds Act (ESEA). It is important to be cognizant of students’ 
        socioeconomic statuses without being condescending and prejudiced. Socioeconomic status is 
        not a limit, it is a barrier.", "<br>","<br>",
                      "Below are articles on Free/Reduced Lunch and its connection to Title I schools:", "<br>",
                      a("Rural Schools and Free/Reduced Lunch", 
                        href = "https://www.nea.org/advocating-for-change/new-from-nea/whos-looking-out-rural-schools"), "<br>",
                      a("NC Community Free/Reduced Lunch Eligibility",
                        href = "https://www.dpi.nc.gov/districts-schools/federal-program-monitoring/title-i-part/community-eligibility-free-and-reduced-price-meals"),
                      "<br>","<br>", HTML(paste0(tags$sup("1"))),
                      strong("Title I"), ": Under the ESEA, this federally funded program identifies schools with a majority of low-income students, based on free and reduced lunch statistics."
                )
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
            } 
            else if (input$select == "Funding Per Pupil"){
                paste("This indicator represents the amount that local, state, and federal governments spend 
                on elementary and secondary education, adjusted for the size of the student body. It is 
                calculated by dividing the current expenditures over the entire academic year for prekindergarten 
                through grade 12 by the number of students in those grades in public schools. Current expenditures 
                include instruction and instruction-related costs, student support services, administration, and 
                operations and exclude funds for school construction and other capital outlays, debt service, and 
                programs outside of public elementary and secondary education. North Carolina ranks 39th in per 
                pupil spending out of 50.", "<br>", "<br>",
                      "Resources on public school funding:", "<br>",
                      a("Interactive Summary of Local - Federal Public School Funding:",
                        href="https://www.dpsnc.net/Page/3771"),
                      a("New Per Pupil Expenditure Requirements",
                        href ="https://www.naesp.org/blog/new-per-pupil-expenditure-requirements/"))
            } 
            else if (input$select == "Students With Disabilities"){
                paste("It is integral to make sure students with disabilities are provided with accessibility services to 
                      achieve their full potential in the classroom. Resources like", HTML(paste0(strong("assistive technology"),tags$sup("1"))), ", transportation,", 
                      HTML(paste0(strong("Exceptional Children (EC) programs"),tags$sup("2"))), ", etc. are mandatory for every school to provide regardless of the 
                      number of students with disabilities or even the type based on the civil rights law Section 504.", "<br>", "<br>",
                      "Below are articles and resources about government protection and resources for students with disabilities:", "<br>",
                      a("DPS EC Services",
                        href="https://www.dpsnc.net/Page/169"),
                      a("Section 504",
                        href="https://www.dpsnc.net/Page/336"),
                      a("NCDPI’s EC Division",
                        href="https://www.dpi.nc.gov/districts-schools/classroom-resources/exceptional-children-division"),
                      a("Assistive Technology",
                        href="https://www.disabilityrightswa.org/publications/assistive-technology-special-education-students/"),
                      "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong("assistive technology"), ": supplementary devices that promote independence 
                      and functionality of people with varying disabilities",
                      "<br>", HTML(paste0(tags$sup("2"))), strong("EC programs"), ": school programs that include students that need intense or 
                      individualized instruction in addition to their standard course of study"
                      )
            } 
            else if (input$select == "ESL Students"){
                paste("This graph shows the number of students enrolled in the English as a Second Language (ESL) or English Language Learners (ELL) Program. 
                      ESL students consist of any student regardless of ethnicity, origin, race, age, etc. who is a non-native English speaker. These programs
                      are created to help children learn English along with other subjects necessary to complete each grade. Unfortunately, ESL students can 
                      face racial bias, discrimination, and bullying in the classroom. Teachers may barr them from participation from school activities, 
                      extracurriculars, and enrichment programs. It is important that school affiliates recognize that language barriers do not stunt intellectual 
                      development. Additionally, ELL teachers must accommodate their students instead of assimilating them by removing the identity of their 
                      native language entirely.", "<br>","<br>",
                      "Below are resources and information on culturally-responsive teaching and Durham Public Schools’ approaches to ESL/ELL programs:", "<br>",
                      a("How to Advocate for ESL/ELL Students",
                        href = "https://www.nea.org/professional-excellence/student-engagement/tools-tips/english-language-learners-what-you-need-know"),
                      a("Real-World Experiences and Anecdotes",
                        href = "https://www.learningforjustice.org/magazine/summer-2017/a-case-for-acculturation"),
                      a("DPS ESL Office",
                        href = "https://central.dpsnc.net/esl"),
                      a("ESL/ELL Teachers",
                        href = "https://www.eslteacheredu.org/what-is-an-esl-teacher/"))
            } 
            else if (input$select == "In-School Suspensions (ISS)"){
                paste("Students of color are more susceptible to harsher punishments in schools.
             Black students are subject to higher disciplinary actions compared to their white peers.
             A reason for this is racial bias leading to the overpolicing of Black students, fueling the", HTML(paste0(strong("school-to-prison pipeline"),tags$sup("1"))), ".", "<br>","<br>",
                      "Below are articles on In-School Suspensions and the School-to-Prison Pipeline:", "<br>", 
                      a("Racial Bias in School Discipline", 
                        href = "https://www.pnas.org/content/116/17/8255"), "<br>",
                      a("School-to-Prison Pipeline", 
                        href = "https://www.nea.org/advocating-for-change/new-from-nea/school-prison-pipeline-time-shut-it-down"),
                      "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong("school-to-prison pipeline"), ": the overly disproportionate policing of minority students, often from low-income households, 
                      that leads to higher punishments including ISS, OSS (out-of-school suspension), juvenile detention, etc."
                      )
            }
            else if(input$select == "Enrollment") {
                paste("This dataset shows the enrollment numbers at each school.")
            }
            else if (input$select == "School and Zone Racial Breakdown") {
                paste("Here are some resouces for differences in school zone and school racial demographics.")
            }
            else if (input$select == "Racial Differential") {
                paste("Here are some resouces for differences in school zone and school racial demographics.")
            }
            else if (input$select == "POC per School"){
                paste("This dataset shows the percentage of students of color in each of the ten schools.", "<br>","<br>",
                      "Below is more information about students of color:", "<br>",
                      a("Racial/Ethnic Enrollment in NC Public Schools",
                        href="https://nces.ed.gov/programs/coe/pdf/coe_cge.pdf"))
                
            }
            else if (input$select == "Racial Demographics"){
                paste("This dataset shows the racial breakdown of each of the ten public schools. 
                      The racial demographics of all 10 schools has changed over time, specifically in the past 30 years. 
                      The number of white students has decreased, while the number of students of color has increased.", "<br>","<br>",
                      "Below is more information about racial demographics in schools:", "<br>",
                      a("See how Racial Demographics have changed",
                        href="https://www.urban.org/features/explore-your-schools-changing-demographics"), "<br>",
                      a("More students of color in public schools",
                        href="https://www.publicschoolreview.com/blog/white-students-are-now-the-minority-in-u-s-public-schools"))
            }
            else if (input$select == "Race per School"){
                paste("Here are some resouces on racial demographics.")
            }
            else if (input$select == "Household Income"){
                paste("This graph shows the median household income for each school zone. 
                The average household income in the United States is $62,843 according to the US census as of 2019. 
                This average household income in North Carolina is $54,602, according to the US census as of 2019.", "<br>","<br>",
                      "Below are links to the US Census Information:", "<br>",
                      a("Nationwide Census", 
                        href="https://www.census.gov/quickfacts/fact/table/US/PST045219"), "<br>", 
                      a("North Carolina Census",
                        href="https://www.census.gov/quickfacts/NC"))
            }
            else if (input$select == "Homesale Price"){
                paste("This graph shows the median home sale price for each school zone. 
                      The average home sale price in the United States is $287,148, according to Zillow.
                      This average home sale price in North Carolina is $210,766, according to Zillow. 
                      Due to the Covid-19 Pandemic, home prices increased.", "<br>", "<br>",
                      "Below is more information about home sale price:", "<br>",
                      a("Zillow Resource",
                        href="https://www.zillow.com/research/zillow-may-2021-market-report-29635/"), "<br>",
                      a("Covid-19 Increase",
                        href="https://www.cnbc.com/2021/06/16/typical-us-home-price-up-record-13point2percent-compared-to-last-year.html"))
            }
            else if (input$select == "Bachelor Degree Rate"){
                paste("This graph shows the percentage of adults with bachelor’s degrees in each school zone. The number of individuals 
                      with bachelor’s degrees greatly differs across racial, income, and gender groups. Additionally, individuals with 
                      more degrees tend to have greater household incomes.", "<br>", "<br>",
                      "Below is more information about bachelor degree rates:", "<br>",
                      a("Bachelor’s Degrees and Race",
                        href="https://nces.ed.gov/fastfacts/display.asp?id=72"), "<br>",
                      a("Bachelor’s Degrees and Income",
                        href="https://www.bls.gov/careeroutlook/2018/data-on-display/education-pays.htm"))
            }
            
            else if (input$select == "Sidewalk Coverage"){
                paste("Areas without sidewalk coverage can become inaccessible for people without cars or other modes of transportation, 
                both private and public. Sidewalks are needed for individuals to safely walk to places such as school, grocery stores, 
                parks, etc. High income areas tend to have more sidewalk coverage than lower income areas.", "<br>", "<br>",
                      "Below is more information about sidewalk coverage:", "<br>",
                      a("Importance of Sidewalks",
                        href="http://guide.saferoutesinfo.org/engineering/sidewalks.cfm"), "<br>",
                      a("Income disparities and Sidewalk Coverage",
                        href="https://www.cityofeastlansing.com/DocumentCenter/View/1583/Income-Disparities-in-Street-Features-That-Encourage-Walking-PDF"))
            }
            
            else if (input$select == "Diversity per District"){
                paste("Here are some resouces about diversity in school districts.")
                
            }
            else if (input$select == "Median Age"){
                paste("This dataset shows the median age of residents in each of the school zones. The median age of residents in a 
                      specific school zone can determine the various assets available in that designated area.")
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
                   "All" = "transparent")
        })
        
        output$map <- renderLeaflet({
            leaflet(displayZone()) %>%
                addProviderTiles("CartoDB.Positron") %>%
                addPolygons(data = displayZone(),
                            fillColor = displayColor(),
                            stroke = TRUE,
                            fillOpacity = 0.39,
                            smoothFactor = 1) %>%
                addMarkers(data = displayVar(), lng = ~LONGITUDE, lat= ~LATITUDE, 
                           label = displayVar()$name, icon = displayIcon(), 
                           clusterOptions = markerClusterOptions()) %>%
                addMarkers(data = displaySchool(), lng = ~LONGITUDE, lat = ~LATITUDE, 
                           label = displaySchool()["name"])
        })
        
        output$home <- renderLeaflet({
            leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>%
                addMarkers(lat = 36.0015926872104, lng = -78.93823945048538, icon = iconSet$uni, label = "Duke University") %>%
                addMarkers(lat = 35.97521590491441, lng = -78.89962935390885, icon = iconSet$uni, label = "North Carolina Central University") %>%
                addMarkers(data = schools, lng = ~LONGITUDE, lat = ~LATITUDE, icon = iconSet$schools, label = schools$NAME)
        })
        
        observeEvent(input$viewMap, {
            updateTabItems(session, "TabItems", selected = "mapstab")
        })
        
        observeEvent(input$viewStat, {
            updateTabItems(session, "TabItems", selected = "statstab")
        })
        
        observeEvent(input$"Our Ten Schools - Home", {
            updateTabItems(session, "TabItems", selected = "home")
        })
        
        output$slickr <- renderSlickR({
            imgs <- list.files(path = "slideshow", pattern = "*.jpg", full.names = TRUE)
            slickR(imgs, width = 250, height = 250) + settings(autoplay = TRUE,
                                                               slidesToShow = 3,
                                                               slidesToScroll = 1)
        })
        
        output$ally <- renderImage({
            return(list(src = "ally.jpg", contentType = "image/png", width = "200px"))
        })
    }
)
