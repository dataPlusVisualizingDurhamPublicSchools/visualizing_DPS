#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Packages
library(shiny)
library(shinydashboard)
library(slickR)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)
library(gotop)

# Load/Rename School Stats Data
Race_SCHOOL_ONLY <- read_excel("data/2021/school_stats_data/Race SCHOOL ONLY.xlsx")
race <- read_excel("data/2021/school_stats_data/race.xlsx")
race_diff <- read_excel("data/2021/school_stats_data/race diff.xlsx")
poc_per_school <- read_excel("data/2021/school_stats_data/poc per school.xlsx")
funding <- read_excel("data/2021/school_stats_data/funding.xlsx")
all_race <- read_excel("data/2021/school_stats_data/all race 1.xlsx")
schoolstats <- read.csv("data/2021/school_stats_data/Data + School Info - School Statistics.csv")
schoolstats22 <- read.csv("data/2020/school_stats_data/School Statistics 2022.csv")
all_race22 <- read_excel("data/2020/school_stats_data/all race 2022.xlsx")
poc_per_school22 <- read_excel("data/2020/school_stats_data/poc per school22.xlsx")
race22 <- read_excel("data/2020/school_stats_data/race2022.xlsx")

# Load/Rename Map Data
durham <- geojsonio::geojson_read("data/2021/map_data/Ten Schools.geojson", what = "sp")
cc <- geojsonio::geojson_read("data/2021/map_data/CC Spaulding.geojson", what = "sp")
eastway <- geojsonio::geojson_read("data/2021/map_data/Eastway.geojson", what = "sp")
ek <- geojsonio::geojson_read("data/2021/map_data/EK Powe.geojson", what = "sp")
fayetteville <- geojsonio::geojson_read("data/2021/map_data/Fayetteville St.geojson", what = "sp")
forest <- geojsonio::geojson_read("data/2021/map_data/Forest View.geojson", what = "sp")
hillside <- geojsonio::geojson_read("data/2021/map_data/Hillside.geojson", what = "sp")
jordan <- geojsonio::geojson_read("data/2021/map_data/CEJordan.geojson", what = "sp")
lakewood <- geojsonio::geojson_read("data/2021/map_data/Lakewood.geojson", what = "sp")
parkwood <- geojsonio::geojson_read("data/2021/map_data/Parkwood.geojson", what = "sp")
southwest <- geojsonio::geojson_read("data/2021/map_data/Southwest.geojson", what = "sp")

# Load/Rename Spatial Data
bus <- read.csv("data/2021/spatial_data/renamed_Bus Stops.csv")
childcare <- read_csv("data/2021/spatial_data/renamed_Childcare Centers_2022.csv")
cultural <- read.csv("data/2021/spatial_data/renamed_Community & Cultural Centers.csv")
gardens <- read.csv("data/2021/spatial_data/renamed_Community Gardens.csv")
grocery <- read.csv("data/2021/spatial_data/renamed_Grocery Stores.csv") #note what we consider a grocery store, not food desert food apartheid
libraries <- read.csv("data/2021/spatial_data/renamed_Libraries.csv")
parks <- read.csv("data/2021/spatial_data/renamed_Parks.csv")
rec <- read.csv("data/2021/spatial_data/renamed_Recreation Centers.csv")
religious <- read.csv("data/2021/spatial_data/renamed_Religious Centers.csv")
schools <- read.csv("data/2021/spatial_data/schools.csv")
hospitals <- read.csv("data/2021/spatial_data/renamed_Hospitals and Clinics.csv")
pantries <- read.csv("data/2021/spatial_data/renamed_Food Pantries.csv")
afterschool <- read.csv("data/2021/spatial_data/renamed_After-School Care Programs.csv")
farmersmark <- read.csv("data/2021/spatial_data/url_Farmer's Markets.csv")

# Load/Rename Schools' Names
schoolstats$name <- c("C.C. Spaulding Elementary", "Eastway Elementary",
                      "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                      "Forest View Elementary", "Lakewood Elementary", "Parkwood Elementary",
                      "Southwest Elementary", "Hillside High","Jordan High", "All")


# Load/Rename Icons
iconSet <- iconList(
    parks = makeIcon("https://img.icons8.com/windows/32/000000/tree.png", iconWidth=20, iconHeight=20),
    rec = makeIcon("https://img.icons8.com/glyph-neue/64/000000/basketball.png", iconWidth=20, iconHeight=20),
    gardens = makeIcon("https://img.icons8.com/dotty/80/000000/flower.png", iconWidth=20, iconHeight=20),
    bus = makeIcon("https://img.icons8.com/material-outlined/24/000000/bus.png", iconWidth=20, iconHeight=20),
    childcare = makeIcon("https://img.icons8.com/material-outlined/24/000000/rocking-horse.png", iconWidth=20, iconHeight=20),
    cultural = makeIcon("https://img.icons8.com/windows/32/000000/crowd.png", iconWidth=20, iconHeight=20),
    grocery = makeIcon("https://img.icons8.com/ios/50/000000/grocery-store.png", iconWidth=20, iconHeight=20),
    libraries = makeIcon("https://img.icons8.com/windows/32/000000/book--v1.png", iconWidth=20, iconHeight=20),
    religious = makeIcon("https://img.icons8.com/fluent-systems-regular/48/000000/chapel.png", iconWidth=20, iconHeight=20),
    uni = makeIcon("https://img.icons8.com/wired/64/000000/university-campus.png", iconWidth = 40, iconHeight = 40),
    schools = makeIcon("https://img.icons8.com/material-sharp/24/000000/school-building.png", iconWidth = 20, iconHeight = 20),
    hospitals = makeIcon("https://img.icons8.com/pastel-glyph/64/000000/hospital-wagon-without-a-siren.png", iconWidth = 20, iconHeight = 20),
    pantries = makeIcon("https://img.icons8.com/ios/50/000000/can-soup.png", iconWidth = 20, iconHeight = 20),
    afterschool = makeIcon("https://img.icons8.com/ios-filled/50/000000/children.png",iconWidth = 20, iconHeight = 20),
    farmersmark = makeIcon("https://img.icons8.com/ios-filled/50/undefined/carrot.png",iconWidth = 20, iconHeight = 20)
)

# customizing the go to top button

sidebar <- {dashboardSidebar(
  
  tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
  
    sidebarMenu(
        id = "TabItems",
        menuItem("Home", tabName = "home", icon = icon("fas fa-home")),
        menuItem("Maps", tabName = "mapstab", icon = icon("fas fa-map-marked-alt")),
        menuItem("School Statistics", tabName = "statstab", icon = icon("fas fa-chart-bar")),
        menuItem("Meet The Team", tabName = "teamstab", icon = icon("fas fa-users"))
    )
)
}

body <- {dashboardBody(
    tabItems(
        #Home Page
        {tabItem(tabName = "home",
                 
                 fluidRow(
                   class = "text-center",
                   column(width = 12,
                          img(src = "landing.jpg"))
                 ),
                 br(), 
                 
                 fluidRow(
                     class = "text-center",
                     box(h3(strong("Visualizing Durham Public Schools")),
                         width = 12,
                         background = "navy",
                         br(),
                         p("This project is inspired by an inter-institutional Bass Connections team from Duke University
                            and North Carolina Central University that is committed to developing more responsible
                           and imaginative ways of partnering with Durham Public Schools.
                            The objective of this project is to provide a centralized web application that will 
                           serve as a tool for those entering Durham Public Schools. Our application aims to 
                           inform future pre-service trainings for university students, support local neighborhood 
                           schools in visualizing their communities, and help varied university offices articulate 
                           what “community” actually looks like."),
                         br(),
                         p("By using spatial data and school-specific data, along with 
                            contextual resources, we hope to provide a holistic view of Durham Public Schools and their communities while
                            highlighting their resources and assets." ),
                         p("Visit the", a("Visualizing DPS and Bass Connections website",
                             href = "https://bassconnections.duke.edu/project-teams/strengthening-partnerships-between-durham-public-schools-and-local-universities-2021", target="_blank"),
                           "for more information!"))),
                 fluidRow(
                     class = "text-center",
                     box(
                         solidHeader = TRUE,
                         br(),
                         width = 12,
                         valueBox(9, "Geospatial Variables", icon = icon("map"), color = "light-blue", width = 4),
                         valueBox(22, "School-Specific Variables",  href = "#shiny-tab-statstab", icon = icon("pencil"), color = "light-blue", width = 4),
                         valueBox(1, "Centralized Web Application", icon = icon("window-restore"), color = "light-blue", width = 4))),
                 fluidRow(
                        class = "text-center",
                        box(title = strong("2 Universities, 10 Public Schools"),
                            solidHeader = TRUE,
                            width = 7,
                            p("The Durham Public Schools District contains 54 public schools: 
                            30 elementary, 9 middle, 2 secondary, 11 high, 1 alternative, and 1 hospital. 
                            Our project focuses on the 10 schools that most frequently partner with Duke University 
                            and North Carolina Central University. These include 8 elementary schools: 
                            C.C. Spaulding, Eastway, E.K. Powe, Fayetteville Street, Forest View, Lakewood,
                            Parkwood, Southwest, and 2 high schools: Hillside and Jordan."),
                            br(),
                            p("The shared goal of both Duke and NCCU is to foster equitable partnerships with Durham
                              Public Schools. Prior Bass Connections research focused on understanding how to achieve
                              this goal and found that an issue preventing meaningful engagement between Duke, NCCU, 
                              and Durham public schools is that ‘many university students lack an understanding of 
                              city and community dynamics.’ Additionally, they found that there is a 
                              ‘lack of student volunteer training with Durham’s context, particularly in the areas 
                              of history, school-specific demographics, and implicit bias and power dynamics that
                              may manifest in schools.’"),
                            br(),
                            p(strong("Motivated by this research, our project explores a way of visualizing schools 
                                     as the center of the community that brings together academics, health and 
                                     social services, youth and community development, and community engagement 
                                     under one roof."))),
                        box(width = 5,
                            background = "light-blue",
                            solidHeader = TRUE,
                            leafletOutput("home"))),
                 fluidRow(class = "text-center",
                          box(title = strong("View Our 10 Schools"),
                              width = 12,
                              background = "light-blue",
                              actionButton("viewMap", "View Geospatial Data"),
                              actionButton("viewStat", "View School Statistics"))),
                 
                 fluidRow(class = "text-center",
                     box(width = 12,
                         solidHeader = TRUE,
                         title = strong("Our Partners"),
                         column(class = 'text-center', width = 2,
                                tags$a(
                                  href="https://www.dpsnc.net/", 
                                  tags$img(src="dpsnc.png", 
                                           title="DPSNC Logo",
                                           class= "img-responsive")
                                )),
                         column(class = 'text-center', width = 2,
                                tags$a(
                                  href="https://www.bullcityschools.org/", 
                                  tags$img(src="dpsf.jpg", 
                                           title="DPSF Logo",
                                           class= "img-responsive")
                                )),
                         column(class = 'text-center', width = 2,
                                tags$a(
                                  href="https://duke.edu/", 
                                  tags$img(src="duke.png", 
                                           title="Duke Logo",
                                           class= "img-responsive")
                                )),
                         column(class = 'text-center', width = 2,
                                tags$a(
                                  href="https://www.nccu.edu/", 
                                  tags$img(src="nccu.png", 
                                           title="NCCU Logo",
                                           class= "img-responsive")
                                )),
                         column(class = 'text-center', width = 2,
                                tags$a(
                                  href="https://bigdata.duke.edu/", 
                                  tags$img(src="data+.jpg", 
                                           title="Data+ Logo",
                                           class= "img-responsive")
                                )),
                         column(class = 'text-center', width = 2,
                                tags$a(
                                  href="https://bassconnections.duke.edu/", 
                                  tags$img(src="bass connections.png", 
                                           title="Bass Connections Logo",
                                           class= "img-responsive")
                                ))),
                     ),
                 
                fluidRow(
                  box(width = 12,
                      background = "light-blue",
                      solidHeader = TRUE,
                      slickROutput("slickr", width = "auto"),
                      use_gotop(color = "black"))
                )
        )},
        
        #School Stats Tab
        {tabItem(tabName = "statstab",
                 fluidRow(
                     #Box Plot Outputs
                     box(width = 12,
                         background = "navy", 
                         solidHeader = TRUE, 
                         title = strong("Charts"), 
                         plotlyOutput("barplots",
                                      width="auto",
                                      height = "auto")
                     )),
                 fluidRow(
                     #Drop Down Widget for Box Plots
                         box(width = 6,
                             solidHeader = TRUE,
                             title = strong("Select a Measurement"),
                             selectInput("select", em("Click the drop down menu to select which measurement you would like to view."), 
                                         choices = list("Advanced Placement (AP) Course Enrollment", "Average Class Size","Bachelor Degree Rate",
                                                        "BIPOC Students per School","CTE Course Enrollment Rate, High School", 
                                                    "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                    "Free and Reduced Lunch","Funding Per Pupil","Graduation Rate","In-School Suspensions (ISS)",
                                                    "Median Age","Median Homesale Price","Median Household Income",
                                                    "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                    "Students Per Device","Student-Teacher Ratio, Elementary School","Student-Teacher Ratio, High School", 
                                                    "Students With Disabilities")
                             ),
                             selectInput("year", em("Click the drop down menu to select which year you would like to view."), 
                                          choices = list("2021", "2022")
                            )),
                         box(width = 6,
                             solidHeader = TRUE,
                             title = strong("Context & Resources"),
                             htmlOutput("resources")
                         )
                 )
        )},
        
        #Meet the team tab
        {tabItem(tabName = "teamstab",
                 
                 fluidRow(
                   box(width = 12,
                       background = "light-blue",
                       class = "text-center",
                       h3(strong("Meet Our Team - Data+ 2022")),
                       br(),
                       fluidRow(
                         column(width = 3,
                                img(align = "center", src = "emily2.jpg")),
                         column(width =3,
                                p("Emily McReynolds is a sophomore at Duke University originally from Greensboro,
                                  North Carolina. She intends to major in Public Policy and obtain a certificate 
                                  in Markets and Management Studies. Emily is passionate about policy reform, 
                                  specifically in our educational and healthcare systems. She hopes this project 
                                  will bring more awareness to the available resources and community centers in 
                                  Durham County that can support our public schools. Emily is eager to see how 
                                  this partnership can build a stronger sense of unity for all.")),
                         column(width = 3,
                                img(src = "aryan.jpg")),
                         column(width = 3,
                                p("Aryan Poonacha is a rising senior at Duke University from Bangalore, India. Currently,
                                  he is studying Data Science, with a minor in Political Science. He is especially interested
                                  in using big data analyses to reveal politically and socially relevant insights to key issues
                                  in policy and social justice. He joined the Data+ team to provide a clearer picture of the state
                                  of Durham public schools, and find better paths to their improvement."))),
                       br(),
                       fluidRow(
                         column(width = 3,
                                img(src = "patience3.jpeg")),
                         column(width = 3,
                                p("Patience Jones is a senior at North Carolina Central University from Durham, 
                                  North Carolina. Currently, she is studying English, Secondary Education, and 
                                  General Psychology. Patience joined the Data+ project team because she was 
                                  interested in learning more about data science and its integration into education
                                  policy. She hopes this dashboard makes an impact on not only these ten community
                                  schools in the dataset, but students in all Durham Public Schools and beyond, 
                                  in hopes to make education more accessible to all students.")),
                         column(width = 3,
                                img(src = "melanie2.png")),
                         column(width = 3,
                                p("Melanie Kaye Moseley is a senior at North Carolina Central University from Oxford, North Carolina.
                                  She is studying Music with a concentration in Instrumental Performance. Melanie joined the Data+ 
                                  project team to contribute to the previous research and efforts that have equipped the 
                                  corresponding Bass Connections team with organized information and statistics to pinpoint the 
                                  resources that would be most beneficial for specific schools. Melanie hopes this dashboard will 
                                  help increase equity within schools, and promote a greater sense of community throughout Durham, 
                                  North Carolina."))),
                       br(),
                       fluidRow(
                         column(width = 3,
                                img(src = "surabhi.jpg")),
                         column(width = 3,
                                p("Surabhi Trivedi is a PhD candidate at Duke's sociology department. He's
                                  interested in how habits and beliefs change, both at the individual and collective
                                  level, and uses longitudinal data, networks, and simulations to try to get at 
                                  this question. He had the privilege to be the project manager for this team, and
                                  believes the team was efficient and industrious so his job was easy. The team 
                                  claims he was helpful, and he likes to believe that is true."))))),
                 
                 fluidRow(
                   box(width = 12,
                       background = "navy",
                       class = "text-center",
                       h3(strong("Meet Our Team - Data+ 2021")),
                       br(),
                       fluidRow(
                         column(width = 3,
                                img(src = "ally3.jpg")),
                         column(width =3,
                                p("Allyson Ashekun is a junior at Duke University from Clearwater, Florida.
                                        Currently, she is studying Public Policy and Computer Science, and her 
                                        academic interests focus primarily on the intersection of those two 
                                        disciplines in areas such as Data Science. Allyson joined the Data+ project
                                        team because she is passionate about equitable education and enjoys coding. 
                                        She hopes this dashboard will be a helpful tool for those entering Durham
                                        Public Schools, and will help improve the quality of partnerships.")),
                         column(width = 3,
                                img(src = "drewheadshot3 copy.jpg")),
                         column(width = 3,
                                p("Drew Greene is a sophomore at Duke University from Richmond, Virginia. 
                                        He intends to study Public Policy with minors in Education and Inequality 
                                        Studies. His academic interests include educational equity and the 
                                        school-to-prison pipeline. Drew joined this project because of the opportunity 
                                        to develop his data science skills. He hopes this project will help adequately
                                        prepare college students to enter schools as well as allow Durham residents to
                                        locate the myriad resources accessible to help form an even stronger community 
                                        built around the schools."))),
                       br(),
                       fluidRow(
                         column(width = 3,
                                img(src = "patience3.jpeg")),
                         column(width = 3,
                                p("Patience Jones is a senior at North Carolina Central University from Durham, 
                                  North Carolina. Currently, she is studying English, Secondary Education, and 
                                  General Psychology. Patience joined the Data+ project team because she was 
                                  interested in learning more about data science and its integration into education
                                  policy. She hopes this dashboard makes an impact on not only these ten community
                                  schools in the dataset, but students in all Durham Public Schools and beyond, 
                                  in hopes to make education more accessible to all students.")),
                         column(width = 3,
                                img(src = "rhea3.jpg")),
                         column(width = 3,
                                p("Rhea Tejwani is a junior at Duke University from Demarest, New Jersey. She is 
                                    studying Computer Science and Economics. Rhea decided to join this project team
                                    because she has a passion for data science and hopes that this app will help 
                                    local universities have a productive relationship with the community. She is 
                                    proud of the work the team accomplished, and is excited to see the long term 
                                    impacts that this web app has!"))),
                       br(),
                       fluidRow(
                         column(width = 3,
                                img(src = "nico3.jpg")),
                         column(width = 3,
                                p("Nico Restrepo Ochoa is a PhD candidate at Duke's sociology department. He's
                                  interested in how habits and beliefs change, both at the individual and collective
                                  level, and uses longitudinal data, networks, and simulations to try to get at 
                                  this question. He had the privilege to be the project manager for this team, and
                                  believes the team was efficient and industrious so his job was easy. The team 
                                  claims he was helpful, and he likes to believe that is true.")))))
        )}, 
        
        #Maps Tab
        {tabItem(tabName = "mapstab",
                fluidRow(
                    box(width  = 12,
                        background = "navy",
                        solidHeader = TRUE,
                        title = strong("Interactive Map"),
                        h4("Hover over the icon to see the name. Click on the icon to reveal its link."),
                        leafletOutput("map"))
                ),
                fluidRow(
                    box(width = 4,
                        solidHeader = TRUE,
                        title = strong("Measurement"),
                        selectInput("zone",
                                    label = em("Choose a school zone to display"),
                                    choices = c("All", "C.C. Spaulding Elementary", "Eastway Elementary",
                                                "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                                "Forest View Elementary", "Hillside High",
                                                "Jordan High","Lakewood Elementary", 
                                                "Parkwood Elementary", "Southwest Elementary"),
                                    multiple = FALSE),
                        selectInput("var",
                                    label = em("Choose a variable to display"),
                                    choices = c("After-School Care Programs", "Bus Stops", 
                                                "Childcare Centers", "Community & Cultural Centers", "Farmers' Markets", "Food Pantries", "Gardens",
                                                "Grocery Stores", "Hospitals and Clinics","Libraries", "Parks", 
                                                "Recreation Centers", "Religious Centers"),
                                    multiple = FALSE)),
                    box(width = 4,
                        solidHeader = TRUE,
                        title = strong("Context"),
                        htmlOutput("context")),
                   
                     #Icon Legend
                    {box(width = 4,
                        solidHeader = TRUE,
                        title = strong("Icon Legend"),
                        htmlOutput("legend"),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "afterschool_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("afterschoolicon")
                                   )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "bus_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("busicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "childcare_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("childcareicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "cultural_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("cultureicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "market_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("marketicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "pantry_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("pantryicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "garden_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("gardenicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "grocery_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("groceryicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "hospital_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("hospitalicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "library_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("libraryicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "park_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("parkicon")
                            )),
                        br(),
                        fluidRow(
                            column( width = 1,
                                   img(src = "rec_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("recicon")
                            )),
                        br(),
                        fluidRow(
                            column(width = 1,
                                   img(src = "religious_icon.png", width = 40, height = 40)),
                            column(width = 1),
                            column(width = 8, htmlOutput("religiousicon")
                            )),
                )},
                )
        )}
    )
)
}



shinyApp(
    ui = dashboardPage(
        skin = "black",
        dashboardHeader(
            title = "Visualizing DPS"),
        sidebar,
        body
    ),
    server = function(input, output, session) { 
        
        # SchoolStats - GGPlots
        output$barplots <- renderPlotly({
            if(input$year == "2021"){
              if(input$select == "Advanced Placement (AP) Course Enrollment") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ADV_COURSES_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ADV_COURSES_PERCENT),], aes(x=reorder(SCHOOL_NAME, -ADV_COURSES_PERCENT), y=ADV_COURSES_PERCENT)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = ADV_COURSES_PERCENT), hjust = -.1, color = "black") +
                  geom_hline(aes(text="Durham County Average = 9.22%", yintercept = 9.22), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Advanced Placement Course Enrollment", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Average Class Size") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Average Class Size", x = "School", y = "Average # of Students")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Bachelor Degree Rate") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
                  geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Bachelor Degree Rate", y = "Bachelor Degree Rate", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "BIPOC Students per School") {
                p <- ggplot(poc_per_school, aes(reorder(place, -number), number)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
                  geom_text(aes(label = number), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Percentage of BIPOC Students" , x = "School", y = "BIPOC Students (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "CTE Course Enrollment Rate, High School") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 53%", yintercept = 53), color ='#01016D') +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "CTE Course Enrollment Rate", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Enrollment") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
                p <-  ggplot(schoolstats_summary[!is.na(schoolstats_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "School Enrollment" , x = "School", y = "Students")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Experienced Teacher Ratio"){
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
                p <- ggplot(schoolstats_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 79%", yintercept = 79), color ='#01016D') +
                  coord_flip() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  theme_minimal() +
                  labs(title = "Experienced Teacher Ratio", x = "School", y = "Experienced Teachers (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Free and Reduced Lunch"){
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Students Receiving Free and Reduced Lunch", x = "School", y = "Students")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Funding Per Pupil") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = $11,672", yintercept = 11672), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  scale_y_continuous(labels=scales::dollar_format()) +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Funding Per Pupil", x = "School", y = "Amount of Funding (USD)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "English as a Second Language (ESL) Student Enrollment") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "ESL Student Enrollment", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Graduation Rate") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(GRADUATION_RATE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$GRADUATION_RATE),], aes(x=reorder(SCHOOL_NAME, -GRADUATION_RATE), y=GRADUATION_RATE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = GRADUATION_RATE), hjust = -.1, color = "black") +
                  geom_hline(aes(text="Durham County Average = 83.5%", yintercept = 83.5), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Graduation Rate", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "In-School Suspensions (ISS)") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 188.92", yintercept = 93.69), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "In-School Suspensions", x = "School", y = "Students Per 1000")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Median Age") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
                p <- ggplot(schoolstats_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
                    geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                    geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average = 35.2", yintercept = 35.2), color ='#01016D') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 1.5)) +
                    labs(title = "Med Age of School Zones", x = "School Zone", y = "Median Age")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Median Homesale Price") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  scale_y_continuous(labels=scales::dollar_format()) +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = $278,000", yintercept = 278000), color ='#01016D') +
                  geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Median Homesale Price", y = "Median Homesale Price ($)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Median Household Income") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  scale_y_continuous(labels=scales::dollar_format()) +
                  geom_hline(aes(text="Durham County Average = $58,190", yintercept = 58190), color ='#01016D') +
                  geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Median Household Income", y = "Median Household Income ($)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Racial Demographics") {
                p3 <- ggplot(all_race, aes(factor(school), number, fill = race)) + 
                  geom_bar(stat="identity", position = "dodge") + 
                  scale_fill_manual(values = c("#1414AB", "#005BAD", "#60A6D4",
                                               "#D1E3F4", "#C6CBCF")) +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Racial Demographics of Schools" , x = "School", y = "Students (%)", fill="Race")
                ggplotly(p3)
              }
              else if(input$select == "School and Zone BIPOC Comparison"){
                p <- ggplot(race, aes(factor(place), number, fill = sorz)) + 
                  geom_bar(stat="identity", position = "dodge") + 
                  coord_flip() +
                  scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "BIPOC Comparison of Schools vs. School Zones" , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
                ggplotly(p, tooltip = c("text", "text1", "number", "place"))
              }
              else if(input$select == "Sidewalk Coverage") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
                  geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Sidewalk Coverage per School Zone", y = "Sidewalk Coverage (%)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Students Per Device") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = .8", yintercept = .8), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Students Per Device", x = "School", y = "Student to Device Ratio")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Student-Teacher Ratio, Elementary School") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
                    geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                    geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average = 20.5", yintercept = 20.5), color ='#01016D') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 1.5)) +
                    labs(title = "Elementary School Student-Teacher Ratio", x = "School", y = "Students per Teacher")
                ggplotly(p, tooltip = c("text"))
            } 
              else if(input$select == "Student-Teacher Ratio, High School") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_HS)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_HS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_HS), y=STUDENT_TEACHER_HS)) +
                    geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                    geom_text(aes(label = STUDENT_TEACHER_HS), hjust = 1.5, color = "black") +
                    geom_hline(aes(text="Durham County Average = 24", yintercept = 24), color ='#01016D') +
                    coord_flip() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 1.5)) +
                    labs(title = "High School Student-Teacher Ratio", x = "School", y = "Students per Teacher")
                ggplotly(p, tooltip = c("text"))
            } 
              else if(input$select == "Students With Disabilities") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
                p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
                    geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                    geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
                    coord_flip() +
                    geom_hline(aes(text="Durham County Average = 13.3%", yintercept = 13.3), color ='#01016D') +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 1.5)) +
                    labs(title = "Percent of Students with Disabilities", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text")) 
            } 
              #not on WebApp
              else if(input$select == "Diversity per School Zone") {
                schoolstats_summary <- schoolstats %>% group_by(SCHOOL_NAME) %>% summarise(DIVERSITY_ZONE)
                p <- ggplot(schoolstats_summary, aes(reorder(SCHOOL_NAME, -DIVERSITY_ZONE), DIVERSITY_ZONE)) + 
                    geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                    coord_flip() +
                    theme_minimal() +
                    geom_hline(aes(text="Durham County Average = 51%", yintercept = 51), color ='#01016D') +
                    geom_text(aes(label = DIVERSITY_ZONE), vjust = 0)+
                    theme(plot.title = element_text(hjust = 1.5)) +
                    labs(title = "Diversity per School Zone", y = "Diversity (%)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
            }
              }
            
            else if(input$year == "2022"){
              if(input$select == "Average Class Size") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Average Class Size", x = "School", y = "Average # of Students")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Bachelor Degree Rate") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
                p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
                  geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Bachelor Degree Rate", y = "Bachelor Degree Rate", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "BIPOC Students per School") {
                p <- ggplot(poc_per_school22, aes(reorder(place, -number), number)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
                  geom_text(aes(label = number), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Percentage of BIPOC Students" , x = "School", y = "BIPOC Students (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "CTE Course Enrollment Rate, High School") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 53%", yintercept = 53), color ='#01016D') +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "CTE Course Enrollment Rate", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Enrollment") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
                p <-  ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "School Enrollment" , x = "School", y = "Students")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "English as a Second Language (ESL) Student Enrollment") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
                  coord_flip() +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "ESL Student Enrollment", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Experienced Teacher Ratio"){
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
                p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 82.4%", yintercept = 82.4), color ='#01016D') +
                  coord_flip() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  theme_minimal() +
                  labs(title = "Experienced Teacher Ratio", x = "School", y = "Experienced Teachers (%)")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Free and Reduced Lunch"){
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Students Receiving Free and Reduced Lunch", x = "School", y = "Students")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Funding Per Pupil") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = $11,672", yintercept = 11672), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  scale_y_continuous(labels=scales::dollar_format()) +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Funding Per Pupil", x = "School", y = "Amount of Funding (USD)")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Graduation Rate") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(GRADUATION_RATE)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$GRADUATION_RATE),], aes(x=reorder(SCHOOL_NAME, -GRADUATION_RATE), y=GRADUATION_RATE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = GRADUATION_RATE), hjust = -.1, color = "black") +
                  geom_hline(aes(text="Durham County Average = 87%", yintercept = 87), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Graduation Rate", x = "School", y = "Students (%)")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "In-School Suspensions (ISS)") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 1.65", yintercept = 1.65), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "In-School Suspensions", x = "School", y = "Students Per 1000")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Median Age") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
                p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = 35.4", yintercept = 35.4), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Median Age of School Zones", x = "School Zone", y = "Median Age")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Median Homesale Price") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
                p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  scale_y_continuous(labels=scales::dollar_format()) +
                  theme_minimal() +
                  geom_hline(aes(text="Durham County Average = $290,500", yintercept = 290500), color ='#01016D') +
                  geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Median Homesale Price", y = "Median Homesale Price ($)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Median Household Income") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
                p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  scale_y_continuous(labels=scales::dollar_format()) +
                  geom_hline(aes(text="Durham County Average = $60,958", yintercept = 60958), color ='#01016D') +
                  geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Median Household Income", y = "Median Household Income ($)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Racial Demographics") {
                p3 <- ggplot(all_race22, aes(factor(school), number, fill = race)) + 
                  geom_bar(stat="identity", position = "dodge") + 
                  scale_fill_manual(values = c("#1414AB", "#005BAD", "#60A6D4",
                                               "#D1E3F4", "#C6CBCF")) +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Racial Demographics of Schools" , x = "School", y = "Students (%)", fill="Race")
                ggplotly(p3)
              }
              else if(input$select == "School and Zone BIPOC Comparison"){
                p <- ggplot(race22, aes(factor(place), number, fill = sorz)) + 
                  geom_bar(stat="identity", position = "dodge") + 
                  coord_flip() +
                  scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "BIPOC Comparison of Schools vs. School Zones" , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
                ggplotly(p, tooltip = c("text", "text1", "number", "place"))
              }
              else if(input$select == "Sidewalk Coverage") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
                p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
                  geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                  coord_flip() +
                  theme_minimal() +
                  geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
                  geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Sidewalk Coverage per School Zone", y = "Sidewalk Coverage (%)", x = "School Zone")
                ggplotly(p, tooltip = c("text"))
              }
              else if(input$select == "Students Per Device") {
                schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
                p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
                  geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                  geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
                  geom_hline(aes(text="Durham County Average = .79", yintercept = .79), color ='#01016D') +
                  coord_flip() +
                  theme_minimal() +
                  theme(plot.title = element_text(hjust = 1.5)) +
                  labs(title = "Students Per Device", x = "School", y = "Student to Device Ratio")
                ggplotly(p, tooltip = c("text"))
              } 
              else if(input$select == "Student-Teacher Ratio, Elementary School") {
                    schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
                    p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
                        geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                        geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
                        geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
                        coord_flip() +
                        theme_minimal() +
                        theme(plot.title = element_text(hjust = 1.5)) +
                        labs(title = "Elementary School Student-Teacher Ratio", x = "School", y = "Students per Teacher")
                    ggplotly(p, tooltip = c("text"))
                } 
              else if(input$select == "Student-Teacher Ratio, High School") {
                    schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_HS)
                    p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_HS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_HS), y=STUDENT_TEACHER_HS)) +
                        geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                        geom_text(aes(label = STUDENT_TEACHER_HS), hjust = 1.5, color = "black") +
                        geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
                        coord_flip() +
                        theme_minimal() +
                        theme(plot.title = element_text(hjust = 1.5)) +
                        labs(title = "High School Student-Teacher Ratio", x = "School", y = "Students per Teacher")
                    ggplotly(p, tooltip = c("text"))
                } 
              else if(input$select == "Students With Disabilities") {
                    schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
                    p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
                        geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
                        geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
                        coord_flip() +
                        geom_hline(aes(text="Durham County Average = 13.7%", yintercept = 13.7), color ='#01016D') +
                        theme_minimal() +
                        theme(plot.title = element_text(hjust = 1.5)) +
                        labs(title = "Percent of Students with Disabilities", x = "School", y = "Students (%)")
                    ggplotly(p, tooltip = c("text")) 
              } 
              #not on WebApp
              else if(input$select == "Diversity per School Zone") {
                    schoolstats22_summary <- schoolstats22 %>% group_by(SCHOOL_NAME) %>% summarise(DIVERSITY_ZONE)
                    p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -DIVERSITY_ZONE), DIVERSITY_ZONE)) + 
                        geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
                        coord_flip() +
                        theme_minimal() +
                        geom_hline(aes(text="Durham County Average = 67%", yintercept = 67), color ='#01016D') +
                        geom_text(aes(label = DIVERSITY_ZONE), vjust = 0)+
                        theme(plot.title = element_text(hjust = 1.5)) +
                        labs(title = "Diversity per School Zone", y = "Diversity (%)", x = "School Zone")
                    ggplotly(p, tooltip = c("text"))
                }
              }
        })
        
        # SchoolStats - Context and Resources
        output$resources <- renderText({
          if(input$select == "Advanced Placement (AP) Course Enrollment") {
            paste("Advanced Placement (AP) courses are challenging, collegiate-level courses that are offered to high school students. AP courses weigh more than honors courses on the high school level.
Students can take these classes for an opportunity to receive college credit upon scoring a three or higher (out of five) on the standardized assessment, which saves the student money on college tuition. AP classes also serve as a way for students to be placed into higher-level courses at their college.
", "<br>","<br>",
                  "Below is more information about AP courses:", "<br>", 
                  a("NCDPI AP Courses", 
                    href = "https://www.dpi.nc.gov/students-families/enhanced-opportunities/advanced-learning-and-gifted-education/advanced-coursework/advanced-placement"), "<br>",
                  a("DPS AP Courses", 
                    href = "https://www.dpsnc.net/Page/430"),
                  a("College Board",
                    href="https://apstudents.collegeboard.org/course-index-page"))
          }else if (input$select == "Average Class Size"){
            paste("Research proves smaller class size is beneficial to student achievement. Smaller classes allow for the teacher to focus less on classroom management, and more on centralized learning. Students have stated they feel more comfortable in smaller classes as well.","<br>","<br>",
                  "Resources Discussing the Importance of Class Size:","<br>",
                  a("The Benefits of Investing in Smaller Class Sizes",
                    href = "https://www.nea.org/advocating-for-change/new-from-nea/educators-and-parents-reset-class-size-debate"), "<br>",
                  a("State Policy View on Class Size",
                    href = "https://www.brookings.edu/research/class-size-what-research-says-and-what-it-means-for-state-policy/"))
          }
          else if (input$select == "CTE Course Enrollment Rate, High School"){
            paste("Career and Technical Education (CTE) courses are designed for high school students to receive real-world experience in the career field they are most interested in. Durham Public Schools started the “3-2-1” initiative in 2019 where all high school students are required to take three CTE courses, participate in two career exposure activities, and get an internship or a job before they graduate. This initiative, as well as all of CTE courses, are created to develop students’ soft skills, gain real-world experience, and help students decide on their post-graduate plans.", "<br>","<br>",
                  "Below is more information about CTE courses in Durham Public Schools and North Carolina:", "<br>",
                  a("DPS CTE Course Initiative",
                    href = "https://www.dpsnc.net/domain/293"), "<br>",
                  a("NCDPI CTE Course Overview",
                    href = "https://www.dpi.nc.gov/districts-schools/classroom-resources/career-and-technical-education")
            )
            
          }else if (input$select == "Experienced Teacher Ratio") {
            paste("Experienced teachers are those who have approximately five or more years of experience with teaching. Although more experienced teachers tend 
              to perform better on their evaluations, research shows teacher experience does not directly correlate to effective teaching. Some schools tend 
              to be more attracted to veteran teachers whereas other schools may prefer employing recently certified teachers that can provide current and unique 
              teaching styles.", "<br>", "<br>", "With the evolution of research-based practices, it is important to continue to educate new and returning teachers 
              on the best teaching practices that are scientifically-proven to promote student success. Good professional development workshops are paramount to provide 
              effective, culturally-responsive teaching practices.", "<br>","<br>","Below are articles on Teacher 
              Experience and Professional Development:", "<br>",
                  a("Effects of Hiring and Recommendations for Supporting Experienced Teachers", 
                    href = "https://learningpolicyinstitute.org/product/does-teaching-experience-increase-teacher-effectiveness-review-research"),"<br>",
                  a("Teacher Experience in Relation to Teacher Effectiveness", 
                    href ="https://www.nea.org/advocating-for-change/new-from-nea/does-teaching-experience-matter-lets-count-ways"),"<br>",
                  a("NCDPI Educator Professional Development", 
                    href ="https://www.dpi.nc.gov/educators/professional-development"),"<br>",
                  a("Importance and Resources for Professional Development", 
                    href ="https://www.nea.org/professional-excellence/professional-learning/teachers"))
          } else if (input$select == "Free and Reduced Lunch") {
            paste("The percentage of students receiving free and reduced lunch is a strong indicator of socioeconomic status. The percentage of students that fall below the poverty line determines if a school is considered ", HTML(paste0(strong("Title I"),tags$sup("1"))), ".", 
                  "Title I schools are eligible to receive grants through the Every Student Succeeds Act (ESEA). It is important to be cognizant of students’ socioeconomic status without being condescending and prejudiced. Socioeconomic status is not a limit, it is a barrier.", "<br>","<br>",
                  "Below are articles on Free/Reduced Lunch and its connection to Title I schools:", "<br>",
                  a("Rural Schools and Free/Reduced Lunch", 
                    href = "https://www.nea.org/advocating-for-change/new-from-nea/whos-looking-out-rural-schools"), "<br>",
                  a("NC Community Free/Reduced Lunch Eligibility",
                    href = "https://www.dpi.nc.gov/districts-schools/federal-program-monitoring/title-i-part/community-eligibility-free-and-reduced-price-meals"),
                  "<br>","<br>", HTML(paste0(tags$sup("1"))),
                  strong("Title I"), ": Under the ESEA, this federally funded program identifies schools with a majority of low-income students, based on free and reduced lunch statistics."
            )
          } else if (input$select == "Student-Teacher Ratio, Elementary School"){
            paste("Research proves smaller student-teacher ratios have a positive effect on student achievement. By allowing more centralized and one-on-one instruction, 
              smaller student-teacher ratios can increase test scores, lower dropout rates, and increase graduation rates. ","<br>","<br>",
                  "Resources on Student-Teacher Ratios:","<br>",
                  a("Infographics and Information on Student-Teacher Ratios",
                    href = "https://www.hunschool.org/resources/student-teacher-ratios"))
          } else if (input$select == "Student-Teacher Ratio, High School"){
            paste("Research proves smaller student-teacher ratios have a positive effect on student achievement. By allowing more centralized and one-on-one instruction, 
              smaller student-teacher ratios can increase test scores, lower dropout rates, and increase graduation rates.","<br>","<br>",
                  "Resources on Student-Teacher Ratios:","<br>",
                  a("Infographics and Information on Student-Teacher Ratios",
                    href = "https://www.hunschool.org/resources/student-teacher-ratios"))
          } else if (input$select == "Students Per Device"){
            paste("Living in a digital age, technology usage in the classroom has increased tremendously, especially during the COVID-19 pandemic. Although technology 
              is a great resource, students may not have equitable access to these devices. It is important for students and teachers to not only have access to these 
              technological devices, but also understand how to use them, which is why professional development is so important. Professional development is necessary 
              to keep educators up to date on new technology to create the most effective learning environment.","<br>","<br>",
                  "Resources on Access and Technology in the Classroom:", "<br>",
                  a("Durham Public Schools’ Technological Services",
                    href = "https://www.dpsnc.net/site/default.aspx?PageType=3&DomainID=207&ModuleInstanceID=8115&ViewID=6446EE88-D30C-497E-9316-3F8874B3E108&RenderLoc=0&FlexDataID=42210&PageID=4738"), "<br>",
                  a("Equitable Access to Technology",
                    href = "https://digitalpromise.org/2019/04/29/equity-in-schools-access-technology/"))
          } 
          else if (input$select == "Funding Per Pupil"){
            paste("This indicator represents the amount that local, state, and federal governments spend on elementary and secondary education adjusted for the 
              size of the student body. It is calculated by dividing the current expenditures over the entire academic year for pre-kindergarten through grade 
              12 by the number of students in those grades in public schools. Current expenditures include instruction and instruction-related costs, student 
              support services, administration, and operations. Excluded from those expenditures are funds for school construction and other capital outlays, 
              debt service, and programs outside of public elementary and secondary education. North Carolina ranks 39th in per pupil spending out of 50.", "<br>", "<br>",
                  "Resources on public school funding:", "<br>",
                  a("Interactive Summary of Local - Federal Public School Funding:",
                    href="https://www.dpsnc.net/Page/3771"),"<br>",
                  a("New Per Pupil Expenditure Requirements",
                    href ="https://www.naesp.org/blog/new-per-pupil-expenditure-requirements/"))
          } 
          else if (input$select == "Students With Disabilities"){
            paste("It is integral to make sure students with disabilities are provided with accessibility services to achieve their full potential in the classroom. 
              Resources like", HTML(paste0(strong("assistive technology"),tags$sup("1"))), ", transportation,", 
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
          else if (input$select == "English as a Second Language (ESL) Student Enrollment"){
            paste("This graph shows the number of students enrolled in the English as a Second Language (ESL) or English Language Learners (ELL) Program. 
        ESL students consist of any student regardless of ethnicity, origin, race, age, etc. who is a non-native English speaker. These programs are c
        reated to help children learn English along with other subjects necessary to complete each grade.", "<br>","<br>", "Unfortunately, 
                ESL students can face racial bias, discrimination, and bullying in the classroom. Teachers may bar them from participating in school 
                activities, extracurriculars, and enrichment programs. It is important that school affiliates recognize that language barriers do not 
                stunt intellectual development. Additionally, ELL teachers must accommodate their students instead of assimilating them by removing 
                the identity of their native language entirely.", "<br>","<br>",
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
            paste("In-school suspensions are described as: Instances in which a child is temporarily removed from his or her regular classrooms for at least 
        half a day but remains under the direct supervision of school personnel.", "<br>", "Students of color are more susceptible to harsher punishments in schools.
                  Black and Brown students are subject to higher disciplinary 
                  actions compared to their white peers. A reason for this is racial bias leading to the over policing of Black and Brown students, fueling the", HTML(paste0(strong("school-to-prison pipeline"),tags$sup("1"))), ".", "<br>","<br>",
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
        paste("This dataset shows the enrollment numbers at each school. Due to the COVID-19 pandemic, there has been a 5% decrease in enrollment. Normally, 
                  enrollment or average daily membership (ADM) is used to determine funding for each school. The higher the ADM, the more money a school receives. Recently, 
                  lawmakers passed a bill stating that they would omit schools from reduction in funding as part of COVID-19 relief. Durham Public Schools continues to strive 
                  for increased enrollment so that all programs can continue to receive adequate funding.", "<br>","<br>", 
              "Below is information about enrollment numbers:", "<br>",
              a("The Decrease in Enrollment in North Carolina", 
                href = "https://abc11.com/nc-schools-school-attendance-enrollment-durham-county/8204335/")
        )
      }
      else if (input$select == "School and Zone BIPOC Comparison") {
        paste("This plot shows the percentage of students of color in the school compared to the percentage of people of color in the school zone. 
                  This measurement shows the huge disparities in community representation for students of color as most of these schools are not racially reflective of 
                  the school zones they inhabit. A contributing factor of this is " , HTML(paste0(strong("gentrification"),tags$sup("1"))),".", "Gentrification has been 
                  an increasing problem, and COVID-19 has only exacerbated it. Black and Brown neighborhoods are becoming increasingly expensive forcing families out of 
                  their homes, which ultimately changes the demographics of those neighborhoods.
                  ", "<br>", "<br>",
              "Below are articles on In-School Suspensions and the School-to-Prison Pipeline:", "<br>", 
              a("COVID-19 and Gentrification in Durham", 
                href = "https://www.dukechronicle.com/article/2020/08/duke-university-covid-rages-housing-hits-home-gentrification-durham"), "<br>",
              a("Redlining in Durham", 
                href = "https://abc11.com/redlining-gentrification-black-neighborhoods-triangle/10373290/"),
              a("Durham’s Racial Inequality, Poverty, and Gentrification", 
                href = "https://fpg.unc.edu/publications/racial-inequality-poverty-and-gentrification-durham-north-carolina"),
              "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong("gentrification"), ": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier 
                  people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents")
      }
      
      else if (input$select == "BIPOC Students per School"){
        paste("This dataset shows the percentage of students of color in each of the ten schools. Each of the ten schools are “majority students of color” 
                  which means representation of these students and ", HTML(paste0(strong("culturally-responsive pedagogy"),tags$sup("1"))), "is integral 
                  for student success. Too often, students of color are underrepresented and cannot relate to content/curriculum because of cultural differences. 
                  Students who are able to connect with their tutors, teachers, administrators, etc. tend to engage with the content more.", "<br>","<br>",
              "Below is more information about students of color::", "<br>",
              a("Racial/Ethnic Enrollment in NC Public Schools",
                href="https://nces.ed.gov/programs/coe/pdf/coe_cge.pdf"),
              a("Why Representation Matters",
                href="https://givingcompass.org/article/why-teacher-representation-matters-to-students-of-color/"),
              a("Benefits of POC Representation",
                href="https://educationpost.org/students-of-color-need-to-see-more-people-of-color-that-shouldnt-be-controversial/"),
              "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong("culturally-responsive pedagogy"), ": style of individualized teaching that is cognizant of the varying 
                  cultures and ethnicities of the classroom"
        )
        
      }
      else if (input$select == "Racial Demographics"){
        paste("This dataset shows the racial breakdown of each of the ten public schools. Durham Public Schools’ student population 
                  is 80.7% students of color, but only 57% of Durham County is people of color. The racial demographics of all 10 schools has changed 
                  over time, specifically in the past 30 years. The number of white students has decreased, while the number of students of color has 
                  increased. Representation of these students and ", HTML(paste0(strong("culturally-responsive pedagogy"),tags$sup("1"))), "is integral for student success. Too often students 
                  of color are underrepresented and cannot relate to the content because of cultural differences. Students who are able to connect with 
                  their tutors, teachers, administrators, and the content tend to engage with the content more.", "<br>","<br>",
              "Below is more information about racial demographics in schools:", "<br>",
              a("Changing School Racial Demographics in Recent Decades",
                href="https://www.urban.org/features/explore-your-schools-changing-demographics"), "<br>",
              a("More Students of Color in Public schools",
                href="https://www.publicschoolreview.com/blog/white-students-are-now-the-minority-in-u-s-public-schools"),
              "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong("culturally-responsive pedagogy"), ": style of individualized teaching that is cognizant of the varying 
                  cultures and ethnicities of the classroom")
      }
      else if (input$select == "Median Household Income"){
        paste("This graph shows the median household income for each school zone. According to the 2020 US census, 
                  the median household income in the United States is $67,521. This is a 2.9% decrease from 2019, which is the 
                  first statistically significant decline we have witnessed since 2011.", "<br>","<br>", "The average household income 
                  in North Carolina is $54,602, while the median is $65,534, according to the US census as of 2019. Most of these 
                  schools fall below the NC and US median, a cause for concern. Poverty directly affects how students learn. 
                  Students’ basic human needs must be met in order to excel in the classroom. ", "<br>", "<br>", "Below are links to 
                  the US Census Information::", "<br>",
              a("Nationwide Census", 
                href="https://www.census.gov/quickfacts/fact/table/US/PST045219"), "<br>", 
              a("North Carolina Census",
                href="https://www.census.gov/quickfacts/NC"))
      }
      else if (input$select == "Median Homesale Price"){
        paste("This graph shows the average home sale price for each school zone. The median home sale price for the United States is $374,900, 
                  according to the US Census Bureau. According to Zillow, the median home sale price in North Carolina is $248,950. Due to the Covid-19 Pandemic, 
                  home prices increased, making the cost of living increase as well (all contributing to  ", HTML(paste0(strong("gentrification"),tags$sup("1"))),
              "). Most of these school zones fall above the North Carolina median. Because Durham has become an increasingly expensive city to live in, 
                  many of the students in the ten schools required free and reduced lunch.", "<br>", "<br>",
              "Below is more information about home sale price:", "<br>",
              a("Zillow Resource",
                href="https://www.zillow.com/research/zillow-may-2021-market-report-29635/"), "<br>",
              a("Covid-19 Increase",
                href="https://www.cnbc.com/2021/06/16/typical-us-home-price-up-record-13point2percent-compared-to-last-year.html"),
              "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong("gentrification"), ": the process of changing low-income neighborhoods, usually with a minority-majority, to market 
                  them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents")
      }
      else if (input$select == "Bachelor Degree Rate"){
        paste("This graph shows the percentage of adults with bachelor’s degrees in each school zone. The number of individuals with bachelor’s degrees 
                  greatly differs across racial, income, and gender groups. Additionally, individuals with more degrees tend to have greater household incomes.", "<br>", "<br>",
              "Below is more information about bachelor degree rates:", "<br>",
              a("Bachelor’s Degrees and Race",
                href="https://nces.ed.gov/fastfacts/display.asp?id=72"), "<br>",
              a("Bachelor’s Degrees and Income",
                href="https://www.bls.gov/careeroutlook/2018/data-on-display/education-pays.htm"))
      }
      
      else if (input$select == "Sidewalk Coverage"){
        paste("Areas without sidewalk coverage can become inaccessible for people without cars or other modes of transportation, 
                  both private and public. Sidewalks are needed for individuals to safely walk to places such as school, grocery stores, parks, etc. 
                  High income areas tend to have more sidewalk coverage than lower income areas.", "<br>", "<br>",
              "Below is more information about sidewalk coverage:", "<br>",
              a("Importance of Sidewalks",
                href="http://guide.saferoutesinfo.org/engineering/sidewalks.cfm"), "<br>",
              a("Income Disparities and Sidewalk Coverage",
                href="https://www.cityofeastlansing.com/DocumentCenter/View/1583/Income-Disparities-in-Street-Features-That-Encourage-Walking-PDF"))
      }
      
      else if (input$select == "Graduation Rate"){
        paste("The graph depicts the percentage of students estimated to graduate from high school in four years or less. 
                  The graduation rate for North Carolina from 2020 is 87.6%, with plans to raise that percentage to 95% by 2030.
                  This is a significant increase from when the first reported graduation rate was 68.3% in 2006.", "<br>", "<br>", "Graduation 
                  rates are important. Delayed graduations can lead to risk for student dropout rates to increase. Dropping out is a 
                  gateway that could possibly lead to students living disadvantaged lifestyles. It is important for stakeholders, 
                  especially school counselors and social workers, to reach out to students with extended absences. External factors 
                  could be contributing to student dropout rates and as a community, we may be able to diminish those barriers.
                  ", "<br>", "<br>",
              "Below are more resources on graduation rates:", "<br>",
              a("Dashboard with Articles and Quick Facts about Graduation Rates",
                href = "https://dashboard.myfuturenc.org/college-and-career-access/high-school-graduation-rate/"))
        
      }
      else if (input$select == "Median Age"){
        paste("This dataset shows the median age of residents in each school zone. The median age of residents 
                  in a specific school zone can determine the various assets available, identify beneficial resources in 
                  the community, and give some insight about school enrollment numbers in the future.")
      }
    })       
        
        # Maps - Connecting variable drop down menu to variable info
        displayVar <- reactive({
            switch(input$var,
                   "Parks" = parks, 
                   "Recreation Centers" = rec, 
                   "Gardens" = gardens, 
                   "Bus Stops" = bus, 
                   "Childcare Centers" = childcare, 
                   "Food Pantries" = pantries,
                   "Farmers' Markets" = farmersmark,
                   "Community & Cultural Centers" = cultural, 
                   "Grocery Stores" = grocery, 
                   "Libraries" = libraries, 
                   "Religious Centers" = religious,
                   "Hospitals and Clinics" = hospitals,
                   "After-School Care Programs" = afterschool)
        })
        
        # Maps - Connecting map variables to their icons
        displayIcon <- reactive({
            switch(input$var,
                   "Parks" = iconSet$parks, 
                   "Recreation Centers" = iconSet$rec, 
                   "Gardens" = iconSet$gardens, 
                   "Bus Stops" = iconSet$bus, 
                   "Childcare Centers" = iconSet$childcare, 
                   "Community & Cultural Centers" = iconSet$cultural, 
                   "Food Pantries" = iconSet$pantries,
                   "Farmers' Markets" = iconSet$farmersmark,
                   "Grocery Stores" = iconSet$grocery, 
                   "Libraries" = iconSet$libraries, 
                   "Religious Centers" = iconSet$religious,
                   "Hospitals and Clinics" = iconSet$hospitals,
                   "After-School Care Programs" = iconSet$afterschool)
        })
        
        # Maps - Connecting name of school to input
        displaySchool <- reactive({
            schoolstats %>% filter(name == input$zone)
        })
        
        # Maps - Connecting school zone drop down menu to school info
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
        
        # Maps - School zone colors
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
        
        # Maps - Leaflet aesthetics
        output$map <- renderLeaflet({
            leaflet(
                displayZone()) %>%
                addProviderTiles("CartoDB.Positron") %>%
                addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
                addResetMapButton() %>%
                addPolygons(data = displayZone(),
                            fillColor = displayColor(),
                            stroke = TRUE,
                            fillOpacity = 0.39,
                            smoothFactor = 1) %>%
                addMarkers(data = displayVar(), lng = ~LONGITUDE, lat= ~LATITUDE, 
                           label = displayVar()$name, popup = displayVar()$URL, icon = displayIcon(), 
                           clusterOptions = markerClusterOptions()) %>%
                addMarkers(data = displaySchool(), lng = ~LONGITUDE, lat = ~LATITUDE, icon = iconSet$schools,
                           label = displaySchool()["name"])
        })
        
        # Maps - Context and Resources
        output$context <- renderText({
          if(input$var == "Parks"){
            paste("The presence of parks in a community is vital to increase community engagement, 
        assist in the economic development of cities, bolster public health, and help children learn. 
        Parks allow people to interact with each other in an outdoor community space. Children are 
        able to play and explore nature in an increasingly digital world, providing benefits such 
        as decreased stress and potentially, obesity rates.",
                  "<br>",
                  "<br>",
                  "Below is more information about parks:",
                  "<br>",
                  a("Why Parks Are Important", href = "https://www.brec.org/index.cfm/page/WhyParksareImportant"),
                  "<br>",
                  a("Why Parks and Recreation are Essential Public Services",
                    href = "https://www.nrpa.org/uploadedFiles/nrpa.org/Advocacy/Resources/Parks-Recreation-Essential-Public-Services-January-2010.pdf"))
          }
          else if(input$var == "Recreation Centers"){
            paste("Recreation centers have varying amenities, frequently including fitness centers, basketball courts, 
        and multipurpose rooms. These facilities can be utilized for afterschool programs, indoor and outdoor 
        recreation, and meeting spaces. Similar to parks, recreation centers promote active lifestyles.", "<br>", "<br>", "Benefits include:", "<br>", "<br>", "-Functioning as a community hub", "<br>", "-Ability to host before- and after-school care programs", "<br>", "May offer inexpensive or free tutoring",
                  "<br>",
                  "<br>",
                  "Below is more information about recreation centers:",
                  "<br>",
                  a("The Positive Impact of Community Recreation Center",
                    href = "https://sportadvisory.com/the-positive-impact-of-community-recreation-centers/"),
                  "<br>",
                  a("Recreation Centers Play an Important Role in Communities",
                    href = "https://www.nrpa.org/publications-research/park-pulse/park-pulse-survey-recreation-centers-role-in-communities/"))
          }
          else if(input$var == "Gardens"){
            paste("Gardens offer numerous benefits to the community including nature therapy, fresh produce, and cleaner air. 
        A study of 63 gardens in upstate New York found that “gardens in low-income neighborhoods (46%) were four times as 
        likely as non low-income gardens to lead to other issues in the neighborhood being addressed; reportedly due to 
        organizing facilitated through the community gardens” (Armstrong). Another study published in Public Health Nutrition 
        noted, “Commonly cited barriers to fruit and vegetable intake include cost, availability and acceptance. Community 
        gardens have the potential to decrease these barriers by lowering the cost of produce, increasing access, and eventually 
        increasing acceptance and improving taste perceptions of fruits and vegetables” (Dibsdall et. al). With the ability to 
        benefit public health and serve as community hubs, gardens are impactful to a community. ",
                  "<br>",
                  "<br>",
                  "Below is more information about parks:",
                  "<br>",
                  a("A Survey of Community Gardens in Upstate New York", 
                    href = "https://nccommunitygardens.ces.ncsu.edu/wp-content/uploads/2014/02/researchArmstrongSurveyNYHealthCommunityDevelopment.pdf?fwd=no"),
                  "<br>",
                  a("Low-income Consumers’ Attitudes and Behaviour Towards
                            Access, Availability and Motivation to Eat Fruit and Vegetables",
                    href = "https://nccommunitygardens.ces.ncsu.edu/wp-content/uploads/2014/02/researchDibsdallLambertBobbinFrewerAccesstoProduce.pdf?fwd=no"),
                  "<br>",
                  a("Research and Benefits of Community Gardens",
                    href = "https://nccommunitygardens.ces.ncsu.edu/resources-3/nccommunitygardens-research/"))
          }
          else if(input$var == "Bus Stops"){
            paste("In order to live a healthy life, people must have access to affordable, nutritious food. 
        Without access to this resource, many, especially those who are low-income, are prone to developing 
        diet-related conditions such as obesity, diabetes, and cardiovascular disease. The areas lacking 
        sufficient healthy food are often called “food deserts.” Food insecurity should not be considered a 
        natural, geographical issue; but, rather an intentional denial of resources for historically marginalized 
        racial groups. Thus, the term “food apartheid” has gained traction in recent years, and is now often 
        preferred. We hope to show areas affected by food apartheid in an attempt to raise awareness about food insecurity.",
                  "<br>",
                  "<br>",
                  "Below is more information about bus stops:",
                  "<br>",
                  a("Transit Equity Dashboard",
                    href = "https://transitcenter.org/introducing-the-transit-equity-dashboard/"))
          }
          else if(input$var == "Childcare Centers"){
            paste("Childcare centers assure parents and guardians that their child(ren) is safe and cared 
        for while simultaneously allowing them to work and earn money for their family. Childcare is 
        particularly useful for single parents who often cannot afford to stay at home instead of working. 
        Having several childcare options near a person’s home can be beneficial in allowing parents to 
        weigh the cost and quality of various centers.",
                  "<br>",
                  "<br>",
                  "Below is more information about childcare centers:",
                  "<br>",
                  a("The Importance of Preschool and Child Care For Working Mothers",
                    href = "https://www.americanprogress.org/issues/education-k-12/reports/2013/05/08/62519/the-importance-of-preschool-and-child-care-for-working-mothers/"))
          }
          else if(input$var == "Community & Cultural Centers"){
            paste("The benefits of building community outside the school building contribute to more powerful 
        relationships and organizing within the school building. Community centers have been found to 
        promote community cohesion and sense of belonging while providing programming that supports 
        self-esteem, health, and academic success. Cultural centers also positively contribute to youth 
        development by providing positive self-images as well as opportunities to learn about cultural 
        practices and traditions of diverse groups.  ",
                  "<br>",
                  "<br>",
                  "Below is more information about community and cultural centers:",
                  "<br>",
                  a("The Positive Impact of Community Centers",
                    href = "https://sportadvisory.com/the-positive-impact-of-community-recreation-centers/"),
                  "<br>",
                  a("Culture and Arts Centers",
                    href = "https://trianglecf.org/impact/impact-cultural-arts/"))
          }
          else if(input$var == "Grocery Stores"){
            paste("  In order to live a healthy life, people must have access to affordable, 
        nutritious food. Without access to this resource, many, especially those who are 
        low-income, are prone to developing diet-related conditions such as obesity, 
        diabetes, and cardiovascular disease. The areas lacking sufficient healthy food 
        are often called “food deserts.” Food insecurity should not be considered a natural, 
        geographical issue; but, rather an intentional denial of resources for historically 
        marginalized racial groups. Thus, the term “food apartheid” has gained traction in 
        recent years, and is now often preferred. We hope to show areas affected by food 
        apartheid in an attempt to raise awareness about food insecurity.",
                  "<br>",
                  "<br>",
                  "Below is more information about grocery stores:",
                  "<br>",
                  a("Food Apartheid: Racialized Access to Healthy Affordable Food", 
                    href = "https://www.nrdc.org/experts/nina-sevilla/food-apartheid-racialized-access-healthy-affordable-food"),
                  "<br>",
                  a("Food Desert VS. Food Apartheid",
                    href = "https://forsythfarmersmarket.com/blog/foodapartheid"),
                  "<br>",
                  a("What Are Food Deserts, and How Do They Impact Health?",
                    href = "https://www.medicalnewstoday.com/articles/what-are-food-deserts"))
          }
          else if(input$var == "Libraries"){
            paste("Found in urban, suburban, and rural areas, libraries often serve
                      as community hubs. Their purpose is not only to provide academic 
                      resources for the community, but also be used to welcome new 
                      residents to the neighborhood, assist the homeless, and offer 
                      informational sessions on a variety of topics. As highlighted 
                      in an article by the Brookings Institute, a Pew Research study 
                      showed “[s]ome 90% of Americans ages 16 and older said that the
                      closing of their local public library would have an impact on 
                      their community, with 63% saying it would have a ‘major’ impact”", HTML(paste0(strong(tags$sub("1")))),
                      ". In Durham, there are seven public libraries, including the recently 
                      renovated Main Library downtown. Each library hosts several events 
                      daily for people of all ages.",
                  "<br>",
                  "<br>",
                  "Below is more information about libraries:",
                  "<br>",
                  a("How Public Libraries Help Build Healthy Communities",
                    href = "https://www.brookings.edu/blog/up-front/2017/03/30/how-public-libraries-help-build-healthy-communities/"),
                  "<br>",
                  a("Durham County Library",
                    href = "https://durhamcountylibrary.org"),
                  "<br>",
                  "<br>",
                  "Sources:","<br>",
                  HTML(paste0(strong(tags$sub("1")))),a("Pew Research",
                    href = "https://www.pewresearch.org/internet/2013/12/11/libraries-in-communities/"))
          }
          else if(input$var == "Religious Centers"){
            paste("Religious centers are huge assets to the community because of various services they provide. 
        These services include donations, food drives, fundraisers, providing safe spaces for various cultures, 
        counseling services, daycare, summer programs, and much more. Additionally, the Durham community has 
        established a rich inter-religion culture, especially in advocacy efforts for the city as a whole. 
        Despite the diversity of religious orientations, Durham residents have shown their willingness to 
        provide resources for all those in need, regardless of religious orientation.",
                  "<br>",
                  "<br>",
                  "Below is more information about religious centers:",
                  "<br>",
                  a("The Benefits of Religiosity and Spirituality on Mental Health",
                    href = "https://www.forbes.com/sites/alicegwalton/2018/09/17/raising-kids-with-religion-or-spirituality-may-protect-their-mental-health-study/?sh=647ed7d13287"))
          }
          else if(input$var == "Hospitals and Clinics"){
            paste("When faced with an emergency, time is of the essence. Being able to get to a hospital within minutes can be beneficial, 
        and can literally save lives. Along with emergency services, hospitals also offer different types of therapy, services for 
        individuals living with long-term illnesses, classes and events, and outpatient labs. Along with medical care and having a 
        centralized location for medical records, hospitals can also provide employment opportunities to local residents.", "<br>", "<br>", 
                  "Additionally, private practices tend to be located near hospitals. These offices give patients a better opportunity to 
        foster meaningful relationships with their nurses and primary care physicians.",
                  "<br>",
                  "<br>",
                  "Below is more information about hospitals and clinics:",
                  "<br>",
                  a("The Benefits of Living Near a Hospital",
                    href = "https://sanatogaridge.com/news-activities/the-benefits-of-living-near-a-hospital/#:~:text=Living%20near%20a%20hospital%20can,a%20Paramedic%20at%20your%20door"),
                  "<br>",
                  a("10 Ways to Improve Healthcare Access",
                    href = "https://stakeholderhealth.org/healthcare-access/"),
                  "<br>",
                  a("Durham County’s “Project Access” Initiative",
                    href = "https://projectaccessdurham.org/about/"))
          }
          else if(input$var == "Food Pantries"){
            paste("Food pantries are partner agencies and churches that obtain donated food from food banks to feed food insecure communities. 
        Food insecurity refers to the lack of access to enough nutritious food to fully meet basic needs because of a lack of financial 
        resources. Although some families are able to rely on the Supplemental Nutrition Assistance Program (SNAP), also known as “food 
        stamps,” there are some families that earn too much to receive SNAP; but, don’t make enough money to afford adequate meals. 
        Older adults and people with disabilities that are on fixed incomes sometimes have to choose between paying for medicine or 
        purchasing nutritious food. Food pantries can provide individuals and families with supplemental food allowing them to 
        reallocate money they would have used on food for rent or other utilities. ",
                  "<br>",
                  "<br>",
                  "Below is more information about pantries:",
                  "<br>",
                  a("Frequently Asked Questions on Food Banks",
                    href = "https://feedingthecarolinas.org/learn-more/food-bank-questions/?gclid=CjwKCAjwyryUBhBSEiwAGN5OCAyOWx3b_Z4r16WfOn18zdsydqDPs8QEpIM_PluJ6dEpsRYmIwemHxoC6koQAvD_BwE"),
                  "<br>",
                  a("Fighting Hunger and Reducing Poverty",
                    href = "https://www.ncjustice.org/publications/btc-brief-nc-ranks-10th-in-hunger-in-the-nation/"),
                  "<br>",
                  a("NC Hunger and Poverty Quick Facts",
                    href = "https://www.foodshuttle.org/hunger-in-nc-1#:~:text=In%202021%2C%20over%201.5%20million,risk%20of%20facing%20food%20insecurity"))
          }
          else if(input$var == "After-School Care Programs"){
            paste("Afterschool programs can promote positive youth development, and  support social, 
        emotional, cognitive, and academic development, reduce risky behaviors, promote physical 
        health, and provide a safe and supportive environment for children and youth",HTML(paste0(strong(tags$sub("1")))), ". Several afterschool programs also offer before school programs allowing 
        parents to drop-off and pick-up their child(ren) in a safe environment without interfering 
        with their work schedule. ",
                  "<br>",
                  "<br>",
                  "Below is more information about after:",
                  "<br>",
                  a("Benefits for Youth, Families, and Communities",
                    href = "https://youth.gov/youth-topics/afterschool-programs/benefits-youth-families-and-communities#:~:text=Afterschool%20programs%20can%20support%20social,environment%20for%20children%20and%20youth"),
                  "<br>",
                  a("Infographics on Afterschool Care Programs in NC",
                    href = "http://www.afterschoolalliance.org/policyStateFacts.cfm?state=NC"),
                  "<br>",
                  a("North Carolina Center for Afterschool Programs",
                    href = "https://ncafterschool.org/"),
                  "<br>",
                  "<br>",
                  "Sources:",
                  "<br>",HTML(paste0(strong(tags$sub("1")))),
                  a("Youth.gov", href = "youth.gov"))
          }
          else if(input$var == "Farmers' Markets"){
          paste("Farmers’ markets provide local citizens with fresh fruits and vegetables at the peak of their growing season. 
        According to the University of Pittsburgh Medical Center, because everything sold is in-season, people that purchase 
        produce from farmers’ markets get to experience the “truest flavors.” Because this produce is grown locally, there 
        is a higher nutritional value. Local produce is typically minimally processed, and grown without the use of pesticides, 
        antibiotics, and genetic modification. Due to the short travel to nearby markets and cheaper cost of produce, Farmers’ 
        markets can be a more affordable option for local residents. ",
                "<br>",
                "<br>",
                "Below is more information about Farmers' Markets:",
                "<br>",
                a("Farmers' Market Coalition", 
                  href = "https://farmersmarketcoalition.org/education/qanda/"))
                
        }
        })
        
        # Maps - Icon legend outputs
        {
        output$afterschoolicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4(HTML(paste0(strong("After-School Care Programs")))))
            else if (input$var == "Parks")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Recreation Centers")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Gardens")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Bus Stops")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Childcare Centers")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Grocery Stores")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Libraries")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Religious Centers")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Food Pantries")
                paste(h4("After-School Care Programs"))
            else if(input$var == "Farmers' Markets")
                paste(h4("After-School Care Programs"))
        })
        
        output$busicon <- renderText({
                if(input$var == "After-School Care Programs")
                    paste(h4("Bus Stops"))
                else if (input$var == "Parks")
                    paste(h4("Bus Stops"))
                else if(input$var == "Recreation Centers")
                    paste(h4("Bus Stops"))
                else if(input$var == "Gardens")
                    paste(h4("Bus Stops"))
                else if(input$var == "Bus Stops")
                    paste(h4(HTML(paste0(strong("Bus Stops")))))
                else if(input$var == "Childcare Centers")
                    paste(h4("Bus Stops"))
                else if(input$var == "Community & Cultural Centers")
                    paste(h4("Bus Stops"))
                else if(input$var == "Grocery Stores")
                    paste(h4("Bus Stops"))
                else if(input$var == "Libraries")
                    paste(h4("Bus Stops"))
                else if(input$var == "Religious Centers")
                    paste(h4("Bus Stops"))
                else if(input$var == "Hospitals and Clinics")
                    paste(h4("Bus Stops"))
                else if(input$var == "Food Pantries")
                    paste(h4("Bus Stops"))
                else if(input$var == "Farmers' Markets")
                    paste(h4("Bus Stops"))
            })
        
        output$childcareicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Childcare Centers"))
            else if (input$var == "Parks")
                paste(h4("Childcare Centers"))
            else if(input$var == "Recreation Centers")
                paste(h4("Childcare Centers"))
            else if(input$var == "Gardens")
                paste(h4("Childcare Centers"))
            else if(input$var == "Bus Stops")
                paste(h4("Childcare Centers"))
            else if(input$var == "Childcare Centers")
                paste(h4(HTML(paste0(strong("Childcare Centers")))))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Childcare Centers"))
            else if(input$var == "Grocery Stores")
                paste(h4("Childcare Centers"))
            else if(input$var == "Libraries")
                paste(h4("Childcare Centers"))
            else if(input$var == "Religious Centers")
                paste(h4("Childcare Centers"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Childcare Centers"))
            else if(input$var == "Food Pantries")
                paste(h4("Childcare Centers"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Childcare Centers"))
        })
        
        output$parkicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Parks"))
            else if (input$var == "Parks")
                paste(h4(HTML(paste0(strong("Parks")))))
            else if(input$var == "Recreation Centers")
                paste(h4("Parks"))
            else if(input$var == "Gardens")
                paste(h4("Parks"))
            else if(input$var == "Bus Stops")
                paste(h4("Parks"))
            else if(input$var == "Childcare Centers")
                paste(h4("Parks"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Parks"))
            else if(input$var == "Grocery Stores")
                paste(h4("Parks"))
            else if(input$var == "Libraries")
                paste(h4("Parks"))
            else if(input$var == "Religious Centers")
                paste(h4("Parks"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Parks"))
            else if(input$var == "Food Pantries")
                paste(h4("Parks"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Parks"))
        })
        
        output$recicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Recreation Centers"))
            else if (input$var == "Parks")
                paste(h4("Recreation Centers"))
            else if(input$var == "Recreation Centers")
                paste(h4(HTML(paste0(strong("Recreation Centers")))))
            else if(input$var == "Gardens")
                paste(h4("Recreation Centers"))
            else if(input$var == "Bus Stops")
                paste(h4("Recreation Centers"))
            else if(input$var == "Childcare Centers")
                paste(h4("Recreation Centers"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Recreation Centers"))
            else if(input$var == "Grocery Stores")
                paste(h4("Recreation Centers"))
            else if(input$var == "Libraries")
                paste(h4("Recreation Centers"))
            else if(input$var == "Religious Centers")
                paste(h4("Recreation Centers"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Recreation Centers"))
            else if(input$var == "Food Pantries")
                paste(h4("Recreation Centers"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Recreation Centers"))
        })
        
        output$gardenicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Gardens"))
            else if (input$var == "Parks")
                paste(h4("Gardens"))
            else if(input$var == "Recreation Centers")
                paste(h4("Gardens"))
            else if(input$var == "Gardens")
                paste(h4(HTML(paste0(strong("Gardens")))))
            else if(input$var == "Bus Stops")
                paste(h4("Gardens"))
            else if(input$var == "Childcare Centers")
                paste(h4("Gardens"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Gardens"))
            else if(input$var == "Grocery Stores")
                paste(h4("Gardens"))
            else if(input$var == "Libraries")
                paste(h4("Gardens"))
            else if(input$var == "Religious Centers")
                paste(h4("Gardens"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Gardens"))
            else if(input$var == "Food Pantries")
                paste(h4("Gardens"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Gardens"))
        })
        
        output$cultureicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Community & Cultural Centers"))
            else if (input$var == "Parks")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Recreation Centers")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Gardens")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Bus Stops")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Childcare Centers")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4(HTML(paste0(strong("Community & Cultural Centers")))))
            else if(input$var == "Grocery Stores")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Libraries")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Religious Centers")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Food Pantries")
                paste(h4("Community & Cultural Centers"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Community & Cultural Centers"))
        })
        
        output$groceryicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Grocery Stores"))
            else if (input$var == "Parks")
                paste(h4("Grocery Stores"))
            else if(input$var == "Recreation Centers")
                paste(h4("Grocery Stores"))
            else if(input$var == "Gardens")
                paste(h4("Grocery Stores"))
            else if(input$var == "Bus Stops")
                paste(h4("Grocery Stores"))
            else if(input$var == "Childcare Centers")
                paste(h4("Grocery Stores"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Grocery Stores"))
            else if(input$var == "Grocery Stores")
                paste(h4(HTML(paste0(strong("Grocery Stores")))))
            else if(input$var == "Libraries")
                paste(h4("Grocery Stores"))
            else if(input$var == "Religious Centers")
                paste(h4("Grocery Stores"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Grocery Stores"))
            else if(input$var == "Food Pantries")
                paste(h4("Grocery Stores"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Grocery Stores"))
        })
        
        output$libraryicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Libraries"))
            else if (input$var == "Parks")
                paste(h4("Libraries"))
            else if(input$var == "Recreation Centers")
                paste(h4("Libraries"))
            else if(input$var == "Gardens")
                paste(h4("Libraries"))
            else if(input$var == "Bus Stops")
                paste(h4("Libraries"))
            else if(input$var == "Childcare Centers")
                paste(h4("Libraries"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Libraries"))
            else if(input$var == "Grocery Stores")
                paste(h4("Libraries"))
            else if(input$var == "Libraries")
                paste(h4(HTML(paste0(strong("Libraries")))))
            else if(input$var == "Religious Centers")
                paste(h4("Libraries"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Libraries"))
            else if(input$var == "Food Pantries")
                paste(h4("Libraries"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Libraries"))
        })
        
        output$religiousicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Religious Centers"))
            else if (input$var == "Parks")
                paste(h4("Religious Centers"))
            else if(input$var == "Recreation Centers")
                paste(h4("Religious Centers"))
            else if(input$var == "Gardens")
                paste(h4("Religious Centers"))
            else if(input$var == "Bus Stops")
                paste(h4("Religious Centers"))
            else if(input$var == "Childcare Centers")
                paste(h4("Religious Centers"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Religious Centers"))
            else if(input$var == "Grocery Stores")
                paste(h4("Religious Centers"))
            else if(input$var == "Libraries")
                paste(h4("Religious Centers"))
            else if(input$var == "Religious Centers")
                paste(h4(HTML(paste0(strong("Religious Centers")))))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Religious Centers"))
            else if(input$var == "Food Pantries")
                paste(h4("Religious Centers"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Religious Centers"))
        })
        
        output$hospitalicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Hospitals & Clinics"))
            else if (input$var == "Parks")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Recreation Centers")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Gardens")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Bus Stops")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Childcare Centers")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Grocery Stores")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Libraries")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Religious Centers")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4(HTML(paste0(strong("Hospitals & Clinics")))))
            else if(input$var == "Food Pantries")
                paste(h4("Hospitals & Clinics"))
            else if(input$var == "Farmers' Markets")
                paste(h4("Hospitals & Clinics"))
        })
        
        output$pantryicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Food Pantries"))
            else if (input$var == "Parks")
                paste(h4("Food Pantries"))
            else if(input$var == "Recreation Centers")
                paste(h4("Food Pantries"))
            else if(input$var == "Gardens")
                paste(h4("Food Pantries"))
            else if(input$var == "Bus Stops")
                paste(h4("Food Pantries"))
            else if(input$var == "Childcare Centers")
                paste(h4("Food Pantries"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Food Pantries"))
            else if(input$var == "Grocery Stores")
                paste(h4("Food Pantries"))
            else if(input$var == "Libraries")
                paste(h4("Food Pantries"))
            else if(input$var == "Religious Centers")
                paste(h4("Food Pantries"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Food Pantries"))
            else if(input$var == "Food Pantries")
                paste(h4(HTML(paste0(strong("Food Pantries")))))
            else if(input$var == "Farmers' Markets")
                paste(h4("Food Pantries"))
        })
        
        output$marketicon <- renderText({
            if(input$var == "After-School Care Programs")
                paste(h4("Farmers' Markets"))
            else if (input$var == "Parks")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Recreation Centers")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Gardens")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Bus Stops")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Childcare Centers")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Community & Cultural Centers")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Grocery Stores")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Libraries")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Religious Centers")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Hospitals and Clinics")
                paste(h4("Farmers' Markets"))
            else if(input$var == "Food Pantries")
                paste(h4("Farmer's Markets"))
            else if(input$var == "Farmers' Markets")
                paste(h4(HTML(paste0(strong("Farmers' Markets")))))
            })
        
        }
        
        #Home Page - Leaflet Map showing Duke, NCCU, and the Ten Schools
        output$home <- renderLeaflet({
            leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>%
                addMarkers(lat = 36.0015926872104, lng = -78.93823945048538, icon = iconSet$uni, label = "Duke University") %>%
                addMarkers(lat = 35.97521590491441, lng = -78.89962935390885, icon = iconSet$uni, label = "North Carolina Central University") %>%
                addMarkers(data = schools, lng = ~LONGITUDE, lat = ~LATITUDE, icon = iconSet$schools, label = schools$NAME)
        })
        
        #Home Page - Got to Maps tab button
        observeEvent(input$viewMap, {
            updateTabItems(session, "TabItems", selected = "mapstab")
        })
        
        #Home Page - Go to School Stats tab button
        observeEvent(input$viewStat, {
            updateTabItems(session, "TabItems", selected = "statstab")
        })
        

        
        #Home Page - Carousal
        output$slickr <- renderSlickR({
            imgs <- list.files(path = "slideshow", pattern = "*.jpg", full.names = TRUE)
            slickR(imgs, width = 200, height = 200) + settings(autoplay = TRUE,
                                                               slidesToShow = 3,
                                                               slidesToScroll = 1)
        })
        
        
    }
)
