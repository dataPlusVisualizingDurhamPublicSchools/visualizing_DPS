#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

library(DT)



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
        menuItem("AP, CTE, & Electives", tabName = "electivestab", icon = icon("book")),
        menuItem("School Sports", tabName = "sportstab", icon = icon("basketball-ball")),
        menuItem("Data Insights", tabName = "insightstab", icon = icon("fas fa-chart-line")),
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
                         valueBox(13, "Geospatial Variables", icon = icon("map"), color = "light-blue", width = 4),
                         valueBox(23, "School-Specific Variables", icon = icon("pencil"), color = "light-blue", width = 4),
                         valueBox(1, "Centralized Web Application", icon = icon("window-restore"), color = "light-blue", width = 4))),
                 fluidRow(
                     class = "text-center",
                     box(title = strong("2 Universities, 16 Public Schools"),
                         solidHeader = TRUE,
                         width = 7,
                         p("The Durham Public Schools District contains 54 public schools: 
                            30 elementary, 9 middle, 2 secondary, 11 high, 1 alternative, and 1 hospital. 
                            Our project focuses on the 16 schools that most frequently partner with Duke University 
                            and North Carolina Central University. These include 10 elementary schools: 
                            C.C. Spaulding, Eastway, E.K. Powe, Fayetteville Street, Forest View, Lakewood,
                            Parkwood, Southwest, Hillandale, and Club Boulevard, 3 middle schools: Lakewood Montessori, 
                           Lowes Grove, and Brogden, and 3 high schools: Hillside, Jordan, and Riverside."),
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
                          box(title = strong("View Our 16 Schools"),
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
                   tabBox(
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "tabset1", width = "auto",
                     #Box Plot Outputs
                     tabPanel("Elementary School",
                              box(width = 12,
                                  background = "navy", 
                                  solidHeader = TRUE, 
                                  title = strong("Elementary School Charts"),
                                  plotlyOutput("es_barplots",
                                      width="auto",
                                      height = "auto"),
                                  h4("All data was derived from ",
                                     a("Durham Neighborhood Compass", href="https://compass.durhamnc.gov/en"), 
                                      ", ", a("NC School Report Cards", href="https://ncreports.ondemand.sas.com/src/?county=Durham"), 
                                      ", ", a("Durham Public Schools", href="https://dpsnc.net"),
                                      ", and", a(" National Center for Education Statistics (NCES)", href="https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=3701260"),
                                      "."
                                     )),
                              fluidRow(
                              #Drop Down Widget for Box Plots
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong("Select a Measurement"),
                                    selectInput("es_select", em("Click the drop down menu to select which measurement you would like to view."), 
                                       choices = list("Average Class Size","Bachelor Degree Rate",
                                                      "BIPOC Students per School",
                                                      "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                      "Free and Reduced Lunch","Funding Per Pupil","In-School Suspensions (ISS)",
                                                      "Median Age","Median Homesale Price","Median Household Income",
                                                      "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                      "Students Per Device","Student-Teacher Ratio, Elementary School", 
                                                      "Students With Disabilities", "Titles Per Student", "WiFi Access")
                                       ),
                                    selectInput("es_year", em("Click the drop down menu to select which year of data collection you would like to view."), 
                                       choices = list("Summer 2021", "Summer 2022")
                                       )
                                    ),
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong("Context & Resources"),
                                    htmlOutput("es_resources")
                                    )
                                )
                              ),
                     tabPanel("Middle School", box(width = 12,
                                                   background = "navy", 
                                                   solidHeader = TRUE, 
                                                   title = strong("Middle School Charts"),
                                                   plotlyOutput("ms_barplots",
                                                                width="auto",
                                                                height = "auto"),
                                                   h4("All data was derived from ",
                                                      a("Durham Neighborhood Compass", href="https://compass.durhamnc.gov/en"), 
                                                      ", ", a("NC School Report Cards", href="https://ncreports.ondemand.sas.com/src/?county=Durham"), 
                                                      ", ", a("Durham Public Schools", href="https://dpsnc.net"),
                                                      ", and", a(" National Center for Education Statistics (NCES)", href="https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=3701260"),
                                                      "."
                                                   )),
                              fluidRow(
                                #Drop Down Widget for Box Plots
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong("Select a Measurement"),
                                    selectInput("ms_select", em("Click the drop down menu to select which measurement you would like to view."), 
                                                choices = list("Average Class Size","Bachelor Degree Rate",
                                                               "BIPOC Students per School","CTE Course Enrollment Rate, Middle School", 
                                                               "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                               "Free and Reduced Lunch","Funding Per Pupil","In-School Suspensions (ISS)",
                                                               "Median Age","Median Homesale Price","Median Household Income",
                                                               "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                               "Students Per Device","Student-Teacher Ratio, Middle School", 
                                                               "Students With Disabilities", "Titles Per Student", "WiFi Access")
                                    ),
                                    selectInput("ms_year", em("Click the drop down menu to select which year of data collection you would like to view."), 
                                                choices = list("Summer 2022")
                                    )
                                ),
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong("Context & Resources"),
                                    htmlOutput("ms_resources")
                                )
                              )),
                     tabPanel("High School", box(width = 12,
                                                 background = "navy", 
                                                 solidHeader = TRUE, 
                                                 title = strong("High School Charts"),
                                                 plotlyOutput("hs_barplots",
                                                              width="auto",
                                                              height = "auto"),
                                                 h4("All data was derived from ",
                                                    a("Durham Neighborhood Compass", href="https://compass.durhamnc.gov/en"), 
                                                    ", ", a("NC School Report Cards", href="https://ncreports.ondemand.sas.com/src/?county=Durham"), 
                                                    ", ", a("Durham Public Schools", href="https://dpsnc.net"),
                                                    ", and", a(" National Center for Education Statistics (NCES)", href="https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=3701260"),
                                                    "."
                                                 )),
                              fluidRow(
                                #Drop Down Widget for Box Plots
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong("Select a Measurement"),
                                    selectInput("hs_select", em("Click the drop down menu to select which measurement you would like to view."), 
                                                choices = list("Advanced Placement (AP) Course Enrollment", "Average Class Size","Bachelor Degree Rate",
                                                               "BIPOC Students per School","CTE Course Enrollment Rate, High School", 
                                                               "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                               "Free and Reduced Lunch","Funding Per Pupil","Graduation Rate","In-School Suspensions (ISS)",
                                                               "Median Age","Median Homesale Price","Median Household Income",
                                                               "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                               "Students Per Device","Student-Teacher Ratio, High School", 
                                                               "Students With Disabilities", "Titles Per Student", "WiFi Access")
                                    ),
                                    selectInput("hs_year", em("Click the drop down menu to select which year of data collection you would like to view."), 
                                                choices = list("Summer 2021", "Summer 2022")
                                    )
                                ),
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong("Context & Resources"),
                                    htmlOutput("hs_resources")
                                )
                              ))
        )))},
        
        #Electives Tab
        {tabItem(tabName = "electivestab",
                fluidRow(
                  tabBox(
                    id = "tabset2", width = "auto", 
                    tabPanel("Advanced Placement Courses", class = "text-center",
                             div(tableOutput("APTable"), style = "font-size:150%"),
                               selectInput("ap_school", em("Choose a school to view the AP Courses available."), 
                                         choices = list("Hillside High",
                                                        "Jordan High",
                                                        "Riverside High")
                             ),
                             fluidRow(
                               box(width = 4,
                                   title = strong("English AP Courses"), background = "olive", solidHeader = TRUE,
                                   htmlOutput("APEnglish", align="left")),
                               box(width = 4,
                                 title = strong("Math AP Courses"), background = "aqua", solidHeader = TRUE,
                                 htmlOutput("APMath", align="left")),
                               box(width = 4,
                                 title = strong("Science AP Courses"), background = "light-blue", solidHeader = TRUE,
                                 htmlOutput("APScience", align="left"))),
                             fluidRow(
                               box(width = 4,
                                   title = strong("Social Sciences AP Courses"), background = "green", solidHeader = TRUE,
                                   htmlOutput("APSocial", align="left")),
                               box(width = 4,
                                   title = strong("World Languages AP Courses"), background = "black", solidHeader = TRUE,
                                   htmlOutput("APWLang", align="left")),
                               box(width = 4,
                                   title = strong("Music and Arts AP Courses"), background = "navy", solidHeader = TRUE,
                                   htmlOutput("APMusArts", align="left"))),
                             fluidRow(
                               box(width = 4,
                                   title = strong("Engineneering AP Courses"), background = "purple", solidHeader = TRUE,
                                   htmlOutput("APEngine", align="left")))
                             ),
                    tabPanel("CTE Courses", class = "text-center",
                             div(tableOutput("CTETable"), style = "font-size:150%"),
                             selectInput("cte_school", em("Choose a school to view the CTE Courses available."), 
                                         choices = list("Hillside High",
                                                        "Jordan High",
                                                        "Riverside High")
                             ),
                             fluidRow(
                               box(width = 4,
                                   title = strong("Agricultural Education"), status = "primary" , solidHeader = TRUE,
                                   htmlOutput("AgCTE", align="left")),
                               box(width = 4,
                                   title = strong("Business, Marketing, and Finance"), status = "primary", solidHeader = TRUE,
                                   htmlOutput("BusCTE", align="left")),
                               box(width = 4,
                                   title = strong("Computer Science and Information Technology Education"), status = "primary", solidHeader = TRUE,
                                   htmlOutput("CompCTE", align="left"))),
                             fluidRow(
                               box(width = 4,
                                   title = strong("Family and Consumer Sciences Education"), status = "primary", solidHeader = TRUE,
                                   htmlOutput("FamCTE", align="left")),
                               box(width = 4,
                                   title = strong("Health Science Education"), status = "primary", solidHeader = TRUE,
                                   htmlOutput("HealthCTE", align="left")),
                               box(width = 4,
                                   title = strong("Trade, Technology, Engineering, and Industrial Education"), status = "primary", solidHeader = TRUE,
                                   htmlOutput("TradeCTE", align="left")))),
                    tabPanel("Arts Programs",
                             fluidRow(
                               box(width = 12,
                                   title = strong("Available Arts Programs In Each School"), background = "navy", solidHeader = TRUE,
                                   column(12, align="center", tableOutput("available_arts")))
                             ),
                             fluidRow(
                               box(width = 6, title = strong("Dance"), status = "primary", solidHeader = TRUE,
                                   p(h4("Dance is a form of physical activity that encourages self-expression, 
                                     improves health, and increases mobility and strength. Children and 
                                     adolescents that participate in dance learn different genres, cultural 
                                     aspects, and regional differences, and have the opportunity to choose 
                                     between cheerleading, gymnastics, interpretive dance, martial arts, and more!")),
                                   br(),
                                   p("Click on each of the schools below to learn more about their dance programs."),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                     href="https://duke.edu/", 
                                     tags$img(src="duke.png", 
                                              title="Duke Logo",
                                              class= "img-responsive")
                                   )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                     href="https://duke.edu/", 
                                     tags$img(src="duke.png",
                                              title="Duke Logo",
                                              class= "img-responsive")
                                   )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive")))
                                   ),
                               box(width = 6, title = strong("Theatre"), status = "primary", solidHeader = TRUE,
                                   p(h4("Theatre is a type of performance art that incorporates and fuses acting, 
                                        singing, and dancing. On the stage, individuals have the opportunity to 
                                        strengthen concentration and memory, improve articulation and fluency, 
                                        and build trust as they interact and collaborate with others. ")),
                                   br(),
                                   p("Click on each of the schools below to learn more about their dance programs."),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png", 
                                                     title="Duke Logo",
                                                     class= "img-responsive")
                                          )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive")
                                          )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive")))
                             )),
                             fluidRow(
                               box(width = 6,
                                   title = strong("Music"), status = "primary", solidHeader = TRUE,
                                   p(h4("Music incorporates vocal and instrumental performance, 
                                   studio production, songwriting, and even  listening enjoyment. 
                                   Music students have the opportunity to learn concepts such as 
                                   theory, history, acoustics, and instrumental and vocal skill 
                                   and technique. Music performance has been proven to teach 
                                   discipline, relieve stress, reduce stage fright, and improve 
                                   academic levels. 
")),
                                   br(),
                                   p("Click on each of the schools below to learn more about their dance programs."),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png", 
                                                     title="Duke Logo",
                                                     class= "img-responsive")
                                          )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive")
                                          )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive")))),
                               box(width = 6,
                                   title = strong("Visual"), status = "primary", solidHeader = TRUE,
                                   p(h4("Visual arts consist of painting, drawing, printmaking, 
                                        sculpture, ceramics, photography, video, filmmaking, 
                                        design, crafts and architecture. Engaging in visual 
                                        arts exposes children and adolescents to positive 
                                        benefits that include; but are not limited to, having a 
                                        space to create innovatively, improving motor skills, and 
                                        expressing emotions.")),
                                   br(),
                                   p("Click on each of the schools below to learn more about their dance programs."),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png", 
                                                     title="Duke Logo",
                                                     class= "img-responsive")
                                          )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive")
                                          )),
                                   column(class = 'text-center', width = 4,
                                          tags$a(
                                            href="https://duke.edu/", 
                                            tags$img(src="duke.png",
                                                     title="Duke Logo",
                                                     class= "img-responsive"))))
                             ))
                  )
                )
        )},
        
        #Sports Tab
        {tabItem(tabName = "sportstab",
                 fluidRow(
                   tabBox(
                     id = "tabset3", width = "auto", 
                     tabPanel("School Sports", class = "text-center",
                              selectInput("school_sports", em("Choose a school to view the Sports available."), 
                                          choices = list("Brogden Middle", "Lowes Grove Middle", "Lakewood Montesorri Middle",
                                                         "Hillside High",
                                                         "Jordan High",
                                                         "Riverside High")
                              ),
                              fluidRow(
                                box(width = 4,
                                    title = strong("Fall Sports"), background = "olive", solidHeader = TRUE,
                                    column(12, align="center", tableOutput("fallsports"))), 
                                box(width = 4,
                                    title = strong("Winter Sports"), background = "aqua", solidHeader = TRUE,
                                    column(12, align="center", tableOutput("wintersports"))),
                                box(width = 4,
                                    title = strong("Spring Sports"), background = "light-blue", solidHeader = TRUE,
                                    column(12, align="center", tableOutput("springsports"))),
                                ),
                              fluidRow(
                                box(width = 4,
                                    title = strong("Available Men's/Boy's Sports"), background = "navy", solidHeader = TRUE,
                                    column(12, align="center", tableOutput("male_sports_icons"))),
                                box(width = 4,
                                    title = strong("Available Women's/Girl's Sports"), background = "teal", solidHeader = TRUE,
                                    column(12, align="center", tableOutput("female_sports_icons"))),
                                box(width = 4,
                                    solidHeader = TRUE,
                                    title = strong("Icon Legend"),
                                    column(12, align="center", tableOutput("sports_icon_legend"))),
                                )
                        
                     ),
                     tabPanel("Community Sports", class = "text-center"
                              
                              )
                   )
                 ),
                
        )},
      
        #Data Insights tab
        {tabItem(tabName = "insightstab",
                 
                 fluidRow(
                   box(width  = 12,
                       solidHeader = TRUE,
                       title = strong("Map Comparison Of School Districts"),
                       h4("Different school districts are differently colored according to the variable selected."),
                       leafletOutput("choropleth")),
                 ), 
                 fluidRow(
                   box(width = 2,
                       solidHeader = TRUE,
                       title = strong("School Zone"),
                       selectInput("insights_zone",
                                   label = em("Choose a school zone to display"),
                                   choices = c("Brogden Middle", "C.C. Spaulding Elementary", "Club Boulevard Elementary",
                                               "Eastway Elementary","E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                               "Forest View Elementary", "Hillandale Elementary","Hillside High",
                                               "Jordan High","Lakewood Elementary", "Lakewood Montessori Middle", "Lowes Grove Middle",
                                               "Parkwood Elementary", "Riverside High", "Southwest Elementary"
                                   ),
                                   multiple = FALSE)),
                   box(width = 10,
                       title = strong("Resources In Each School Zone"),
                       plotlyOutput("insights_individualplots",
                                    width="auto",
                                    height = "auto"))
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
                                    img(align = "center", src = "emily250.jpg")),
                             column(width =3,
                                    p("Emily McReynolds is a sophomore at Duke University originally from Greensboro,
                                  North Carolina. She intends to major in Public Policy and obtain a certificate 
                                  in Markets and Management Studies. Emily is passionate about policy reform, 
                                  specifically in our educational and healthcare systems. She hopes this project 
                                  will bring more awareness to the available resources and community centers in 
                                  Durham County that can support our public schools. Emily is eager to see how 
                                  this partnership can build a stronger sense of unity for all.")),
                             column(width = 3,
                                    img(src = "aryan250.jpg")),
                             column(width = 3,
                                    p("Aryan Poonacha is a rising senior at Duke University from Bangalore, India. Currently,
                                  he is studying Data Science, with a minor in Political Science. He is especially interested
                                  in using big data analyses to reveal politically and socially relevant insights to key issues
                                  in policy and social justice. He joined the Data+ team to provide a clearer picture of the state
                                  of Durham public schools, and find better paths to their improvement."))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(src = "patience3.jpg")),
                             column(width = 3,
                                    p("Patience Jones is a senior at North Carolina Central University from Durham, 
                                  North Carolina. Currently, she is studying English, Secondary Education, and 
                                  General Psychology. Patience joined the Data+ project team because she was 
                                  interested in learning more about data science and its integration into education
                                  policy. She hopes this dashboard makes an impact on not only these ten community
                                  schools in the dataset, but students in all Durham Public Schools and beyond, 
                                  in hopes to make education more accessible to all students.")),
                             column(width = 3,
                                    img(src = "mel250.jpg")),
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
                     box(width  = 7,
                         solidHeader = TRUE,
                         title = strong("Interactive Map"),
                         h4("Hover over the icon to see the name. Click on the icon to reveal its link."),
                         leafletOutput("map")),
                     box(width = 5,
                         solidHeader = TRUE,
                         title = strong("Context"),
                         htmlOutput("context")),
                     
                 ),
                 fluidRow(
                     box(width = 4,
                         solidHeader = TRUE,
                         title = strong("Measurement"),
                         selectInput("zone",
                                     label = em("Choose a school zone to display"),
                                     choices = c("All", "Brogden Middle", "C.C. Spaulding Elementary", "Club Boulevard Elementary",
                                                 "Eastway Elementary","E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                                 "Forest View Elementary", "Hillandale Elementary","Hillside High",
                                                 "Jordan High","Lakewood Elementary", "Lakewood Montessori Middle", "Lowes Grove Middle",
                                                 "Parkwood Elementary", "Riverside High", "Southwest Elementary"
                                     ),
                                     multiple = FALSE),
                         selectInput("var",
                                     label = em("Choose a variable to display"),
                                     choices = c("After-School Care Programs", "Bus Stops", 
                                                 "Childcare Centers", "Community & Cultural Centers", "Community Arts","Farmers' Markets", "Food Pantries", "Gardens",
                                                 "Grocery Stores", "Hospitals and Clinics","Libraries", "Parks", 
                                                 "Recreation Centers", "Religious Centers"),
                                     multiple = FALSE)),
                     box(width = 4,
                         solidHeader = TRUE,
                         title = strong("Selected Variable Resources"),
                         em("Select a variable to see a list of all the resources with the selected school zone."),
                         br(),
                         br(),
                         dataTableOutput("list"),
                     ),
                     
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
                          br(),
                          fluidRow(
                            column( width = 1,
                                    img(src = "arts_icon.png", width = 40, height = 40, align = "left")),
                            column(width = 1),
                            column(width = 8, htmlOutput("artsicon")
                            ))
                     )},
                 )
        )}
    )
)
}

dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "Visualizing DPS"),
    sidebar,
    body
)


