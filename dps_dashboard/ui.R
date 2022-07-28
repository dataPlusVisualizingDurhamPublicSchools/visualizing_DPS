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
library(shiny.i18n)

library(DT)


i18n <- Translator$new(translation_json_path = "data/Translations/testTranslation.json")
i18n$set_translation_language("English")


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
        menuItem(i18n$t("Home"), tabName = "home", icon = icon("fas fa-home")),
        menuItem(i18n$t("Maps"), tabName = "mapstab", icon = icon("fas fa-map-marked-alt")),
        menuItem(i18n$t("School Statistics"), tabName = "statstab", icon = icon("fas fa-chart-bar")),
        menuItem(i18n$t("AP & CTE Courses"), tabName = "coursestab", icon = icon("book")),
        menuItem(i18n$t("School Sports"), tabName = "sportstab", icon = icon("basketball-ball")),
        menuItem(i18n$t("Arts Programs"), tabName = "artstab", icon = icon("paint-brush")),
        menuItem(i18n$t("Data Insights"), tabName = "insightstab", icon = icon("fas fa-chart-line")),
        menuItem(i18n$t("Meet The Team"), tabName = "teamstab", icon = icon("fas fa-users"))
    )
)
}

body <- {dashboardBody(
  shiny.i18n::usei18n(i18n),
  div(style = "float: right;",
      selectInput('selected_language',
                  i18n$t("Change Language"),
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation())
  ),
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
                     box(h3(strong(i18n$t("Visualizing Durham Public Schools"))),
                         width = 12,
                         background = "navy",
                         br(),
                         p(i18n$t("This project is inspired by an inter-institutional Bass Connections team from Duke University and North Carolina Central University that is committed to developing more responsible and imaginative ways of partnering with Durham Public Schools. The objective of this project is to provide a centralized web application that will serve as a tool for those entering Durham Public Schools. Our application aims to inform future pre-service trainings for university students, support local neighborhood schools in visualizing their communities, and help varied university offices articulate what “community” actually looks like.")),
                         br(),
                         p(i18n$t("By using spatial data and school-specific data, along with contextual resources, we hope to provide a holistic view of Durham Public Schools and their communities while highlighting their resources and assets.")),
                         p(i18n$t("Visit the"), a("Visualizing DPS and Bass Connections website",
                                          href = "https://bassconnections.duke.edu/project-teams/strengthening-partnerships-between-durham-public-schools-and-local-universities-2021", target="_blank"),
                           i18n$t("for more information!")))),
                 fluidRow(
                     class = "text-center",
                     box(
                         solidHeader = TRUE,
                         br(),
                         width = 12,
                         valueBox(13, i18n$t("Geospatial Variables"), icon = icon("map"), color = "light-blue", width = 4),
                         valueBox(23, i18n$t("School-Specific Variables"), icon = icon("pencil"), color = "light-blue", width = 4),
                         valueBox(1, i18n$t("Centralized Web Application"), icon = icon("window-restore"), color = "light-blue", width = 4))),
                 fluidRow(
                     class = "text-center",
                     box(title = strong(i18n$t("2 Universities, 16 Public Schools")),
                         solidHeader = TRUE,
                         width = 7,
                         p(i18n$t("The Durham Public Schools District contains 54 public schools: 30 elementary, 9 middle, 2 secondary, 11 high, 1 alternative, and 1 hospital. Our project focuses on the 16 schools that most frequently partner with Duke University and North Carolina Central University"),
                           i18n$t(", or are part of the"),
                           a("Principal Leadership Fellowship", href="https://www.bullcityschools.org/principal-leadership-fellowship"),
                           i18n$t("organized by the Durham Public Schools Foundation."),
                           i18n$t("These include 10 elementary schools: C.C. Spaulding, Eastway, E.K. Powe, Fayetteville Street, Forest View, Lakewood, Parkwood, Southwest, Hillandale, and Club Boulevard, 3 middle schools: Lakewood Montessori, Lowes Grove, and Brogden, and 3 high schools: Hillside, Jordan, and Riverside.")),
                         br(),
                         p(i18n$t("The shared goal of both Duke and NCCU is to foster equitable partnerships with Durham Public Schools. Prior Bass Connections research focused on understanding how to achieve this goal and found that an issue preventing meaningful engagement between Duke, NCCU, and Durham public schools is that ‘many university students lack an understanding of city and community dynamics.’ Additionally, they found that there is a ‘lack of student volunteer training with Durham’s context, particularly in the areas of history, school-specific demographics, and implicit bias and power dynamics that may manifest in schools.’")),
                         br(),
                         p(strong(i18n$t("Motivated by this research, our project explores a way of visualizing schools as the center of the community that brings together academics, health and social services, youth and community development, and community engagement under one roof.")))),
                     box(width = 5,
                         background = "light-blue",
                         solidHeader = TRUE,
                         leafletOutput("home"))),
                 fluidRow(class = "text-center",
                          box(title = strong(i18n$t("View Our 16 Schools")),
                              width = 12,
                              background = "light-blue",
                              actionButton("viewMap", i18n$t("View Geospatial Data")),
                              actionButton("viewStat", i18n$t("View School Statistics")))),
                 
                 fluidRow(class = "text-center",
                          box(width = 12,
                              solidHeader = TRUE,
                              title = strong(i18n$t("Our Partners")),
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
                                         href="https://sites.duke.edu/uacs/", 
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
        
        #Maps Tab
        {tabItem(tabName = "mapstab",
                 fluidRow(
                   box(width  = 7,
                       solidHeader = TRUE,
                       title = strong(i18n$t("Interactive Map")),
                       h4(i18n$t("Hover over the icon to see the name. Click on the icon to reveal its link.")),
                       h4(i18n$t("Click the"), icon('search'), i18n$t("icon to search any address!")),
                       leafletOutput("map")),
                   box(width = 5,
                       solidHeader = TRUE,
                       title = strong(i18n$t("Context")),
                       htmlOutput("context")),
                   
                 ),
                 fluidRow(
                   box(width = 4,
                       solidHeader = TRUE,
                       title = strong(i18n$t("Measurement")),
                       selectInput("zone",
                                   label = em(i18n$t("Choose a school zone to display")),
                                   choices = c("All", "Brogden Middle", "C.C. Spaulding Elementary", "Club Boulevard Elementary",
                                               "Eastway Elementary","E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                               "Forest View Elementary", "Hillandale Elementary","Hillside High",
                                               "Jordan High","Lakewood Elementary", "Lakewood Montessori Middle", "Lowes Grove Middle",
                                               "Parkwood Elementary", "Riverside High", "Southwest Elementary"
                                   ),
                                   multiple = FALSE),
                       selectInput("var",
                                   label = em(i18n$t("Choose a variable to display")),
                                   choices = c("After-School Care Programs", "Bus Stops", 
                                               "Childcare Centers", "Community and Cultural Centers", "Community Arts", "Community Sports","Farmers' Markets", "Food Pantries", "Gardens",
                                               "Grocery Stores", "Hospitals and Clinics","Libraries", "Parks", 
                                               "Recreation Centers", "Religious Centers"
                                   ),
                                   multiple = FALSE)
                   ),
                   box(width = 4,
                       solidHeader = TRUE,
                       title = strong(i18n$t("Selected Variable Resources")),
                       em(i18n$t("Select a variable to see a list of all the resources with the selected school zone.")),
                       br(),
                       br(),
                       dataTableOutput("list"),
                   ),
                   
                   #Icon Legend
                   {box(width = 4,
                        solidHeader = TRUE,
                        title = strong(i18n$t("Icon Legend")),
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
                          column( width = 1,
                                  img(src = "arts_icon.png", width = 40, height = 40, align = "left")),
                          column(width = 1),
                          column(width = 8, htmlOutput("artsicon")
                          )),
                        br(),
                        fluidRow(
                          column( width = 1,
                                  img(src = "commsportsicon.png", width = 40, height = 40, align = "left")),
                          column(width = 1),
                          column(width = 8, htmlOutput("sportsicon")
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
                                  img(src = "recicon.png", width = 40, height = 40, align = "left")),
                          column(width = 1),
                          column(width = 8, htmlOutput("recicon")
                          )),
                        br(),
                        fluidRow(
                          column(width = 1,
                                 img(src = "religious_icon.png", width = 40, height = 40)),
                          column(width = 1),
                          column(width = 8, htmlOutput("religiousicon")
                          ))
                   )}
        )  

        )},
        
        #School Stats Tab
        {tabItem(tabName = "statstab",
                 fluidRow(
                   tabBox(
                     # The id lets us use input$tabset1 on the server to find the current tab
                     id = "tabset1", width = "auto",
                     #Box Plot Outputs
                     tabPanel(i18n$t("Elementary School"),
                              box(width = 12,
                                  background = "navy", 
                                  solidHeader = TRUE, 
                                  title = strong(i18n$t("Elementary School Charts")),
                                  plotlyOutput("es_barplots",
                                      width="auto",
                                      height = "auto"),
                                  h4(i18n$t("All data was derived from"),
                                     a("Durham Neighborhood Compass", href="https://compass.durhamnc.gov/en"), 
                                      ", ", a("NC School Report Cards", href="https://ncreports.ondemand.sas.com/src/?county=Durham"), 
                                      ", ", a("Durham Public Schools", href="https://dpsnc.net"),
                                     i18n$t(", and"), a(" National Center for Education Statistics (NCES)", href="https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=3701260"),
                                      "."
                                     )),
                              fluidRow(
                              #Drop Down Widget for Box Plots
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("Select a Measurement")),
                                    selectInput("es_select", em(i18n$t("Click the drop down menu to select which measurement you would like to view.")), 
                                       choices = list("Average Class Size","Bachelor Degree Rate",
                                                      "BIPOC Students per School",
                                                      "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                      "Free and Reduced Lunch","Funding Per Pupil","In-School Suspensions (ISS)",
                                                      "Median Age","Median Homesale Price","Median Household Income",
                                                      "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                      "Students Per Device","Student-Teacher Ratio, Elementary School", 
                                                      "Students With Disabilities", "Titles Per Student", "WiFi Access Points Per Classroom")
                                       ),
                                    selectInput("es_year", em(i18n$t("Click the drop down menu to select which year of data collection you would like to view.")), 
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
                     tabPanel(i18n$t("Middle School"), box(width = 12,
                                                   background = "navy", 
                                                   solidHeader = TRUE, 
                                                   title = strong(i18n$t("Middle School Charts")),
                                                   plotlyOutput("ms_barplots",
                                                                width="auto",
                                                                height = "auto"),
                                                   h4(i18n$t("All data was derived from"),
                                                      a("Durham Neighborhood Compass", href="https://compass.durhamnc.gov/en"), 
                                                      ", ", a("NC School Report Cards", href="https://ncreports.ondemand.sas.com/src/?county=Durham"), 
                                                      ", ", a("Durham Public Schools", href="https://dpsnc.net"),
                                                      i18n$t(", and"), a(" National Center for Education Statistics (NCES)", href="https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=3701260"),
                                                      "."
                                                   )),
                              fluidRow(
                                #Drop Down Widget for Box Plots
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("Select a Measurement")),
                                    selectInput("ms_select", em(i18n$t("Click the drop down menu to select which measurement you would like to view.")), 
                                                choices = list("Average Class Size","Bachelor Degree Rate",
                                                               "BIPOC Students per School","CTE Course Enrollment Rate, Middle School", 
                                                               "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                               "Free and Reduced Lunch","Funding Per Pupil","In-School Suspensions (ISS)",
                                                               "Median Age","Median Homesale Price","Median Household Income",
                                                               "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                               "Students Per Device","Student-Teacher Ratio, Middle School", 
                                                               "Students With Disabilities", "Titles Per Student", "WiFi Access Points Per Classroom")
                                    ),
                                    selectInput("ms_year", em(i18n$t("Click the drop down menu to select which year of data collection you would like to view.")), 
                                                choices = list("Summer 2022")
                                    )
                                ),
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("Context & Resources")),
                                    htmlOutput("ms_resources")
                                )
                              )),
                     tabPanel(i18n$t("High School"), box(width = 12,
                                                 background = "navy", 
                                                 solidHeader = TRUE, 
                                                 title = strong(i18n$t("High School Charts")),
                                                 plotlyOutput("hs_barplots",
                                                              width="auto",
                                                              height = "auto"),
                                                 h4(i18n$t("All data was derived from"),
                                                    a("Durham Neighborhood Compass", href="https://compass.durhamnc.gov/en"), 
                                                    ", ", a("NC School Report Cards", href="https://ncreports.ondemand.sas.com/src/?county=Durham"), 
                                                    ", ", a("Durham Public Schools", href="https://dpsnc.net"),
                                                    i18n$t(", and"), a(" National Center for Education Statistics (NCES)", href="https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=3701260"),
                                                    "."
                                                 )),
                              fluidRow(
                                #Drop Down Widget for Box Plots
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("Select a Measurement")),
                                    selectInput("hs_select", em(i18n$t("Click the drop down menu to select which measurement you would like to view.")), 
                                                choices = list("Advanced Placement (AP) Course Enrollment", "Average Class Size","Bachelor Degree Rate",
                                                               "BIPOC Students per School","CTE Course Enrollment Rate, High School", 
                                                               "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                               "Free and Reduced Lunch","Funding Per Pupil","Graduation Rate","In-School Suspensions (ISS)",
                                                               "Median Age","Median Homesale Price","Median Household Income",
                                                               "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                               "Students Per Device","Student-Teacher Ratio, High School", 
                                                               "Students With Disabilities", "Titles Per Student", "WiFi Access Points Per Classroom")
                                    ),
                                    selectInput("hs_year", em(i18n$t("Click the drop down menu to select which year of data collection you would like to view.")), 
                                                choices = list("Summer 2021", "Summer 2022")
                                    )
                                ),
                                box(width = 6,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("Context & Resources")),
                                    htmlOutput("hs_resources")
                                )
                              ))
        )))},
        
        #AP & CTE Tab
        {tabItem(tabName = "coursestab",
                fluidRow(
                  tabBox(
                    id = "tabset2", width = "auto", 
                    tabPanel(i18n$t("Advanced Placement Courses"), class = "text-center",
                             div(tableOutput("APTable"), style = "font-size:150%"),
                               selectInput("ap_school", em(i18n$t("Choose a school to view the AP Courses available.")), 
                                         choices = list("Hillside High",
                                                        "Jordan High",
                                                        "Riverside High")
                             ),
                             fluidRow(
                               box(width = 4,
                                   title = strong(i18n$t("English AP Courses")), background = "olive", solidHeader = TRUE,
                                   htmlOutput("APEnglish", align="left")),
                               box(width = 4,
                                 title = strong(i18n$t("Math AP Courses")), background = "aqua", solidHeader = TRUE,
                                 htmlOutput("APMath", align="left")),
                               box(width = 4,
                                 title = strong(i18n$t("Science AP Courses")), background = "light-blue", solidHeader = TRUE,
                                 htmlOutput("APScience", align="left"))),
                             fluidRow(
                               box(width = 4,
                                   title = strong(i18n$t("Social Sciences AP Courses")), background = "green", solidHeader = TRUE,
                                   htmlOutput("APSocial", align="left")),
                               box(width = 4,
                                   title = strong(i18n$t("World Languages AP Courses")), background = "black", solidHeader = TRUE,
                                   htmlOutput("APWLang", align="left")),
                               box(width = 4,
                                   title = strong(i18n$t("Music and Arts AP Courses")), background = "navy", solidHeader = TRUE,
                                   htmlOutput("APMusArts", align="left"))),
                             fluidRow(
                               box(width = 4,
                                   title = strong(i18n$t("Engineneering AP Courses")), background = "purple", solidHeader = TRUE,
                                   htmlOutput("APEngine", align="left")))
                             ),
                    tabPanel(i18n$t("CTE Courses"), class = "text-center",
                             div(tableOutput("CTETable"), style = "font-size:150%"),
                             selectInput("cte_school", em(i18n$t("Choose a school to view the CTE Courses available.")), 
                                         choices = list("Hillside High",
                                                        "Jordan High",
                                                        "Riverside High")
                             ),
                             fluidRow(
                               box(width = 4,
                                   title = strong(i18n$t("Agricultural Education")), status = "primary" , solidHeader = TRUE,
                                   htmlOutput("AgCTE", align="left")),
                               box(width = 4,
                                   title = strong(i18n$t("Business, Marketing, and Finance")), status = "primary", solidHeader = TRUE,
                                   htmlOutput("BusCTE", align="left")),
                               box(width = 4,
                                   title = strong(i18n$t("Computer Science and Information Technology Education")), status = "primary", solidHeader = TRUE,
                                   htmlOutput("CompCTE", align="left"))),
                             fluidRow(
                               box(width = 4,
                                   title = strong(i18n$t("Family and Consumer Sciences Education")), status = "primary", solidHeader = TRUE,
                                   htmlOutput("FamCTE", align="left")),
                               box(width = 4,
                                   title = strong(i18n$t("Health Science Education")), status = "primary", solidHeader = TRUE,
                                   htmlOutput("HealthCTE", align="left")),
                               box(width = 4,
                                   title = strong(i18n$t("Trade, Technology, Engineering, and Industrial Education")), status = "primary", solidHeader = TRUE,
                                   htmlOutput("TradeCTE", align="left"))))
                  )
                )
        )},
        
        #Sports Tab - static
        {tabItem(tabName = "sportstab",
                 fluidRow(
                   tabBox(
                     id = "tabset3", width = "auto", 
                              selectInput("school_sports", em(i18n$t("Choose a school to view the Sports available.")), 
                                          choices = list("Brogden Middle", "Lowes Grove Middle", "Lakewood Montesorri Middle",
                                                         "Hillside High",
                                                         "Jordan High",
                                                         "Riverside High")
                              ),
                              fluidRow(
                                box(width = 4,
                                    title = strong(i18n$t("Fall Sports")), background = "olive", solidHeader = TRUE,
                                    htmlOutput("fallsports", align="center")),
                                box(width = 4,
                                    title = strong(i18n$t("Winter Sports")), background = "aqua", solidHeader = TRUE,
                                    htmlOutput("wintersports", align="center")),
                                box(width = 4,
                                    title = strong(i18n$t("Spring Sports")), background = "light-blue", solidHeader = TRUE,
                                    htmlOutput("springsports", align="center"))),
                              fluidRow(
                                box(width = 6,
                                    title = strong(i18n$t("Available Boy's/Men's Sports")), background = "navy", solidHeader = TRUE,
                                    htmlOutput("male_sports_list", align="center")),
                                box(width = 6,
                                    title = strong(i18n$t("Available Girl's/Women's Sports")), background = "navy", solidHeader = TRUE,
                                    htmlOutput("female_sports_list", align="center")),
                              ),
                              fluidRow(
                                box(width = 12,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("Context")),
                                    htmlOutput("sports_context"))
                              )
                   )
                 )
        )},
        
        #Sports Tab - dynamic
        # {tabItem(tabName = "sportstab",
        #          fluidRow(
        #            tabBox(
        #              id = "tabset3", width = "auto",
        #              tabPanel("School Sports", class = "text-center",
        #                       selectInput("school_sports", em("Choose a school to view the Sports available."),
        #                                   choices = list("Brogden Middle", "Lowes Grove Middle", "Lakewood Montesorri Middle",
        #                                                  "Hillside High",
        #                                                  "Jordan High",
        #                                                  "Riverside High")
        #                       ),
        #                       fluidRow(
        #                         box(width = 4,
        #                             title = strong("Fall Sports"), background = "olive", solidHeader = TRUE,
        #                             column(12, align="center", tableOutput("fallsports"))),
        #                         box(width = 4,
        #                             title = strong("Winter Sports"), background = "aqua", solidHeader = TRUE,
        #                             column(12, align="center", tableOutput("wintersports"))),
        #                         box(width = 4,
        #                             title = strong("Spring Sports"), background = "light-blue", solidHeader = TRUE,
        #                             column(12, align="center", tableOutput("springsports"))),
        #                         ),
        #                       fluidRow(
        #                         box(width = 6,
        #                             title = strong("Available Men's/Boy's Sports"), background = "navy", solidHeader = TRUE,
        #                             column(12, align="center", tableOutput("male_sports_list"))),
        #                         box(width = 6,
        #                             title = strong("Available Women's/Girl's Sports"), background = "teal", solidHeader = TRUE,
        #                             column(12, align="center", tableOutput("female_sports_list"))),
        # 
        #                         # box(width = 4,
        #                         #     solidHeader = TRUE,
        #                         #     title = strong("Icon Legend"),
        #                         #     column(12, align="center", tableOutput("sports_icon_legend")))
        # 
        #                         ),
        # 
        #                       fluidRow(
        #                         box(width = 12,
        #                             solidHeader = TRUE,
        #                             title = strong("Context"),
        #                             htmlOutput("sports_context"))
        #                       )
        # 
        #              ),
        #            )
        #          ),
        # 
        # )},
        
        #Arts Tab

        {tabItem(tabName = "artstab",
                 fluidRow(
                   box(width = 12,
                       title = strong(i18n$t("Available Arts Programs In Each School")), background = "navy", solidHeader = TRUE,
                       column(12, align="center", tableOutput("available_arts")))
                 ),
                 fluidRow(
                   box(width = 6, title = strong(i18n$t("Durham Public Schools and the Arts")), status = "primary", solidHeader = TRUE,
                       p(h4(i18n$t("Durham Public Schools’ appreciation for the arts is apparent throughout their public institutions. They provide curricula for the arts, upcoming events in the school system, resources for K-12 students interested in the arts, and news about arts programs in DPS. Vist"), 
                            a(i18n$t("Arts at DPS"), href="https://www.dpsnc.net/Arts#:~:text=Arts%20Education%20at%20Durham%20Public,body%20of%20knowledge%20and%20skills."),
                                   i18n$t("to learn more."))),
                       br(),
                       p(h4(em(strong(i18n$t("Learn more about some of the schools' arts programs by clicking on their logos below:"))))),
                       column(class = 'text-center', width = 4,
                              tags$a(
                                href="https://vimeo.com/718773555", 
                                tags$img(src="cc spaulding.png", 
                                         title="cc spaulding Logo",
                                         class= "img-responsive")),
                              tags$a(
                                href="https://jhscoursecatalog.weebly.com/fine-arts.html", 
                                tags$img(src="jordan high.png", 
                                         title="Jordan Logo",
                                         class= "img-responsive"))
                              
                              
                       ),
                       column(class = 'text-center', width = 4,
                              tags$a(
                                href="https://durhamvoice.org/?p=4549", 
                                tags$img(src="eastway.png", 
                                         title="Eastway Logo",
                                         class= "img-responsive")),
                              tags$a(
                                href="https://docs.google.com/document/d/1UfdYKPoG8UohnTRVbgnHrMZD6PZwRZVrQ1KgmEn5PBM/edit", 
                                tags$img(src="hillside high.png",
                                         title="Hillside Logo",
                                         class= "img-responsive")
                              )),
                       column(class = 'text-center', width = 4,
                              tags$a(
                                href="https://www.youtube.com/watch?v=2LcqFlBR9iw", 
                                tags$img(src="brogden.png",
                                         title="Brogden Logo",
                                         class= "img-responsive")),
                              tags$a(
                                href="https://www.riversidefab.org/boosters", 
                                tags$img(src="riverside.png",
                                         title="Riverside Logo",
                                         class= "img-responsive")))
                   ),
                   box(width = 6, title = strong(i18n$t("Durham County and the Arts")), status = "primary", solidHeader = TRUE,
                       p(h4(i18n$t("Durham has a rich history of highlighting the arts. In the mid-20th century the non-profit organization "),
                            a(i18n$t("Durham Arts Council"), href="https://durhamarts.org/"),
                            i18n$t("was founded to promote and provide access to various opportunities and resources for those in the arts. The Durham Arts Council also provides a directory of artists to network with one another through the"),
                            a(i18n$t("Durham Arts Network"), href="https://www.durhamartsnetwork.org/"),
                            i18n$t(". The city of Durham funded the"), a("Cultural & Public Art Program ", 
                                                                 href="https://www.durhamnc.gov/450/Cultural-Public-Art-Development"),
                            i18n$t("to “ illuminate residents’ history” and highlight Durham’s “rich cultural heritage”. Durham provides many opportunities for the public to indulge in cultural arts and for artists to showcase their work"),
                            a(i18n$t("Discover Durham"), href="https://www.discoverdurham.com/things-to-do/arts/"),
                            i18n$t("provides an extensive list of events for visitors and residents to do surrounding the arts. This includes festivals, concerts, performances, museums, art shows, etc."),
                            br(),br(),
                            a(i18n$t("The Nasher Museum of Art at Duke University"),
                              href="https://nasher.duke.edu/"),
                            i18n$t("emphasizes works by diverse artists who have been historically underrepresented, or excluded, by mainstream arts institutions, and maintains a particular focus on artists of African descent, as well as European medieval art, European and American paintings, Outsider art, classical antiquities, African art and ancient American art."),
                            i18n$t("North Carolina Central University students host various musical ensembles, such as the"),
                            a(i18n$t("NCCU Jazz Vocal Ensemble"),
                              href="https://www.nccu.edu/cash/music/ensembles/vocal-jazz"),
                            i18n$t("and the"),
                            a(i18n$t("NCCU Sound Machine Marching Band"),
                              href="https://www.nccusoundmachine.com/"),
                            i18n$t(", that perform throughout the city of Durham, North Carolina and the United States."),
                            a(i18n$t("WNCU 90.7 FM is Central’s Jazz radio station"),
                              href="https://www.wncu.org/"),
                            i18n$t("which provides “diverse music entertainment and serve as an educational resource for our community and abroad.” NCCU also provides volunteer opportunities for their students in the"),
                            a(i18n$t("KidzNotes program"),
                              href="https://kidznotes.org/"),
                            i18n$t("via Fayetteville Street Elementary’s AT&T Believe Program.")
                            
                            ))
                   )
                 ) 
        )},
      
        #Data Insights tab
        {tabItem(tabName = "insightstab",
                 
                 # fluidRow(
                 #   box(width  = 12,
                 #       solidHeader = TRUE,
                 #       title = strong("Map Comparison Of School Districts"),
                 #       h4("Different school districts are differently colored according to the variable selected."),
                 #       leafletOutput("choropleth")),
                 # ), 
                 fluidRow(class= 'text-center',
                          box(width = 12,
                              solidHeader = TRUE,
                              title = strong("Resources in Selected School Zone"),
                              p(h4(align="left",i18n$t("These plots reveal the total number of each resource in each school district. Different school districts have different populations; therefore, there are different corresponding numbers of resources. These plots are useful for getting a sense of the of different types of resources available in each district at a glance.")), br()),
                              fluidRow(
                                box(width = 2,
                                    solidHeader = TRUE,
                                    title = strong(i18n$t("School Zone")),
                                    selectInput("insights_zone",
                                                label = em(i18n$t("Choose a school zone to display")),
                                                choices = c("Brogden Middle", "C.C. Spaulding Elementary", "Club Boulevard Elementary",
                                                            "Eastway Elementary","E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                                            "Forest View Elementary", "Hillandale Elementary","Hillside High",
                                                            "Jordan High","Lakewood Elementary", "Lakewood Montessori Middle", "Lowes Grove Middle",
                                                            "Parkwood Elementary", "Riverside High", "Southwest Elementary"
                                                ),
                                                multiple = FALSE)),
                                box(width = 10,
                                    solidHeader = TRUE,
                                    plotlyOutput("insights_individualplots",
                                                 width="auto",
                                                 height = "auto"))
                              ))
                 )
                 

                 
        )},
        
        #Meet the team tab
        {tabItem(tabName = "teamstab",
                 
                 fluidRow(
                     box(width = 12,
                         background = "light-blue",
                         class = "text-center",
                         h3(strong(i18n$t("Meet Our Team - Data+ 2022"))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(align = "center", src = "emily250.jpg")),
                             column(width =3,
                                    p(i18n$t("Emily McReynolds is a sophomore at Duke University originally from Greensboro, North Carolina. She intends to major in Public Policy and obtain a certificate in Markets and Management Studies. Emily is passionate about policy reform, specifically in our educational and healthcare systems. She hopes this project will bring more awareness to the available resources and community centers in Durham County that can support our public schools. Emily is eager to see how this partnership can build a stronger sense of unity for all."))),
                             column(width = 3,
                                    img(src = "aryan250.jpg")),
                             column(width = 3,
                                    p(i18n$t("Aryan Poonacha is a rising senior at Duke University from Bangalore, India. Currently, he is studying Data Science, with a minor in Political Science. He is especially interested in using big data analyses to reveal politically and socially relevant insights to key issues in policy and social justice. He joined the Data+ team to provide a clearer picture of Durham Public Schools, and find better paths to their improvement.")))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(src = "patience3.jpg")),
                             column(width = 3,
                                    p(i18n$t("Patience Jones is a senior at North Carolina Central University from Durham, North Carolina. Currently, she is studying English, Secondary Education, and General Psychology. Patience continued the Data+ project team because she was wanted to continue the work she did the previous summer in hopes to improve the dashboard to make it more accessible. With her background in education policy, she hopes to integrate her own knowledge with this dashboard to make an impact on Durham Public Schools and beyond."))),
                             column(width = 3,
                                    img(src = "mel250.jpg")),
                             column(width = 3,
                                    p(i18n$t("Melanie Kaye Moseley is a senior at North Carolina Central University from Oxford, North Carolina. She is studying Music with a concentration in Instrumental Performance. Melanie joined the Data+ project team to contribute to the previous research and efforts that have equipped the corresponding Bass Connections team with organized information and statistics to pinpoint the resources that would be most beneficial for specific schools. Melanie hopes this dashboard will help increase equity within schools, and promote a greater sense of community throughout Durham, North Carolina.")))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(src = "surabhi.jpg")),
                             column(width = 3,
                                    p(i18n$t("Surabhi Trivedi is a masters student in Interdisciplinary Data Science at Duke University. Her interest lies at the intersection of data science and public policy, and specifically social policy. For the summer, she is interning at the World Bank and the Urban Institute as a data scientist while volunteering to help with the project.")))))),
                 
                 fluidRow(
                     box(width = 12,
                         background = "navy",
                         class = "text-center",
                         h3(strong(i18n$t("Meet Our Team - Data+ 2021"))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(src = "ally3.jpg")),
                             column(width =3,
                                    p(i18n$t("Allyson Ashekun is a junior at Duke University from Clearwater, Florida. Currently, she is studying Public Policy and Computer Science, and her academic interests focus primarily on the intersection of those two disciplines in areas such as Data Science. Allyson joined the Data+ project team because she is passionate about equitable education and enjoys coding. She hopes this dashboard will be a helpful tool for those entering Durham Public Schools, and will help improve the quality of partnerships."))),
                             column(width = 3,
                                    img(src = "drewheadshot3 copy.jpg")),
                             column(width = 3,
                                    p(i18n$t("Drew Greene is a sophomore at Duke University from Richmond, Virginia. He intends to study Public Policy with minors in Education and Inequality Studies. His academic interests include educational equity and the school-to-prison pipeline. Drew joined this project because of the opportunity to develop his data science skills. He hopes this project will help adequately prepare college students to enter schools as well as allow Durham residents tolocate the myriad resources accessible to help form an even stronger community built around the schools.")))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(src = "patience3.jpg")),
                             column(width = 3,
                                    p(i18n$t("Patience Jones is a senior at North Carolina Central University from Durham, North Carolina. Currently, she is studying English, Secondary Education, and General Psychology. Patience joined the Data+ project team because she was interested in learning more about data science and its integration into education policy. She hopes this dashboard makes an impact on not only these ten community schools in the dataset, but students in all Durham Public Schools and beyond, in hopes to make education more accessible to all students."))),
                             column(width = 3,
                                    img(src = "rhea3.jpg")),
                             column(width = 3,
                                    p(i18n$t("Rhea Tejwani is a junior at Duke University from Demarest, New Jersey. She is studying Computer Science and Economics. Rhea decided to join this project teambecause she has a passion for data science and hopes that this app will help local universities have a productive relationship with the community. She is proud of the work the team accomplished, and is excited to see the long term impacts that this web app has!")))),
                         br(),
                         fluidRow(
                             column(width = 3,
                                    img(src = "nico3.jpg")),
                             column(width = 3,
                                    p(i18n$t("Nico Restrepo Ochoa is a PhD candidate at Duke's sociology department. He's interested in how habits and beliefs change, both at the individual and collective level, and uses longitudinal data, networks, and simulations to try to get at this question. He had the privilege to be the project manager for this team, and believes the team was efficient and industrious so his job was easy. The team claims he was helpful, and he likes to believe that is true."))))))
        )} 
        
        
    )
)
}

dashboardPage(
    skin = "black",
    dashboardHeader(
        title = i18n$t("Visualizing DPS")),
    sidebar,
    body
)

