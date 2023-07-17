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


i18n <- Translator$new(translation_json_path = "./data/Translations/fullTranslation.json")
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
    menuItem(i18n$t("Data Insights"), tabName = "insightstab", icon = icon("fas fa-chart-line")),
    menuItem(i18n$t("AP & CTE Courses"), tabName = "coursestab", icon = icon("book")),
    menuItem(i18n$t("School Sports"), tabName = "sportstab", icon = icon("basketball-ball")),
    menuItem(i18n$t("Arts Programs"), tabName = "artstab", icon = icon("paint-brush")),
    menuItem(i18n$t("Duke/NCCU Engagement"), tabName = "engagementtab", icon = icon("fas fa-handshake-angle")),
    menuItem(i18n$t("Feedback"), tabName = "feedback", icon = icon("fas fa-comment")),
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
                   p(i18n$t("The inspiration for this project is rooted in the inter-institutional Bass Connections team from Duke University and North Carolina Central University, which is committed to developing more responsible and imaginative ways of partnering with Durham Public Schools. This project aims to provide a centralized web application that will serve as a tool for those entering Durham Public Schools. In addition, our application aims to inform future pre-service training for university students, support local neighborhood schools in visualizing their communities, and help various university offices articulate what “community” actually looks like.")),
                   br(),
                   p(i18n$t("Using spatial and school-specific data, along with contextual resources, we hope to provide a holistic view of Durham Public Schools and their communities while highlighting their resources and assets.")),
                   p(i18n$t("Visit the"), a("Durham UACS Research Collective website",
                                            href = "https://sites.duke.edu/uacs/", target="_blank"),
                     i18n$t("for more information!")))),
             fluidRow(
               class = "text-center",
               box(
                 solidHeader = TRUE,
                 br(),
                 width = 12,
                 valueBox(17, i18n$t("Geospatial Variables"), icon = icon("map"), color = "light-blue", width = 4),
                 valueBox(41, i18n$t("School-Specific Variables"), icon = icon("pencil"), color = "light-blue", width = 4),
                 valueBox(1, i18n$t("Centralized Web Application"), icon = icon("window-restore"), color = "light-blue", width = 4))),
             fluidRow(
               class = "text-center",
               box(title = strong(i18n$t("2 Universities, 50 Public Schools")),
                   solidHeader = TRUE,
                   width = 7,
                   p(i18n$t("The Durham Public School District has 50 public schools: 27 elementary, 11 middle, 12 high.")),
                   br(),
                   p(i18n$t("Duke and NCCU both aim to foster equitable partnerships with Durham Public Schools. Prior Bass Connections research focused on understanding how to achieve this goal and found that one of the main barriers to meaningful engagement between Duke, NCCU, and Durham public schools is that “many university students lack an understanding of city and community dynamics.” Additionally, they found that there is a “lack of student volunteer training with Durham’s context, particularly in the areas of history, school-specific demographics, and implicit bias and power dynamics that may manifest in schools.”")),
                   br(),
                   p(strong(i18n$t("Motivated by this research, our project explores a way of visualizing schools as centers of the community that bring academics, health and social services, youth and community development, and community engagement together under one roof.")))),
               box(width = 5,
                   background = "light-blue",
                   solidHeader = TRUE,
                   leafletOutput("home"))),
             fluidRow(class = "text-center",
                      box(title = strong(i18n$t("View Our 50 Schools")),
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
                               choices = c("All", "Bethesda Elementary", "Burton Elementary","C.C. Spaulding Elementary","Club Boulevard Elementary","Creekside Elementary",
                                           "Eastway Elementary", "Easley Elementary", "Eno Valley Elementary", "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                                           "Forest View Elementary", "George Watts Elementary", "Glenn Elementary",  "Holt Elementary","Hope Valley Elementary",
                                           "Hillandale Elementary","Lakewood Elementary", "Mangum Elementary","Merrick-Moore Elementary","Oak Grove Elementary","Pearsontown Elementary","Parkwood Elementary","R.N. Harris Elementary","Southwest Elementary",
                                           "Sandy Ridge Elementary","Spring Valley Elementary","W.G. Pearson Elementary", "Y.E. Smith Elementary","Brogden Middle", 
                                           "Carrington Middle","Lucas Middle","Lakewood Montessori Middle", "Lowes Grove Middle", "Neal Middle","Rogers Herr Middle", 
                                           "Shepard Middle", "Sherwood Githens Middle","City of Medicine Academy", "Durham School of the Arts","J.D. Clement Early College",
                                           "Hillside High","Holton Career","Jordan High","Lakewiew High","Middle College", "Morehead Montessori School",
                                           "Northern High","Riverside High","School for Creative Studies", 
                                           "Southern School of Energy and Sustainability"   
                                            
                                           
                               ),
                               
                               multiple = FALSE),
                   selectInput("var",
                               label = em(i18n$t("Choose a variable to display")),
                               choices = c("After-School Care Programs", "Bus Stops", 
                                           "Childcare Centers", "Community and Cultural Centers", "Community Arts", "Community Sports","Farmers' Markets", "Food Pantries", "Gardens",
                                           "Grocery Stores", "Hospitals and Clinics","Libraries", "Homeless Shelters", "Parks", "Pharmacies",  
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
                             img(src = "homelesshelter.jpg", width = 40, height = 40, align = "left")),
                      column(width = 1),
                      column(width = 8, htmlOutput("homelesshelter")
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
                      column(width = 1,
                             img(src = "pharmacy.jpg", width = 40, height = 40, align = "left")),
                      column(width = 1),
                      column(width = 8, htmlOutput("pharmacy")
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
                                           height = "750px"),
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
                                            choices = list("Summer 2021", "Summer 2022", "Summer 2023")
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
                                            choices = list("Summer 2022", "Summer 2023")
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
                                            choices = list("Summer 2021", "Summer 2022", "Summer 2023")
                                )
                            ),
                            box(width = 6,
                                solidHeader = TRUE,
                                title = strong(i18n$t("Context & Resources")),
                                htmlOutput("hs_resources")
                            )
                          ))
               )))},
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
                          p(h4(align="left",i18n$t("These plots reveal the total number of selected resources in each school zone. These plots are useful for getting a sense of the different types of resources available in each zone at a glance.")), br()),
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
                                                        "Parkwood Elementary", "Riverside High", "Southwest Elementary", "Eno Valley Elementary", "Glenn Elementary", 
                                                        "Creekside Elementary", "Easley Elementary", "Burton Elementary", "Bethesda Elementary", "George Watts Elementary", 
                                                        "Holt Elementary", "Hope Valley Elementary", "Mangum Elementary", "Morehead Montessori Elementary", "Merrick-Moore Elementary", 
                                                        "Oak Grove Elementary", "Pearsontown Elementary", "W.G. Pearson Elementary", "Sandy Ridge Elementary",  "R.N. Harris Elementary", 
                                                        "Spring Valley Elementary", 
                                                        "Y.E. Smith Elementary",
                                                        "Sherwood Githens Middle",
                                                        "Lucas Middle",
                                                        "Carrington Middle",
                                                        "Neal Middle",
                                                        "Rogers-Herr Middle",
                                                        "Shepard Middle", 
                                                        "Durham School of Arts",
                                                        "School for Creative Studies",
                                                        "JD Clement High",
                                                        "Durham School of Technology",
                                                        "Middle College High",
                                                        "Holton Career and Resource Center",
                                                        "Northern High",
                                                        "Southern School of Energy and Sustainability",
                                                        "Lakeview High"
                                                        
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
                                                     "Riverside High",
                                                     "City of Medicine Academy",
                                                     "Durham School of Technology",
                                                     "Northern High",
                                                     "Southern School of Energy and Sustainability")
                                      
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
                             choices = list("Brogden Middle", 
                                            "Lowes Grove Middle", 
                                            "Lakewood Montesorri Middle",
                                            "Sherwood Githens Middle", 
                                            "Lucas Middle", 
                                            "Carrington Middle", 
                                            "Neal Middle", 
                                            "Rogers Herr Middle", 
                                            "Shepard Middle", 
                                            "Durham School of the Arts Middle",
                                            "Hillside High",
                                            "Durham School of Technology",
                                            "Jordan High",
                                            "Riverside High",
                                            "Northern High",
                                            "Southern High")
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
                   p(h4(i18n$t("Durham Public Schools’ appreciation for the arts is apparent throughout their public institutions. They provide curriculum for the arts, upcoming events in the school system, resources for K-12 students interested in the arts, and news about arts programs in DPS. Visit"), 
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
                   p(h4(i18n$t("Durham has a rich history of highlighting the arts. Durham has a rich history of highlighting the arts. Founded in the mid-20th century, the non-profit organization "),
                        a(i18n$t("Durham Arts Council"), href="https://durhamarts.org/"),
                        i18n$t("promotes and provides access to various opportunities and resources for those in the arts. The Durham Arts Council also offers a directory of artists to network with one another through the "),
                        a(i18n$t("Durham Arts Network"), href="https://www.durhamartsnetwork.org/"),
                        i18n$t(". In addition, the city of Durham funded the"), a("Cultural & Public Art Program ", 
                                                                     href="https://www.durhamnc.gov/450/Cultural-Public-Art-Development"),
                        i18n$t("to “ illuminate residents’ history” and highlight Durham’s “rich cultural heritage”. Durham provides many opportunities for the public to indulge in cultural arts and for artists to showcase their work. "),
                        a(i18n$t("Discover Durham"), href="https://www.discoverdurham.com/things-to-do/arts/"),
                        i18n$t("provides an extensive list of events for visitors and residents to do around the arts: including festivals, concerts, performances, museums, art shows, etc. "),
                        br(),br(),
                        a(i18n$t("The Nasher Museum of Art at Duke University"),
                          href="https://nasher.duke.edu/"),
                        i18n$t("emphasizes works by diverse artists who have been historically underrepresented, or excluded, by mainstream art institutions and maintains a particular focus on artists of African descent, as well as European medieval art, European and American paintings, Outsider art, classical antiquities, African art, and ancient American art."),
                        i18n$t("North Carolina Central University students host various musical ensembles, such as the"),
                        a(i18n$t("NCCU Jazz Vocal Ensemble"),
                          href="https://www.nccu.edu/cash/music/ensembles/vocal-jazz"),
                        i18n$t("and the"),
                        a(i18n$t("NCCU Sound Machine Marching Band"),
                          href="https://www.nccusoundmachine.com/"),
                        i18n$t(", that perform throughout the city of Durham, North Carolina, and the United States."),
                        a(i18n$t("WNCU 90.7 FM is Central’s Jazz radio station"),
                          href="https://www.wncu.org/"),
                        i18n$t("providing “diverse music entertainment and serving as an educational resource for our community and abroad.” NCCU also provides volunteer opportunities for their students in the"),
                        a(i18n$t("KidzNotes program"),
                          href="https://kidznotes.org/"),
                        i18n$t("via Fayetteville Street Elementary’s AT&T Believe Program.")
                        
                   ))
               )
             ) 
    )},
    
   
    ##NCCU/DUKE engagement tab
    {tabItem(tabName = "engagementtab",
             selectInput("tab", em(i18n$t("Choose a Division to View Community Engagement Oppurtunities")),
                         choices = list("Staff/Faculty",
                                        "Undergraduate Students"),
                         multiple = FALSE),
             br(),
             dataTableOutput("engagetable"),
             
             fluidRow(
               box(width = 12,
                   title = strong(i18n$t("Context")),
                   i18n$t("From "),
                   a(i18n$t("creating free math workshops for girls who attend Durham Public Schools "), href="https://trinity.duke.edu/news/how-trinity-faculty-and-students-are-sharing-resources-support-durham-public-schools"), 
                   i18n$t("to "), 
                   a(i18n$t("faculty research projects that engage with "), href="https://facultyadvancement.duke.edu/seven-faculty-projects-community-impact-racial-and-social-equity-issues"),
                   a(i18n$t("“Racial and Social Equity in Local Contexts”, "), href = "https://facultyadvancement.duke.edu/racial-and-social-equity-local-context-engaging-durhams-priority-areas-community-impact"),
                   i18n$t("Duke and NCCU staff and faculty and undergraduate students are leading DPS-facing initiatives to form purposeful partnerships between Duke, NCCU, and DPS. This list includes initiatives that support the whole-child and whole-community framework of community schools, both inside and outside programs of education. All data was derived from "),
                   a(i18n$t("Duke Office of Durham & Community Affairs, "), href="https://community.duke.edu/"), 
                   a(i18n$t("Duke CampusGroups, "), href="https://dukegroups.com/home_login"), 
                   i18n$t("and, "), 
                   a(i18n$t("NCCU Office of Community Engagement and Service."), href="https://www.nccu.edu/oces")))
               
             
             #fluidRow(
               #box(width = 12,
                   #background = "light-blue",
                   #solidHeader = TRUE,
                   #slickROutput("carou", width = "auto", height = "auto"),
                   #use_gotop(color = "black")),
             #),
  
                  
    )},
    
    #Meet the team tab
    {tabItem(tabName = "teamstab",
             
             fluidRow(
               box(width = 12,
                   background = "navy",
                   class = "text-center",
                   h3(strong(i18n$t("Meet Our Team - Data+ 2023"))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(align = "center", src = "ethan.jpg")),
                     column(width =3,
                            p(i18n$t("Ethan Shang is a rising sophomore at Duke University from Chapel Hill, North Carolina. Currently, he is studying Data Science, with a minor in Economics. He is especially interested in using big data analyses to reveal politically and socially relevant insights to key issues in policy and social justice. He joined the Data+ team to provide a clearer picture of Durham Public Schools, and to support a vibrant community that has surrounded him from an early age."))),
                     column(width = 3,
                            img(src = "sreya.jpeg")),
                     column(width = 3,
                            p(i18n$t("Sreya Gnanavel is a sophomore at Duke University originally from Cary, North Carolina. She intends to major in Data Science and minor in Finance. Sreya is passionate about using data science to help uncover meaningful insights and use to make informed decisions. She hopes this project will bring more awareness to the available resources in the Durham Public System and help expand the reach to other community schools. She was motivated to join this project as it aligns with her goal of applying her data science skills to create positive change particularly by promoting equal access to resources and opportunities for all students.")))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(src = "unzila.jpg")),
                     column(width = 3,
                            p(i18n$t("Unzila Sakina Babar, a sophomore at Duke University, is a passionate computer and data scientist hailing from Lahore, Pakistan. Alongside her academic pursuits, she possesses a keen interest in the education system. Unzila's deep-rooted desire to revolutionize the schooling system in her home country fuels her involvement in this project. She envisions fostering a robust sense of community within Pakistan School Systems while raising awareness about the abundant resources and communities available in the Public Schools of Durham County.  She seeks to synergize her own expertise with the capabilities of this dashboard to create a lasting impact on Durham Public Schools and extend this influence beyond its boundaries. "))),
                     column(width = 3,
                            img(src = "lauren.jpeg")),
                     column(width = 3,
                            p(i18n$t("Lauren Walker is a Junior at Duke University from Needham, Massachusetts. She is studying Computer Science with a minor in Statistics. Lauren joined this Data+ project team because she is eager to strengthen the relationship and partnership between Durham Public Schools and local universities. She also enjoys coding and is looking forward to improving her data science skills. She hopes that this dashboard will serve as a valuable resource for those looking to learn more about the resources offered throughout the Durham Public School system and make education more accessible and equitable.")))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(src = "surabhi.jpg")),
                     column(width = 3,
                            p(i18n$t("Surabhi Trivedi is a masters student in Interdisciplinary Data Science at Duke University. Her interest lies at the intersection of data science and public policy, and specifically social policy. For the summer, she is interning at the World Bank and the Urban Institute as a data scientist while volunteering to help with the project.")))))),
             
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
                            p(i18n$t("Emily McReynolds is a sophomore at Duke University originally from Greensboro, North Carolina. She intends to major in Public Policy and obtain a certificate in Markets and Management Studies. Emily is passionate about policy reform, specifically in our education and healthcare systems. She hopes this project will bring more awareness to the available resources and community centers in Durham County that can support our public schools. Emily is eager to see how this partnership can build a stronger sense of unity for all."))),
                     column(width = 3,
                            img(src = "aryan250.jpg")),
                     column(width = 3,
                            p(i18n$t("Aryan Poonacha is a rising senior at Duke University from Bangalore, India. Currently, he is studying Data Science with a minor in Political Science. He is especially interested in using big data analyses to reveal politically and socially relevant insights to key issues in policy and social justice. He joined the Data+ team to provide a clearer picture of Durham Public Schools and find better paths to their improvement.")))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(src = "patience3.jpg")),
                     column(width = 3,
                            p(i18n$t("Patience Jones is a senior at North Carolina Central University from Durham, North Carolina. Currently, she is studying English, Secondary Education, and General Psychology. Patience continued the Data+ project team in hopes of improving the dashboard to make it more accessible. With her background in education policy, she hopes to integrate her own knowledge with the dashboard to make an impact on Durham Public Schools and beyond."))),
                     column(width = 3,
                            img(src = "mel250.jpg")),
                     column(width = 3,
                            p(i18n$t("Melanie Kaye Moseley is a senior at North Carolina Central University from Oxford, North Carolina. She is studying Music with a concentration in Instrumental Performance. Melanie joined the Data+ project team to contribute to the existing research and efforts that have equipped the Bass Connections team with organized information  to pinpoint the resources that would be most beneficial for schools. Melanie hopes this dashboard will help increase equity within schools and promote a greater sense of community throughout Durham.")))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(src = "surabhi.jpg")),
                     column(width = 3,
                            p(i18n$t("Surabhi Trivedi is a masters student in Interdisciplinary Data Science at Duke University. Her interest lies at the intersection of data science and public policy, specifically social policy. For the summer, she is interning at the World Bank and the Urban Institute as a data scientist while volunteering to help with this project. Allyson Ashekun is a junior at Duke University from Clearwater, Florida. Currently, she is studying Public Policy and Computer Science, and her academic interests focus primarily on the intersection of those two disciplines in areas such as Data Science.")))))),
             
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
                            p(i18n$t("Drew Greene is a sophomore at Duke University from Richmond, Virginia. He intends to study Public Policy with minors in Education and Inequality Studies. His academic interests include educational equity and the school-to-prison pipeline. Drew joined this project because of the opportunity to develop his data science skills. He hopes this project will help Durham residents to locate the myriad resources accessible to help form an even stronger community built around public schools.")))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(src = "patience3.jpg")),
                     column(width = 3,
                            p(i18n$t("Patience Jones is a senior at North Carolina Central University from Durham, North Carolina. Currently, she is studying English, Secondary Education, and General Psychology. Patience joined the Data+ project team because she was interested in learning more about data science and its integration into education policy. She hopes this dashboard makes an impact on students in all Durham Public Schools and beyond, in hopes of making education more accessible to all students."))),
                     column(width = 3,
                            img(src = "rhea3.jpg")),
                     column(width = 3,
                            p(i18n$t("Rhea decided to join this project team because she has a passion for data science and hopes that this app will help local universities maintain a productive relationship with the community. She is proud of the work the team has accomplished, and is excited to see the long term impacts it has!")))),
                   br(),
                   fluidRow(
                     column(width = 3,
                            img(src = "nico3.jpg")),
                     column(width = 3,
                            p(i18n$t("Nico Restrepo Ochoa is a PhD candidate at Duke's sociology department. He's interested in how habits and beliefs change, both at the individual and collective level, and uses longitudinal data, networks, and simulations to try to get at this question. He had the privilege to be the project manager for this team, and believes the team was efficient and industrious so his job was easy. The team claims he was helpful, and he likes to believe that is true."))))))
    )},
    
    #feedback
    {tabItem(tabName = "feedback",
          
             fluidRow(
               class = "text-center",
               size = 20,
               box(width = 12,
                   background = "light-blue",
                   solidHeader = TRUE,
                   title = a(i18n$t("We want to hear your feedback!"), href= "https://forms.gle/eQ5vUEz3W99CmMa4A", style = "color:black;font-size:30px;text-decoration:underline"),
                   style = "color:black;font-size:18px",
                   "Click the text above to fill out our feedback form")),
             fluidRow(
               class = "text-center",
               size = 20,
               box(width = 12,
                   #background = "white",
                   solidHeader = TRUE,
                   title = a(i18n$t("Learn More!"), style = "color:black;font-size:30px"),
                   a(i18n$t("Community School Partnership"), href="https://sites.duke.edu/uacs/outputs/", style = "color:black;font-size:18px;text-decoration:underline"),
                   br(),
                   a(i18n$t("Data+ 2023"), href="https://bigdata.duke.edu/projects/strengthening-partnerships-durham-schools-local-universities/", style = "color:black;font-size:18px;text-decoration:underline"),
                   br(),
                   a(i18n$t("Bass Connecions 2022-2023"), href="https://bassconnections.duke.edu/project-teams/strengthening-partnerships-between-durham-public-schools-and-local-universities-2022", style = "color:black;font-size:18px;text-decoration:underline"),
                   br(),
                   a(i18n$t("Duke and NCCU Collaboration Published in Community Schooling Journal"), href="https://community.duke.edu/impact-story/duke-public-school-collaboration-published-in-journal/", style = "color:black;font-size:18px;text-decoration:underline"),
                   br(),
                   a(i18n$t("How Trinity Faculty and Students Are Sharing Resources in Support of Durham Public Schools"), href="https://trinity.duke.edu/news/how-trinity-faculty-and-students-are-sharing-resources-support-durham-public-schools", style = "color:black;font-size:18px;text-decoration:underline"),
                   br(),
                   img(src = "project_logo.png", align="center"))),
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

