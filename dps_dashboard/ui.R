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
  navbarPage("Visualizing DPS",
             #Home Page
             tabPanel(i18n$t("Home"), tabName = "home", icon = icon("fas fa-home"),
                      fluidRow(
                        class = "text-center",
                        column(width = 12,
                               img(src = "landing.jpg"))
                      ),
                      br(), 
                      
                      fluidRow(
                        class = "text-center",
                        box(h3(strong(htmlOutput("header1"))),
                            width = 12,
                            background = "navy",
                            htmlOutput("home_text1"))),
                      fluidRow(
                        class = "text-center",
                        box(
                          solidHeader = TRUE,
                          br(),
                          width = 12,
                          valueBox(17, htmlOutput("header2"), icon = icon("map"), color = "light-blue", width = 4),
                          valueBox(41, htmlOutput("header3"), icon = icon("pencil"), color = "light-blue", width = 4),
                          valueBox(1, htmlOutput("header4"), icon = icon("window-restore"), color = "light-blue", width = 4))),
                      fluidRow(
                        class = "text-center",
                        box(title = strong(htmlOutput("header5")),
                            solidHeader = TRUE,
                            width = 7,
                            htmlOutput("home_text2"),
                        box(width = 5,
                            background = "light-blue",
                            solidHeader = TRUE,
                            leafletOutput("home"))),
                      fluidRow(class = "text-center",
                               box(title = htmlOutput("header6")),
                                   width = 12,
                                   background = "light-blue",
                                   actionButton("viewMap", htmlOutput("header7")),
                                   actionButton("viewStat", htmlOutput("header8")))),
                      
                      fluidRow(class = "text-center",
                               box(width = 12,
                                   solidHeader = TRUE,
                                   title = strong(htmlOutput("header9")),
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
             ),
             
             #Maps Tab
             tabPanel(i18n$t("Maps"), tabName = "mapstab", icon = icon("fas fa-map-marked-alt"),
                      fluidRow(
                        box(width  = 7,
                            solidHeader = TRUE,
                            title = strong(htmlOutput("mapshead")),
                            h4(htmlOutput("mapsdescription")),
                            h4(htmlOutput("mapsdesc2"), icon('search'), htmlOutput("mapsdesc3")),
                            leafletOutput("map")),
                        box(width = 5,
                            solidHeader = TRUE,
                            title = strong(htmlOutput("mapscontext")),
                            htmlOutput("context")),
                        
                      ),
                      fluidRow( 
                        box(width = 4,
                            solidHeader = TRUE,
                            title = strong(htmlOutput("mapsmeasure")),
                            selectInput("zone",
                                        label = em(htmlOutput("mapsdrop1")),
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
                                        label = em(htmlOutput("mapsdrop2")),
                                        choices = c("After-School Care Programs", "Bus Stops", 
                                                    "Childcare Centers", "Community and Cultural Centers", "Community Arts", "Community Sports","Farmers' Markets", "Food Pantries", "Gardens",
                                                    "Grocery Stores", "Hospitals and Clinics","Libraries", "Homeless Shelters", "Parks", "Pharmacies",  
                                                    "Recreation Centers", "Religious Centers", "Restaurants"
                                        ),
                                        multiple = FALSE)
                        ),
                        box(width = 4,
                            solidHeader = TRUE,
                            title = strong(htmlOutput("mapsvariable")),
                            em(htmlOutput("mapsvardesc")),
                            br(),
                            br(),
                            dataTableOutput("list"),
                        ),
                        
                        #Icon Legend
                        {box(width = 4,
                             solidHeader = TRUE,
                             title = strong(htmlOutput("mapsicon")),
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
                               column(width = 1,
                                      img(src = "restaurant.png", width = 40, height = 40, align = "left")),
                               column(width = 1),
                               column(width = 8, htmlOutput("restaurant")
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
             ),
             
             #School Stats Tab
             tabPanel("School Statistics", icon = icon("fas fa-chart-bar"),
                      fluidRow(
                        tabBox(
                          # The id lets us use input$tabset1 on the server to find the current tab
                          id = "tabset1", width = "auto",
                          #Box Plot Outputs
                          tabPanel(i18n$t("Elementary School"),
                                   box(width = 12,
                                       background = "navy", 
                                       solidHeader = TRUE, 
                                       title = strong(htmlOutput("statselem")),
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
                                         title = strong(htmlOutput("statshead")),
                                         selectInput("es_select", em(htmlOutput("statsdrop1e")), 
                                                     choices = list("Average Class Size","Bachelor Degree Rate",
                                                                    "BIPOC Students per School",
                                                                    "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                                    "Free and Reduced Lunch","Funding Per Pupil","In-School Suspensions (ISS)",
                                                                    "Median Age","Median Homesale Price","Median Household Income",
                                                                    "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                                    "Students Per Device","Student-Teacher Ratio, Elementary School", 
                                                                    "Students With Disabilities", "Titles Per Student", "WiFi Access Points Per Classroom")
                                         ),
                                         selectInput("es_year", em(htmlOutput("statsdrop2e")), 
                                                     choices = list("Summer 2021", "Summer 2022", "Summer 2023")
                                         )
                                     ),
                                     box(width = 6,
                                         solidHeader = TRUE,
                                         title = strong(htmlOutput("statshead2")),
                                         htmlOutput("es_resources")
                                     )
                                   )
                          ),
                          tabPanel(i18n$t("Middle School"), box(width = 12,
                                                                background = "navy", 
                                                                solidHeader = TRUE, 
                                                                title = strong(htmlOutput("statsmiddle")),
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
                                         selectInput("ms_select", em(htmlOutput("statsdrop1m")), 
                                                     choices = list("Average Class Size","Bachelor Degree Rate",
                                                                    "BIPOC Students per School","CTE Course Enrollment Rate, Middle School", 
                                                                    "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                                    "Free and Reduced Lunch","Funding Per Pupil","In-School Suspensions (ISS)",
                                                                    "Median Age","Median Homesale Price","Median Household Income",
                                                                    "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                                    "Students Per Device","Student-Teacher Ratio, Middle School", 
                                                                    "Students With Disabilities", "Titles Per Student", "WiFi Access Points Per Classroom")
                                         ),
                                         selectInput("ms_year", em(htmlOutput("statsdrop2m")), 
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
                                                              title = strong(htmlOutput("statshigh")),
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
                                         selectInput("hs_select", em(htmlOutput("statsdrop1h")), 
                                                     choices = list("Advanced Placement (AP) Course Enrollment", "Average Class Size","Bachelor Degree Rate",
                                                                    "BIPOC Students per School","CTE Course Enrollment Rate, High School", 
                                                                    "English as a Second Language (ESL) Student Enrollment","Enrollment","Experienced Teacher Ratio",
                                                                    "Free and Reduced Lunch","Funding Per Pupil","Graduation Rate","In-School Suspensions (ISS)",
                                                                    "Median Age","Median Homesale Price","Median Household Income",
                                                                    "Racial Demographics", "School and Zone BIPOC Comparison","Sidewalk Coverage",
                                                                    "Students Per Device","Student-Teacher Ratio, High School", 
                                                                    "Students With Disabilities", "Titles Per Student", "WiFi Access Points Per Classroom")
                                         ),
                                         selectInput("hs_year", em(htmlOutput("statsdrop2h")), 
                                                     choices = list("Summer 2021", "Summer 2022", "Summer 2023")
                                         )
                                     ),
                                     box(width = 6,
                                         solidHeader = TRUE,
                                         title = strong(i18n$t("Context & Resources")),
                                         htmlOutput("hs_resources")
                                     )
                                   ))
                        ))
             ),
             
             #Data Insights tab
             tabPanel("Data Insights", icon = icon("fas fa-chart-line"),
                      fluidRow(class= 'text-center',
                               box(width = 12,
                                   solidHeader = TRUE,
                                   title = strong(htmlOutput("datainsights_text")),
                                   p(h4(align="left",htmlOutput("datainsightsdesc")), br()),
                                   fluidRow(
                                     box(width = 2,
                                         solidHeader = TRUE,
                                         title = strong(htmlOutput("insightshead")),
                                         selectInput("insights_zone",
                                                     label = em(htmlOutput("insightsdesc")),
                                                     choices = c("Bethesda Elementary", "Burton Elementary","C.C. Spaulding Elementary","Club Boulevard Elementary","Creekside Elementary",
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
                                                     multiple = FALSE)),
                                     box(width = 10,
                                         solidHeader = TRUE,
                                         plotlyOutput("insights_individualplots",
                                                      width="auto",
                                                      height = "auto"))
                                   ))
                      )
             ),
             
             #AP & CTE Tab
             tabPanel("Courses", icon = icon("book"),
                      fluidRow(
                        tabBox(
                          id = "tabset2", width = "auto", 
                          tabPanel(i18n$t("Advanced Placement Courses"), class = "text-center",
                                   div(tableOutput("APTable"), style = "font-size:150%"),
                                   selectInput("ap_school", em(htmlOutput("choose_ap")), 
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
                                         title = strong(htmlOutput("ap_eng")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APEnglish", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("ap_math")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APMath", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("ap_sci")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APScience", align="left"))),
                                   fluidRow(
                                     box(width = 4,
                                         title = strong(htmlOutput("ap_socsci")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APSocial", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("ap_lang")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APWLang", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("ap_arts")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APMusArts", align="left"))),
                                   fluidRow(
                                     box(width = 4,
                                         title = strong(htmlOutput("ap_engineering")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("APEngine", align="left")))
                          ),
                          tabPanel(htmlOutput("cte"), class = "text-center",
                                   div(tableOutput("CTETable"), style = "font-size:150%"),
                                   selectInput("cte_school", em(htmlOutput("choose_cte")), 
                                               choices = list("Hillside High",
                                                              "Jordan High",
                                                              "Riverside High")
                                   ),
                                   fluidRow(
                                     box(width = 4,
                                         title = strong(htmlOutput("cte_ag")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("AgCTE", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("cte_bus")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("BusCTE", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("cte_cs")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("CompCTE", align="left"))),
                                   fluidRow(
                                     box(width = 4,
                                         title = strong(htmlOutput("cte_fam")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("FamCTE", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("cte_health")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("HealthCTE", align="left")),
                                     box(width = 4,
                                         title = strong(htmlOutput("cte_trade")), status = "primary", solidHeader = TRUE,
                                         htmlOutput("TradeCTE", align="left"))))
                        )
                      )
             ),
             
             #Sports Tab - static
             tabPanel("Sports", icon = icon("basketball-ball"),
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
                                htmlOutput("sports_context", , style = "solid-header:TRUE"))
                          )
                        )
                      )
             ),
             
             #Arts Tab
             tabPanel("Arts", icon = icon("paint-brush"),
                      fluidRow(
                        box(width = 12,
                            background = "navy",
                            htmlOutput("arts_header"),
                            column(12, align="center", tableOutput("available_arts")))
                      ),
                      fluidRow(
                        box(width = 6, 
                            #title = strong(htmlOutput("arts_dps_header"), status = "primary", solidHeader = TRUE),
                            htmlOutput("arts_dps_header"),
                            htmlOutput("arts_dps"),
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
                        
                        box(width = 6, 
                            htmlOutput("arts_durham")
                        )
                      ) 
             ),
             
             ##NCCU/DUKE engagement tab
             tabPanel("Duke/NCCU Engagement", icon = icon("fas fa-handshake-angle"),
                      tabBox(
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", width = "auto",
                        #Table Outputs
                        tabPanel(htmlOutput("engage_service"), 
                                 selectInput("tab1", em(i18n$t("Choose a Division to View Community Engagement Oppurtunities")),
                                             choices = list("Staff/Faculty",
                                                            "Undergraduate Students"),
                                             multiple = FALSE),
                                 br(),
                                 dataTableOutput("engagetable_1")),
                        tabPanel(htmlOutput("engage_teach")),
                        tabPanel(htmlOutput("engage_research"),
                                 selectInput("tab3", em(i18n$t("Choose a Division to View Community Engagement Oppurtunities")),
                                             choices = list("Staff/Faculty",
                                                            "Undergraduate Students"),
                                             multiple = FALSE),
                                 br(),
                                 dataTableOutput("engagetable_3"))
                      ),
                      
                      fluidRow(
                        box(width = 12,
                            htmlOutput("engage_context")))
                      
                      
                      #fluidRow(
                      #box(width = 12,
                      #background = "light-blue",
                      #solidHeader = TRUE,
                      #slickROutput("carou", width = "auto", height = "auto"),
                      #use_gotop(color = "black")),
                      #),
             ),
             
             
             #feedback
             tabPanel("Feedback", icon = icon("fas fa-comment"),
                      fluidRow(
                        class = "text-center",
                        size = 20,
                        box(width = 12,
                            background = "light-blue",
                            solidHeader = TRUE,
                            title = htmlOutput("feedback1", style = "color:black;font-size:30px;text-decoration:underline"),
                            htmlOutput("feedback2", style = "color:black;font-size:18px"))),
                      fluidRow(
                        class = "text-center",
                        size = 20,
                        box(width = 12,
                            #background = "white",
                            solidHeader = TRUE,
                            title = a(i18n$t("Learn More!"), style = "color:black;font-size:30px"),
                            htmlOutput("links"),
                            img(src = "project_logo.png", align="center"))),
             ),
             
             #Meet the team tab
             tabPanel("Meet the Team", icon = icon("fas fa-users"),
                      fluidRow(
                        box(width = 12,
                            background = "navy",
                            class = "text-center",
                            h3(strong(htmlOutput("team23"))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(align = "center", src = "ethan.jpg")),
                              column(width =3,
                                     p(htmlOutput("ethan"))),
                              column(width = 3,
                                     img(src = "sreya.jpeg")),
                              column(width = 3,
                                     p(htmlOutput("sreya")))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "unzila.jpg")),
                              column(width = 3,
                                     p(htmlOutput("unzila"))),
                              column(width = 3,
                                     img(src = "lauren.jpeg")),
                              column(width = 3,
                                     p(htmlOutput("lauren")))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "pragya.png")),
                              column(width = 3,
                                     p(htmlOutput("pragya")))))),
                      
                      fluidRow(
                        box(width = 12,
                            background = "light-blue",
                            class = "text-center",
                            h3(strong(htmlOutput("team22"))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(align = "center", src = "emily250.jpg")),
                              column(width =3,
                                     p(htmlOutput("emily"))),
                              column(width = 3,
                                     img(src = "aryan250.jpg")),
                              column(width = 3,
                                     p(htmlOutput("aryan")))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "patience3.jpg")),
                              column(width = 3,
                                     p(htmlOutput("patience1"))),
                              column(width = 3,
                                     img(src = "mel250.jpg")),
                              column(width = 3,
                                     p(htmlOutput("melanie")))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "surabhi.jpg")),
                              column(width = 3,
                                     p(htmlOutput("surabhi")))))),
                      
                      fluidRow(
                        box(width = 12,
                            background = "navy",
                            class = "text-center",
                            h3(strong(htmlOutput("team21"))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "ally3.jpg")),
                              column(width =3,
                                     p(htmlOutput("allyson"))),
                              column(width = 3,
                                     img(src = "drewheadshot3 copy.jpg")),
                              column(width = 3,
                                     p(htmlOutput("drew")))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "patience3.jpg")),
                              column(width = 3,
                                     p(htmlOutput("patience2"))),
                              column(width = 3,
                                     img(src = "rhea3.jpg")),
                              column(width = 3,
                                     p(htmlOutput("rhea")))),
                            br(),
                            fluidRow(
                              column(width = 3,
                                     img(src = "nico3.jpg")),
                              column(width = 3,
                                     p(htmlOutput("nico"))))))
             ),
             collapsible = FALSE,
             position = ("fixed-top")
  ),
  
  
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
  
)
}

dashboardPage(
  skin = "black",
  dashboardHeader(
    title = i18n$t("Visualizing DPS")),
  dashboardSidebar(disable = TRUE), 
  body
)

