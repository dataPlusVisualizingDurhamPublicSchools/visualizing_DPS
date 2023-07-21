#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(tidyr)
library(readxl)
library(gotop)
library(shiny.i18n)
library(DT)



cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# Load/Rename School Stats Data
{
  Race_SCHOOL_ONLY <- read_excel("./data/2021/school_stats_data/Race SCHOOL ONLY.xlsx")
  race <- read_excel("./data/2021/school_stats_data/race.xlsx")
  ES_racecomp_21 <- read_excel("./data/2021/school_stats_data/ES_racecomp_21.xlsx")
  HS_racecomp_21 <- read_excel("./data/2021/school_stats_data/HS_racecomp_21.xlsx")
  race22 <- read_excel("./data/2022/school_stats_data/race2022.xlsx")
  ES_racecomp_22 <- read_excel("./data/2022/school_stats_data/ES_racecomp_22.xlsx")
  MS_racecomp_22 <- read_excel("./data/2022/school_stats_data/MS_racecomp_22.xlsx")
  HS_racecomp_22 <- read_excel("./data/2022/school_stats_data/HS_racecomp_22.xlsx")
  race_diff <- read_excel("./data/2021/school_stats_data/race diff.xlsx")
  poc_per_school <- read_excel("./data/2021/school_stats_data/poc per school.xlsx")
  ES_poc_per_school_21 <- read_excel("./data/2021/school_stats_data/ES_poc_per_school_21.xlsx")
  HS_poc_per_school_21 <- read_excel("./data/2021/school_stats_data/HS_poc_per_school_21.xlsx")
  poc_per_school22 <- read_excel("./data/2022/school_stats_data/poc per school22.xlsx")
  ES_poc_per_school_22 <- read_excel("./data/2022/school_stats_data/ES_poc_per_school_22.xlsx")
  MS_poc_per_school_22 <- read_excel("./data/2022/school_stats_data/MS_poc_per_school_22.xlsx")
  HS_poc_per_school_22 <- read_excel("./data/2022/school_stats_data/HS_poc_per_school_22.xlsx")
  funding <- read_excel("./data/2021/school_stats_data/funding.xlsx")
  all_race <- read_excel("./data/2021/school_stats_data/all race 1.xlsx")
  ES_all_race <- read_excel("./data/2021/school_stats_data/ES_all race_2021.xlsx")
  HS_all_race <- read_excel("./data/2021/school_stats_data/HS_all race_2021.xlsx")
  all_race22 <- read_excel("./data/2022/school_stats_data/all race 2022.xlsx")
  ES_all_race22 <- read_excel("./data/2022/school_stats_data/ES_all race 2022.xlsx")
  HS_all_race22 <- read_excel("./data/2022/school_stats_data/HS_all race 2022.xlsx")
  MS_all_race22 <- read_excel("./data/2022/school_stats_data/MS_all race 2022.xlsx")
  
  
  schoolstats <- read.csv("./data/2021/school_stats_data/Data + School Info - School Statistics.csv")
  schoolstats22 <- read.csv("./data/2022/school_stats_data/School Statistics 2022.csv")
  ES_stats_21 <- read.csv("./data/2021/school_stats_data/ES_stats_21.csv")
  MS_stats_21 <- read.csv("./data/2021/school_stats_data/MS_stats_21.csv")
  HS_stats_21 <- read.csv("./data/2021/school_stats_data/HS_stats_21.csv")
  ES_stats_22 <- read.csv("./data/2022/school_stats_data/ES_stats_22.csv")
  MS_stats_22 <- read.csv("./data/2022/school_stats_data/MS_stats_22.csv")
  HS_stats_22 <- read.csv("./data/2022/school_stats_data/HS_stats_22.csv")
  
  APCourses <- read_excel("./data/2022/AP Courses.xlsx")
  
  #data for 2023 summer 
  ES_stats_23 <- read.csv("./data/2023/school_stats_data/ES_stats_23.csv")
  ES_racecomp_23 <- read_excel("./data/2023/school_stats_data/ES_racecomp_23.xlsx")
  ES_poc_per_school_23 <- read_excel("./data/2023/school_stats_data/ES_poc_per_school_23.xlsx")
  ES_all_race23 <- read_excel("./data/2023/school_stats_data/ES_all race 2023.xlsx")
  HS_stats_23 <- read.csv("./data/2023/school_stats_data/HS_stats_23.csv")
  HS_racecomp_23 <- read_excel("./data/2023/school_stats_data/HS_racecomp_23.xlsx")
  HS_poc_per_school_23 <- read_excel("./data/2023/school_stats_data/HS_poc_per_school_23.xlsx")
  HS_all_race23 <- read_excel("./data/2023/school_stats_data/HS_all race 2023.xlsx")
  
  MS_stats_23 <- read.csv("./data/2023/school_stats_data/MS_stats_23.csv")
  MS_racecomp_23 <- read_excel("./data/2023/school_stats_data/MS_racecomp_23.xlsx")
  MS_poc_per_school_23 <- read_excel("./data/2023/school_stats_data/MS_poc_per_school_23.xlsx")
  MS_all_race23 <- read_excel("./data/2023/school_stats_data/MS_all race 2023.xlsx")
  
  
  HS_stats_23 <- read.csv("./data/2023/school_stats_data/HS_stats_23.csv")
  HS_racecomp_23 <- read_excel("./data/2023/school_stats_data/HS_racecomp_23.xlsx")
  HS_poc_per_school_23 <- read_excel("./data/2023/school_stats_data/HS_poc_per_school_23.xlsx")
  HS_all_race23 <- read_excel("./data/2023/school_stats_data/HS_all race 2023.xlsx")
  
  schoolstats23 <- read.csv("./data/2023/school_stats_data/all_school_stats.csv")
  
  
  #data for the data insights tab
  counts_2021 <- read.csv("./data/2021/spatial_data/counts.csv", skip = 1)
  counts_grouped_2021 <- read.csv("./data/2021/spatial_data/counts grouped.csv")
  pop<- read.csv("./data/2021/school_stats_data/population.csv")
  
  CTECourses <- read_excel("./data/2022/CTE Courses.xlsx")
  sports_22 <- read.csv("./data/2022/school_stats_data/sports.csv")
  
  #data for engagement tab
  faculty_service <- read.csv("./data/2023/Faculty_Resources.csv")
  student_service <- read.csv("./data/2023/us_service.csv")
  faculty_research <- read.csv("./data/2023/sf_research.csv")
  student_research <- read.csv("./data/2023/us_research.csv")
  
  
}

# Load/Rename Map Data
{
  durham <- geojsonio::geojson_read("./data/2021/map_data/All.geojson", what = "sp")
  cc <- geojsonio::geojson_read("./data/2021/map_data/C.C. Spaulding Elementary.geojson", what = "sp")
  eastway <- geojsonio::geojson_read("./data/2021/map_data/Eastway Elementary.geojson", what = "sp")
  ek <- geojsonio::geojson_read("./data/2021/map_data/E.K. Powe Elementary.geojson", what = "sp")
  fayetteville <- geojsonio::geojson_read("./data/2021/map_data/Fayetteville Street Elementary.geojson", what = "sp")
  forest <- geojsonio::geojson_read("./data/2021/map_data/Forest View Elementary.geojson", what = "sp")
  hillside <- geojsonio::geojson_read("./data/2021/map_data/Hillside High.geojson", what = "sp")
  jordan <- geojsonio::geojson_read("./data/2021/map_data/Jordan High.geojson", what = "sp")
  lakewoodES <- geojsonio::geojson_read("./data/2021/map_data/Lakewood Elementary.geojson", what = "sp")
  parkwood <- geojsonio::geojson_read("./data/2021/map_data/Parkwood Elementary.geojson", what = "sp")
  southwest <- geojsonio::geojson_read("./data/2021/map_data/Southwest Elementary.geojson", what = "sp")
  clubblv <- geojsonio::geojson_read("./data/2021/map_data/Club Boulevard Elementary.geojson", what = "sp")
  lakewoodMS <- geojsonio::geojson_read("./data/2021/map_data/Lakewood Middle.geojson", what = "sp")
  hillandale <- geojsonio::geojson_read("./data/2021/map_data/Hillandale Elementary.geojson", what = "sp")
  brogden <- geojsonio::geojson_read("./data/2021/map_data/Brogden Middle.geojson", what = "sp")
  lowesgr <- geojsonio::geojson_read("./data/2021/map_data/Lowes Grove Middle.geojson", what = "sp")
  riverside <- geojsonio::geojson_read("./data/2021/map_data/Riverside High.geojson", what = "sp")
  #new 2023 data
  carrington <- geojsonio::geojson_read("./data/2023/map_data/Carrington Middle.geojson", what = "sp")
  cityofmedicine <- geojsonio::geojson_read("./data/2023/map_data/City of Medicine Academy.geojson", what = "sp")
  durhamschoolofarts <- geojsonio::geojson_read("./data/2023/map_data/Durham School of the Arts.geojson", what = "sp")
  holtoncareer <- geojsonio::geojson_read("./data/2023/map_data/Holton Career.geojson", what = "sp")
  jdclement <- geojsonio::geojson_read("./data/2023/map_data/J.D. Clement Early College.geojson", what = "sp")
  lakeview <- geojsonio::geojson_read("./data/2023/map_data/Lakeview High.geojson", what = "sp")
  lucas <- geojsonio::geojson_read("./data/2023/map_data/Lucas Middle.geojson", what = "sp")
  middlecollege <- geojsonio::geojson_read("./data/2023/map_data/Middle College.geojson", what = "sp")
  morehead <- geojsonio::geojson_read("./data/2023/map_data/Morehead Montessori School.geojson", what = "sp")
  neal <- geojsonio::geojson_read("./data/2023/map_data/Neal Middle.geojson", what = "sp")
  northern <- geojsonio::geojson_read("./data/2023/map_data/Northern High.geojson", what = "sp")
  pearsontown <- geojsonio::geojson_read("./data/2023/map_data/Pearsontown Elementary.geojson", what = "sp")
  rnharris <- geojsonio::geojson_read("./data/2023/map_data/R.N. Harris Elementary.geojson", what = "sp")
  rogersherr <- geojsonio::geojson_read("./data/2023/map_data/Rogers Herr Middle.geojson", what = "sp")
  sandyridge <- geojsonio::geojson_read("./data/2023/map_data/Sandy Ridge Elementary.geojson", what = "sp")
  creativestudies <- geojsonio::geojson_read("./data/2023/map_data/School for Creative Studies.geojson", what = "sp")
  shepardmiddle <- geojsonio::geojson_read("./data/2023/map_data/Shepard Middle.geojson", what = "sp")
  sherwoodgithens <- geojsonio::geojson_read("./data/2023/map_data/Sherwood Githens Middle.geojson", what = "sp")
  southernenergy <- geojsonio::geojson_read("./data/2023/map_data/Southern School of Energy and Sustainability.geojson", what = "sp")
  springvalley <- geojsonio::geojson_read("./data/2023/map_data/Spring Valley Elementary.geojson", what = "sp")
  wgpearson <- geojsonio::geojson_read("./data/2023/map_data/W.G. Pearson Elementary.geojson", what = "sp")
  yesmith <- geojsonio::geojson_read("./data/2023/map_data/Y.E. Smith Elementary.geojson", what = "sp")
  bethesda <- geojsonio::geojson_read("./data/2023/map_data/Bethesda Elementary.geojson", what = "sp")
  burton <- geojsonio::geojson_read("./data/2023/map_data/Burton Elementary.geojson", what = "sp")
  creekside <- geojsonio::geojson_read("./data/2023/map_data/Creekside Elementary.geojson", what = "sp")
  easley <- geojsonio::geojson_read("./data/2023/map_data/Easley Elementary.geojson", what = "sp")
  enovalley <- geojsonio::geojson_read("./data/2023/map_data/Eno Valley Elementary.geojson", what = "sp")
  georgewatts <- geojsonio::geojson_read("./data/2023/map_data/George Watts Elementary.geojson", what = "sp")
  glenn <- geojsonio::geojson_read("./data/2023/map_data/Glenn Elementary.geojson", what = "sp")
  holt <- geojsonio::geojson_read("./data/2023/map_data/Holt Elementary.geojson", what = "sp")
  hopevalley <- geojsonio::geojson_read("./data/2023/map_data/Hope Valley Elementary.geojson", what = "sp")
  mangum <- geojsonio::geojson_read("./data/2023/map_data/Mangum Elementary.geojson", what = "sp")
  merrickmoore <- geojsonio::geojson_read("./data/2023/map_data/Merrick-Moore Elementary.geojson", what = "sp")
  oakgrove <- geojsonio::geojson_read("./data/2023/map_data/Oak Grove Elementary.geojson", what = "sp")
  
  
  
}

# Load/Rename Spatial Data
{
  bus <- read.csv("./data/2021/spatial_data/renamed_Bus Stops.csv")
  childcare <- read_csv("./data/2021/spatial_data/renamed_Childcare Centers.csv")
  cultural <- read.csv("./data/2021/spatial_data/renamed_Community & Cultural Centers.csv")
  gardens <- read.csv("./data/2021/spatial_data/renamed_Community Gardens.csv")
  grocery <- read.csv("./data/2021/spatial_data/renamed_Grocery Stores.csv") 
  #grocery <- read.csv("./data/2021/spatial_data/renamed_Grocery Stores.csv") 
  libraries <- read.csv("./data/2021/spatial_data/renamed_Libraries.csv")
  parks <- read.csv("./data/2021/spatial_data/renamed_Parks.csv")
  rec <- read.csv("./data/2021/spatial_data/renamed_Recreation Centers.csv")
  religious <- read.csv("./data/2021/spatial_data/renamed_Religious Centers.csv")
  schools <- read.csv("./data/2021/schools.csv")
  hospitals <- read.csv("./data/2021/spatial_data/renamed_Hospitals and Clinics.csv")
  pantries <- read.csv("./data/2021/spatial_data/renamed_Food Pantries.csv")
  afterschool <- read.csv("./data/2021/spatial_data/renamed_After-School Care Programs.csv")
  farmersmark <- read.csv("./data/2021/spatial_data/renamed_Farmer's Markets.csv") 
  commarts <- read.csv("./data/2021/spatial_data/renamed_Community Arts.csv")
  sports <- read.csv("./data/2021/spatial_data/renamed_Community Sports.csv")
  pharmacies <- read.csv("./data/2023/spatial_data/pharmacies.csv")
  shelters <- read.csv("./data/2023/spatial_data/homeless_shelter.csv")
  restaurants <- read.csv("./data/2023/spatial_data/restaurants.csv")
  
}

# Load/Rename Schools' Names
schoolstats$name <- c("Bethesda Elementary", "Burton Elementary","C.C. Spaulding Elementary","Club Boulevard Elementary","Creekside Elementary",
                      "Eastway Elementary", "Easley Elementary", "Eno Valley Elementary", "E.K. Powe Elementary", "Fayetteville Street Elementary", 
                      "Forest View Elementary", "George Watts Elementary", "Glenn Elementary",  "Holt Elementary","Hope Valley Elementary",
                      "Hillandale Elementary","Lakewood Elementary", "Mangum Elementary","Merrick-Moore Elementary","Oak Grove Elementary","Pearsontown Elementary","Parkwood Elementary","R.N. Harris Elementary","Southwest Elementary",
                      "Sandy Ridge Elementary","Spring Valley Elementary","W.G. Pearson Elementary", "Y.E. Smith Elementary","Brogden Middle", 
                      "Carrington Middle","Lucas Middle","Lakewood Montessori Middle", "Lowes Grove Middle", "Neal Middle","Rogers Herr Middle", 
                      "Shepard Middle", "Sherwood Githens Middle","City of Medicine Academy", "Durham School of the Arts","Durham School of Technology","J.D. Clement Early College",
                      "Hillside High","Holton Career","Jordan High","Lakewiew High","Middle College", "Morehead Montessori School",
                      "Northern High","Riverside High","Southern High", "School for Creative Studies")

# Load/Rename Icons
{
  iconSet <- iconList(
    parks = makeIcon("https://img.icons8.com/windows/32/000000/tree.png", iconWidth=20, iconHeight=20),
    rec = makeIcon("https://img.icons8.com/external-glyph-mangsaabguru-/64/000000/external-recreational-kid-hobby-glyph-glyph-mangsaabguru-.png", iconWidth=20, iconHeight=20),
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
    farmersmark = makeIcon("https://img.icons8.com/ios-filled/50/undefined/carrot.png",iconWidth = 20, iconHeight = 20),
    commarts = makeIcon("https://img.icons8.com/ios-filled/50/000000/theatre-mask.png",iconWidth = 20, iconHeight = 20),
    pharmacies = makeIcon("https://img.icons8.com/ios-filled/50/000000/pharmacy.png", iconWidth=20, iconHeight=20),
    shelters = makeIcon("https://img.icons8.com/ios-filled/50/000000/roofing.png", iconWidth=20, iconHeight=20), 
    sports = makeIcon("https://img.icons8.com/android/24/000000/basketball.png",iconWidth = 20, iconHeight = 20),
    restaurants = makeIcon("https://img.icons8.com/ios-filled/50/000000/restaurant.png", iconWidth=20, iconHeight=20)
  )
}



translator <- Translator$new(translation_json_path = "./data/Translations/fullTranslation.json")

function(input, output, session) {
  
  #observeEvent(input$selected_language, {
  #Here is where we update language in session
  #shiny.i18n::update_lang(session, input$selected_language)
  #})
  
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  
  #Home Page
  #headers
  observeEvent(i18n(),
               output$header1<- renderText({
                 paste(i18n()$t("Visualizing Durham Public Schools"))
               }))
  
  observeEvent(i18n(),
               output$change<- renderText({
                 paste(i18n()$t("Change Language"))
               }))
  
  observeEvent(i18n(),
               output$header2<- renderText({
                 paste(i18n()$t("Geospatial Variables"))
               }))
  
  observeEvent(i18n(),
               output$header3<- renderText({
                 paste(i18n()$t("School-Specific Variables"))
               }))
  
  observeEvent(i18n(),
               output$header4<- renderText({
                 paste(i18n()$t("Centralized Web Application"))
               }))
  
  observeEvent(i18n(),
               output$header5<- renderText({
                 paste(i18n()$t("2 Universities, 50 Public Schools"))
               }))
  
  observeEvent(i18n(),
               output$header6<- renderText({
                 paste(i18n()$t("View Our 50 Schools"))
               }))
  
  observeEvent(i18n(),
               output$header7<- renderText({
                 paste(i18n()$t("View Geospatial Data"))
               }))
  
  observeEvent(i18n(),
               output$header8<- renderText({
                 paste(i18n()$t("View School Statistics"))
               }))
  
  observeEvent(i18n(),
               output$header9<- renderText({
                 paste(i18n()$t("Our Partners"))
               }))
  
  observeEvent(i18n(),
               output$header9<- renderText({
                 paste(i18n()$t("Our Partners"))
               }))
  
  
  #body
  observeEvent(i18n(),
               output$home_text1 <- renderText({
                 paste(i18n()$t("The Durham Public School District has 50 public schools: 27 elementary, 11 middle, 12 high."),
                       br(),
                       i18n()$t("Using spatial and school-specific data, along with contextual resources, we hope to provide a holistic view of Durham Public Schools and their communities while highlighting their resources and assets."),
                       i18n()$t("Visit the"), 
                       tags$a(href = "https://sites.duke.edu/uacs/", target="_blank", i18n()$t("Durham UACS Research Collective website")),
                       #a(i18n()$t("Durham UACS Research Collective website", href = "https://sites.duke.edu/uacs/")),
                       i18n()$t("for more information!"))
               }))
  
  observeEvent(i18n(),
               output$home_text2 <- renderText({
                 paste(i18n()$t("The inspiration for this project is rooted in the inter-institutional Bass Connections team from Duke University and North Carolina Central University, which is committed to developing more responsible and imaginative ways of partnering with Durham Public Schools. This project aims to provide a centralized web application that will serve as a tool for those entering Durham Public Schools. In addition, our application aims to inform future pre-service training for university students, support local neighborhood schools in visualizing their communities, and help various university offices articulate what “community” actually looks like."),
                       br(),
                       i18n()$t("Duke and NCCU both aim to foster equitable partnerships with Durham Public Schools. Prior Bass Connections research focused on understanding how to achieve this goal and found that one of the main barriers to meaningful engagement between Duke, NCCU, and Durham public schools is that “many university students lack an understanding of city and community dynamics.” Additionally, they found that there is a “lack of student volunteer training with Durham’s context, particularly in the areas of history, school-specific demographics, and implicit bias and power dynamics that may manifest in schools.”"),
                       br(),
                       strong(i18n()$t("Motivated by this research, our project explores a way of visualizing schools as centers of the community that bring academics, health and social services, youth and community development, and community engagement together under one roof.")))
               }))
  
  #Meet the Team
  #Headers
  observeEvent(i18n(),
               output$team23<- renderText({
                 paste(i18n()$t("Meet Our Team - Data+ 2023"))
               }))
  observeEvent(i18n(),
               output$team22 <- renderText({
                 paste(i18n()$t("Meet Our Team - Data+ 2022"))
               }))
  observeEvent(i18n(),
               output$team21 <- renderText({
                 paste(i18n()$t("Meet Our Team - Data+ 2021"))
               }))
  #Ethan
  observeEvent(i18n(),
               output$ethan <- renderText({
                 paste(i18n()$t("Ethan Shang is a rising sophomore at Duke University from Chapel Hill, North Carolina. Currently, he is studying Data Science, with a minor in Economics. He is especially interested in using big data analyses to reveal politically and socially relevant insights to key issues in policy and social justice. He joined the Data+ team to provide a clearer picture of Durham Public Schools, and to support a vibrant community that has surrounded him from an early age."))
               }))
  #Sreya
  observeEvent(i18n(),
               output$sreya <- renderText({
                 paste(i18n()$t("Sreya Gnanavel is a sophomore at Duke University originally from Cary, North Carolina. She intends to major in Data Science and minor in Finance. Sreya is passionate about using data science to help uncover meaningful insights and use to make informed decisions. She hopes this project will bring more awareness to the available resources in the Durham Public System and help expand the reach to other community schools. She was motivated to join this project as it aligns with her goal of applying her data science skills to create positive change particularly by promoting equal access to resources and opportunities for all students."))
               }))
  #Unzila
  observeEvent(i18n(),
               output$unzila <- renderText({
                 paste(i18n()$t("Unzila Sakina Babar, a sophomore at Duke University, is a passionate computer and data scientist hailing from Lahore, Pakistan. Alongside her academic pursuits, she possesses a keen interest in the education system. Unzila's deep-rooted desire to revolutionize the schooling system in her home country fuels her involvement in this project. She envisions fostering a robust sense of community within Pakistan School Systems while raising awareness about the abundant resources and communities available in the Public Schools of Durham County.  She seeks to synergize her own expertise with the capabilities of this dashboard to create a lasting impact on Durham Public Schools and extend this influence beyond its boundaries."))
               }))
  #Pragya
  observeEvent(i18n(),
               output$pragya <- renderText({
                 paste(i18n()$t("Pragya Raghuvanshi is a master’s student in Interdisciplinary Data Science at Duke University, originally from India. She is passionate about driving insights for the public sector and education system by applying data science techniques to solve real world problems.  Back in India, besides working as a data analyst, she also worked as a volunteer in an organization to educate and empower the local village communities. This summer she is working at a solar finance tech firm as a Data Science intern to assist in the growth of solar power in Nigeria by harnessing the power of data."))
               }))
  #Lauren
  observeEvent(i18n(),
               output$lauren <- renderText({
                 paste(i18n()$t("Lauren Walker is a Junior at Duke University from Needham, Massachusetts. She is studying Computer Science with a minor in Statistics. Lauren joined this Data+ project team because she is eager to strengthen the relationship and partnership between Durham Public Schools and local universities. She also enjoys coding and is looking forward to improving her data science skills. She hopes that this dashboard will serve as a valuable resource for those looking to learn more about the resources offered throughout the Durham Public School system and make education more accessible and equitable."))
               }))
  #Emily
  observeEvent(i18n(),
               output$emily <- renderText({
                 paste(i18n()$t("Emily McReynolds is a sophomore at Duke University originally from Greensboro, North Carolina. She intends to major in Public Policy and obtain a certificate in Markets and Management Studies. Emily is passionate about policy reform, specifically in our education and healthcare systems. She hopes this project will bring more awareness to the available resources and community centers in Durham County that can support our public schools. Emily is eager to see how this partnership can build a stronger sense of unity for all."))
               }))
  #Aryan
  observeEvent(i18n(),
               output$aryan <- renderText({
                 paste(i18n()$t("Aryan Poonacha is a rising senior at Duke University from Bangalore, India. Currently, he is studying Data Science with a minor in Political Science. He is especially interested in using big data analyses to reveal politically and socially relevant insights to key issues in policy and social justice. He joined the Data+ team to provide a clearer picture of Durham Public Schools and find better paths to their improvement."))
               }))
  #Patience
  observeEvent(i18n(),
               output$patience1 <- renderText({
                 paste(i18n()$t("Patience Jones is a senior at North Carolina Central University from Durham, North Carolina. Currently, she is studying English, Secondary Education, and General Psychology. Patience continued the Data+ project team in hopes of improving the dashboard to make it more accessible. With her background in education policy, she hopes to integrate her own knowledge with the dashboard to make an impact on Durham Public Schools and beyond."))
               }))
  #Melanie
  observeEvent(i18n(),
               output$melanie <- renderText({
                 paste(i18n()$t("Melanie Kaye Moseley is a senior at North Carolina Central University from Oxford, North Carolina. She is studying Music with a concentration in Instrumental Performance. Melanie joined the Data+ project team to contribute to the existing research and efforts that have equipped the Bass Connections team with organized information  to pinpoint the resources that would be most beneficial for schools. Melanie hopes this dashboard will help increase equity within schools and promote a greater sense of community throughout Durham."))
               }))
  #Surabhi
  observeEvent(i18n(),
               output$surabhi <- renderText({
                 paste(i18n()$t("Surabhi Trivedi is a masters student in Interdisciplinary Data Science at Duke University. Her interest lies at the intersection of data science and public policy, specifically social policy. For the summer, she is interning at the World Bank and the Urban Institute as a data scientist while volunteering to help with this project."))
               }))
  #Allyson
  observeEvent(i18n(),
               output$allyson <- renderText({
                 paste(i18n()$t("Allyson Ashekun is a junior at Duke University from Clearwater, Florida. Currently, she is studying Public Policy and Computer Science, and her academic interests focus primarily on the intersection of those two disciplines in areas such as Data Science. Allyson joined the Data+ project team because she is passionate about equitable education and enjoys coding. She hopes this dashboard will be a helpful tool for those entering Durham Public Schools, and will help improve the quality of partnerships."))
               }))
  #Drew
  observeEvent(i18n(),
               output$drew <- renderText({
                 paste(i18n()$t("Drew Greene is a sophomore at Duke University from Richmond, Virginia. He intends to study Public Policy with minors in Education and Inequality Studies. His academic interests include educational equity and the school-to-prison pipeline. Drew joined this project because of the opportunity to develop his data science skills. He hopes this project will help Durham residents to locate the myriad resources accessible to help form an even stronger community built around public schools."))
               }))
  #Patience
  observeEvent(i18n(),
               output$patience2 <- renderText({
                 paste(i18n()$t("Patience Jones is a senior at North Carolina Central University from Durham, North Carolina. Currently, she is studying English, Secondary Education, and General Psychology. Patience joined the Data+ project team because she was interested in learning more about data science and its integration into education policy. She hopes this dashboard makes an impact on students in all Durham Public Schools and beyond, in hopes of making education more accessible to all students."))
               }))
  #Rhea
  observeEvent(i18n(),
               output$rhea <- renderText({
                 paste(i18n()$t("Rhea Tejwani is a junior at Duke University from Demarest, New Jersey. She is studying Computer Science and Economics. Rhea decided to join this project team because she has a passion for data science and hopes that this app will help local universities maintain a productive relationship with the community. She is proud of the work the team has accomplished, and is excited to see the long term impacts it has!"))
               }))
  #Nico
  observeEvent(i18n(),
               output$nico <- renderText({
                 paste(i18n()$t("Nico Restrepo Ochoa is a PhD candidate at Duke's sociology department. He's interested in how habits and beliefs change, both at the individual and collective level, and uses longitudinal data, networks, and simulations to try to get at this question. He had the privilege to be the project manager for this team, and believes the team was efficient and industrious so his job was easy. The team claims he was helpful, and he likes to believe that is true."))
               }))
  
  
  # SchoolStats - GGPlots
  {
    output$es_barplots <- renderPlotly({
      if(input$es_year == "Summer 2021"){
        if(input$es_select == "Average Class Size") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Bachelor Degree Rate") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$BACHELOR_DEG_RATE),], aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "BIPOC Students per School") {
          p <- ggplot(ES_poc_per_school_21, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Enrollment") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats_summary[!is.na(schoolstats_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Experienced Teacher Ratio"){
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$EXP_TEACHER_RATIO),], aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 79%", yintercept = 79), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme_minimal() +
            labs(title = i18n()$t("Experienced Teacher Ratio"), x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Free and Reduced Lunch"){
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Funding Per Pupil") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $11,672", yintercept = 11672), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "In-School Suspensions (ISS)") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 188.92", yintercept = 93.69), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Median Age") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_AGE),], aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.2", yintercept = 35.2), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Med Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Median Homesale Price") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_HOMESALE_PRICE),], aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $278,000", yintercept = 278000), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Median Household Income") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_HOUSEHOLD_INC),], aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $58,190", yintercept = 58190), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Racial Demographics") {
          
          p <- ggplot(ES_all_race, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
          
        }
        else if(input$es_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(ES_racecomp_21, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$es_select == "Sidewalk Coverage") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$SIDEWALK_COVG),], aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Students Per Device") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .8", yintercept = .8), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Student-Teacher Ratio, Elementary School") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 20.5", yintercept = 20.5), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Elementary School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Students With Disabilities") {
          schoolstats_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.3%", yintercept = 13.3), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
          
        }
        
        else if(input$es_select == "Titles Per Student") {
          schoolstats21_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats21_summary[!is.na(schoolstats21_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        }
        
        else if(input$es_select == "WiFi Access Points Per Classroom") {
          schoolstats21_summary <- ES_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats21_summary[!is.na(schoolstats21_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        
      }
      
      else if(input$es_year == "Summer 2022"){
        if(input$es_select == "Average Class Size") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Bachelor Degree Rate") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "BIPOC Students per School") {
          p <- ggplot(ES_poc_per_school_22, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 81), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Enrollment") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Experienced Teacher Ratio"){
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 82.4%", yintercept = 82.4), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 1.5)) +
            theme_minimal() +
            labs(title = i18n()$t("Experienced Teacher Ratio"), x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Free and Reduced Lunch"){
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Funding Per Pupil") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $11,672", yintercept = 12945), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "In-School Suspensions (ISS)") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 1.65", yintercept = 1.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Median Age") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.4", yintercept = 35.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Median Homesale Price") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $290,500", yintercept = 290500), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Median Household Income") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $60,958", yintercept = 60958), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Racial Demographics") {
          
          p <- ggplot(ES_all_race22, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
          
        }
        else if(input$es_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(ES_racecomp_22, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$es_select == "Sidewalk Coverage") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Students Per Device") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .79", yintercept = .79), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Student-Teacher Ratio, Elementary School") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Elementary School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Students With Disabilities") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.7%", yintercept = 13.7), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$es_select == "Titles Per Student") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$es_select == "WiFi Access Points Per Classroom") {
          schoolstats22_summary <- ES_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
      }
      else if (input$es_year == "Summer 2023") {
        if(input$es_select == "Average Class Size") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white", width = 1.0) +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Bachelor Degree Rate") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$BACHELOR_DEG_RATE),], aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "BIPOC Students per School") {
          p <- ggplot(ES_poc_per_school_23, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Enrollment") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats_summary[!is.na(schoolstats_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Experienced Teacher Ratio"){
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$EXP_TEACHER_RATIO),], aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 79%", yintercept = 79), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme_minimal() +
            labs(title = i18n()$t("Experienced Teacher Ratio"), x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Free and Reduced Lunch"){
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Funding Per Pupil") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $11,672", yintercept = 11672), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "In-School Suspensions (ISS)") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 188.92", yintercept = 93.69), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Median Age") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_AGE),], aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.2", yintercept = 35.2), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Med Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Median Homesale Price") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_HOMESALE_PRICE),], aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $278,000", yintercept = 278000), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Median Household Income") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_HOUSEHOLD_INC),], aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $58,190", yintercept = 58190), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Racial Demographics") {
          
          p <- ggplot(ES_all_race23, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
          
        }
        else if(input$es_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(ES_racecomp_23, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$es_select == "Sidewalk Coverage") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$SIDEWALK_COVG),], aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$es_select == "Students Per Device") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .8", yintercept = .8), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Student-Teacher Ratio, Elementary School") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 20.5", yintercept = 20.5), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Elementary School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$es_select == "Students With Disabilities") {
          schoolstats_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.3%", yintercept = 13.3), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
          
        }
        
        else if(input$es_select == "Titles Per Student") {
          schoolstats21_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats21_summary[!is.na(schoolstats21_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        }
        
        else if(input$es_select == "WiFi Access Points Per Classroom") {
          schoolstats21_summary <- ES_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats21_summary[!is.na(schoolstats21_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        }
      }
    })
    
    
    output$ms_barplots <- renderPlotly({
      if(input$ms_year == "Summer 2022"){
        if(input$ms_select == "Average Class Size") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Bachelor Degree Rate") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "BIPOC Students per School") {
          p <- ggplot(MS_poc_per_school_22, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "CTE Course Enrollment Rate, Middle School") {
          schoolstats_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 56%", yintercept = 56), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("CTE Course Enrollment Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        }  
        else if(input$ms_select == "Enrollment") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Experienced Teacher Ratio"){
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
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
        else if(input$ms_select == "Free and Reduced Lunch"){
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Funding Per Pupil") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $12,945", yintercept = 12945), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "In-School Suspensions (ISS)") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 1.65", yintercept = 1.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Median Age") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.4", yintercept = 35.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Median Homesale Price") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $290,500", yintercept = 290500), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Median Household Income") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $60,958", yintercept = 60958), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Racial Demographics") {
          
          p <- ggplot(MS_all_race22, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
          
        }
        else if(input$ms_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(MS_racecomp_22, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$ms_select == "Sidewalk Coverage") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Students Per Device") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .79", yintercept = .79), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Student-Teacher Ratio, Middle School") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_MS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_MS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_MS), y=STUDENT_TEACHER_MS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_MS), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Middle School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Students With Disabilities") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.7%", yintercept = 13.7), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$ms_select == "Titles Per Student") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$ms_select == "WiFi Access Points Per Classroom") {
          schoolstats22_summary <- MS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
      } 
      else if(input$ms_year == "Summer 2023"){
        if(input$ms_select == "Average Class Size") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Bachelor Degree Rate") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats23_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "BIPOC Students per School") {
          p <- ggplot(MS_poc_per_school_23, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "CTE Course Enrollment Rate, Middle School") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 56%", yintercept = 56), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("CTE Course Enrollment Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Enrollment") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats23_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Experienced Teacher Ratio"){
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats23_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 82.4%", yintercept = 82.4), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 1.5)) +
            theme_minimal() +
            labs(title = "Experienced Teacher Ratio", x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Free and Reduced Lunch"){
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Funding Per Pupil") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $12,945", yintercept = 12945), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "In-School Suspensions (ISS)") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 1.65", yintercept = 1.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Median Age") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats23_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.4", yintercept = 35.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Median Homesale Price") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats23_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $290,500", yintercept = 290500), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Median Household Income") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats23_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $60,958", yintercept = 60958), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Racial Demographics") {
          
          p <- ggplot(MS_all_race23, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
          
        }
        else if(input$ms_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(MS_racecomp_23, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$ms_select == "Sidewalk Coverage") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats23_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$ms_select == "Students Per Device") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .79", yintercept = .79), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Student-Teacher Ratio, Middle School") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_MS)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$STUDENT_TEACHER_MS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_MS), y=STUDENT_TEACHER_MS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_MS), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Middle School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$ms_select == "Students With Disabilities") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.7%", yintercept = 13.7), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$ms_select == "Titles Per Student") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$ms_select == "WiFi Access Points Per Classroom") {
          schoolstats23_summary <- MS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats23_summary[!is.na(schoolstats23_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
      }
    })
    
    output$hs_barplots <- renderPlotly({
      if(input$hs_year == "Summer 2021"){
        if(input$hs_select == "Advanced Placement (AP) Course Enrollment") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(ADV_COURSES_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ADV_COURSES_PERCENT),], aes(x=reorder(SCHOOL_NAME, -ADV_COURSES_PERCENT), y=ADV_COURSES_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ADV_COURSES_PERCENT), hjust = -.1, color = "black") +
            geom_hline(aes(text="Durham County Average = 9.22%", yintercept = 9.22), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Advanced Placement Course Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Average Class Size") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Bachelor Degree Rate") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$BACHELOR_DEG_RATE),], aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "BIPOC Students per School") {
          p <- ggplot(HS_poc_per_school_21, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "CTE Course Enrollment Rate, High School") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 53%", yintercept = 53), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("CTE Course Enrollment Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Enrollment") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats_summary[!is.na(schoolstats_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Experienced Teacher Ratio"){
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$EXP_TEACHER_RATIO),], aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 79%", yintercept = 79), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme_minimal() +
            labs(title = i18n()$t("Experienced Teacher Ratio"), x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Free and Reduced Lunch"){
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Funding Per Pupil") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $11,672", yintercept = 11672), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Graduation Rate") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(GRADUATION_RATE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$GRADUATION_RATE),], aes(x=reorder(SCHOOL_NAME, -GRADUATION_RATE), y=GRADUATION_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = GRADUATION_RATE), hjust = -.1, color = "black") +
            geom_hline(aes(text="Durham County Average = 83.5%", yintercept = 83.5), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Graduation Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "In-School Suspensions (ISS)") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 188.92", yintercept = 93.69), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Median Age") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_AGE),], aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.2", yintercept = 35.2), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Med Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Median Homesale Price") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_HOMESALE_PRICE),], aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $278,000", yintercept = 278000), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Median Household Income") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$MED_HOUSEHOLD_INC),], aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $58,190", yintercept = 58190), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Racial Demographics") {
          p <- ggplot(HS_all_race, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
        }
        else if(input$hs_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(HS_racecomp_21, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$hs_select == "Sidewalk Coverage") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$SIDEWALK_COVG),], aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Students Per Device") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .8", yintercept = .8), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Student-Teacher Ratio, Elementary School") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 20.5", yintercept = 20.5), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Elementary School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Student-Teacher Ratio, High School") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_HS)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$STUDENT_TEACHER_HS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_HS), y=STUDENT_TEACHER_HS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_HS), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 24", yintercept = 24), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("High School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Students With Disabilities") {
          schoolstats_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats_summary[!is.na(schoolstats_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.3%", yintercept = 13.3), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        }
        
        else if(input$hs_select == "Titles Per Student") {
          schoolstats21_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats21_summary[!is.na(schoolstats21_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        }
        
        else if(input$hs_select == "WiFi Access Points Per Classroom") {
          schoolstats21_summary <- HS_stats_21 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats21_summary[!is.na(schoolstats21_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        
      }
      
      else if(input$hs_year == "Summer 2022"){
        if(input$hs_select == "Average Class Size") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Bachelor Degree Rate") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "BIPOC Students per School") {
          p <- ggplot(HS_poc_per_school_22, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "CTE Course Enrollment Rate, High School") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 56%", yintercept = 56), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("CTE Course Enrollment Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Enrollment") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Experienced Teacher Ratio"){
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 82.4%", yintercept = 82.4), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 1.5)) +
            theme_minimal() +
            labs(title = i18n()$t("Experienced Teacher Ratio"), x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Free and Reduced Lunch"){
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Funding Per Pupil") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $12,945", yintercept = 12945), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Graduation Rate") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(GRADUATION_RATE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$GRADUATION_RATE),], aes(x=reorder(SCHOOL_NAME, -GRADUATION_RATE), y=GRADUATION_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = GRADUATION_RATE), hjust = -.1, color = "black") +
            geom_hline(aes(text="Durham County Average = 87%", yintercept = 87), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Graduation Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "In-School Suspensions (ISS)") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 1.65", yintercept = 1.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Median Age") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.4", yintercept = 35.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Median Homesale Price") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $290,500", yintercept = 290500), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Median Household Income") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $60,958", yintercept = 60958), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Racial Demographics") {
          p <- ggplot(HS_all_race22, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
        }
        else if(input$hs_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(HS_racecomp_22, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$hs_select == "Sidewalk Coverage") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Students Per Device") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .79", yintercept = .79), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Student-Teacher Ratio, Elementary School") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Elementary School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Student-Teacher Ratio, High School") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_HS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_HS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_HS), y=STUDENT_TEACHER_HS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_HS), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("High School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Students With Disabilities") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.7%", yintercept = 13.7), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$hs_select == "Titles Per Student") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$hs_select == "WiFi Access Points Per Classroom") {
          schoolstats22_summary <- HS_stats_22 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
      }
      else if(input$hs_year == "Summer 2023"){
        if(input$hs_select == "Average Class Size") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(AVG_CLASS_SIZE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$AVG_CLASS_SIZE),], aes(x=reorder(SCHOOL_NAME, -AVG_CLASS_SIZE), y=AVG_CLASS_SIZE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = AVG_CLASS_SIZE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 19", yintercept = 19), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Average Class Size"), x = "School", y = "Average # of Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Bachelor Degree Rate") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(BACHELOR_DEG_RATE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -BACHELOR_DEG_RATE), y=BACHELOR_DEG_RATE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 44.1%", yintercept = 44.1), color ='#01016D') +
            geom_text(aes(label = BACHELOR_DEG_RATE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Bachelor Degree Rate"), y = "Bachelor Degree Rate", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "BIPOC Students per School") {
          p <- ggplot(HS_poc_per_school_23, aes(reorder(place, -number), number)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 80.7%", yintercept = 80.7), color ='#01016D') +
            geom_text(aes(label = number), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percentage of BIPOC Students") , x = "School", y = "BIPOC Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "CTE Course Enrollment Rate, High School") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(CTE_RATE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$CTE_RATE),], aes(x=reorder(SCHOOL_NAME, -CTE_RATE), y=CTE_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = CTE_RATE), hjust = -.1, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 56%", yintercept = 56), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("CTE Course Enrollment Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Enrollment") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(ENROLLMENT_NA)
          p <-  ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ENROLLMENT_NA),], aes(reorder(SCHOOL_NAME, -ENROLLMENT_NA), ENROLLMENT_NA)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = ENROLLMENT_NA, text = ENROLLMENT_NA), vjust = 0, color = "black")+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("School Enrollment") , x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "English as a Second Language (ESL) Student Enrollment") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(ESL_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$ESL_PERCENT),], aes(x= reorder(SCHOOL_NAME, -ESL_PERCENT), ESL_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = ESL_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = 15.8%", yintercept = 15.8), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("ESL Student Enrollment"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Experienced Teacher Ratio"){
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(EXP_TEACHER_RATIO) 
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -EXP_TEACHER_RATIO), y = EXP_TEACHER_RATIO)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = EXP_TEACHER_RATIO), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 82.4%", yintercept = 82.4), color ='#01016D') +
            coord_flip() +
            theme(plot.title = element_text(hjust = 1.5)) +
            theme_minimal() +
            labs(title = i18n()$t("Experienced Teacher Ratio"), x = "School", y = "Experienced Teachers (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Free and Reduced Lunch"){
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(FREE_RED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FREE_RED_PERCENT),], aes(x=reorder(SCHOOL_NAME, -FREE_RED_PERCENT), y=FREE_RED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FREE_RED_PERCENT), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 51.65%", yintercept = 51.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Receiving Free and Reduced Lunch"), x = "School", y = "Students")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Funding Per Pupil") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(FUNDING_PER_PUPIL)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$FUNDING_PER_PUPIL),], aes(x=reorder(SCHOOL_NAME, -FUNDING_PER_PUPIL), y=FUNDING_PER_PUPIL)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = FUNDING_PER_PUPIL), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = $12,945", yintercept = 12945), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Funding Per Pupil"), x = "School", y = "Amount of Funding (USD)")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Graduation Rate") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(GRADUATION_RATE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$GRADUATION_RATE),], aes(x=reorder(SCHOOL_NAME, -GRADUATION_RATE), y=GRADUATION_RATE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = GRADUATION_RATE), hjust = -.1, color = "black") +
            geom_hline(aes(text="Durham County Average = 87%", yintercept = 87), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Graduation Rate"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "In-School Suspensions (ISS)") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(IN_SCHOOL_SUSP_PER_1000)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$IN_SCHOOL_SUSP_PER_1000),], aes(x=reorder(SCHOOL_NAME, -IN_SCHOOL_SUSP_PER_1000), y=IN_SCHOOL_SUSP_PER_1000)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = IN_SCHOOL_SUSP_PER_1000), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 1.65", yintercept = 1.65), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("In-School Suspensions"), x = "School", y = "Students Per 1000")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Median Age") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_AGE)
          p <- ggplot(schoolstats22_summary, aes(x=reorder(SCHOOL_NAME, -MED_AGE), y=MED_AGE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = MED_AGE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 35.4", yintercept = 35.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Age of School Zones"), x = "School Zone", y = "Median Age")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Median Homesale Price") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOMESALE_PRICE)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOMESALE_PRICE), MED_HOMESALE_PRICE)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            scale_y_continuous(labels=scales::dollar_format()) +
            theme_minimal() +
            geom_hline(aes(text="Durham County Average = $290,500", yintercept = 290500), color ='#01016D') +
            geom_text(aes(label = MED_HOMESALE_PRICE), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Homesale Price"), y = "Median Homesale Price ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Median Household Income") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(MED_HOUSEHOLD_INC)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -MED_HOUSEHOLD_INC), MED_HOUSEHOLD_INC)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels=scales::dollar_format()) +
            geom_hline(aes(text="Durham County Average = $60,958", yintercept = 60958), color ='#01016D') +
            geom_text(aes(label = MED_HOUSEHOLD_INC), vjust = 0)+
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Median Household Income"), y = "Median Household Income ($)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Racial Demographics") {
          p <- ggplot(HS_all_race23, aes(fill=race, y=number, x=as.factor(school))) + 
            geom_bar(position="fill", stat="identity")+ ggtitle(i18n()$t("Racial Demographics")) + ylab("Percentage") + xlab("School Name")+
            coord_flip() +
            theme_minimal() +
            scale_fill_manual(values=cbPalette) +
            theme(plot.title = element_text(hjust = 0.5))
          ggplotly(p, tooltip = c("race", "number"))
        }
        else if(input$hs_select == "School and Zone BIPOC Comparison"){
          p <- ggplot(HS_racecomp_23, aes(factor(place), number, fill = sorz)) + 
            geom_bar(stat="identity", position = "dodge") + 
            coord_flip() +
            scale_fill_manual(values = c("#D1E3F4", "#76B9F0")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("BIPOC Comparison of Schools vs. School Zones") , x = "School/School Zone", y = "BIPOC Students (%)", fill=" ")
          ggplotly(p, tooltip = c("text", "text1", "number", "place"))
        }
        else if(input$hs_select == "Sidewalk Coverage") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(SIDEWALK_COVG)
          p <- ggplot(schoolstats22_summary, aes(reorder(SCHOOL_NAME, -SIDEWALK_COVG), SIDEWALK_COVG)) + 
            geom_bar(stat="identity", position = "dodge", fill="#76B9F0") + 
            coord_flip() +
            theme_minimal() +
            geom_text(aes(label = SIDEWALK_COVG), vjust = 0)+
            geom_hline(aes(text="Durham County Average = 35%", yintercept = 35), color ='#01016D') +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Sidewalk Coverage per School Zone"), y = "Sidewalk Coverage (%)", x = "School Zone")
          ggplotly(p, tooltip = c("text"))
        }
        else if(input$hs_select == "Students Per Device") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENTS_PER_DEVICE)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENTS_PER_DEVICE),], aes(x=reorder(SCHOOL_NAME, -STUDENTS_PER_DEVICE), y=STUDENTS_PER_DEVICE)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENTS_PER_DEVICE), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = .79", yintercept = .79), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Students Per Device"), x = "School", y = "Student to Device Ratio")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Student-Teacher Ratio, Elementary School") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_ELEM)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_ELEM),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_ELEM), y=STUDENT_TEACHER_ELEM)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_ELEM), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Elementary School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Student-Teacher Ratio, High School") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(STUDENT_TEACHER_HS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$STUDENT_TEACHER_HS),], aes(x=reorder(SCHOOL_NAME, -STUDENT_TEACHER_HS), y=STUDENT_TEACHER_HS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = STUDENT_TEACHER_HS), hjust = 1.5, color = "black") +
            geom_hline(aes(text="Durham County Average = 15.4", yintercept = 15.4), color ='#01016D') +
            coord_flip() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("High School Student-Teacher Ratio"), x = "School", y = "Students per Teacher")
          ggplotly(p, tooltip = c("text"))
        } 
        else if(input$hs_select == "Students With Disabilities") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(DISABLED_PERCENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$DISABLED_PERCENT),], aes(x= reorder(SCHOOL_NAME, -DISABLED_PERCENT), y=DISABLED_PERCENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = DISABLED_PERCENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 13.7%", yintercept = 13.7), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Percent of Students with Disabilities"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$hs_select == "Titles Per Student") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(TITLES_PER_STUDENT)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$TITLES_PER_STUDENT),], aes(x= reorder(SCHOOL_NAME, -TITLES_PER_STUDENT), y=TITLES_PER_STUDENT)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = TITLES_PER_STUDENT), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 17.16%", yintercept = 17.16), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("Titles Per Student"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        } 
        else if(input$hs_select == "WiFi Access Points Per Classroom") {
          schoolstats22_summary <- HS_stats_23 %>% group_by(SCHOOL_NAME) %>% summarise(WIFI_ACCESS_PTS)
          p <- ggplot(schoolstats22_summary[!is.na(schoolstats22_summary$WIFI_ACCESS_PTS),], aes(x= reorder(SCHOOL_NAME, -WIFI_ACCESS_PTS), y=WIFI_ACCESS_PTS)) +
            geom_bar(stat = 'identity', fill = "#76B9F0", color = "white") +
            geom_text(aes(label = WIFI_ACCESS_PTS), hjust = 1.5, color = "black") +
            coord_flip() +
            geom_hline(aes(text="Durham County Average = 1.06%", yintercept = 1.06), color ='#01016D') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 1.5)) +
            labs(title = i18n()$t("WiFi Access Points Per Classroom"), x = "School", y = "Students (%)")
          ggplotly(p, tooltip = c("text")) 
        }
      }
    })
    
  }
  
  # SchoolStats - Context and Resources
  {  
    observeEvent(i18n(),
                 output$es_resources <- renderText({
                   if (input$es_select == "Average Class Size"){
                     paste(i18n()$t("Research proves smaller class size is beneficial to student achievement. Smaller classes allow for the teacher to focus less on classroom management, and more on centralized learning. Students have stated they feel more comfortable in smaller classes as well."),"<br>","<br>",
                           i18n()$t("Resources Discussing the Importance of Class Size:"),"<br>",
                           a(i18n()$t("The Benefits of Investing in Smaller Class Sizes"),
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/educators-and-parents-reset-class-size-debate"),"<br>",
                           a(i18n()$t("State Policy View on Class Size"),
                             href = "https://www.brookings.edu/research/class-size-what-research-says-and-what-it-means-for-state-policy/"))
                   }
                   else if (input$es_select == "Bachelor Degree Rate"){
                     paste(i18n()$t("This graph shows the percentage of adults with bachelor’s degrees in each school zone. The number of individuals with bachelor’s degrees greatly differs across racial, income, and gender groups. Additionally, individuals with more degrees tend to have greater household incomes."), "<br>", "<br>",
                           i18n()$t("Below is more information about bachelor degree rates:"), "<br>",
                           a(i18n()$t("Bachelor’s Degrees and Race"),
                             href="https://nces.ed.gov/fastfacts/display.asp?id=72"), "<br>",
                           a(i18n()$t("Bachelor’s Degrees and Income"),
                             href="https://www.bls.gov/careeroutlook/2018/data-on-display/education-pays.htm"))
                   }
                   else if (input$es_select == "BIPOC Students per School"){
                     paste(i18n()$t("This dataset shows the percentage of BIPOC students in each of the 16 schools. Each of the schools are “majority students of color” which means representation of these students and "), 
                           strong(i18n()$t("culturally-responsive pedagogy")),HTML(paste0(tags$sup("1"))), i18n()$t("is integral for student success. Too often, BIPOC students are underrepresented and cannot relate to content/curriculum because of cultural differences. Students who are able to connect with their tutors, teachers, administrators, etc. tend to engage with the content more."), 
                           "<br>","<br>",
                           i18n()$t("Below is more information about BIPOC students:"), "<br>",
                           a(i18n()$t("Racial/Ethnic Enrollment in NC Public Schools"),
                             href="https://nces.ed.gov/programs/coe/pdf/coe_cge.pdf"),
                           a(i18n()$t("Why Representation Matters"),
                             href="https://givingcompass.org/article/why-teacher-representation-matters-to-students-of-color/"),
                           a(i18n()$t("Benefits of POC Representation"),
                             href="https://educationpost.org/students-of-color-need-to-see-more-people-of-color-that-shouldnt-be-controversial/"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("culturally-responsive pedagogy")), 
                           i18n()$t(": style of individualized teaching that is cognizant of the varying cultures and ethnicities of the classroom"))
                     
                   }
                   else if (input$es_select == "English as a Second Language (ESL) Student Enrollment"){
                     paste(i18n()$t("This graph shows the number of students enrolled in the English as a Second Language (ESL) or English Language Learners (ELL) Program. ESL students consist of any student regardless of ethnicity, origin, race, age, etc. who is a non-native English speaker. These programs are created to help children learn English along with other subjects necessary to complete each grade."), 
                           "<br>","<br>", 
                           i18n()$t("Unfortunately, ESL students can face racial bias, discrimination, and bullying in the classroom. Teachers may prevent them from participating in school activities, extracurriculars, and enrichment programs. It is important that school affiliates recognize that language barriers do not stunt intellectual development. Additionally, ELL teachers must accommodate their students instead of assimilating them by removing the identity of their native language entirely."), "<br>","<br>",
                           i18n()$t("Below are resources and information on culturally-responsive teaching and Durham Public Schools’ approaches to ESL/ELL programs:"), "<br>",
                           a(i18n()$t("How to Advocate for ESL/ELL Students"),
                             href = "https://www.nea.org/professional-excellence/student-engagement/tools-tips/english-language-learners-what-you-need-know"),"<br>",
                           a(i18n()$t("Real-World Experiences and Anecdotes"),
                             href = "https://www.learningforjustice.org/magazine/summer-2017/a-case-for-acculturation"),"<br>",
                           a(i18n()$t("DPS ESL Office"),
                             href = "https://central.dpsnc.net/esl"),"<br>",
                           a(i18n()$t("ESL/ELL Teachers"),
                             href = "https://www.eslteacheredu.org/what-is-an-esl-teacher/"))
                   } 
                   else if(input$es_select == "Enrollment") {
                     paste(i18n()$t("This dataset shows the enrollment numbers at each school. Due to the COVID-19 pandemic, there has been a 5% decrease in enrollment. Normally, enrollment or average daily membership (ADM) is used to determine funding for each school. The higher the ADM, the more money a school receives. Recently, lawmakers passed a bill stating that they would omit schools from reduction in funding as part of COVID-19 relief. Durham Public Schools continues to strive for increased enrollment so that all programs can continue to receive adequate funding."), "<br>","<br>", 
                           i18n()$t("Below is information about enrollment numbers:"), "<br>",
                           a(i18n()$t("The Decrease in Enrollment in North Carolina"), 
                             href = "https://abc11.com/nc-schools-school-attendance-enrollment-durham-county/8204335/")
                     )
                   }
                   else if (input$es_select == "Experienced Teacher Ratio") {
                     paste(i18n()$t("Experienced teachers are those who have approximately five or more years of experience with teaching. Although more experienced teachers tend to perform better on their evaluations, research shows teacher experience does not directly correlate to effective teaching. Some schools tend to be more attracted to veteran teachers whereas other schools may prefer employing recently certified teachers that can provide current and unique teaching styles."),
                           "<br>","<br>",
                           i18n()$t("With the evolution of research-based practices, it is important to continue to educate new and returning teachers on the best teaching practices that are scientifically-proven to promote student success. Good professional development workshops are paramount to provide effective, culturally-responsive teaching practices."), "<br>","<br>",
                           i18n()$t("Below are articles on Teacher Experience and Professional Development:"), "<br>",
                           a(i18n()$t("Effects of Hiring and Recommendations for Supporting Experienced Teachers"), 
                             href = "https://learningpolicyinstitute.org/product/does-teaching-experience-increase-teacher-effectiveness-review-research"),"<br>",
                           a(i18n()$t("Teacher Experience in Relation to Teacher Effectiveness"), 
                             href ="https://www.nea.org/advocating-for-change/new-from-nea/does-teaching-experience-matter-lets-count-ways"),"<br>",
                           a(i18n()$t("NCDPI Educator Professional Development"), 
                             href ="https://www.dpi.nc.gov/educators/professional-development"),"<br>",
                           a(i18n()$t("Importance and Resources for Professional Development"), 
                             href ="https://www.nea.org/professional-excellence/professional-learning/teachers"))
                     
                   } 
                   else if (input$es_select == "Free and Reduced Lunch") {
                     paste(i18n()$t("The percentage of students receiving free and reduced lunch is a strong indicator of socioeconomic status. The percentage of students that fall below the poverty line determines if a school is considered "), 
                           HTML(paste0(strong(i18n()$t("Title I")),tags$sup("1"))), ".", 
                           i18n()$t("Title I schools are eligible to receive grants through the Every Student Succeeds Act (ESEA). It is important to be cognizant of students’ socioeconomic status without being condescending and discriminatory. Socioeconomic status is not a limit, it is a barrier."),
                           "<br>","<br>",
                           i18n()$t("Below are articles on Free/Reduced Lunch and its connection to Title I schools:"), "<br>",
                           a(i18n()$t("Rural Schools and Free/Reduced Lunch"), 
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/whos-looking-out-rural-schools"), "<br>",
                           a(i18n()$t("NC Community Free/Reduced Lunch Eligibility"),
                             href = "https://www.dpi.nc.gov/districts-schools/federal-program-monitoring/title-i-part/community-eligibility-free-and-reduced-price-meals"),
                           "<br>","<br>", HTML(paste0(tags$sup("1"))),
                           strong("Title I"), i18n()$t(": Under the ESEA, this federally funded program identifies schools with a majority of low-income students, based on free and reduced lunch statistics."
                           ))
                   } 
                   else if (input$es_select == "Funding Per Pupil"){
                     paste(i18n()$t("This indicator represents the amount that local, state, and federal governments spend on elementary and secondary education adjusted for the size of the student body. It is calculated by dividing the current expenditures over the entire academic year for Pre-Kindergarten through Grade 12 by the number of students in those grades in public schools. Current expenditures include instruction and instruction-related costs, student support services, administration, and operations. Excluded from those expenditures are funds for school construction and other capital outlays, debt service, and programs outside of public elementary and secondary education."), "<br>", "<br>",
                           i18n()$t("Resources on public school funding:"), "<br>",
                           a(i18n()$t("Interactive Summary of Local - Federal Public School Funding:"),
                             href="https://www.dpsnc.net/Page/3771"),"<br>",
                           a(i18n()$t("New Per Pupil Expenditure Requirements"),
                             href ="https://www.naesp.org/blog/new-per-pupil-expenditure-requirements/"))
                   } 
                   else if (input$es_select == "In-School Suspensions (ISS)"){
                     paste(i18n()$t("In-school suspensions are described as: Instances in which a child is temporarily removed from his or her regular classrooms for at least half a day but remains under the direct supervision of school personnel"), 
                           HTML(paste0(strong(tags$sub("1")))), ".",
                           "<br>", "<br>",
                           i18n()$t("BIPOC students are more susceptible to harsher punishments in schools."),
                           i18n()$t("Black and Brown students are subject to higher disciplinary actions compared to their white peers. A reason for this is racial bias leading to the over policing of Black and Brown students, fueling the"), 
                           strong(i18n()$t("school-to-prison pipeline")),HTML(paste0(tags$sup("1"))),".", "<br>","<br>",
                           i18n()$t("Below are articles on In-School Suspensions and the School-to-Prison Pipeline:"), "<br>", 
                           a(i18n()$t("Racial Bias in School Discipline"), 
                             href = "https://www.pnas.org/content/116/17/8255"), "<br>",
                           a(i18n()$t("School-to-Prison Pipeline"), 
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/school-prison-pipeline-time-shut-it-down"),
                           "<br>", "<br>", HTML(paste0(tags$sup("1"))), strong(i18n()$t("school-to-prison pipeline")), 
                           i18n()$t(": the overly disproportionate policing of minority students, often from low-income households, that leads to higher punishments including ISS, OSS (out-of-school suspension), juvenile detention, etc."),
                           "<br>", "<br>",
                           i18n()$t("Sources:"),
                           "<br>",HTML(paste0(strong(tags$sub("1")))),
                           a(i18n()$t("supportiveschooldiscipline.org"), href = "https://supportiveschooldiscipline.org/"))
                   }
                   else if (input$es_select == "Median Age"){
                     paste(i18n()$t("This dataset shows the median age of residents in each school zone. The median age of residents in a specific school zone can determine the various assets available, identify beneficial resources in the community, and give some insight about school enrollment numbers in the future."))
                   }
                   else if (input$es_select == "Median Homesale Price"){
                     paste(i18n()$t("This graph shows the average home sale price for each school zone. In 2021, the median home sale price for the United States is $397,100 and the average home sale price is $464,200, according to the US Census Bureau. Specifically for the South, the median home sale price is $355,900 and the average home sale price is $411,100."),
                           "<br>", "<br>",
                           i18n()$t("Due to the COVID-19 Pandemic, home prices have increased. This made the cost of living increase as well, all contributing to"), 
                           strong(i18n()$t("gentrification")),HTML(paste0(tags$sup("1"))),".", 
                           i18n()$t("Most of these school zones fall above the North Carolina median. Because Durham has become an increasingly expensive city to live in, many of the students in the sixteen schools required free and reduced lunch."), "<br>", "<br>",
                           i18n()$t("Below is more information about home sale price:"), "<br>",
                           a(i18n()$t("Home Sale Price Data"),
                             href="https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx"), "<br>",
                           a(i18n()$t("Covid-19 Increase"),
                             href="https://www.cnbc.com/2021/06/16/typical-us-home-price-up-record-13point2percent-compared-to-last-year.html"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("gentrification")), i18n()$t(": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents"))
                   }
                   else if (input$es_select == "Median Household Income"){
                     paste(i18n()$t("This graph shows the median household income for each school zone. According to the 2020 US census, the median household income in the United States is $67,521. This is a 2.9% decrease from 2019, which is the first statistically significant decline since 2011."), 
                           "<br>","<br>", 
                           i18n()$t("According to the 2020 US census, the average household income in North Carolina is $56,642, while the per capita income in the past twelve months is $31,993. All of these schools fall below the NC and US median, which is a cause for concern. Poverty directly affects how students learn and those suffering from food insecurity, for example, are hindered from reaching their full potential. Students’ basic human needs must be met in order to excel in the classroom."), "<br>", "<br>", 
                           i18n()$t("Below are links to 
                  the US Census Information:"), "<br>",
                           a(i18n()$t("Nationwide Census"), 
                             href="https://www.census.gov/quickfacts/fact/table/US/PST045219"), "<br>", 
                           a(i18n()$t("North Carolina Census"),
                             href="https://www.census.gov/quickfacts/NC"))
                   }
                   else if (input$es_select == "Racial Demographics"){
                     paste(i18n()$t("This dataset shows the racial breakdown of each of the 16 public schools. Durham Public Schools’ student population is 80.7% BIPOC students, but only 57% of Durham County is people of color. The racial demographics of all 16 schools has changed over time, specifically in the past 30 years. The number of white students has decreased, while the number of BIPOC students has increased."),
                           "<br>", "<br>",
                           i18n()$t("Representation of these students and "), 
                           strong(i18n()$t("culturally-responsive pedagogy")),HTML(paste0(tags$sup("1"))), 
                           i18n()$t("is integral for student success. Too often BIPOC students are underrepresented and cannot relate to the content because of cultural differences. Students who are able to connect with their tutors, teachers, administrators, and the content tend to engage with the content more."), "<br>","<br>",
                           i18n()$t("Below is more information about racial demographics in schools:"), "<br>",
                           a(i18n()$t("Changing School Racial Demographics in Recent Decades"),
                             href="https://www.urban.org/features/explore-your-schools-changing-demographics"), "<br>",
                           a(i18n()$t("More BIPOC students in Public schools"),
                             href="https://www.publicschoolreview.com/blog/white-students-are-now-the-minority-in-u-s-public-schools"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("culturally-responsive pedagogy")), i18n()$t(": style of individualized teaching that is cognizant of the varying cultures and ethnicities of the classroom"))
                   }
                   else if (input$es_select == "School and Zone BIPOC Comparison") {
                     paste(i18n()$t("This plot shows the percentage of BIPOC students in the school compared to the percentage of people of color in the school zone. This measurement shows the huge disparities in community representation for BIPOC students as most of these schools are not racially reflective of the school zones they inhabit. A contributing factor of this is ") , 
                           strong(i18n()$t("gentrification")),HTML(paste0(tags$sup("1"))),".", 
                           "Gentrification has been an increasing problem, and the COVID-19 pandemic has only exacerbated it. Black and Brown neighborhoods are becoming increasingly expensive forcing families out of their homes, which ultimately changes the demographics of those neighborhoods.", "<br>", "<br>",
                           i18n()$t("Below are articles on In-School Suspensions and the School-to-Prison Pipeline:"), "<br>", 
                           a(i18n()$t("COVID-19 and Gentrification in Durham"), 
                             href = "https://www.dukechronicle.com/article/2020/08/duke-university-covid-rages-housing-hits-home-gentrification-durham"), "<br>",
                           a(i18n()$t("Redlining in Durham"), 
                             href = "https://abc11.com/redlining-gentrification-black-neighborhoods-triangle/10373290/"),
                           a(i18n()$t("Durham’s Racial Inequality, Poverty, and Gentrification"), 
                             href = "https://fpg.unc.edu/publications/racial-inequality-poverty-and-gentrification-durham-north-carolina"),
                           "<br>", "<br>",
                           HTML(paste0(tags$sup("1"))), strong(i18n()$t("gentrification")), 
                           i18n()$t(": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents"))
                   }
                   else if (input$es_select == "Sidewalk Coverage"){
                     paste(i18n()$t("Areas without sidewalk coverage can become inaccessible for people without cars or other modes of transportation, both private and public. Sidewalks are needed for individuals to safely walk to places such as school, grocery stores, parks, etc. Especially for younger students, sidewalks are essential for their safety. According to a UNC Highway Safety Research Center study, the probability of a crash occurring near or at a paved sidewalk is 88.2% lower than an unpaved site."),"<br>", "<br>", 
                           i18n()$t("Sidewalks also promote more physical activity and healthier lifestyles. In the US specifically, obesity rates have been steadily increasing across all populations and ages. Sidewalk coverage provides people with a safe and accessible way to increase their daily physical activity."),"<br>", "<br>",
                           i18n()$t("High income areas tend to have more sidewalk coverage than lower income areas. 89% of high-income communities have sidewalks, whereas only 49% of low income communities have sidewalk coverage."), "<br>", "<br>",
                           i18n()$t("Below is more information about sidewalk coverage:"), "<br>",
                           a(i18n()$t("Importance of Sidewalks"),
                             href="http://guide.saferoutesinfo.org/engineering/sidewalks.cfm"), "<br>",
                           a(i18n()$t("Income Disparities and Sidewalk Coverage"),
                             href="https://www.cityofeastlansing.com/DocumentCenter/View/1583/Income-Disparities-in-Street-Features-That-Encourage-Walking-PDF"))
                   }
                   else if (input$es_select == "Students Per Device"){
                     paste(i18n()$t("Living in a digital age, technology usage in the classroom has increased tremendously, especially during the COVID-19 pandemic. Although technology is a great resource, students may not have equitable access to these devices. It is important for students and teachers to not only have access to these technological devices, but also understand how to use them, which is why professional development is so important. Professional development is necessary to keep educators up to date on new technology to create the most effective learning environment."),
                           "<br>","<br>",
                           i18n()$t("Resources on Access and Technology in the Classroom:"), "<br>",
                           a(i18n()$t("Durham Public Schools’ Technological Services"),
                             href = "https://www.dpsnc.net/site/default.aspx?PageType=3&DomainID=207&ModuleInstanceID=8115&ViewID=6446EE88-D30C-497E-9316-3F8874B3E108&RenderLoc=0&FlexDataID=42210&PageID=4738"), "<br>",
                           a(i18n()$t("Equitable Access to Technology"),
                             href = "https://digitalpromise.org/2019/04/29/equity-in-schools-access-technology/"))
                   }
                   else if (input$es_select == "Student-Teacher Ratio, Elementary School"){
                     paste(i18n()$t("Research proves smaller student-teacher ratios have a positive effect on student achievement. By allowing more centralized and one-on-one instruction, smaller student-teacher ratios can increase test scores, lower dropout rates, and increase graduation rates."), 
                           "<br>","<br>", i18n()$t("For younger, elementary school-age students, having a positive, one-on-one relationship with his or her teacher will teach the student at a young age that they have an adult they can trust and rely on. In smaller sized classrooms, students can feel more comfortable and willing to share their thoughts without being afraidof failing or feeling embarrassed. This dynamic fosters a more productive learning environment for all."),"<br>","<br>",
                           i18n()$t("Resources on Student-Teacher Ratios:"),"<br>",
                           a(i18n()$t("Infographics and Information on Student-Teacher Ratios"),
                             href = "https://www.hunschool.org/resources/student-teacher-ratios"))
                     
                   } 
                   else if (input$es_select == "Students With Disabilities"){
                     paste(i18n()$t("According to the Americans with Disabilities Act, an individual is considered to have a disability if they have a condition that impairs them to do certain activities and interact with those around them. It is integral to make sure students with disabilities are provided with accessibility services to achieve their full potential in the classroom. Resources like"), 
                           HTML(paste0(strong(i18n()$t("assistive technology")),tags$sup("1"))), 
                           i18n()$t(", transportation,"), 
                           HTML(paste0(strong(i18n()$t("Exceptional Children (EC) programs")),tags$sup("2"))), 
                           i18n()$t("etc. are mandatory for every school to provide regardless of the number of students with disabilities or even the type based on the civil rights law Section 504."), 
                           "<br>", "<br>",
                           i18n()$t("Below are articles and resources about government protection and resources for students with disabilities:"), "<br>",
                           a(i18n()$t("DPS EC Services"),
                             href="https://www.dpsnc.net/Page/169"),
                           a(i18n()$t("Section 504"),
                             href="https://www.dpsnc.net/Page/336"),
                           a(i18n()$t("NCDPI’s EC Division"),
                             href="https://www.dpi.nc.gov/districts-schools/classroom-resources/exceptional-children-division"),
                           a(i18n()$t("Assistive Technology"),
                             href="https://www.disabilityrightswa.org/publications/assistive-technology-special-education-students/"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("Assistive Technology")), i18n()$t(": supplementary devices that promote independence and functionality of people with varying disabilities"),
                           "<br>", HTML(paste0(tags$sup("2"))), strong(i18n()$t("EC programs")), i18n()$t(": school programs that include students that need intense or individualized instruction in addition to their standard course of study")
                     )
                   } 
                   else if (input$es_select == "Titles Per Student"){
                     paste(i18n()$t("A school library’s number of book titles per student indicates the availability of different book titles for students to select. A higher number of book titles per student indicates a wider variety of books to choose from. Having more book titles per student enables more opportunities for whole-class novel reading, which is essential for learning book analysis, encouraging engaging discussions, and prompting creative writing."), 
                           "<br>", "<br>",
                           i18n()$t("Having a wide selection of books in a library also promotes more reading and intellectual stimulation for students. With a higher number of titles per student, it is more likely that most, if not all, students can find literature that engages them."),
                           "<br>","<br>",
                           i18n()$t("Below are more resources on the importance of book titles:"), 
                           "<br>",
                           a("Importance of Culturally-Diverse Literature",
                             href = "https://ila.onlinelibrary.wiley.com/doi/full/10.1002/trtr.1326"), "<br>",
                           a("Creating an Effective and Diverse Classroom Library",
                             href = "https://digitalcommons.wou.edu/cgi/viewcontent.cgi?article=1187&context=theses"), "<br>",
                           a(i18n()$t("Top 10 Benefits of Reading for All Ages"),
                             href = "https://markhampubliclibrary.ca/blogs/post/top-10-benefits-of-reading-for-all-ages/"), "<br>",
                           a(i18n()$t("North Carolina School Report Cards"),
                             href = "https://ncreports.ondemand.sas.com/src/?county=Durham"))
                   }
                   else if (input$es_select == "WiFi Access Points Per Classroom"){
                     paste(strong(i18n()$t("Note: ")), i18n()$t("Due to the COVID-19 pandemic, this data only extends until December of 2020. Therefore, only the 2019-2020 school year is reflected above."),
                           "<br>", "<br>",
                           i18n()$t("The visualization depicts the number of wireless access points for each classroom. A school with a higher number of access points overall will have better internet coverage and quality than a school with less access points."),
                           "<br>", "<br>",
                           i18n()$t("As things begin to modernize with time, the internet has become a vital resource for all. Specifically in schools, teachers can display information and teach through slideshows, animations, videos, games, and more. Incorporating various forms of online tools can make the students become more engaged, while enabling the educator to explore new teaching methods. As a result, students will become more comfortable with using technology."),
                           "<br>", "<br>",
                           i18n()$t("However, not all students have the same access to technology in their homes. Most schools have accounted for this disparity by providing students with free tablets or laptops. During the COVID-19 pandemic, when students of all ages had to adjust to online instruction, several counties provided WiFI hubs so that students’ learning was not hindered."))
                   }
                 }))
    
    observeEvent(i18n(),
                 output$ms_resources <- renderText({
                   if (input$ms_select == "Average Class Size"){
                     paste(i18n()$t("Research proves smaller class size is beneficial to student achievement. Smaller classes allow for the teacher to focus less on classroom management, and more on centralized learning. Students have stated they feel more comfortable in smaller classes as well."),"<br>","<br>",
                           i18n()$t("Resources Discussing the Importance of Class Size:"),"<br>",
                           a(i18n()$t("The Benefits of Investing in Smaller Class Sizes"),
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/educators-and-parents-reset-class-size-debate"), "<br>",
                           a(i18n()$t("State Policy View on Class Size"),
                             href = "https://www.brookings.edu/research/class-size-what-research-says-and-what-it-means-for-state-policy/"))     
                     
                   } 
                   else if (input$ms_select == "CTE Course Enrollment Rate, Middle School"){
                     paste(i18n()$t("Career and Technical Education (CTE) courses are designed for high school students to receive real-world experience in the career field they are most interested in. Durham Public Schools started the “3-2-1” initiative in 2019 where all high school students are required to take three CTE courses, participate in two career exposure activities, and get an internship or a job before they graduate. This initiative, as well as all CTE courses, are created to develop students’ soft skills, gain real-world experience, and help students decide on their post-graduate plans."),
                           "<br>","<br>",
                           i18n()$t("Below is more information about CTE courses in Durham Public Schools and North Carolina:"), "<br>",
                           a(i18n()$t("DPS CTE Course Initiative"),
                             href = "https://www.dpsnc.net/domain/293"), "<br>",
                           a(i18n()$t("NCDPI CTE Course Overview"),
                             href = "https://www.dpi.nc.gov/districts-schools/classroom-resources/career-and-technical-education"))
                   }
                   else if (input$ms_select == "Experienced Teacher Ratio") {
                     paste(i18n()$t("Experienced teachers are those who have approximately five or more years of experience with teaching. Although more experienced teachers tend to perform better on their evaluations, research shows teacher experience does not directly correlate to effective teaching. Some schools tend to be more attracted to veteran teachers whereas other schools may prefer employing recently certified teachers that can provide current and unique teaching styles."), "<br>","<br>",
                           i18n()$t("Below are articles on Teacher Experience and Professional Development:"), "<br>",
                           a(i18n()$t("Effects of Hiring and Recommendations for Supporting Experienced Teachers"), 
                             href = "https://learningpolicyinstitute.org/product/does-teaching-experience-increase-teacher-effectiveness-review-research"),"<br>",
                           a(i18n()$t("Teacher Experience in Relation to Teacher Effectiveness"), 
                             href ="https://www.nea.org/advocating-for-change/new-from-nea/does-teaching-experience-matter-lets-count-ways"),"<br>",
                           a(i18n()$t("NCDPI Educator Professional Development"), 
                             href ="https://www.dpi.nc.gov/educators/professional-development"),"<br>",
                           a(i18n()$t("Importance and Resources for Professional Development"), 
                             href ="https://www.nea.org/professional-excellence/professional-learning/teachers"))
                     
                     
                   } 
                   else if (input$ms_select == "Free and Reduced Lunch") {
                     paste(i18n()$t("The percentage of students receiving free and reduced lunch is a strong indicator of socioeconomic status. The percentage of students that fall below the poverty line determines if a school is considered "), HTML(paste0(strong(i18n()$t("Title I")),tags$sup("1"))), ".", 
                           i18n()$t("Title I schools are eligible to receive grants through the Every Student Succeeds Act (ESEA). It is important to be cognizant of students’ socioeconomic status without being condescending and discriminatory. Socioeconomic status is not a limit, it is a barrier."), "<br>","<br>",
                           i18n()$t("Below are articles on Free/Reduced Lunch and its connection to Title I schools:"), "<br>",
                           a(i18n()$t("Rural Schools and Free/Reduced Lunch"), 
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/whos-looking-out-rural-schools"), "<br>",
                           a(i18n()$t("NC Community Free/Reduced Lunch Eligibility"),
                             href = "https://www.dpi.nc.gov/districts-schools/federal-program-monitoring/title-i-part/community-eligibility-free-and-reduced-price-meals"),
                           "<br>","<br>", HTML(paste0(tags$sup("1"))),
                           strong(i18n()$t("Title I")), i18n()$t(": Under the ESEA, this federally funded program identifies schools with a majority of low-income students, based on free and reduced lunch statistics."))
                   } 
                   else if (input$ms_select == "Student-Teacher Ratio, Middle School"){
                     paste(i18n()$t("Research proves smaller student-teacher ratios have a positive effect on student achievement. By allowing more centralized and one-on-one instruction, smaller student-teacher ratios can increase test scores, lower dropout rates, and increase graduation rates.")
                           , "<br>","<br>",
                           i18n()$t("Resources on Student-Teacher Ratios:"),"<br>",
                           a(i18n()$t("Infographics and Information on Student-Teacher Ratios"),
                             href = "https://www.hunschool.org/resources/student-teacher-ratios"))
                     
                   } 
                   else if (input$ms_select == "Students Per Device"){
                     paste(i18n()$t("Living in a digital age, technology usage in the classroom has increased tremendously, especially during the COVID-19 pandemic. Although technology is a great resource, students may not have equitable access to these devices. It is important for students and teachers to not only have access to these technological devices, but also understand how to use them, which is why professional development is so important. Professional development is necessary to keep educators up to date on new technology to create the most effective learning environment."),"<br>","<br>",
                           i18n()$t("Resources on Access and Technology in the Classroom:"), "<br>",
                           a(i18n()$t("Durham Public Schools’ Technological Services"),
                             href = "https://www.dpsnc.net/site/default.aspx?PageType=3&DomainID=207&ModuleInstanceID=8115&ViewID=6446EE88-D30C-497E-9316-3F8874B3E108&RenderLoc=0&FlexDataID=42210&PageID=4738"), "<br>",
                           a(i18n()$t("Equitable Access to Technology"),
                             href = "https://digitalpromise.org/2019/04/29/equity-in-schools-access-technology/"))
                     
                   } 
                   else if (input$ms_select == "Funding Per Pupil"){
                     paste(i18n()$t("This indicator represents the amount that local, state, and federal governments spend on elementary and secondary education adjusted for the size of the student body. It is calculated by dividing the current expenditures over the entire academic year for Pre-Kindergarten through Grade 12 by the number of students in those grades in public schools. Current expenditures include instruction and instruction-related costs, student support services, administration, and operations. Excluded from those expenditures are funds for school construction and other capital outlays, debt service, and programs outside of public elementary and secondary education. North Carolina ranks 39th in per pupil spending out of 50."), "<br>", "<br>",
                           i18n()$t("Resources on public school funding:"), "<br>",
                           a(i18n()$t("Interactive Summary of Local - Federal Public School Funding:"),
                             href="https://www.dpsnc.net/Page/3771"),"<br>",
                           a(i18n()$t("New Per Pupil Expenditure Requirements"),
                             href ="https://www.naesp.org/blog/new-per-pupil-expenditure-requirements/"))
                     
                   } 
                   else if (input$ms_select == "Students With Disabilities"){
                     paste(i18n()$t("According to the Americans with Disabilities Act, an individual is considered to have a disability if they have a condition that impairs them to do certain activities and interact with those around them. It is integral to make sure students with disabilities are provided with accessibility services to achieve their full potential in the classroom. Resources like"), 
                           HTML(paste0(strong(i18n()$t("assistive technology")),tags$sup("1"))), 
                           (",transportation,"), 
                           HTML(paste0(strong(i18n()$t("Exceptional Children (EC) programs")),tags$sup("2"))), 
                           i18n()$t(", etc. are mandatory for every school to provide regardless of the number of students with disabilities or even the type based on the civil rights law Section 504."), "<br>", "<br>",
                           i18n()$t("Below are articles and resources about government protection and resources for students with disabilities:"), "<br>",
                           a(i18n()$t("DPS EC Services"),
                             href="https://www.dpsnc.net/Page/169"),"<br>",
                           a(i18n()$t("Section 504"),
                             href="https://www.dpsnc.net/Page/336"),"<br>",
                           a(i18n()$t("NCDPI’s EC Division"),
                             href="https://www.dpi.nc.gov/districts-schools/classroom-resources/exceptional-children-division"),"<br>",
                           a(i18n()$t("Assistive Technology"),
                             href="https://www.disabilityrightswa.org/publications/assistive-technology-special-education-students/"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("assistive technology: ")), 
                           i18n()$t(": supplementary devices that promote independence and functionality of people with varying disabilities"),
                           "<br>", HTML(paste0(tags$sup("2"))), strong(i18n()$t("EC programs")), 
                           i18n()$t(": school programs that include students that need intense or individualized instruction in addition to their standard course of study")
                     )
                     
                   } 
                   else if (input$ms_select == "English as a Second Language (ESL) Student Enrollment"){
                     paste(i18n()$t("This graph shows the number of students enrolled in the English as a Second Language (ESL) or English Language Learners (ELL) Program. ESL students consist of any student regardless of ethnicity, origin, race, age, etc. who is a non-native English speaker. These programs are created to help children learn English along with other subjects necessary to complete each grade."), "<br>","<br>", 
                           i18n()$t("Unfortunately, ESL students can face racial bias, discrimination, and bullying in the classroom. Teachers may prevent them from participating in school activities, extracurriculars, and enrichment programs. It is important that school affiliates recognize that language barriers do not stunt intellectual development. Additionally, ELL teachers must accommodate their students instead of assimilating them by removing the identity of their native language entirely."), "<br>","<br>",
                           i18n()$t("Below are resources and information on culturally-responsive teaching and Durham Public Schools’ approaches to ESL/ELL programs:"), "<br>",
                           a(i18n()$t("How to Advocate for ESL/ELL Students"),
                             href = "https://www.nea.org/professional-excellence/student-engagement/tools-tips/english-language-learners-what-you-need-know"),"<br>",
                           a(i18n()$t("Real-World Experiences and Anecdotes"),
                             href = "https://www.learningforjustice.org/magazine/summer-2017/a-case-for-acculturation"),"<br>",
                           a(i18n()$t("DPS ESL Office"),
                             href = "https://central.dpsnc.net/esl"),"<br>",
                           a(i18n()$t("ESL/ELL Teachers"),
                             href = "https://www.eslteacheredu.org/what-is-an-esl-teacher/"))
                     
                   } 
                   else if (input$ms_select == "In-School Suspensions (ISS)"){
                     paste(i18n()$t("In-school suspensions are described as: Instances in which a child is temporarily removed from his or her regular classrooms for at least half a day but remains under the direct supervision of school personnel."), "<br>", 
                           "<br>",i18n()$t("BIPOC students are more susceptible to harsher punishments in schools. Black and Brown students are subject to higher disciplinary actions compared to their white peers. A reason for this is racial bias leading to the over policing of Black and Brown students, fueling the"), 
                           strong(i18n()$t("school-to-prison pipeline")),HTML(paste0(tags$sup("1"))), ".", "<br>","<br>",
                           i18n()$t("Below are articles on In-School Suspensions and the School-to-Prison Pipeline:"), "<br>", 
                           a(i18n()$t("Racial Bias in School Discipline"), 
                             href = "https://www.pnas.org/content/116/17/8255"), "<br>",
                           a(i18n()$t("School-to-Prison Pipeline"), 
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/school-prison-pipeline-time-shut-it-down"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("school-to-prison pipeline")), i18n()$t(": the overly disproportionate policing of minority students, often from low-income households, that leads to higher punishments including ISS, OSS (out-of-school suspension), juvenile detention, etc.")
                     )
                     
                     
                   }
                   else if(input$ms_select == "Enrollment") {
                     paste(i18n()$t("This dataset shows the enrollment numbers at each school. Due to the COVID-19 pandemic, there has been a 5% decrease in enrollment. Normally, enrollment or average daily membership (ADM) is used to determine funding for each school. The higher the ADM, the more money a school receives. Recently, lawmakers passed a bill stating that they would omit schools from reduction in funding as part of COVID-19 relief. Durham Public Schools continues to strive for increased enrollment so that all programs can continue to receive adequate funding."), "<br>","<br>", 
                           i18n()$t("Below is information about enrollment numbers:"), "<br>",
                           a(i18n()$t("The Decrease in Enrollment in North Carolina"), 
                             href = "https://abc11.com/nc-schools-school-attendance-enrollment-durham-county/8204335/")
                     )
                     
                   }
                   else if (input$ms_select == "School and Zone BIPOC Comparison") {
                     paste(i18n()$t("This plot shows the percentage of BIPOC students in the school compared to the percentage of people of color in the school zone. This measurement shows the huge disparities in community representation for BIPOC students as most of these schools are not racially reflective of the school zones they inhabit. A contributing factor of this is ") , HTML(paste0(strong(i18n()$t("gentrification")),tags$sup("1"))),".", 
                           i18n()$t("Gentrification has been an increasing problem, and the COVID-19 pandemic has only exacerbated it. Black and Brown neighborhoods are becoming increasingly expensive forcing families out of their homes, which ultimately changes the demographics of those neighborhoods."), "<br>", "<br>",
                           i18n()$t("Below are articles on In-School Suspensions and the School-to-Prison Pipeline:"), "<br>", 
                           a(i18n()$t("COVID-19 and Gentrification in Durham"), 
                             href = "https://www.dukechronicle.com/article/2020/08/duke-university-covid-rages-housing-hits-home-gentrification-durham"), "<br>",
                           a(i18n()$t("Redlining in Durham"), 
                             href = "https://abc11.com/redlining-gentrification-black-neighborhoods-triangle/10373290/"),
                           a(i18n()$t("Durham’s Racial Inequality, Poverty, and Gentrification"), 
                             href = "https://fpg.unc.edu/publications/racial-inequality-poverty-and-gentrification-durham-north-carolina"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("gentrification")), 
                           i18n()$t(": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents"))
                     
                   }
                   else if (input$ms_select == "BIPOC Students per School"){
                     paste(i18n()$t("This dataset shows the percentage of BIPOC students in each of the 16 schools. Each of the schools are “majority students of color” which means representation of these students and" 
                     ), 
                     HTML(paste0(strong(i18n()$t("culturally-responsive pedagogy")),tags$sup("1"))), 
                     i18n()$t("is integral for student success. Too often, BIPOC students are underrepresented and cannot relate to content/curriculum because of cultural differences. Students who are able to connect with their tutors, teachers, administrators, etc. tend to engage with the content more."), "<br>","<br>",
                     i18n()$t("Below is more information about BIPOC students:"), "<br>",
                     a(i18n()$t("Racial/Ethnic Enrollment in NC Public Schools"),
                       href="https://nces.ed.gov/programs/coe/pdf/coe_cge.pdf"),
                     a(i18n()$t("Why Representation Matters"),
                       href="https://givingcompass.org/article/why-teacher-representation-matters-to-students-of-color/"),
                     a(i18n()$t("Benefits of POC Representation"),
                       href="https://educationpost.org/students-of-color-need-to-see-more-people-of-color-that-shouldnt-be-controversial/"),
                     "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("culturally-responsive pedagogy")), i18n()$t(": style of individualized teaching that is cognizant of the varying cultures and ethnicities of the classroom")
                     )
                     
                   }
                   else if (input$ms_select == "Racial Demographics"){
                     paste(i18n()$t("This dataset shows the racial breakdown of each of the 16 public schools. Durham Public Schools’ student population is 80.7% BIPOC students, but only 57% of Durham County is people of color. The racial demographics of all 16 schools has changed over time, specifically in the past 30 years. The number of white students has decreased, while the number of BIPOC students has increased."),
                           "<br>", "<br>", i18n()$t("Representation of these students and "), 
                           HTML(paste0(strong(i18n()$t("culturally-responsive pedagogy")),tags$sup("1"))), 
                           i18n()$t("is integral for student success. Too often BIPOC students are underrepresented and cannot relate to the content because of cultural differences. Students who are able to connect with their tutors, teachers, administrators, and the content tend to engage with the content more."), "<br>","<br>",
                           i18n()$t("Below is more information about racial demographics in schools:"), "<br>",
                           a(i18n()$t("Changing School Racial Demographics in Recent Decades"),
                             href="https://www.urban.org/features/explore-your-schools-changing-demographics"), "<br>",
                           a(i18n()$t("More BIPOC students in Public schools"),
                             href="https://www.publicschoolreview.com/blog/white-students-are-now-the-minority-in-u-s-public-schools"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("culturally-responsive pedagogy")), i18n()$t(": style of individualized teaching that is cognizant of the varying cultures and ethnicities of the classroom"))
                     
                     
                   }
                   else if (input$ms_select == "Median Household Income"){
                     paste(i18n()$t("This graph shows the median household income for each school zone. According to the 2020 US census, the median household income in the United States is $67,521. This is a 2.9% decrease from 2019, which is the first statistically significant decline since 2011."), 
                           "<br>","<br>", i18n()$t("According to the 2020 US census, the average household income in North Carolina is $56,642, while the per capita income in the past twelve months is $31,993. All of these schools fall below the NC and US median, which is a cause for concern. Poverty directly affects how students learn and those suffering from food insecurity, for example, are hindered from reaching their full potential. Students’ basic human needs must be met in order to excel in the classroom."), "<br>",
                           "<br>", i18n()$t("Below are links to the US Census Information:"),
                           "<br>",
                           a(i18n()$t("Nationwide Census"), 
                             href="https://www.census.gov/quickfacts/fact/table/US/PST045219"), "<br>", 
                           a(i18n()$t("North Carolina Census"),
                             href="https://www.census.gov/quickfacts/NC"))
                     
                   }
                   else if (input$ms_select == "Median Homesale Price"){
                     paste(i18n()$t("This graph shows the average home sale price for each school zone. In 2021, the median home sale price for the United States is $397,100 and the average home sale price is $464,200, according to the US Census Bureau. Specifically for the South, the median home sale price is $355,900 and the average home sale price is $411,100."),
                           "<br>", "<br>", 
                           i18n()$t("Due to the COVID-19 Pandemic, home prices increased, making the cost of living increase as well, all contributing to "), 
                           strong(i18n()$t("gentrification")),HTML(paste0(tags$sup("1"))),
                           i18n()$t(". Most of these school zones fall above the North Carolina median. Because Durham has become an increasingly expensive city to live in, many of the students in the ten schools required free and reduced lunch."), "<br>", "<br>",
                           i18n()$t("Below is more information about home sale price:"), "<br>",
                           a(i18n()$t("Home Sale Price Data"),
                             href="https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx"), "<br>",
                           a(i18n()$t("Covid-19 Increase"),
                             href="https://www.cnbc.com/2021/06/16/typical-us-home-price-up-record-13point2percent-compared-to-last-year.html"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("gentrification")), i18n()$t(": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents"))
                   }
                   else if (input$ms_select == "Bachelor Degree Rate"){
                     paste(i18n()$t("This graph shows the percentage of adults with bachelor’s degrees in each school zone. The number of individuals with bachelor’s degrees greatly differs across racial, income, and gender groups. Additionally, individuals with more degrees tend to have greater household incomes."), "<br>", "<br>",
                           i18n()$t("Below is more information about bachelor degree rates:"), "<br>",
                           a(i18n()$t("Bachelor’s Degrees and Race"),
                             href="https://nces.ed.gov/fastfacts/display.asp?id=72"), "<br>",
                           a(i18n()$t("Bachelor’s Degrees and Income"),
                             href="https://www.bls.gov/careeroutlook/2018/data-on-display/education-pays.htm"))
                     
                   }
                   else if (input$ms_select == "Sidewalk Coverage"){
                     paste(i18n()$t("Areas without sidewalk coverage can become inaccessible for people without cars or other modes of transportation, both private and public. Sidewalks are needed for individuals to safely walk to places such as school, grocery stores, parks, etc. Especially for younger students, sidewalks are essential for their safety. According to a UNC Highway Safety Research Center study, the probability of a crash occurring near or at a paved sidewalk is 88.2% lower than an unpaved site."),"<br>", "<br>", 
                           i18n()$t("Sidewalks also promote more physical activity and healthier lifestyles. In the US specifically, obesity rates have been steadily increasing across all populations and ages. Sidewalk coverage provides people with a safe and accessible way to increase their daily physical activity.") ,"<br>", "<br>",
                           i18n()$t("High income areas tend to have more sidewalk coverage than lower income areas. 89% of high-income communities have sidewalks, whereas only 49% of low income communities have sidewalk coverage."), "<br>", "<br>",
                           i18n()$t("Below is more information about sidewalk coverage:"), "<br>",
                           a(i18n()$t("Importance of Sidewalks"),
                             href="http://guide.saferoutesinfo.org/engineering/sidewalks.cfm"), "<br>",
                           a(i18n()$t("Income Disparities and Sidewalk Coverage"),
                             href="https://www.cityofeastlansing.com/DocumentCenter/View/1583/Income-Disparities-in-Street-Features-That-Encourage-Walking-PDF"))
                     
                   }
                   else if (input$ms_select == "Median Age"){
                     paste(i18n()$t("This dataset shows the median age of residents in each school zone. The median age of residents in a specific school zone can determine the various assets available, identify beneficial resources in the community, and give some insight about school enrollment numbers in the future."))
                     
                   }
                   else if (input$ms_select == "Titles Per Student"){
                     paste(i18n()$t("A school library’s number of book titles per student indicates the availability of different book titles for students to select. A higher number of book titles per student indicates a wider variety of books to choose from. Having more book titles per student enables more opportunities for whole-class novel reading, which is essential for learning book analysis, encouraging engaging discussions, and prompting creative writing."), 
                           "<br>", "<br>",
                           i18n()$t("Having a wide selection of books in a library also promotes more reading and intellectual stimulation for students. With a higher number of titles per student, it is more likely that most, if not all, students can find literature that engages them."),
                           "<br>","<br>",
                           i18n()$t("Below are more resources on the importance of book titles:"), 
                           "<br>",
                           a(i18n()$t("Importance of Culturally-Diverse Literature"),
                             href = "https://ila.onlinelibrary.wiley.com/doi/full/10.1002/trtr.1326"), "<br>",
                           a(i18n()$t("Creating an Effective and Diverse Classroom Library"),
                             href = "https://digitalcommons.wou.edu/cgi/viewcontent.cgi?article=1187&context=theses"))
                     
                   }
                   else if (input$ms_select == "WiFi Access Points Per Classroom"){
                     paste(strong(i18n()$t("Note: ")), i18n()$t("Due to the COVID-19 pandemic, this data only extends until December of 2020. Therefore, only the 2019-2020 school year is reflected above."),
                           "<br>", "<br>",
                           i18n()$t("The visualization depicts the number of wireless access points for each classroom. A school with a higher number of access points overall will have better internet coverage and quality than a school with less access points."),
                           "<br>", "<br>",
                           i18n()$t("As society modernizes, the internet has become a vital resource for all. Specifically in schools, teachers can display information and teach through slideshows, animations, videos, games, and more. Incorporating various forms of online tools can make the students become more engaged, while enabling the educator to explore new teaching methods. As a result, students will become more comfortable with using technology."),
                           "<br>", "<br>",
                           i18n()$t("However, not all students have the same access to technology in their homes. Most schools have accounted for this disparity by providing students with free tablets or laptops. During the COVID-19 pandemic, when students of all ages had to adjust to online instruction, several counties provided WiFI hubs so that students’ learning was not hindered."),
                           "<br>", "<br>",
                           i18n()$t("Below are more resources on Wi-Fi Access:"), "<br>",
                           a(i18n()$t("Impact of High-Speed Internet"),
                             href = "https://digitalcommons.unomaha.edu/cgi/viewcontent.cgi?article=1050&context=studentwork"))
                   }
                 }))
    
    observeEvent(i18n(),
                 output$hs_resources <- renderText({
                   if(input$hs_select == "Advanced Placement (AP) Course Enrollment") {
                     paste(i18n()$t("Advanced Placement (AP) courses are challenging, collegiate-level courses that are offered to high school students. AP courses weigh more than honors courses on the high school level. Students can take these classes for an opportunity to receive college credit upon scoring a three or higher (out of five) on the standardized assessment, which saves the student money on college tuition. AP classes also serve as a way for students to be placed into higher-level courses at their college."), "<br>","<br>",
                           i18n()$t("Below is more information about AP courses:"), "<br>", 
                           a(i18n()$t("NCDPI AP Courses"), 
                             href = "https://www.dpi.nc.gov/students-families/enhanced-opportunities/advanced-learning-and-gifted-education/advanced-coursework/advanced-placement"), "<br>",
                           a(i18n()$t("DPS AP Courses"), 
                             href = "https://www.dpsnc.net/Page/430"),
                           a(i18n()$t("College Board"),
                             href="https://apstudents.collegeboard.org/course-index-page"))
                     
                     
                   }
                   else if (input$hs_select == "Average Class Size"){
                     paste(i18n()$t("Research proves smaller class size is beneficial to student achievement. Smaller classes allow for the teacher to focus less on classroom management, and more on centralized learning. Students have stated they feel more comfortable in smaller classes as well."),"<br>","<br>",
                           i18n()$t("Resources Discussing the Importance of Class Size:"),"<br>",
                           a(i18n()$t("The Benefits of Investing in Smaller Class Sizes"),
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/educators-and-parents-reset-class-size-debate"), "<br>",
                           a(i18n()$t("State Policy View on Class Size"),
                             href = "https://www.brookings.edu/research/class-size-what-research-says-and-what-it-means-for-state-policy/"))     
                     
                   } 
                   else if (input$hs_select == "CTE Course Enrollment Rate, High School"){
                     paste(i18n()$t("Career and Technical Education (CTE) courses are designed for high school students to receive real-world experience in the career field they are most interested in. Durham Public Schools started the “3-2-1” initiative in 2019 where all high school students are required to take three CTE courses, participate in two career exposure activities, and get an internship or a job before they graduate. This initiative, as well as all CTE courses, are created to develop students’ soft skills, gain real-world experience, and help students decide on their post-graduate plans."), "<br>","<br>",
                           i18n()$t("Below is more information about CTE courses in Durham Public Schools and North Carolina:"), "<br>",
                           a(i18n()$t("DPS CTE Course Initiative"),
                             href = "https://www.dpsnc.net/domain/293"), "<br>",
                           a(i18n()$t("NCDPI CTE Course Overview"),
                             href = "https://www.dpi.nc.gov/districts-schools/classroom-resources/career-and-technical-education"))
                     
                     
                   }
                   else if (input$hs_select == "Experienced Teacher Ratio") {
                     paste(i18n()$t("Experienced teachers are those who have approximately five or more years of experience with teaching. Although more experienced teachers tend to perform better on their evaluations, research shows teacher experience does not directly correlate to effective teaching. Some schools tend to be more attracted to veteran teachers whereas other schools may prefer employing recently certified teachers that can provide current and unique teaching styles."),
                           "<br>", "<br>", 
                           i18n()$t("With the evolution of research-based practices, it is important to continue to educate new and returning teachers on the best teaching practices that are scientifically-proven to promote student success. Good professional development workshops are paramount to provide effective, culturally-responsive teaching practices."), "<br>","<br>",i18n()$t("Below are articles on Teacher Experience and Professional Development:"), "<br>",
                           a(i18n()$t("Effects of Hiring and Recommendations for Supporting Experienced Teachers"), 
                             href = "https://learningpolicyinstitute.org/product/does-teaching-experience-increase-teacher-effectiveness-review-research"),"<br>",
                           a(i18n()$t("Teacher Experience in Relation to Teacher Effectiveness"), 
                             href ="https://www.nea.org/advocating-for-change/new-from-nea/does-teaching-experience-matter-lets-count-ways"),"<br>",
                           a("NCDPI Educator Professional Development", 
                             href ="https://www.dpi.nc.gov/educators/professional-development"),"<br>",
                           a(i18n()$t("Importance and Resources for Professional Development"), 
                             href ="https://www.nea.org/professional-excellence/professional-learning/teachers"))
                     
                   } 
                   else if (input$hs_select == "Free and Reduced Lunch") {
                     paste(i18n()$t("The percentage of students receiving free and reduced lunch is a strong indicator of socioeconomic status. The percentage of students that fall below the poverty line determines if a school is considered "), strong(i18n()$t("Title I")),HTML(paste0(tags$sup("1"))), ".", 
                           i18n()$t("Title I schools are eligible to receive grants through the Every Student Succeeds Act (ESEA). It is important to be cognizant of students’ socioeconomic status without being condescending and discriminatory. Socioeconomic status is not a limit, it is a barrier."), "<br>","<br>",
                           i18n()$t("Below are articles on Free/Reduced Lunch and its connection to Title I schools:"), "<br>",
                           a(i18n()$t("Rural Schools and Free/Reduced Lunch"), 
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/whos-looking-out-rural-schools"), "<br>",
                           a(i18n()$t("NC Community Free/Reduced Lunch Eligibility"),
                             href = "https://www.dpi.nc.gov/districts-schools/federal-program-monitoring/title-i-part/community-eligibility-free-and-reduced-price-meals"),
                           "<br>","<br>", HTML(paste0(tags$sup("1"))),
                           strong(i18n()$t("Title I")), 
                           i18n()$t(": Under the ESEA, this federally funded program identifies schools with a majority of low-income students, based on free and reduced lunch statistics."
                           ))
                     
                   } 
                   else if (input$hs_select == "Student-Teacher Ratio, High School"){
                     paste(i18n()$t("Research proves smaller class size is beneficial to student achievement. Smaller classes allow for the teacher to focus less on classroom management, and more on centralized learning. Students have stated they feel more comfortable in smaller classes as well."),"<br>","<br>",
                           i18n()$t("For high school students especially, classes become more challenging as they become older and begin to take more advanced courses, such as AP or IB classes. In these higher-stress environments, smaller sized classrooms enable teachers to better gauge if the students are making progress and fully understanding the material. Additionally, educators have the opportunity to make the curriculum and assignments more personalized to cater to the student body."), "<br>","<br>",
                           i18n()$t("Resources on Student-Teacher Ratios:"),"<br>",
                           a(i18n()$t("Infographics and Information on Student-Teacher Ratios"),
                             href = "https://www.hunschool.org/resources/student-teacher-ratios"))
                     
                     
                   } 
                   else if (input$hs_select == "Students Per Device"){
                     paste(i18n()$t("Living in a digital age, technology usage in the classroom has increased tremendously, especially during the COVID-19 pandemic. Although technology is a great resource, students may not have equitable access to these devices. It is important for students and teachers to not only have access to these technological devices, but also understand how to use them, which is why professional development is so important. Professional development is necessary to keep educators up to date on new technology to create the most effective learning environment."),"<br>","<br>",
                           i18n()$t("Resources on Access and Technology in the Classroom:"), "<br>",
                           a(i18n()$t("Durham Public Schools’ Technological Services"),
                             href = "https://www.dpsnc.net/site/default.aspx?PageType=3&DomainID=207&ModuleInstanceID=8115&ViewID=6446EE88-D30C-497E-9316-3F8874B3E108&RenderLoc=0&FlexDataID=42210&PageID=4738"), "<br>",
                           a(i18n()$t("Equitable Access to Technology"),
                             href = "https://digitalpromise.org/2019/04/29/equity-in-schools-access-technology/"))
                     
                   } 
                   else if (input$hs_select == "Funding Per Pupil"){
                     paste(i18n()$t("This indicator represents the amount that local, state, and federal governments spend on elementary and secondary education adjusted for the size of the student body. It is calculated by dividing the current expenditures over the entire academic year for Pre-Kindergarten through Grade 12 by the number of students in those grades in public schools. Current expenditures include instruction and instruction-related costs, student support services, administration, and operations. Excluded from those expenditures are funds for school construction and other capital outlays, debt service, and programs outside of public elementary and secondary education."), "<br>", "<br>",
                           i18n()$t("Resources on public school funding:"), "<br>",
                           a(i18n()$t("Interactive Summary of Local - Federal Public School Funding:"),
                             href="https://www.dpsnc.net/Page/3771"),"<br>",
                           a(i18n()$t("New Per Pupil Expenditure Requirements"),
                             href ="https://www.naesp.org/blog/new-per-pupil-expenditure-requirements/"))
                     
                   } 
                   else if (input$hs_select == "Students With Disabilities"){
                     paste(i18n()$t("According to the Americans with Disabilities Act, an individual is considered to have a disability if they have a condition that impairs them to do certain activities and interact with those around them. It is integral to make sure students with disabilities are provided with accessibility services to achieve their full potential in the classroom. Resources like"), 
                           HTML(paste0(strong(i18n()$t("assistive technology")),tags$sup("1"))), 
                           (",transportation,"), 
                           HTML(paste0(strong(i18n()$t("Exceptional Children (EC) programs")),tags$sup("2"))), 
                           i18n()$t(", etc. are mandatory for every school to provide regardless of the number of students with disabilities or even the type based on the civil rights law Section 504."), "<br>", "<br>",
                           i18n()$t("Below are articles and resources about government protection and resources for students with disabilities:"), "<br>",
                           a(i18n()$t("DPS EC Services"),
                             href="https://www.dpsnc.net/Page/169"), "<br>",
                           a(i18n()$t("Section 504"),
                             href="https://www.dpsnc.net/Page/336"),"<br>",
                           a(i18n()$t("NCDPI’s EC Division"),
                             href="https://www.dpi.nc.gov/districts-schools/classroom-resources/exceptional-children-division"),"<br>",
                           a(i18n()$t("Assistive Technology"),
                             href="https://www.disabilityrightswa.org/publications/assistive-technology-special-education-students/"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("assistive technology")), 
                           i18n()$t(": supplementary devices that promote independence and functionality of people with varying disabilities"),
                           "<br>", HTML(paste0(tags$sup("2"))), strong(i18n()$t("EC programs")), 
                           i18n()$t(": school programs that include students that need intense or individualized instruction in addition to their standard course of study")
                     )
                     
                   }  
                   else if (input$hs_select == "English as a Second Language (ESL) Student Enrollment"){
                     paste(i18n()$t("This graph shows the number of students enrolled in the English as a Second Language (ESL) or English Language Learners (ELL) Program. ESL students consist of any student regardless of ethnicity, origin, race, age, etc. who is a non-native English speaker. These programs are created to help children learn English along with other subjects necessary to complete each grade."), "<br>","<br>", 
                           i18n()$t("Unfortunately, ESL students can face racial bias, discrimination, and bullying in the classroom. Teachers may prevent them from participating in school activities, extracurriculars, and enrichment programs. It is important that school affiliates recognize that language barriers do not stunt intellectual development. Additionally, ELL teachers must accommodate their students instead of assimilating them by removing the identity of their native language entirely."), "<br>","<br>",
                           i18n()$t("Below are resources and information on culturally-responsive teaching and Durham Public Schools’ approaches to ESL/ELL programs:"), "<br>",
                           a(i18n()$t("How to Advocate for ESL/ELL Students"),
                             href = "https://www.nea.org/professional-excellence/student-engagement/tools-tips/english-language-learners-what-you-need-know"), "<br>",
                           a(i18n()$t("Real-World Experiences and Anecdotes"),
                             href = "https://www.learningforjustice.org/magazine/summer-2017/a-case-for-acculturation"), "<br>",
                           a(i18n()$t("DPS ESL Office"),
                             href = "https://central.dpsnc.net/esl"), "<br>",
                           a(i18n()$t("ESL/ELL Teachers"),
                             href = "https://www.eslteacheredu.org/what-is-an-esl-teacher/"))
                     
                   }  
                   else if (input$hs_select == "In-School Suspensions (ISS)"){
                     paste(i18n()$t("In-school suspensions are described as: Instances in which a child is temporarily removed from his or her regular classrooms for at least half a day but remains under the direct supervision of school personnel."), 
                           i18n()$t("BIPOC students are more susceptible to harsher punishments in schools. Black and Brown students are subject to higher disciplinary actions compared to their white peers. A reason for this is racial bias leading to the over policing of Black and Brown students, fueling the"),
                           strong(i18n()$t("school-to-prison pipeline")),HTML(paste0(tags$sup("1"))), ".", "<br>","<br>",
                           i18n()$t("Below are articles on In-School Suspensions and the School-to-Prison Pipeline:"), "<br>", 
                           a(i18n()$t("Racial Bias in School Discipline"), 
                             href = "https://www.pnas.org/content/116/17/8255"), "<br>",
                           a(i18n()$t("School-to-Prison Pipeline"), 
                             href = "https://www.nea.org/advocating-for-change/new-from-nea/school-prison-pipeline-time-shut-it-down"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("school-to-prison pipeline")), i18n()$t(": the overly disproportionate policing of minority students, often from low-income households, that leads to higher punishments including ISS, OSS (out-of-school suspension), juvenile detention, etc.")
                     )
                     
                     
                   }
                   else if(input$hs_select == "Enrollment") {
                     paste(i18n()$t("This dataset shows the enrollment numbers at each school. Due to the COVID-19 pandemic, there has been a 5% decrease in enrollment. Normally, enrollment or average daily membership (ADM) is used to determine funding for each school. The higher the ADM, the more money a school receives. Recently, lawmakers passed a bill stating that they would omit schools from reduction in funding as part of COVID-19 relief. Durham Public Schools continues to strive for increased enrollment so that all programs can continue to receive adequate funding."), "<br>","<br>", 
                           i18n()$t("Below is information about enrollment numbers:"), "<br>",
                           a(i18n()$t("The Decrease in Enrollment in North Carolina"), 
                             href = "https://abc11.com/nc-schools-school-attendance-enrollment-durham-county/8204335/")
                     )
                     
                   }
                   else if (input$hs_select == "School and Zone BIPOC Comparison") {
                     paste(i18n()$t("This plot shows the percentage of BIPOC students in the school compared to the percentage of people of color in the school zone. This measurement shows the huge disparities in community representation for BIPOC students as most of these schools are not racially reflective of the school zones they inhabit. A contributing factor of this is ") , HTML(paste0(strong(i18n()$t("gentrification")),tags$sup("1"))),".", 
                           i18n()$t("Gentrification has been an increasing problem, and the COVID-19 pandemic has only exacerbated it. Black and Brown neighborhoods are becoming increasingly expensive forcing families out of their homes, which ultimately changes the demographics of those neighborhoods."), "<br>", "<br>",
                           i18n()$t("Below are articles on In-School Suspensions and the School-to-Prison Pipeline:"), "<br>", 
                           a(i18n()$t("COVID-19 and Gentrification in Durham"), 
                             href = "https://www.dukechronicle.com/article/2020/08/duke-university-covid-rages-housing-hits-home-gentrification-durham"), "<br>",
                           a(i18n()$t("Redlining in Durham"), 
                             href = "https://abc11.com/redlining-gentrification-black-neighborhoods-triangle/10373290/"),
                           a(i18n()$t("Durham’s Racial Inequality, Poverty, and Gentrification"), 
                             href = "https://fpg.unc.edu/publications/racial-inequality-poverty-and-gentrification-durham-north-carolina"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("gentrification")), 
                           i18n()$t(": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents"))
                     
                   }
                   else if (input$hs_select == "BIPOC Students per School"){
                     paste(i18n()$t("This dataset shows the percentage of BIPOC students in each of the 16 schools. Each of the schools are “majority students of color” which means representation of these students and" 
                     ), 
                     HTML(paste0(strong(i18n()$t("culturally-responsive pedagogy")),tags$sup("1"))), 
                     i18n()$t("is integral for student success. Too often, BIPOC students are underrepresented and cannot relate to content/curriculum because of cultural differences. Students who are able to connect with their tutors, teachers, administrators, etc. tend to engage with the content more."), "<br>","<br>",
                     i18n()$t("Below is more information about BIPOC students:"), "<br>",
                     a(i18n()$t("Racial/Ethnic Enrollment in NC Public Schools"),
                       href="https://nces.ed.gov/programs/coe/pdf/coe_cge.pdf"),
                     a(i18n()$t("Why Representation Matters"),
                       href="https://givingcompass.org/article/why-teacher-representation-matters-to-students-of-color/"),
                     a(i18n()$t("Benefits of POC Representation"),
                       href="https://educationpost.org/students-of-color-need-to-see-more-people-of-color-that-shouldnt-be-controversial/"),
                     "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("culturally-responsive pedagogy")), i18n()$t(": style of individualized teaching that is cognizant of the varying cultures and ethnicities of the classroom")
                     )
                     
                   }
                   else if (input$hs_select == "Racial Demographics"){
                     paste(i18n()$t("This dataset shows the racial breakdown of each of the 16 public schools. Durham Public Schools’ student population is 80.7% BIPOC students, but only 57% of Durham County is people of color. The racial demographics of all 16 schools has changed over time, specifically in the past 30 years. The number of white students has decreased, while the number of BIPOC students has increased."),
                           "<br>", "<br>", i18n()$t("Representation of these students and "), 
                           HTML(paste0(strong(i18n()$t("culturally-responsive pedagogy")),tags$sup("1"))), 
                           i18n()$t("is integral for student success. Too often BIPOC students are underrepresented and cannot relate to the content because of cultural differences. Students who are able to connect with their tutors, teachers, administrators, and the content tend to engage with the content more."), "<br>","<br>",
                           i18n()$t("Below is more information about racial demographics in schools:"), "<br>",
                           a(i18n()$t("Changing School Racial Demographics in Recent Decades"),
                             href="https://www.urban.org/features/explore-your-schools-changing-demographics"), "<br>",
                           a(i18n()$t("More BIPOC students in Public schools"),
                             href="https://www.publicschoolreview.com/blog/white-students-are-now-the-minority-in-u-s-public-schools"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("culturally-responsive pedagogy")), i18n()$t(": style of individualized teaching that is cognizant of the varying cultures and ethnicities of the classroom"))
                     
                     
                   }
                   else if (input$hs_select == "Median Household Income"){
                     paste(i18n()$t("This graph shows the median household income for each school zone. According to the 2020 US census, the median household income in the United States is $67,521. This is a 2.9% decrease from 2019, which is the first statistically significant decline since 2011."), 
                           "<br>","<br>", 
                           i18n()$t("According to the 2020 US census, the average household income in North Carolina is $56,642, while the per capita income in the past twelve months is $31,993. All of these schools fall below the NC and US median, which is a cause for concern. Poverty directly affects how students learn and those suffering from food insecurity, for example, are hindered from reaching their full potential. Students’ basic human needs must be met in order to excel in the classroom."), "<br>",
                           "<br>", i18n()$t("Below are links to the US Census Information:"),
                           "<br>",
                           a(i18n()$t("Nationwide Census"), 
                             href="https://www.census.gov/quickfacts/fact/table/US/PST045219"), "<br>", 
                           a(i18n()$t("North Carolina Census"),
                             href="https://www.census.gov/quickfacts/NC"))
                     
                   }
                   else if (input$hs_select == "Median Homesale Price"){
                     paste(i18n()$t("This graph shows the average home sale price for each school zone. In 2021, the median home sale price for the United States is $397,100 and the average home sale price is $464,200, according to the US Census Bureau. Specifically for the South, the median home sale price is $355,900 and the average home sale price is $411,100."),
                           "<br>", "<br>", 
                           i18n()$t("Due to the COVID-19 Pandemic, home prices increased, making the cost of living increase as well, all contributing to "), 
                           strong(i18n()$t("gentrification")),HTML(paste0(tags$sup("1"))),
                           i18n()$t(". Most of these school zones fall above the North Carolina median. Because Durham has become an increasingly expensive city to live in, many of the students in the ten schools required free and reduced lunch."), "<br>", "<br>",
                           i18n()$t("Below is more information about home sale price:"), "<br>",
                           a(i18n()$t("Home Sale Price Data"),
                             href="https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx"), "<br>",
                           a(i18n()$t("Covid-19 Increase"),
                             href="https://www.cnbc.com/2021/06/16/typical-us-home-price-up-record-13point2percent-compared-to-last-year.html"),
                           "<br>", "<br>",HTML(paste0(tags$sup("1"))), strong(i18n()$t("gentrification")), i18n()$t(": the process of changing low-income neighborhoods, usually with a minority-majority, to market them to wealthier people (i.e. targeted businesses, flipping foreclosed homes, raising rent, etc.), ultimately displacing the current residents"))
                   }
                   else if (input$hs_select == "Bachelor Degree Rate"){
                     paste(i18n()$t("This graph shows the percentage of adults with bachelor’s degrees in each school zone. The number of individuals with bachelor’s degrees greatly differs across racial, income, and gender groups. Additionally, individuals with more degrees tend to have greater household incomes."), "<br>", "<br>",
                           i18n()$t("Below is more information about bachelor degree rates:"), "<br>",
                           a(i18n()$t("Bachelor’s Degrees and Race"),
                             href="https://nces.ed.gov/fastfacts/display.asp?id=72"), "<br>",
                           a(i18n()$t("Bachelor’s Degrees and Income"),
                             href="https://www.bls.gov/careeroutlook/2018/data-on-display/education-pays.htm"))
                     
                   }
                   else if (input$hs_select == "Sidewalk Coverage"){
                     paste(i18n()$t("Areas without sidewalk coverage can become inaccessible for people without cars or other modes of transportation, both private and public. Sidewalks are needed for individuals to safely walk to places such as school, grocery stores, parks, etc. Especially for younger students, sidewalks are essential for their safety. According to a UNC Highway Safety Research Center study, the probability of a crash occurring near or at a paved sidewalk is 88.2% lower than an unpaved site."),"<br>", "<br>", 
                           i18n()$t("Sidewalks also promote more physical activity and healthier lifestyles. In the US specifically, obesity rates have been steadily increasing across all populations and ages. Sidewalk coverage provides people with a safe and accessible way to increase their daily physical activity.") ,"<br>", "<br>",
                           i18n()$t("High income areas tend to have more sidewalk coverage than lower income areas. 89% of high-income communities have sidewalks, whereas only 49% of low income communities have sidewalk coverage."), "<br>", "<br>",
                           i18n()$t("Below is more information about sidewalk coverage:"), "<br>",
                           a(i18n()$t("Importance of Sidewalks"),
                             href="http://guide.saferoutesinfo.org/engineering/sidewalks.cfm"), "<br>",
                           a(i18n()$t("Income Disparities and Sidewalk Coverage"),
                             href="https://www.cityofeastlansing.com/DocumentCenter/View/1583/Income-Disparities-in-Street-Features-That-Encourage-Walking-PDF"))
                     
                   }
                   else if (input$hs_select == "Median Age"){
                     paste(i18n()$t("This dataset shows the median age of residents in each school zone. The median age of residents in a specific school zone can determine the various assets available, identify beneficial resources in the community, and give some insight about school enrollment numbers in the future."))
                     
                   }
                   else if (input$hs_select == "Graduation Rate"){
                     paste(i18n()$t("The graph depicts the percentage of students estimated to graduate from high school in four years or less. The graduation rate for North Carolina from 2020 is 87.6%, with plans to raise that percentage to 95% by 2030. This is a significant increase from when the first reported graduation rate was 68.3% in 2006."), "<br>", "<br>",
                           i18n()$t("Below are more resources on graduation rates:"), "<br>",
                           a(i18n()$t("Dashboard with Articles and Quick Facts about Graduation Rates"),
                             href = "https://dashboard.myfuturenc.org/college-and-career-access/high-school-graduation-rate/"))
                     
                   }
                   else if (input$hs_select == "Titles Per Student"){
                     paste(i18n()$t("A school library’s number of book titles per student indicates the availability of different book titles for students to select. A higher number of book titles per student indicates a wider variety of books to choose from. Having more book titles per student enables more opportunities for whole-class novel reading, which is essential for learning book analysis, encouraging engaging discussions, and prompting creative writing."), 
                           "<br>", "<br>",
                           i18n()$t("Having a wide selection of books in a library also promotes more reading and intellectual stimulation for students. With a higher number of titles per student, it is more likely that most, if not all, students can find literature that engages them."),
                           "<br>","<br>",
                           i18n()$t("Below are more resources on the importance of book titles:"), 
                           "<br>",
                           a(i18n()$t("Importance of Culturally-Diverse Literature"),
                             href = "https://ila.onlinelibrary.wiley.com/doi/full/10.1002/trtr.1326"), "<br>",
                           a(i18n()$t("Creating an Effective and Diverse Classroom Library"),
                             href = "https://digitalcommons.wou.edu/cgi/viewcontent.cgi?article=1187&context=theses"))
                     
                   }
                   else if (input$hs_select == "WiFi Access Points Per Classroom"){
                     paste(strong(i18n()$t("Note: ")), i18n()$t("Due to the COVID-19 pandemic, this data only extends until December of 2020. Therefore, only the 2019-2020 school year is reflected above."),
                           "<br>", "<br>",
                           i18n()$t("The visualization depicts the number of wireless access points for each classroom. A school with a higher number of access points overall will have better internet coverage and quality than a school with less access points."),
                           "<br>", "<br>",
                           i18n()$t("As society modernizes, the internet has become a vital resource for all. Specifically in schools, teachers can display information and teach through slideshows, animations, videos, games, and more. Incorporating various forms of online tools can make the students become more engaged, while enabling the educator to explore new teaching methods. As a result, students will become more comfortable with using technology."),
                           "<br>", "<br>",
                           i18n()$t("However, not all students have the same access to technology in their homes. Most schools have accounted for this disparity by providing students with free tablets or laptops. During the COVID-19 pandemic, when students of all ages had to adjust to online instruction, several counties provided WiFI hubs so that students’ learning was not hindered."),
                           "<br>", "<br>",
                           i18n()$t("Below are more resources on Wi-Fi Access:"), "<br>",
                           a(i18n()$t("Impact of High-Speed Internet"),
                             href = "https://digitalcommons.unomaha.edu/cgi/viewcontent.cgi?article=1050&context=studentwork"))
                   }
                   
                 })) 
    
  }
  
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
           "Community and Cultural Centers" = cultural, 
           "Grocery Stores" = grocery, 
           "Libraries" = libraries, 
           "Religious Centers" = religious,
           "Hospitals and Clinics" = hospitals,
           "After-School Care Programs" = afterschool,
           "Community Arts" = commarts,
           "Pharmacies" = pharmacies,
           "Restaurants" = restaurants, 
           "Homeless Shelters" = shelters,
           "Community Sports" = sports)
  })
  
  # Maps - Connecting map variables to their icons
  displayIcon <- reactive({
    switch(input$var,
           "Parks" = iconSet$parks, 
           "Recreation Centers" = iconSet$rec, 
           "Gardens" = iconSet$gardens, 
           "Bus Stops" = iconSet$bus, 
           "Childcare Centers" = iconSet$childcare, 
           "Community and Cultural Centers" = iconSet$cultural, 
           "Food Pantries" = iconSet$pantries,
           "Farmers' Markets" = iconSet$farmersmark,
           "Grocery Stores" = iconSet$grocery, 
           "Libraries" = iconSet$libraries, 
           "Religious Centers" = iconSet$religious,
           "Hospitals and Clinics" = iconSet$hospitals,
           "After-School Care Programs" = iconSet$afterschool,
           "Community Arts" = iconSet$commarts,
           "Pharmacies" = iconSet$pharmacies,
           "Restaurants" = iconSet$restaurants,
           "Homeless Shelters" = iconSet$shelters,
           "Community Sports" = iconSet$sports)
  })
  
  # Maps - Connecting name of school to input
  displaySchool <- reactive({
    schoolstats %>% filter(name == input$zone)
  })
  
  # Maps - Connecting school zone drop down menu to school info
  displayZone <- reactive({
    switch(input$zone,
           "Brogden Middle" = brogden,
           "C.C. Spaulding Elementary" = cc,
           "Club Boulevard Elementary" = clubblv,
           "Eastway Elementary" = eastway,
           "E.K. Powe Elementary" = ek, 
           "Fayetteville Street Elementary" = fayetteville, 
           "Forest View Elementary" = forest,
           "Hillandale Elementary" = hillandale,
           "Hillside High" = hillside,
           "Jordan High" = jordan,
           "Lakewood Elementary" = lakewoodES,
           "Lakewood Montessori Middle" = lakewoodMS,
           "Lowes Grove Middle" = lowesgr,
           "Parkwood Elementary" = parkwood, 
           "Riverside High" = riverside,
           "Southwest Elementary" = southwest,
           "Bethesda Elementary" = bethesda,
           "Burton Elementary" = burton, 
           "Carrington Middle" = carrington, 
           "City of Medicine Academy" = citymedicine, 
           "Creekside Elementary" = creekside,
           "Lakeview High" = lakeview,
           "Y.E. Smith Elementary" = yesmith,
           "W.G. Pearson Elementary" = wgpearson,
           "Spring Valley Elementary" = springvalley,
           "Southern School of Energy and Sustainability" = southernenergy,
           "Sherwood Githens Middle" = sherwoodgithens,
           "Shepard Middle" = shepardmiddle,
           "School for Creative Studies"= creativestudies,
           "Sandy Ridge Elementary" = sandyridge,
           "Rogers-Herr Middle" = rogersherr,
           "R.N. Harris Elementary" = rnharris,
           "Durham School of the Arts" = durhamschoolofarts, 
           "Easley Elementary" = easley, 
           "Eno Valley Elementary" = enovalley, 
           "George Watts Elementary" = georgewatts, 
           "Glenn Elementary" = glenn, 
           "Holt Elementary" = holt, 
           "Oak Grove Elementary" = oakgrove,
           "Mangum Elementary" = mangum,
           "Hope Valley Elementary" = hopevalley,
           "Holton Career" = holtoncareer, 
           "J.D. Clement Early College" = jdclement, 
           "Lucas Middle" = lucas, 
           "Merrick Moore Elementary" = merrickmoore, 
           "Middle College" = middlecollege, 
           "Morehead Montessori" = morehead, 
           "Neal Middle" = neal,
           "Northern High" = northern, 
           "Pearsontown Elementary"  = pearsontown,
           "All" = durham
    )
  })
  
  # Maps - School zone colors
  displayColor <- reactive({
    switch(input$zone,
           "Brogden Middle" = "purple",
           "C.C. Spaulding Elementary" = "red", 
           "Club Boulevard Elementary" = "purple",
           "Eastway Elementary" = "orange",
           "E.K. Powe Elementary" = "yellow", 
           "Fayetteville Street Elementary" = "green", 
           "Forest View Elementary" = "blue",
           "Hillandale Elementary" = "purple",
           "Hillside High" = "violet",
           "Jordan High" = "pink",
           "Lakewood Elementary" = "darkred", 
           "Lakewood Montessori Middle" = "purple",
           "Lowes Grove Middle" = "purple",
           "Parkwood Elementary" = "lightblue", 
           "Riverside High" = "purple",
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
  
  #helper function to make links look better and be clickable
  createLink <- function(val) {
    sprintf('<a href="%s" target="_blank">%s</a>',val, val)
  }
  
  #outputting the list of resources for that subset
  output$list <- renderDataTable({
    if(input$var == "Parks")
    {
      temp_df <- parks[grepl(input$zone, parks$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS","URL")]
    }
    else if(input$var == "Recreation Centers")
    {
      temp_df <- rec[grepl(input$zone, rec$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("NAME","ADDRESS","URL")]
    }
    else if(input$var == "Gardens")
    {
      temp_df <- gardens[grepl(input$zone, gardens$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("NAME","ADDRESS","URL")]
    }
    else if(input$var == "Bus Stops")
    {
      temp_df <- bus[grepl(input$zone, bus$school_zones), ]
      temp_df[c("STOP_NAME")]
    } 
    else if(input$var == "Childcare Centers")
    {
      temp_df <- childcare[grepl(input$zone, childcare$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","URL")]
    }
    else if(input$var == "Community and Cultural Centers")
    {
      temp_df <- cultural[grepl(input$zone, cultural$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS", "URL")]
    }
    else if(input$var == "Grocery Stores")
    {
      temp_df <- grocery[grepl(input$zone, grocery$school_zones), ]
      temp_df[c("STORE_NAME","ADDRESS")]
    }
    else if(input$var == "Libraries")
    {
      temp_df <- libraries[grepl(input$zone, libraries$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS", "URL")]
    }
    else if(input$var == "Religious Centers")
    {
      temp_df <- religious[grepl(input$zone, religious$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name", "URL")]
    }
    else if(input$var == "Hospitals and Clinics")
    {
      temp_df <- hospitals[grepl(input$zone, hospitals$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS", "URL")]
    }
    else if(input$var == "After-School Care Programs")
    {
      temp_df <- afterschool[grepl(input$zone, afterschool$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS", "URL")]
    }
    else if(input$var == "Food Pantries")
    {
      temp_df <- pantries[grepl(input$zone, pantries$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS", "URL")]
    }
    else if(input$var == "Farmers' Markets")
    {
      temp_df <- farmersmark[grepl(input$zone, farmersmark$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","ADDRESS", "URL")]
    }
    else if(input$var == "Community Arts")
    {
      temp_df <- commarts[grepl(input$zone, commarts$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","Type","ADDRESS", "URL")]
      
    }
    else if(input$var == "Pharmacies")
    {
      temp_df <- pharmacies[grepl(input$zone, pharmacies$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name", "ADDRESS", "URL")]
    }
    else if(input$var == "Homeless Shelters")
    {
      temp_df <- shelters[grepl(input$zone, shelters$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name", "ADDRESS", "URL")]
    }
    else if(input$var == "Restaurants")
    {
      temp_df <- restaurants[grepl(input$zone, restaurants$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("NAME","TYPE", "ADDRESS", "URL")]
    }
    else if(input$var == "Community Sports")
    {
      temp_df <- sports[grepl(input$zone, sports$school_zones), ]
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("name","Type","ADDRESS", "URL")]
    }
  }, escape = FALSE, options = list(pageLength = 5, scrollX = TRUE)
  )
  
  
  
  #Engagement tab 
  #translations
  observeEvent(i18n(),
  output$engage_context <- renderText({
    paste(title = strong(i18n()$t("Context"), style = "font-size:20px"),
          br(),
          i18n()$t("From "),
          a(i18n()$t("creating free math workshops for girls who attend Durham Public Schools "), href = "https://trinity.duke.edu/news/how-trinity-faculty-and-students-are-sharing-resources-support-durham-public-schools"),
          i18n()$t("to "), 
          a(i18n()$t("faculty research projects that engage with "), href = "https://facultyadvancement.duke.edu/seven-faculty-projects-community-impact-racial-and-social-equity-issues"),
          a(i18n()$t("“Racial and Social Equity in Local Contexts”, "), href = "https://facultyadvancement.duke.edu/racial-and-social-equity-local-context-engaging-durhams-priority-areas-community-impact"),
          i18n()$t("Duke and NCCU staff and faculty and undergraduate students are leading DPS-facing initiatives to form purposeful partnerships between Duke, NCCU, and DPS. This list - which has been categorized by service-oriented initiatives, teaching/learning-oriented initiatives, and research-oriented initiatives - includes opportunities that support the whole-child and whole-community framework of community schools, both inside and outside programs of education. All data was derived from "),
          a(i18n()$t("NCCU, "), href="https://www.nccu.edu/"),
          a(i18n()$t("Duke, "), href="https://duke.edu/"),
          i18n()$t("and, "),
          a(i18n()$t("Duke CampusGroups."), href="https://dukegroups.com/home_login"))
    
  }))

observeEvent(i18n(),
  output$engage_service <- renderText({
    paste(i18n()$t("Service"))
  }))

observeEvent(i18n(),
             output$engage_research <- renderText({
               paste(i18n()$t("Research"))
             }))

observeEvent(i18n(),
             output$engage_teach <- renderText({
               paste(i18n()$t("Teaching and Learning"))
             }))

  #plots
  #service
  output$engagetable_1 <- renderDataTable({
    if(input$tab1 == "Staff/Faculty")
    {
      temp_df <- faculty_service
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("School","Name","URL","Subject")]
    }
    
    else if(input$tab1 == "Undergraduate Students"){
      temp_df <- student_service
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("School","Name","URL","Subject")]
    }
    
  }, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  
  #research
  output$engagetable_3 <- renderDataTable({
    if(input$tab3 == "Staff/Faculty")
    {
      temp_df <- faculty_research
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("School","Name","URL","Subject")]
    }
    
    else if(input$tab3 == "Undergraduate Students"){
      temp_df <- student_research
      temp_df$URL <- createLink(temp_df$URL)
      temp_df[c("School","Name","URL","Program")]
    }
    
  }, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  
  
  #Engagement Tab - Carousal
  output$carou <- renderSlickR({
    imgs <- list.files(path = "data/2023/engagement_slides", pattern = "*.png", full.names = TRUE)
    slickR(imgs, width = 200, height = 200) + settings(autoplay = TRUE,
                                                       slidesToShow = 4,
                                                       slidesToScroll = 1)
  })
  
  # output$choropleth <- renderLeaflet({
  #   leaflet(
  #     displayZone()) %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  #     addResetMapButton() %>%
  #     addPolygons(data = durham,
  #                 fillColor = displayColor(),
  #                 stroke = TRUE,
  #                 fillOpacity = 0.39,
  #                 smoothFactor = 1)
  # })
  
  output$insights_individualplots <- renderPlotly({
    counts_grouped<-counts_grouped_2021
    counts_grouped<-counts_grouped_2021[!(counts_grouped_2021$name=="All School"),]
    counts_grouped$name <- str_replace_all(counts_grouped$name, 'Lakewood Elementary School', 'Lakewood Elementary')
    counts_grouped$name <- str_replace_all(counts_grouped$name, 'Lakewood Middle School', 'Lakewood Middle')
    #counts_grouped$name <- str_replace_all(counts_grouped$name, ' School', '')
    counts_grouped <- subset(counts_grouped, name == input$insights_zone)
    ggplot(data=counts_grouped, aes(x=varname, y=count)) + geom_bar(stat="identity", fill="lightblue") + coord_flip(ylim=c(0,200)) +
      ylab("Number of Resource") + xlab("Resource Type")
  })
  
  observeEvent(i18n(),
               output$context <- renderText({
                 if(input$var == "After-School Care Programs"){
                   paste(i18n()$t("Afterschool programs can promote positive youth development, and  support social, emotional, cognitive, and academic development, reduce risky behaviors, promote physical health, and provide a safe and supportive environment for children and youth"),HTML(paste0(strong(tags$sub("1")))), 
                         i18n()$t(". Several afterschool programs also offer before school programs allowing parents to drop-off and pick-up their child(ren) in a safe environment without interfering with their work schedule. "),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about after-school care programs:"),
                         "<br>",
                         a(i18n()$t("Benefits for Youth, Families, and Communities"),
                           href = "https://youth.gov/youth-topics/afterschool-programs/benefits-youth-families-and-communities#:~:text=Afterschool%20programs%20can%20support%20social,environment%20for%20children%20and%20youth"),
                         "<br>",
                         a(i18n()$t("Infographics on Afterschool Care Programs in NC"),
                           href = "http://www.afterschoolalliance.org/policyStateFacts.cfm?state=NC"),
                         "<br>",
                         a(i18n()$t("North Carolina Center for Afterschool Programs"),
                           href = "https://ncafterschool.org/"),
                         "<br>",
                         "<br>",
                         i18n()$t("Sources:"),
                         "<br>",HTML(paste0(strong(tags$sub("1")))),
                         a(i18n()$t("Youth.gov"), href = "youth.gov"))
                 }
                 else if(input$var == "Bus Stops"){
                   paste(i18n()$t("For those without their own vehicle, GoDurham and GoTriangle buses serve as the primary form of public transit in Durham County. According to the Federal Transportation Administration, GoDurham averaged nearly 16,000 unlinked passenger trips (each time a passenger boards) per day in 2020. Widespread bus stops play a crucial role in improving equity throughout a region. A strong public transit system can lead to decreased food insecurity (particularly in areas lacking healthy, affordable food), and can lead to increased job opportunities (ride vs. walk to work) and public health (doctor’s office visits)."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about bus stops:"),
                         "<br>",
                         a(i18n()$t("Transit Equity Dashboard"),
                           href = "https://transitcenter.org/introducing-the-transit-equity-dashboard/"))
                 }
                 else if(input$var == "Childcare Centers"){
                   paste(i18n()$t("Childcare centers assure parents and guardians that their child(ren) is safe and cared for while simultaneously allowing them to work and earn money for their family. Childcare is particularly useful for single parents who often cannot afford to stay at home instead of working. Having several childcare options near a person’s home can be beneficial in allowing parents to weigh the cost and quality of various centers."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about childcare centers:"),
                         "<br>",
                         a(i18n()$t("The Importance of Preschool and Child Care For Working Mothers"),
                           href = "https://www.americanprogress.org/issues/education-k-12/reports/2013/05/08/62519/the-importance-of-preschool-and-child-care-for-working-mothers/"))
                 }
                 else if(input$var == "Community and Cultural Centers"){
                   paste(i18n()$t("The benefits of building community outside the school building contribute to more powerful relationships and organizing within the school building. Community centers have been found to promote community cohesion and sense of belonging while providing programming that supports self-esteem, health, and academic success. Cultural centers also positively contribute to youth development by providing positive self-images as well as opportunities to learn about cultural practices and traditions of diverse groups."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about community and cultural centers:"),
                         "<br>",
                         a(i18n()$t("The Positive Impact of Community Centers"),
                           href = "https://sportadvisory.com/the-positive-impact-of-community-recreation-centers/"),
                         "<br>",
                         a(i18n()$t("Culture and Arts Centers"),
                           href = "https://trianglecf.org/impact/impact-cultural-arts/"))
                 }
                 else if(input$var == "Community Arts"){
                   paste(i18n()$t("Durham, North Carolina offers an array of arts programs that foster a plethora of individual and community benefits. Fine arts (painting, design, and photography), and performance arts (dance, theatre, and music) have been proven to boost self-confidence, improve cognition, and deepen cultural and self-understanding by helping students overcome insecurities, combining subject matters such as math, english, science, and history, as well as exposing students to different art styles and music genres."),
                         "<br>",
                         "<br>",
                         strong(i18n()$t("Dance:")),
                         i18n()$t("Dance is a form of physical activity that encourages self-expression, improves health, and increases mobility and strength. Children and adolescents that participate in dance learn different genres, cultural aspects and regional differences, and have the opportunity to choose between cheerleading, gymnastics, interpretive dance, martial arts, and more!"),
                         "<br>",
                         a(i18n()$t("Health Benefits of Dance"), 
                           href = "https://www.prudential.co.th/corp/prudential-th/en/we-do-pulse/health-wellness/10-health-benefits-of-dancing/"),
                         "<br>",
                         "<br>",
                         strong(i18n()$t("Theatre:")),
                         i18n()$t("Theatre is a type of performance art that incorporates and fuses acting, singing, and dancing. On the stage, individuals have the opportunity to strengthen concentration and memory, improve articulation and fluency, and build trust as they interact and collaborate with others."),
                         "<br>",
                         a(i18n()$t("Benefits of Theatre Education"), 
                           href = "https://www.acttooplayers.com/benefits-of-theatre-education"),
                         "<br>",
                         "<br>",
                         strong(i18n()$t("Music:")),
                         i18n()$t("Music incorporates vocal and instrumental performance, studio production, songwriting, and even  listening enjoyment. Music students have the opportunity to learn concepts such as theory, history, acoustics, and instrumental and vocal skills and techniques. Music performance has been proven to teach discipline, relieve stress, reduce stage fright, and improve academic levels."),
                         "<br>",
                         a(i18n()$t("Benefits of Musical Incorporation"), 
                           href = "https://www.stamfordschools.org.uk/wp-content/uploads/2021/08/18-benefits-of-playing-a-musical-instrument.pdf"),
                         "<br>",
                         "<br>",
                         strong(i18n()$t("Visual:")),
                         i18n()$t("Visual arts consists of painting, drawing, printmaking, sculpture, ceramics, photography, video, filmmaking, design, crafts and architecture. Engaging in visual arts exposes children and adolescents to positive benefits that include; but are not limited to, having a space to create innovatively, improving motor skills, and expressing emotions."), 
                         "<br>",
                         a(i18n()$t("Benefits of Visual Arts"), 
                           href = "https://www.linkedin.com/pulse/20-benefits-visual-arts-children-aniruddah-andalib"),
                         "<br>",
                         "<br>",
                         strong(i18n()$t("Museums:")),
                         i18n()$t("Museums care for and display artistic, cultural, historical, and scientific artifacts. Museums help people of all walks of life understand the world around them, and embolden viewers to examine exhibits and develop perspective in informal learning environments."),
                         "<br>",
                         a(i18n()$t("The Impact of Museums"), 
                           href = "https://www.museumsassociation.org/campaigns/museums-change-lives/the-impact-of-museums/#"), 
                         "<br>",
                         a(i18n()$t("Why Visit Museums"), 
                           href = "https://www.colleendilen.com/2009/07/31/10-reasons-to-visit-a-museum/"),
                         "<br>",
                         "<br>",
                         strong(i18n()$t("Martial Arts:")),
                         i18n()$t("Martial arts consists of various forms of self-defense or combat that promote safety,  hand-eye coordination, discipline, health and weight loss, and several mental health benefits including boosted self-esteem, increased focus, and mental calmness."),
                         "<br>",
                         a(i18n()$t("Benefits of Martial Arts"), 
                           href = "https://wayofmartialarts.com/benefits-of-martial-arts/"))
                 }
                 else if(input$var == "Community Sports"){
                   paste(i18n()$t("DPS provides a wide range of sports across middle and high schools to promote teambuilding, responsibility, discipline, and leadership. Participation in school sports provides students with the daily exercise requirements suggested in the"), 
                         a(i18n()$t("CDC guidelines."),
                           href = "https://www.cdc.gov/physicalactivity/basics/children/index.htm"),
                         a(i18n()$t("The Office of Disease Prevention and Health Promotion"),
                           href = "https://health.gov/our-work/nutrition-physical-activity/physical-activity-guidelines/current-guidelines/scientific-report"),
                         i18n()$t(" concluded that “higher amounts of physical activity are associated with more favorable status for multiple health indicators, including cardiorespiratory and muscular fitness, bone health, and weight status or adiposity,” in their"),
                         i18n()$t(". Visit"),
                         a(i18n()$t("DPS’s Athletics webpage"),
                           href = "https://www.dpsathletics.com/page/show/5921314-dps-athletics"),
                         i18n()$t("for more information."))
                 }
                 else if(input$var == "Farmers' Markets"){
                   paste(i18n()$t("Farmers’ markets provide local citizens with fresh fruits and vegetables at the peak of their growing season. According to the University of Pittsburgh Medical Center, because everything sold is in-season, people that purchase produce from farmers’ markets get to experience the “truest flavors.” Because this produce is grown locally, there is a higher nutritional value. Local produce is typically minimally processed, and grown without the use of pesticides, antibiotics, and genetic modification. Due to the short travel to nearby markets and cheaper cost of produce, farmers’ markets can be a more affordable option for local residents."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about Farmers' Markets:"),
                         "<br>",
                         a(i18n()$t("Farmers' Market Coalition"), 
                           href = "https://farmersmarketcoalition.org/education/qanda/"))
                   
                 }
                 else if(input$var == "Food Pantries"){
                   paste(i18n()$t("Food pantries are partner agencies and churches that obtain donated food from food banks to feed food insecure communities. Food insecurity refers to the lack of access to enough nutritious food to fully meet basic needs because of a lack of financial resources. Although some families are able to rely on the Supplemental Nutrition Assistance Program (SNAP), also known as “food stamps,” there are some families that earn too much to receive SNAP; but, don’t make enough money to afford adequate meals. Older adults and people with disabilities that are on fixed incomes sometimes have to choose between paying for medicine or purchasing nutritious food. Food pantries can provide individuals and families with supplemental food allowing them to reallocate money they would have used on food for rent or other utilities."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about pantries:"),
                         "<br>",
                         a(i18n()$t("Frequently Asked Questions on Food Banks"),
                           href = "https://feedingthecarolinas.org/learn-more/food-bank-questions/?gclid=CjwKCAjwyryUBhBSEiwAGN5OCAyOWx3b_Z4r16WfOn18zdsydqDPs8QEpIM_PluJ6dEpsRYmIwemHxoC6koQAvD_BwE"),
                         "<br>",
                         a(i18n()$t("Fighting Hunger and Reducing Poverty"),
                           href = "https://www.ncjustice.org/publications/btc-brief-nc-ranks-10th-in-hunger-in-the-nation/"),
                         "<br>",
                         a(i18n()$t("NC Hunger and Poverty Quick Facts"),
                           href = "https://www.foodshuttle.org/hunger-in-nc-1#:~:text=In%202021%2C%20over%201.5%20million,risk%20of%20facing%20food%20insecurity"))
                 }
                 else if(input$var == "Gardens"){
                   paste(i18n()$t("Gardens offer numerous benefits to the community including nature therapy, fresh produce, and cleaner air. A study published in Public Health Nutrition noted, “Commonly cited barriers to fruit and vegetable intake include cost, availability and acceptance. Community gardens have the potential to decrease these barriers by lowering the cost of produce, increasing access, and eventually increasing acceptance and improving taste perceptions of fruits and vegetables” (Dibsdall et. al). With the ability to benefit public health and serve as community hubs, gardens are impactful to a community."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about gardens:"),
                         "<br>",
                         a(i18n()$t("A Survey of Community Gardens in Upstate New York"), 
                           href = "https://nccommunitygardens.ces.ncsu.edu/wp-content/uploads/2014/02/researchArmstrongSurveyNYHealthCommunityDevelopment.pdf?fwd=no"),
                         "<br>",
                         a(i18n()$t("Low-income Consumers’ Attitudes and Behaviour Towards Access, Availability and Motivation to Eat Fruit and Vegetables"),
                           href = "https://nccommunitygardens.ces.ncsu.edu/wp-content/uploads/2014/02/researchDibsdallLambertBobbinFrewerAccesstoProduce.pdf?fwd=no"),
                         "<br>",
                         a(i18n()$t("Research and Benefits of Community Gardens"),
                           href = "https://nccommunitygardens.ces.ncsu.edu/resources-3/nccommunitygardens-research/"))
                 }
                 else if(input$var == "Grocery Stores"){
                   paste(i18n()$t("In order to live a healthy life, people must have access to affordable, nutritious food. Without access to this resource, many, especially those who are low-income, are prone to developing diet-related conditions such as obesity, diabetes, and cardiovascular disease. The areas lacking sufficient healthy food are often called “food deserts.” Food insecurity should not be considered a natural, geographical issue; but, rather an intentional denial of resources for historically marginalized racial groups. Thus, the term “food apartheid” has gained traction in recent years, and is now often preferred. We hope to show areas affected by food apartheid in an attempt to raise awareness about food insecurity."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about grocery stores:"),
                         "<br>",
                         a(i18n()$t("Food Apartheid: Racialized Access to Healthy Affordable Food"), 
                           href = "https://www.nrdc.org/experts/nina-sevilla/food-apartheid-racialized-access-healthy-affordable-food"),
                         "<br>",
                         a(i18n()$t("Food Desert VS. Food Apartheid"),
                           href = "https://forsythfarmersmarket.com/blog/foodapartheid"),
                         "<br>",
                         a(i18n()$t("What Are Food Deserts, and How Do They Impact Health?"),
                           href = "https://www.medicalnewstoday.com/articles/what-are-food-deserts"))
                 }
                 else if(input$var == "Hospitals and Clinics"){
                   paste(i18n()$t("When faced with an emergency, time is of the essence. Being able to get to a hospital within minutes can be beneficial, and can literally save lives. Along with emergency services, hospitals also offer different types of therapy, services for individuals living with long-term illnesses, classes and events, and outpatient labs. Along with medical care and having a centralized location for medical records, hospitals can also provide employment opportunities to local residents."), "<br>", "<br>", 
                         i18n()$t("Additionally, private practices tend to be located near hospitals. These offices give patients a better opportunity to foster meaningful relationships with their nurses and primary care physicians."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about hospitals and clinics:"),
                         "<br>",
                         a(i18n()$t("The Benefits of Living Near a Hospital"),
                           href = "https://sanatogaridge.com/news-activities/the-benefits-of-living-near-a-hospital/#:~:text=Living%20near%20a%20hospital%20can,a%20Paramedic%20at%20your%20door"),
                         "<br>",
                         a(i18n()$t("10 Ways to Improve Healthcare Access"),
                           href = "https://stakeholderhealth.org/healthcare-access/"),
                         "<br>",
                         a(i18n()$t("Durham County’s “Project Access” Initiative"),
                           href = "https://projectaccessdurham.org/about/"))
                 }
                 else if(input$var == "Libraries"){
                   paste(i18n()$t("Found in urban, suburban, and rural areas, libraries often serve as community hubs. Their purpose is not only to provide academic resources for the community, but can also be used to welcome new residents to the neighborhood, assist the homeless, and offer informational sessions on a variety of topics. As highlighted in an article by the Brookings Institute, a Pew Research study showed “[s]ome 90% of Americans ages 16 and older said that the closing of their local public library would have an impact on their community, with 63% saying it would have a ‘major’ impact”"), 
                         HTML(paste0(strong(tags$sub("1")))),
                         i18n()$t(". In Durham, there are seven public libraries, including the recently renovated Main Library downtown. Each library hosts several events daily for people of all ages."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about libraries:"),
                         "<br>",
                         a(i18n()$t("How Public Libraries Help Build Healthy Communities"),
                           href = "https://www.brookings.edu/blog/up-front/2017/03/30/how-public-libraries-help-build-healthy-communities/"),
                         "<br>",
                         a(i18n()$t("Durham County Library"),
                           href = "https://durhamcountylibrary.org"),
                         "<br>",
                         "<br>",
                         i18n()$t("Sources:"),"<br>",
                         HTML(paste0(strong(tags$sub("1")))),a("Pew Research",
                                                               href = "https://www.pewresearch.org/internet/2013/12/11/libraries-in-communities/"))
                 }
                 else if(input$var == "Parks"){
                   paste(i18n()$t("The presence of parks in a community is vital to increase community engagement, assist in the economic development of cities, bolster public health, and help children learn. Parks allow people to interact with each other in an outdoor community space. Children are able to play and explore nature in an increasingly digital world, providing benefits such as decreased stress and potentially, obesity rates."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about parks:"),
                         "<br>",
                         a(i18n()$t("Why Parks Are Important"), href = "https://www.brec.org/index.cfm/page/WhyParksareImportant"),
                         "<br>",
                         a(i18n()$t("Why Parks and Recreation are Essential Public Services"),
                           href = "https://www.nrpa.org/uploadedFiles/nrpa.org/Advocacy/Resources/Parks-Recreation-Essential-Public-Services-January-2010.pdf"))
                 }
                 else if(input$var == "Recreation Centers"){
                   paste(i18n()$t("Recreation centers have varying amenities, frequently including fitness centers, basketball courts, and multipurpose rooms. These facilities can be utilized for afterschool programs, indoor and outdoor recreation, and meeting spaces. Similar to parks, recreation centers promote active lifestyles."), "<br>", "<br>", 
                         i18n()$t("Benefits include:"), "<br>", "<br>", 
                         i18n()$t("-Functioning as a community hub"), "<br>", 
                         i18n()$t("-Hosting before- and after-school care programs"), "<br>", 
                         i18n()$t("-May offer inexpensive or free tutoring"),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about recreation centers:"),
                         "<br>",
                         a(i18n()$t("The Positive Impact of Community Recreation Center"),
                           href = "https://sportadvisory.com/the-positive-impact-of-community-recreation-centers/"),
                         "<br>",
                         a(i18n()$t("Recreation Centers Play an Important Role in Communities"),
                           href = "https://www.nrpa.org/publications-research/park-pulse/park-pulse-survey-recreation-centers-role-in-communities/"))
                 }
                 else if(input$var == "Pharmacies"){
                   paste(i18n()$t("Pharmacies are essential for several reasons. They provide convenient access to prescribed medications and over-the-counter drugs, ensuring patients have timely access to the treatments they need. Pharmacists, as highly trained healthcare professionals, offer expertise and guidance on medication usage, potential side effects, and drug interactions, promoting the safe and effective use of medications.
Moreover, pharmacies contribute to public health by offering services like immunizations, health screenings, and wellness programs. They play a critical role in disease prevention and early detection. During public health emergencies, pharmacies are crucial for the distribution of vaccines and medications, supporting community health and well-being. Overall, pharmacies are vital healthcare destinations, ensuring medication access and providing expert advice to improve patient outcomes and promote community health.
"),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about pharmacies:"),
                         "<br>",
                         a(i18n()$t("The Role of Community Pharmacists in Patient Safety"),
                           href = "https://psnet.ahrq.gov/perspective/role-community-pharmacists-patient-safety"))
                 }
                 else if(input$var == "Homeless Shelters"){
                   paste(i18n()$t("Homeless shelters provide immediate assistance and support to individuals facing homelessness. They offer temporary housing, meals, clothing, and hygiene facilities. These shelters also collaborate with social workers, counselors, and job placement agencies to provide comprehensive support for individuals seeking to transition out of homelessness. Programs such as job training, education, mental health counseling, and substance abuse rehabilitation are offered to address the underlying causes of homelessness. Homeless shelters also contribute to the community by providing employment opportunities, vocational training, and community engagement programs. They serve as vital hubs for support, facilitating access to essential services and creating opportunities for both individuals experiencing homelessness and the community at large.

."),
                         "<br>",
                         "<br>",
                         
                         i18n()$t("Below is more information about homeless shelters:"),
                         "<br>",
                         a(i18n()$t("Emergency Shelters"),
                           href = "https://endhomelessness.org/topics/emergency-shelters/"))
                 }
                 else if(input$var == "Religious Centers"){
                   paste(i18n()$t("Religious centers are huge assets to the community because of various services they provide. These services include donations, food drives, fundraisers, providing safe spaces for various cultures, counseling services, daycare, summer programs, and much more. Additionally, the Durham community has established a rich inter-religion culture, especially in advocacy efforts for the city as a whole. Durham residents have shown their willingness to provide resources for all those in need, regardless of religious orientation."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about religious centers:"),
                         "<br>",
                         a(i18n()$t("The Benefits of Religiosity and Spirituality on Mental Health"),
                           href = "https://www.forbes.com/sites/alicegwalton/2018/09/17/raising-kids-with-religion-or-spirituality-may-protect-their-mental-health-study/?sh=647ed7d13287"))
                 }
                 else if(input$var == "Restaurants"){
                   paste(i18n()$t("Restaurants are vital to communities, offering much more than just meals. They serve as social hubs where people connect, fostering a sense of belonging. Culinary diversity is celebrated as restaurants showcase a wide range of cuisines, promoting cultural exchange and appreciation. Additionally, restaurants often prioritize sourcing from local farmers and producers, supporting the community's food system. Through community engagement, sponsorships, and collaborations, restaurants strengthen the social fabric."),
                         "<br>",
                         "<br>",
                         i18n()$t("Below is more information about restaurants:"),
                         "<br>",
                         a(i18n()$t("The Importance of Restaurants to Local Community"),
                           href = "https://nowcomment.com/documents/297471?scroll_to=2644186"))
                 }
               }))
  
  
  # Maps - Icon legend outputs
  observeEvent(i18n(), 
               {
                 output$afterschoolicon <- renderText({
                   
                   if(input$var == "After-School Care Programs")
                     paste(h4(HTML(paste0(strong(i18n()$t("After-School Care Programs"))))))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("After-School Care Programs")))
                   
                   
                 })
                 
                 output$busicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Bus Stops")
                     paste(h4(HTML(paste0(strong(i18n()$t("Bus Stops"))))))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Bus Stops")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Bus Stops")))
                   
                 })
                 
                 output$childcareicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(HTML(paste0(strong(i18n()$t("Childcare Centers"))))))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Childcare Centers")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Childcare Centers")))
                 })
                 
                 output$parkicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Parks")))
                   else if (input$var == "Parks")
                     paste(h4(HTML(paste0(strong(i18n()$t("Parks"))))))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Parks")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Parks")))
                 })
                 
                 output$recicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(HTML(paste0(strong(i18n()$t("Recreation Centers"))))))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Recreation Centers")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Recreation Centers")))
                 })
                 
                 output$gardenicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Gardens")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Gardens")
                     paste(h4(HTML(paste0(strong(i18n()$t("Gardens"))))))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Gardens")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Gardens")))
                 })
                 
                 output$cultureicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(HTML(paste0(strong(i18n()$t("Community and Cultural Centers"))))))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Community and Cultural Centers")))
                 })
                 
                 output$artsicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Community Arts")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Community Arts")
                     paste(h4(HTML(paste0(strong(i18n()$t("Community Arts"))))))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Community Arts")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Community Arts")))
                 })
                 
                 output$groceryicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(HTML(paste0(strong(i18n()$t("Grocery Stores"))))))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Grocery Stores")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Grocery Stores")))
                 })
                 
                 output$libraryicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Libraries")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Libraries")
                     paste(h4(HTML(paste0(strong(i18n()$t("Libraries"))))))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Libraries")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Libraries")))
                 })
                 
                 output$religiousicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Religious Centers")
                     paste(h4(HTML(paste0(strong(i18n()$t("Religious Centers"))))))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Religious Centers")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Religious Centers")))
                 })
                 
                 output$hospitalicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(HTML(paste0(strong(i18n()$t("Hospitals & Clinics"))))))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Hospitals & Clinics")))
                 })
                 
                 output$pantryicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Food Pantries")
                     paste(h4(HTML(paste0(strong(i18n()$t("Food Pantries"))))))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Food Pantries")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Food Pantries")))
                 })
                 
                 output$marketicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(HTML(paste0(strong(i18n()$t("Farmers' Markets"))))))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Farmers' Markets")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Farmers' Markets")))
                 })
                 
                 output$sportsicon <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Community Sports")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Community Sports")
                     paste(h4(HTML(paste0(strong(i18n()$t("Community Sports"))))))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Community Sports")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Community Sports")))
                 })
                 
                 output$pharmacy <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Pharmacies")))
                   else if(input$var == "Pharmacies")
                     paste(h4(HTML(paste0(strong(i18n()$t("Pharmacies"))))))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Pharmacies")))
                 })
                 
                 output$homelesshelter <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Restaurants")
                     paste(h4(i18n()$t("Homeless Shelters")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(HTML(paste0(strong(i18n()$t("Homeless Shelters"))))))
                 })
                 output$restaurant <- renderText({
                   if(input$var == "After-School Care Programs")
                     paste(h4(i18n()$t("Restaurants")))
                   else if (input$var == "Parks")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Recreation Centers")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Gardens")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Bus Stops")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Childcare Centers")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Community and Cultural Centers")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Grocery Stores")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Libraries")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Religious Centers")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Hospitals and Clinics")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Food Pantries")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Farmers' Markets")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Community Arts")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Community Sports")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Pharmacies")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Homeless Shelters")
                     paste(h4(i18n()$t("Restaurants")))
                   else if(input$var == "Restaurants")
                     paste(h4(HTML(paste0(strong(i18n()$t("Restaurants"))))))
                 })
                 
               })
  
  #Home Page - Leaflet Map showing Duke, NCCU, and the Ten Schools
  output$home <- renderLeaflet({
    leaflet() %>%
      setView(lng = -78.8970, lat = 35.9940, zoom = 10.5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lat = 36.0015926872104, lng = -78.93823945048538, icon = iconSet$uni, label = "Duke University") %>%
      addMarkers(lat = 35.97521590491441, lng = -78.89962935390885, icon = iconSet$uni, label = "North Carolina Central University") %>%
      addMarkers(data = schools, lng = ~LONGITUDE, lat = ~LATITUDE, icon = iconSet$schools, label = ~NAME)
  })
  
  #Home Page - Got to Maps tab button
  observeEvent(input$viewMap, {
    updateTabItems(session, "TabItems", selected = "mapstab")
  })
  
  #Home Page - Go to School Stats tab button
  observeEvent(input$viewStat, {
    updateTabItems(session, "TabItems", selected = "statstab")
  })
  
  #Home Page - Leaflet Map
  observeEvent(input$"Our Ten Schools - Home", {
    updateTabItems(session, "TabItems", selected = "home")
  })
  
  #Home Page - Carousal
  output$slickr <- renderSlickR({
    imgs <- list.files(path = "slideshow", pattern = "*.jpg", full.names = TRUE)
    slickR(imgs, width = 200, height = 200) + settings(autoplay = TRUE,
                                                       slidesToShow = 3,
                                                       slidesToScroll = 1)
  })
  
  #AP Table
  output$APTable <- renderTable(APCourses, bordered = TRUE, striped = TRUE, width = "150%", align = "c", digits = 0)
  
  #AP Courses
  observeEvent(i18n(), {
    output$APEnglish <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP English Language")),
          h4(i18n()$t("- AP English Literature"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP Language and Composition")),
          h4(i18n()$t("- AP Literature and Composition"))
        )
      } else if (input$ap_school == "Jordan High") {
        paste(
          h4(i18n()$t("- AP English Language")),
          h4(i18n()$t("- AP English Literature"))
        )
      } else if (input$ap_school == "City of Medicine Academy") {
        paste(
          h4(i18n()$t("- AP English Language and Composition")),
          h4(i18n()$t("- AP English Literature and Composition"))
        )
      } else if (input$ap_school == "Durham School of Technology") {
        paste(
          h4(i18n()$t("- AP English Language and Composition")),
          h4(i18n()$t("- AP English Literature and Composition"))
        )
      } else if (input$ap_school == "Northern High") {
        paste(
          h4(i18n()$t("- AP English Language and Composition")),
          h4(i18n()$t("- AP English Literature and Composition"))
        )
      } else if (input$ap_school == "Southern School of Energy and Sustainability") {
        paste(
          h4(i18n()$t("- AP English Language and Composition")),
          h4(i18n()$t("- AP English Literature and Composition"))
        )
      }
    })
    
    output$APMath <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP Statistics")),
          h4(i18n()$t("- AP Calculus AB")),
          h4(i18n()$t("- AP Calculus BC"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP Calculus AB")),
          h4(i18n()$t("- AP Statistics"))
        )
      } else if (input$ap_school == "Jordan High") {
        paste(
          h4(i18n()$t("- AP Calculus AB")),
          h4(i18n()$t("- AP Calculus BC")),
          h4(i18n()$t("- AP Statistics"))
        )
      } else if (input$ap_school == "Durham School of Technology") {
        paste(
          h4(i18n()$t("- AP Calculus AB"))
        )
      } else if (input$ap_school == "Northern High") {
        paste(
          h4(i18n()$t("- AP Calculus AB")),
          h4(i18n()$t("- AP Calculus BC")),
          h4(i18n()$t("- AP Statistics"))
        )
      } else if (input$ap_school == "Southern School of Energy and Sustainability") {
        paste(
          h4(i18n()$t("- AP Calculus AB")),
          h4(i18n()$t("- AP Statistics"))
        )
      }
    })
    
    output$APScience <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP Biology")),
          h4(i18n()$t("- AP Chemistry")),
          h4(i18n()$t("- AP Environmental Science")),
          h4(i18n()$t("- AP Physics I")),
          h4(i18n()$t("- AP Physics II")),
          h4(i18n()$t("- AP Physics C: Mechanics"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP Environmental Science")),
          h4(i18n()$t("- AP Physics I")),
          h4(i18n()$t("- AP Physics II")),
          h4(i18n()$t("- AP Biology")),
          h4(i18n()$t("- AP Physics C: Mechanics")),
          h4(i18n()$t("- AP Physics C: Electricity and Magnetism")),
          h4(i18n()$t("- AP Chemistry"))
        )
      } else if (input$ap_school == "Jordan High") {
        paste(
          h4(i18n()$t("- AP Chemistry")),
          h4(i18n()$t("- AP Biology")),
          h4(i18n()$t("- AP Environmental Science")),
          h4(i18n()$t("- AP Physics C: Mechanics")),
          h4(i18n()$t("- AP Physics 1: Algebra Based")),
          h4(i18n()$t("- AP Physics 2: Algebra Based"))
        )
      } else if (input$ap_school == "City of Medicine Academy") {
        paste(
          h4(i18n()$t("- AP Biology")),
          h4(i18n()$t("- AP Environmental Science"))
        )
      } else if (input$ap_school == "Durham School of Technology") {
        paste(
          h4(i18n()$t("- AP Environmental Science"))
        )
      } else if (input$ap_school == "Northern High") {
        paste(
          h4(i18n()$t("- AP Biology")),
          h4(i18n()$t("- AP Environmental Science"))
        )
      } else if (input$ap_school == "Southern School of Energy and Sustainability") {
        paste(
          h4(i18n()$t("- AP Biology")),
          h4(i18n()$t("- AP Chemistry")),
          h4(i18n()$t("- AP Environmental Science"))
        )
      }
    })
    
    output$APSocial <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP Psychology")),
          h4(i18n()$t("- AP U.S. Government and Politics")),
          h4(i18n()$t("- AP U.S. History")),
          h4(i18n()$t("- AP European History")),
          h4(i18n()$t("- AP Human Geography")),
          h4(i18n()$t("- AP Macroeconomics")),
          h4(i18n()$t("- AP Microeconomics")),
          h4(i18n()$t("- AP Comparative Government and Politics")),
          h4(i18n()$t("- AP World History"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP European History")),
          h4(i18n()$t("- AP Human Geography")),
          h4(i18n()$t("- AP U.S. Government and Politics")),
          h4(i18n()$t("- AP U.S. History")),
          h4(i18n()$t("- AP Comparative Government and Politics"))
        )
      } else if (input$ap_school == "Jordan High") {
        paste(
          h4(i18n()$t("- AP U.S. Government and Politics")),
          h4(i18n()$t("- AP U.S. History")),
          h4(i18n()$t("- AP World History")),
          h4(i18n()$t("- AP Human Geography")),
          h4(i18n()$t("- AP European History")),
          h4(i18n()$t("- AP Psychology")),
          h4(i18n()$t("- AP Macroeconomics")),
          h4(i18n()$t("- AP Microeconomics"))
        )
      } else if (input$ap_school == "City of Medicine Academy") {
        paste(
          h4(i18n()$t("- AP Psychology")),
          h4(i18n()$t("- AP U.S. Government and Politics"))
        )
      } else if (input$ap_school == "Durham School of Technology") {
        paste(
          h4(i18n()$t("- AP U.S. Government and Politics"))
        )
      } else if (input$ap_school == "Northern High") {
        paste(
          h4(i18n()$t("- AP U.S. Government and Politics")),
          h4(i18n()$t("- AP U.S. History")),
          h4(i18n()$t("- AP Human Geography")),
          h4(i18n()$t("- AP Psychology"))
        )
      } else if (input$ap_school == "Southern School of Energy and Sustainability") {
        paste(
          h4(i18n()$t("- AP U.S. Government and Politics")),
          h4(i18n()$t("- AP U.S. History")),
          h4(i18n()$t("- AP Human Geography")),
          h4(i18n()$t("- AP Psychology"))
        )
      }
    })
    
    output$APWLang <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP Spanish Language and Culture")),
          h4(i18n()$t("- AP Spanish Literature and Culture")),
          h4(i18n()$t("- AP Latin")),
          h4(i18n()$t("- AP French Language and Culture"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP Spanish Language and Culture")),
          h4(i18n()$t("- AP French Language and Culture"))
        )
      } else if (input$ap_school == "Jordan High") {
        paste(
          h4(i18n()$t("- AP Spanish Language and Culture")),
          h4(i18n()$t("- AP Spanish Literature and Culture")),
          h4(i18n()$t("- AP French Language and Culture")),
          h4(i18n()$t("- AP German Language and Culture"))
        )
      } else if (input$ap_school == "Southern School of Energy and Sustainability") {
        paste(
          h4(i18n()$t("- AP Spanish Language and Culture")),
          h4(i18n()$t("- AP Spanish Literature and Culture"))
        )
      }
    })
    
    output$APMusArts <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP Music Theory")),
          h4(i18n()$t("- AP Studio Art: 2-D Design")),
          h4(i18n()$t("- AP Studio Art: Drawing"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP Music Theory")),
          h4(i18n()$t("- AP Studio Art: 2-D Design")),
          h4(i18n()$t("- AP Studio Art: Drawing"))
        )
      } else if (input$ap_school == "Northern High") {
        paste(
          h4(i18n()$t("- AP Studio Art: 2-D Design"))
        )
      } else if (input$ap_school == "Jordan High") {
        paste(
          h4(i18n()$t("- AP Music Theory")),
          h4(i18n()$t("- AP Art History")),
          h4(i18n()$t("- AP Studio Art: 2-D Design")),
          h4(i18n()$t("- AP Studio Art: 3-D Design")),
          h4(i18n()$t("- AP Studio Art: Drawing"))
        )
      }
    })
    
    output$APEngine <- renderText({
      if (input$ap_school == "Riverside High") {
        paste(
          h4(i18n()$t("- AP Computer Science A")),
          h4(i18n()$t("- AP Computer Science Principles")),
          h4(i18n()$t("- AP Physics C: Electricity and Magnetism")),
          h4(i18n()$t("- AP Physics C: Mechanics"))
        )
      } else if (input$ap_school == "Hillside High") {
        paste(
          h4(i18n()$t("- AP Computer Science A")),
          h4(i18n()$t("- AP Computer Science Principles")),
          h4(i18n()$t("- AP Physics C: Electricity and Magnetism")),
          h4(i18n()$t("- AP Physics C: Mechanics"))
        )
      } else if (input$ap_school == "Durham School of Technology") {
        paste(
          h4(i18n()$t("- AP Computer Science A"))
        )
      } else if (input$ap_school == "Southern School of Energy and Sustainability") {
        paste(
          h4(i18n()$t("- AP Computer Science Principles"))
        )
      }
    })
  })
  
  
  observeEvent(input$viewAP, {
    updateTabItems(session, "TabItems", selected = "coursestab")
  })
  
  observeEvent(input$viewCTE, {
    updateTabsetPanel(session, "TabItems", selected = "coursestab")
  })
  
  #CTE Table
  output$CTETable <- renderTable(CTECourses, bordered = TRUE, striped = TRUE, width = "150%", align = "c", digits = 0)
  
  #CTE Courses
  observeEvent(i18n(), 
               {
                 output$AgCTE <- renderText({
                   if(input$cte_school == "Riverside High"){
                     paste(h4(strong(i18n()$t("N/A"))))
                   }
                   else if(input$cte_school == "Hillside High"){
                     paste(h4(strong(i18n()$t("Financial Planning"))),
                           h4(strong(i18n()$t("NAF Academy of Finance"))))
                   }
                   else if(input$cte_school == "Jordan High"){
                     paste(h4(strong(i18n()$t("Animal Systems"))),
                           h4(em(i18n()$t("- Certified Veterinarian Assistant"))),
                           h4(strong(i18n()$t("Biotechnology and Agriscience Research (Local Pathway)"))),
                           h4(strong(i18n()$t("Plant Systems"))))
                   }
                 })
                 output$BusCTE <- renderText({
                   if(input$cte_school == "Riverside High"){
                     paste(h4(strong(i18n()$t("Accounting"))),
                           h4(strong(i18n()$t("Entrepreneurship"))),
                           h4(strong(i18n()$t("Financial Planning"))),
                           h4(strong(i18n()$t("Sports & Entertainment Marketing"))))
                   }
                   else if(input$cte_school == "Hillside High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                   else if(input$cte_school == "Jordan High"){
                     paste(h4(strong(i18n()$t("Entrepreneurship"))),
                           h4(strong(i18n()$t("Sports & Entertainment Marketing"))),
                           h4(strong(i18n()$t("Travel & Tourism"))))
                   }
                 })
                 output$CompCTE <- renderText({
                   if(input$cte_school == "Riverside High"){
                     paste(h4(strong(i18n()$t("Adobe Academy"))),
                           h4(em(i18n()$t("- Adobe Illustrator"))),
                           h4(em(i18n()$t("- Adobe Photoshop"))),
                           h4(strong(i18n()$t("AP Computer Science"))),
                           h4(strong(i18n()$t("Digital Design and Animation"))),
                           h4(strong(i18n()$t("Autodesk 3DS Max"))),
                           h4(strong(i18n()$t("Game Art Design"))))
                   }
                   else if(input$cte_school == "Hillside High"){
                     paste(h4(strong(i18n()$t("Digital Design and Animation"))),
                           h4(em(i18n()$t("- Autodesk 3DS Max"))),
                           h4(strong(i18n()$t("NAF Academy of Information Technology"))),
                           h4(strong(i18n()$t("Python Programming"))),
                           h4(em(i18n()$t("- MTA 98-381 Introduction to Programming Using Python"))),
                           h4(em(i18n()$t("- PCAP Python Certified Associate"))))
                   }
                   else if(input$cte_school == "Jordan High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                 })
                 output$FamCTE <- renderText({
                   if(input$cte_school == "Riverside High"){
                     paste(h4(strong(i18n()$t("Early Childhood Development & Services"))),
                           h4(strong(i18n()$t("Food & Nutrition"))))
                   }
                   else if(input$cte_school == "Hillside High"){
                     paste(h4(strong(i18n()$t("Early Childhood Development & Services"))),
                           h4(strong(i18n()$t("Interior Design"))))
                   }
                   else if(input$cte_school == "Jordan High"){
                     paste(h4(strong(i18n()$t("Culinary Arts Applications"))),
                           h4(em(i18n()$t("- Pre-Professional Assessment and Certification in Culinary Arts"))),
                           h4(strong(i18n()$t("Food & Nutrition"))))
                   }
                 })
                 output$HealthCTE <- renderText({
                   if(input$cte_school == "Riverside High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                   else if(input$cte_school == "Hillside High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                   else if(input$cte_school == "Jordan High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                 })
                 output$TradeCTE <- renderText({
                   if(input$cte_school == "Riverside High"){
                     paste(h4(strong(i18n()$t("PLTW Engineering"))),
                           h4(strong(i18n()$t("Technology Engineering and Design"))))
                   }
                   else if(input$cte_school == "Hillside High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                   else if(input$cte_school == "Jordan High"){
                     paste(h4(i18n()$t("N/A")))
                   }
                 })
               })
  
  
  #Sports - Static
  
  observeEvent(i18n(),
               output$fall<- renderText({
                 paste(title = strong(i18n()$t("Fall Sports")))
               }))
  
  observeEvent(i18n(),
               output$spring<- renderText({
                 paste(title = strong(i18n()$t("Spring Sports")))
               }))
  
  observeEvent(i18n(),
               output$winter<- renderText({
                 paste(title = strong(i18n()$t("Winter Sports")))
               }))
  
  observeEvent(i18n(),
               output$available_women<- renderText({
                 paste(title = strong(i18n()$t("Available Girl's/Women's Sports")))
               }))
  
  observeEvent(i18n(),
               output$available_men<- renderText({
                 paste(title = strong(i18n()$t("Available Boy's/Men's Sports")))
               }))
  
  observeEvent(i18n(), 
               {
                 output$fallsports <- renderText({
                   if(input$school_sports == "Brogden Middle"){
                     paste(h4(i18n()$t("Boy's Cross Country")),
                           h4(i18n()$t("Boy’s Soccer")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Girl's Cross Country"))
                     )
                   }
                   else if(input$school_sports == "Lowes Grove Middle"){
                     paste(h4(i18n()$t("Boy's Cross Country")),
                           h4(i18n()$t("Girl's Cross Country")),
                           h4(i18n()$t("Boy’s Soccer")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Volleyball"))
                     )
                   }
                   else if(input$school_sports == "Lakewood Montesorri Middle"){
                     paste(h4(i18n()$t("Boy's Cross Country")),
                           h4(i18n()$t("Boy’s Soccer")),
                           h4(i18n()$t("Volleyball"))
                     )
                   }
                   else if(input$school_sports == "Riverside High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Hillside High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Men's Cross Country")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis")),
                           h4(i18n()$t("Women's Track")),
                           h4(i18n()$t("Indoor Track"))
                     )
                   }
                   else if(input$school_sports == "Durham School of Technology"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Men's Cross Country")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis")),
                           h4(i18n()$t("Women's Track")),
                           h4(i18n()$t("Indoor Track"))
                     )
                   }
                   else if(input$school_sports == "Jordan High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Northern High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Southern High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Riverside High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Varsity Football")),
                           h4(i18n()$t("JV Football")),
                           h4(i18n()$t("Men’s Varsity Soccer")),
                           h4(i18n()$t("Men’s JV Soccer")),
                           h4(i18n()$t("Women's Golf")),
                           h4(i18n()$t("Women's Varsity Volleyball")),
                           h4(i18n()$t("Women's JV Volleyball")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Sherwood Githens Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Men’s Soccer")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Ultimate Frisbee")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Lucas Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Men’s Soccer"))
                     )
                   }
                   else if(input$school_sports == "Carrington Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Men’s Soccer")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Women's Tennis"))
                     )
                   }
                   else if(input$school_sports == "Neal Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Men’s Soccer")),
                           h4(i18n()$t("Volleyball"))
                     )
                   }
                   
                   else if(input$school_sports == "Rogers Herr Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Men’s Soccer")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Volleyball"))
                     )
                   }
                   else if(input$school_sports == "Shepard Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Men’s Soccer")),
                           h4(i18n()$t("Volleyball"))
                     )
                   }
                   else if(input$school_sports == "Durham School of the Arts Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Men’s Soccer")),
                           h4(i18n()$t("Volleyball"))
                     )
                   }
                 })
                 
                 output$wintersports <- renderText({
                   if(input$school_sports == "Brogden Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Lowes Grove Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball"))
                     )
                   }
                   else if(input$school_sports == "Lakewood Montesorri Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball"))
                     )
                   }
                   else if(input$school_sports == "Riverside High"){
                     paste(h4(i18n()$t("Gymnastics")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Men's Varsity Basketball")),
                           h4(i18n()$t("Men's JV Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Women's Varsity Basketball")),
                           h4(i18n()$t("Women's JV Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Hillside High"){
                     paste(h4(i18n()$t("Men's Varsity Basketball")),
                           h4(i18n()$t("Men's JV Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Women's Varsity Basketball")),
                           h4(i18n()$t("Women's JV Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track"))
                     )
                   }
                   else if(input$school_sports == "Jordan High"){
                     paste(h4(i18n()$t("Gymnastics")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Men's Varsity Basketball")),
                           h4(i18n()$t("Men's JV Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Women's Varsity Basketball")),
                           h4(i18n()$t("Women's JV Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   } 
                   else if(input$school_sports == "Durham School of Technology"){
                     paste(h4(i18n()$t("Men's Varisty Basketball")),
                           h4(i18n()$t("Men's JV Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Women's Varsity Basketball")),
                           h4(i18n()$t("Women's JV Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track"))
                     )
                   }
                   else if(input$school_sports == "Northern High"){
                     paste(h4(i18n()$t("Gymnastics")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Men's Varsity Basketball")),
                           h4(i18n()$t("Men's JV Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Women's Varsity Basketball")),
                           h4(i18n()$t("Women's JV Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Southern High"){
                     paste(h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Men's Varsity Basketball")),
                           h4(i18n()$t("Men's JV Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Women's Varsity Basketball")),
                           h4(i18n()$t("Women's JV Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Sherwood Githens Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Cheerleading"))
                     )
                   }
                   else if(input$school_sports == "Lucas Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Carrington Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Neal Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball")),
                           h4(i18n()$t("Wrestling"))
                     )
                   }
                   else if(input$school_sports == "Rogers Herr Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball")),
                           h4(i18n()$t("Cheerleading"))
                     )
                   }
                   else if(input$school_sports == "Shepard Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Girl’s Basketball"))
                     )
                   }
                   else if(input$school_sports == "Durham School of the Arts Middle"){
                     paste(h4(i18n()$t("Boy’s Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Girl’s Basketball"))
                     )
                   }
                 })
                 output$springsports <- renderText({
                   if(input$school_sports == "Brogden Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Girl’s Soccer")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Lowes Grove Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Soccer")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Lakewood Montesorri Middle"){
                     paste(h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Girl’s Soccer")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Riverside High"){
                     paste(h4(i18n()$t("Varsity Baseball")),
                           h4(i18n()$t("JV Baseball")),
                           h4(i18n()$t("Varsity Softball")),
                           h4(i18n()$t("JV Softball")),
                           h4(i18n()$t("Men's Golf")),
                           h4(i18n()$t("Men's Varsity Lacrosse")),
                           h4(i18n()$t("Men's JV Lacrosse")),
                           h4(i18n()$t("Men's Tennis")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Women's Varsity Soccer")),
                           h4(i18n()$t("Women's JV Soccer")),
                           h4(i18n()$t("Women's Varsity Lacrosse")),
                           h4(i18n()$t("Women's JV Lacrosse")),
                           h4(i18n()$t("Men's Varsity Volleyball")),
                           h4(i18n()$t("Men's JV Volleyball"))
                     )
                   }
                   else if(input$school_sports == "Hillside High"){
                     paste(h4(i18n()$t("Varsity Baseball")),
                           h4(i18n()$t("JV Baseball")),
                           h4(i18n()$t("Varsity Softball")),
                           h4(i18n()$t("JV Softball")),
                           h4(i18n()$t("Men's Golf")),
                           h4(i18n()$t("Men's Varsity Lacrosse")),
                           h4(i18n()$t("Men's JV Lacrosse")),
                           h4(i18n()$t("Men's Tennis")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Women's Varsity Lacrosse")),
                           h4(i18n()$t("Women's JV Lacrosse")),
                           h4(i18n()$t("Women's Varsity Soccer")),
                           h4(i18n()$t("Women's JV Soccer"))
                     )
                   }
                   else if(input$school_sports == "Durham School of Technology"){
                     paste(h4(i18n()$t("Varsity Baseball")),
                           h4(i18n()$t("JV Baseball")),
                           h4(i18n()$t("Varsity Softball")),
                           h4(i18n()$t("JV Softball")),
                           h4(i18n()$t("Men's Golf")),
                           h4(i18n()$t("Men's Varsity Lacrosse")),
                           h4(i18n()$t("Men's JV Lacrosse")),
                           h4(i18n()$t("Men's Tennis")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Women's Varsity Lacrosse")),
                           h4(i18n()$t("Women's JV Lacrosse")),
                           h4(i18n()$t("Women's Varsity Soccer")),
                           h4(i18n()$t("Women's JV Soccer"))
                     )
                   }
                   else if(input$school_sports == "Jordan High"){
                     paste(h4(i18n()$t("Varsity Baseball")),
                           h4(i18n()$t("JV Baseball")),
                           h4(i18n()$t("Varsity Softball")),
                           h4(i18n()$t("JV Softball")),
                           h4(i18n()$t("Men's Golf")),
                           h4(i18n()$t("Men's JV Lacrosse")),
                           h4(i18n()$t("Men's Lacrosse")),
                           h4(i18n()$t("Men's Tennis")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Women's JV Lacrosse")),
                           h4(i18n()$t("Women's JV Soccer")),
                           h4(i18n()$t("Women's Lacrosse")),
                           h4(i18n()$t("Women's Soccer"))
                     )
                   }
                   else if(input$school_sports == "Northern High"){
                     paste(h4(i18n()$t("Varsity Baseball")),
                           h4(i18n()$t("JV Baseball")),
                           h4(i18n()$t("Varsity Softball")),
                           h4(i18n()$t("JV Softball")),
                           h4(i18n()$t("Men's Golf")),
                           h4(i18n()$t("Men's Varsity Lacrosse")),
                           h4(i18n()$t("Men's JV Lacrosse")),
                           h4(i18n()$t("Men's Tennis")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Women's Varsity Lacrosse")),
                           h4(i18n()$t("Women's JV Lacrosse")),
                           h4(i18n()$t("Women's Varsity Soccer")),
                           h4(i18n()$t("Women's JV Soccer"))
                     )
                   }
                   else if(input$school_sports == "Southern High"){
                     paste(h4(i18n()$t("Varsity Baseball")),
                           h4(i18n()$t("JV Baseball")),
                           h4(i18n()$t("Varsity Softball")),
                           h4(i18n()$t("JV Softball")),
                           h4(i18n()$t("Men's Golf")),
                           h4(i18n()$t("Men's Varsity Lacrosse")),
                           h4(i18n()$t("Men's JV Lacrosse")),
                           h4(i18n()$t("Men's Tennis")),
                           h4(i18n()$t("Women's Volleyball")),
                           h4(i18n()$t("Women's Varsity Lacrosse")),
                           h4(i18n()$t("Women's JV Lacrosse")),
                           h4(i18n()$t("Women's Varsity Soccer")),
                           h4(i18n()$t("Women's JV Soccer"))
                     )
                   }
                   else if(input$school_sports == "Sherwood Githens Middle"){
                     paste(h4(i18n()$t("Boy's Lacrosse")),
                           h4(i18n()$t("Girl’s Lacrosse")),
                           h4(i18n()$t("Boy’s Tennis")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Soccer")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Lucas Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girls’s Track")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Carrington Middle"){
                     paste(h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Boy’s Tennis")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Soccer")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Neal Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Rogers Herr Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Girl's Soccer")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Shepard Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Girl's Soccer")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   else if(input$school_sports == "Durham School of the Arts Middle"){
                     paste(h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Boy’s Track")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Girl’s Track")),
                           h4(i18n()$t("Girl's Soccer")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                 })
                 
                 output$male_sports_list <- renderText({
                   if(input$school_sports == "Brogden Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track"))
                     )
                   }
                   else if(input$school_sports == "Lowes Grove Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track"))
                     )
                   }
                   
                   else if(input$school_sports == "Lakewood Montesorri Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track"))
                     )
                   }
                   
                   else if(input$school_sports == "Hillside High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis"))
                     )
                   }
                   else if(input$school_sports == "Durham School of Technology"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis"))
                     )
                   }
                   
                   else if(input$school_sports == "Jordan High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Gymnastics")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Lacrosse"))
                     )
                   }
                   
                   else if(input$school_sports == "Riverside High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Gymnastics"))
                     )
                   }
                   else if(input$school_sports == "Northern High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Gymnastics"))
                     )
                   }
                   else if(input$school_sports == "Southern High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Cheerleading"))
                     )
                   }
                   
                   else if(input$school_sports == "Sherwood Githens Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Ultimate Frisbee")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Lucas Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Carrington Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Neal Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Rogers Herr Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Shepard Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Durham School of the Arts Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   
                 })
                 
                 output$female_sports_list <- renderText({
                   if(input$school_sports == "Brogden Middle"){
                     paste(h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Softball"))
                     )
                   }
                   
                   else if(input$school_sports == "Lowes Grove Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track"))
                     )
                   }
                   
                   else if(input$school_sports == "Lakewood Montesorri Middle"){
                     paste(h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Track"))
                     )
                   }
                   
                   else if(input$school_sports == "Hillside High"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track and Field"))
                     )
                   }
                   else if(input$school_sports == "Durham School of Technology"){
                     paste(h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track and Field"))
                     )
                   }
                   else if(input$school_sports == "Jordan High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Gymnastics"))
                     )
                   }
                   
                   else if(input$school_sports == "Riverside High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Gymnastics")),
                           h4(i18n()$t("Cheerleading"))
                     )
                   }
                   else if(input$school_sports == "Northern High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Track and Field")),
                           h4(i18n()$t("Gymnastics")),
                           h4(i18n()$t("Cheerleading"))
                     )
                   }
                   else if(input$school_sports == "Southern High"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Field Hockey")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Swimming")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Indoor Track")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer")),
                           h4(i18n()$t("Cheerleading"))
                     )
                   }
                   
                   else if(input$school_sports == "Sherwood Githens Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Ultimate Frisbee")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Lucas Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Carrington Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Neal Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Golf")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Rogers Herr Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Lacrosse")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Shepard Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Football")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   else if(input$school_sports == "Durham School of the Arts Middle"){
                     paste(h4(i18n()$t("Cross Country")),
                           h4(i18n()$t("Track")),
                           h4(i18n()$t("Basketball")),
                           h4(i18n()$t("Softball")),
                           h4(i18n()$t("Baseball")),
                           h4(i18n()$t("Volleyball")),
                           h4(i18n()$t("Tennis")),
                           h4(i18n()$t("Wrestling")),
                           h4(i18n()$t("Cheerleading")),
                           h4(i18n()$t("Soccer"))
                     )
                   }
                   
                 })
                 
                 output$sports_context <- renderText({
                   paste(title = strong(i18n()$t("Context"), style = "font-size:20px"),
                         br(),
                        i18n()$t("DPS provides a wide range of sports across middle and high schools to promote teambuilding, responsibility, discipline, and leadership. Participation in school sports provides students with the daily exercise requirements suggested in the"),
                         a(i18n()$t("CDC guidelines."), href = "https://www.cdc.gov/physicalactivity/basics/children/index.htm"),
                         i18n()$t("The Office of Disease Prevention and Health Promotion"), 
                         i18n()$t(" concluded that “higher amounts of physical activity are associated with more favorable status for multiple health indicators, including cardiorespiratory and muscular fitness, bone health, and weight status or adiposity,” in their"),
                         a(i18n()$t("2018 Physical Activity Guidelines for Americans Report"), href = "https://health.gov/our-work/nutrition-physical-activity/physical-activity-guidelines/current-guidelines/scientific-report"),
                         i18n()$t(". Visit"),
                         a(i18n()$t("DPS’s Athletics webpage"), href = " https://www.dpsathletics.com/page/show/5921314-dps-athletics"),
                         i18n()$t("for more information."))
                 })
                 
               })
  
  #Sports - dynamic
  # {
  #   output$fallsports <- renderTable({
  #     sports <- subset(sports_22, season == 'fall' & schoolname == input$school_sports)
  #     sports$gender[sports$gender == 'All'] <- ''
  #     sports <- sports%>%
  #       unite(sport_name, gender, sport, sep=" ")
  #     sports$sport_name <- trimws(sports$sport_name)
  #     sports %>% select(sport_name)
  #   },colnames = FALSE, align = 'c', spacing = 'l')
  # 
  #   output$wintersports <- renderTable({
  #     sports <- subset(sports_22, season == 'winter' & schoolname == input$school_sports)
  #     sports$gender[sports$gender == 'All'] <- ''
  #     sports <- sports%>%
  #       unite(sport_name, gender, sport, sep=" ")
  #     sports$sport_name <- trimws(sports$sport_name)
  #     sports %>% select(sport_name)
  #   }, colnames = FALSE, align = 'c', spacing = 'l')
  # 
  #   output$springsports <- renderTable({
  #     sports <- subset(sports_22, season == 'spring' & schoolname == input$school_sports)
  #     sports$gender[sports$gender == 'All'] <- ''
  #     sports <- sports%>%
  #       unite(sport_name, gender, sport, sep=" ")
  #     sports$sport_name <- trimws(sports$sport_name)
  #     sports %>% select(sport_name)
  #   }, colnames = FALSE, align = 'c', spacing = 'l')
  # 
  #   output$male_sports_list <- renderTable ({
  #     sports <- sports_22
  #     sports <- subset(sports, (gender == 'All' | gender == "Men's" | gender == "Boy's") & schoolname == input$school_sports)
  #     sports <- subset(sports, !duplicated(sport))
  #     sports %>% select(sport)
  #   }, sanitize.text.function = function(x) x, align = 'c', colnames = FALSE)
  # 
  #   output$female_sports_list <- renderTable ({
  #     sports <- sports_22
  #     sports <- subset(sports, (gender == 'All' | gender == "Women's" | gender == "Girl's") & schoolname == input$school_sports)
  #     sports <- subset(sports, !duplicated(sport))
  #     sports %>% select(sport)
  #   }, sanitize.text.function = function(x) x, align = 'c', colnames = FALSE)
  # 
  #   output$sports_context <- renderText({
  #       paste("DPS provides a wide range of sports across middle and high schools to promote teambuilding,
  #        responsibility, discipline, and leadership. Participation in school sports provides students with the
  #        daily exercise requirements suggested in the ",a("CDC guidelines", href = "https://www.cdc.gov/physicalactivity/basics/children/index.htm"),
  #             "The Office of Disease Prevention and Health Promotion concluded that 'higher amounts of physi
  #              cal activity are associated with more favorable status for multiple health indicators, including
  #             cardiorespiratory and muscular fitness, bone health, and weight status or adiposity,'
  #             in their",
  #             a("2018 Physical Activity Guidelines for Americans Report", href = "https://health.gov/our-work/nutrition-physical-activity/physical-activity-guidelines/current-guidelines/scientific-report"),
  #             "Visit ",
  #             a("DPS’s Athletics webpage", href = " https://www.dpsathletics.com/page/show/5921314-dps-athletics"),
  #             "for more information.")
  #     })
  # }
  
  
  #Arts Programs
  #Translations
  
observeEvent(i18n(),
  output$arts_header<- renderText({
                 paste(title = strong(i18n()$t("Available Arts Programs In Each School"), style = "font-size:20px;solid-header:TRUE"),
                       br(),
                       br())
               }))
  
observeEvent(i18n(), 
  output$arts_dps <- renderText({
    paste(title = strong(i18n()$t("Durham Public Schools and the Arts"), style = "font-size:20px;solid-header:TRUE"),
          br(),
          br(),
    tags$span(i18n()$t("Durham Public Schools’ appreciation for the arts is apparent throughout their public institutions. They provide curriculum for the arts, upcoming events in the school system, resources for K-12 students interested in the arts, and news about arts programs in DPS. Visit "), style = "font-size: 17px;"),
    tags$a(href = "https://sites.duke.edu/uacs/outputs/", target = "_blank", tags$span(i18n()$t("Arts at DPS"), style = "font-size: 17px;")),
    tags$span(i18n()$t("to learn more."), style = "font-size: 17px;"),
    br(),
    br(),
    tags$strong(i18n()$t("Learn more about some of the schools' arts programs by clicking on their logos below:"), style = "font-size: 17px;")
  )
}))

observeEvent(i18n(), 
             output$arts_durham <- renderText({
               paste(title = strong(i18n()$t("Durham County and the Arts"), style = "font-size:20px;solid-header:TRUE"),
                     br(),
                     br(),
                     tags$span(i18n()$t("Durham has a rich history of highlighting the arts. Founded in the mid-20th century, the non-profit organization "), style = "font-size: 17px;"),
                     tags$a(href = "https://durhamarts.org/", target = "_blank", tags$span(i18n()$t("Durham Arts Council"), style = "font-size: 17px;")),
                     tags$span(i18n()$t("promotes and provides access to various opportunities and resources for those in the arts. The Durham Arts Council also offers a directory of artists to network with one another through the "), style = "font-size: 17px;"),
                     tags$a(href = "https://www.durhamartsnetwork.org/", target = "_blank", tags$span(i18n()$t("Durham Arts Network"), style = "font-size: 17px;")),
                     tags$span(i18n()$t(". In addition, the city of Durham funded the"), style = "font-size: 17px;"),
                     tags$a(href="https://www.durhamnc.gov/450/Cultural-Public-Art-Development", target = "_blank", tags$span(i18n()$t("Cultural & Public Art Program "), style = "font-size: 17px;")),
                     tags$span(i18n()$t("to “ illuminate residents’ history” and highlight Durham’s “rich cultural heritage”. Durham provides many opportunities for the public to indulge in cultural arts and for artists to showcase their work. "), style = "font-size: 17px;"),
                     tags$a(href="https://www.discoverdurham.com/things-to-do/arts/", target = "_blank", tags$span(i18n()$t("Discover Durham"), style = "font-size: 17px;")),
                     tags$span(i18n()$t("provides an extensive list of events for visitors and residents to do around the arts: including festivals, concerts, performances, museums, art shows, etc. "), style = "font-size: 17px;"),
                     br(),
                     br(),
                     tags$a(href="https://nasher.duke.edu/", target = "_blank", tags$span(i18n()$t("The Nasher Museum of Art at Duke University"), style = "font-size: 17px;")),
                     tags$span(i18n()$t("emphasizes works by diverse artists who have been historically underrepresented, or excluded, by mainstream art institutions and maintains a particular focus on artists of African descent, as well as European medieval art, European and American paintings, Outsider art, classical antiquities, African art, and ancient American art."), style = "font-size: 17px;"),
                     tags$span(i18n()$t("North Carolina Central University students host various musical ensembles, such as the"), style = "font-size: 17px;"),
                     tags$a(href="https://www.nccu.edu/cash/music/ensembles/vocal-jazz", target = "_blank", tags$span(i18n()$t("NCCU Jazz Vocal Ensemble"), style = "font-size: 17px;")),
                     tags$span(i18n()$t("and the"), style = "font-size: 17px;"),
                     tags$a(href="https://www.nccusoundmachine.com/", target = "_blank", tags$span(i18n()$t("NCCU Sound Machine Marching Band"), style = "font-size: 17px;")),
                     tags$span(i18n()$t(", that perform throughout the city of Durham, North Carolina, and the United States."), style = "font-size: 17px;"),
                     tags$a(href="https://www.wncu.org/", target = "_blank", tags$span(i18n()$t("WNCU 90.7 FM is Central’s Jazz radio station"), style = "font-size: 17px;")),
                     tags$span(i18n()$t("providing “diverse music entertainment and serving as an educational resource for our community and abroad.” NCCU also provides volunteer opportunities for their students in the"), style = "font-size: 17px;"),
                     tags$a(href="https://kidznotes.org/", target = "_blank", tags$span(i18n()$t("KidzNotes program"), style = "font-size: 17px;")),
                     tags$span(i18n()$t("via Fayetteville Street Elementary’s AT&T Believe Program."), style = "font-size: 17px;")
                    
               )
             }))
  
  
  observeEvent(i18n(),
               {
                 output$available_arts <- renderTable ({
                   schoolstats23 <- schoolstats23 %>% select(SCHOOL_NAME, ARTS_PROGRAMS) %>% drop_na()
                   schoolstats23$Music <- ifelse(grepl("Music", schoolstats23$ARTS_PROGRAMS), '<i class="fas fa-check"></i>', '')
                   schoolstats23$Visual_Arts <- ifelse(grepl("Visual.Arts", schoolstats23$ARTS_PROGRAMS), '<i class="fas fa-check"></i>', '')
                   schoolstats23$Theatre <- ifelse(grepl("Theatre.Arts", schoolstats23$ARTS_PROGRAMS), '<i class="fas fa-check"></i>', '')
                   schoolstats23$Dance <- ifelse(grepl("Dance", schoolstats23$ARTS_PROGRAMS), '<i class="fas fa-check"></i>', '')
                   schoolstats23 %>% rename(School = SCHOOL_NAME) %>% select(School, Music, Visual_Arts, Theatre, Dance)
                 }, sanitize.text.function = function(x) x, align = 'c', bordered = TRUE)
                 
               }) 
  
  #Feedback Tab
  observeEvent(i18n(),
               output$feedback1<- renderText({
                 paste(tags$a(href = "https://forms.gle/eQ5vUEz3W99CmMa4A", target="_blank", i18n()$t("We want to hear your feedback!"), style = "color:black;font-size:30px;text-decoration:underline"))
               }))
  
  observeEvent(i18n(),
               output$feedback2<- renderText({
                 paste(i18n()$t("Click the text above to fill out our feedback form"))
               }))
  
  
  observeEvent(i18n(),
               output$links<- renderText({
                 paste(i18n()$t("Learn More!"),
                       br(),
                       tags$a(href = "https://sites.duke.edu/uacs/outputs/", target="_blank", i18n()$t("Community School Partnership"), style = "color:black;font-size:18px;text-decoration:underline"),
                       br(),
                       tags$a(href ="https://bigdata.duke.edu/projects/strengthening-partnerships-durham-schools-local-universities/",  target="_blank", i18n()$t("Data+ 2023"), style = "color:black;font-size:18px;text-decoration:underline"),
                       br(),
                       tags$a(href = "https://bassconnections.duke.edu/project-teams/strengthening-partnerships-between-durham-public-schools-and-local-universities-2022",  target="_blank", i18n()$t("Bass Connecions 2022-2023"), style = "color:black;font-size:18px;text-decoration:underline"),
                       br(),
                       tags$a(href ="https://community.duke.edu/impact-story/duke-public-school-collaboration-published-in-journal/", target="_blank", i18n()$t("Duke and NCCU Collaboration Published in Community Schooling Journal"), style = "color:black;font-size:18px;text-decoration:underline"),
                       br(),
                       tags$a(href ="https://trinity.duke.edu/news/how-trinity-faculty-and-students-are-sharing-resources-support-durham-public-schools", target="_blank", i18n()$t("How Trinity Faculty and Students Are Sharing Resources in Support of Durham Public Schools"), style = "color:black;font-size:18px;text-decoration:underline"),
                       br()
                 )
               }))
}
