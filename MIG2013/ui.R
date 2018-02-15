#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# UI for KNOMAD > MIG2013 data visualization to tell the story of the US migration
# out to various countries. 
# Data source: knomad.org


library(shiny)

# Create a list of countries for selectInput()
library(countrycode)
library(dplyr)
countryList = countrycode_data %>% select(country.name.en, iso3c) %>% 
              filter(!is.na(iso3c)) %>% mutate(list=paste(iso3c,country.name.en,sep=' - '))

countryList = countryList$list

# Create a list of directional input
directionList = c("Immigrate to","Emigrate from")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   
   # Application title
   titlePanel("Bilateral Migration Data Visualization"), 
   
   # the following code makes the selected tab title render in black font
   
   # tags$style(HTML("
   #                 .tabs-above > .nav > li[class=active] > a {
   #                 background-color: #FFF;
   #                 color: #000;
   #                 }")),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
         # h3(""),
         br(),
         #hr(),
         # 
         
         selectInput("direction","Choose Migration Direction",
                     directionList,
                     selected="Emigrate from"
                     #selected="Immigrate to"
         ),
         
         br(),
         selectInput("country","Choose a country",
                     countryList,
                     selected= "USA - United States of America"
                     ),
         br(),
         # hr(),
    
         
         submitButton("Submit"),
         
         br(),
         hr(),
         
         a("Data Source:",
           href="https://www.knomad.org/sites/default/files/2017-03/bilateralmigrationmatrix20130.xlsx",
           "bilateralmigrationmatrix20130.xlsx"),
         br(),
         a("available at KNOMAD.org", 
           href="http://www.knomad.org"),
         
         br(),
         hr(),
         tags$div("These estimates are based on"),
         tags$a("the Migration and Remittances 
               Factbook 2016, which includes new bilateral data on migration stocks, 
                World Bank.  ", href="https://www.worldbank.org/prospects/migrationandremittances"),
         
         br(),
         br(),
         tags$p("The database of the UN Population Division (UNPD) is the most comprehensive 
                source of information on international migrant stocks for the period 1960–2013. "),
         tags$a("Read more ...",href="https://www.knomad.org/data/faqs")
         
         ),  # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type="tabs",
                     
                     tabPanel("Migration (Hover for breakdown)",
                              br(),
                              #tags$h5("Hover for details"),
                              #br(),
                              #htmlOutput("tab1_text"),
                              #hr(),
                              plotly::plotlyOutput("Mig2013")
                              
                     ), # tabPanel - US mig outflow
                     
                     tabPanel("Top 20 countries",
                              br(), 
                              htmlOutput("tab2_text"), 
                              hr(),
                              tableOutput("tab2_data")
                                   
                     ) # tablPanel - Data
               
         ) # tabsetPanel
      ) # mainPanel
   ) #sidebarLayer
   )) #shinyUI
