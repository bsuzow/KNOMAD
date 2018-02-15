#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
# 
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#
# 2.9.18  BYL
# This shiny presents the numbers of US citizens who migrated to other countries
# in 2013. 
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
   library(readxl)  #http://readxl.tidyverse.org/
   library(dplyr)
   library(data.table)
   library(ggplot2)
   library(plotly)
   library(reshape2)
   
   # Load the data
   
   Mig2013PRE = reactive ({
      
      # Uncomment the following statement if reloading of the data is necessary.
      
      # download.file("https://www.knomad.org/sites/default/files/2017-03/bilateralmigrationmatrix20130.xlsx",
      #               "Mig2013.xlsx", mode="wb") 
      
      Mig2013 = read_excel("Mig2013.xlsx",sheet="Bilateral Migration 2013", range="A2:HJ219")
      names(Mig2013)[1]="Country"  # shorten the col name
      
      # If the "immigration to" is selected for direction, transpose the dataframe.
      
      if (input$direction=="Immigrate to") {
         df = Mig2013
         row.names(df)= df$Country
         Mig2013inflow=t(df)                                    # transpose the df
         Mig2013inflow=as.data.frame(Mig2013inflow)             # t() returns a matrix object. convert it to a df
         Mig2013inflow = Mig2013inflow[2:nrow(Mig2013inflow),]  # remove the first row that repeats country names
         
         Mig2013inflow$Country = row.names(Mig2013inflow)        # make the row names a column named Country
         row.names(Mig2013inflow) = c(1:nrow(Mig2013inflow))    # assign sequential numbers to row names
         
         Mig2013inflow = Mig2013inflow %>% select(Country,everything())  # move the Country col to the first position
         
      } else {
         
         Mig2013
      }# if (input$direction)...
      
   })  #Mig2013PRE - reactive
   
   
   # extract the row pertinent to the country selected in UI
   
   Mig2013DF = reactive({
      
      #library(dplyr)
      selectedCountry = strsplit(input$country," ")[[1]][1]  # extract the selected coutry code
      
      # add the country 3-char code to the dataset
      library(countrycode)
      Mig2013 = Mig2013PRE() %>% mutate(Code=countrycode(Country,"country.name","iso3c"))
      
      country.df = Mig2013 %>% filter(Code == selectedCountry)
      country.df = melt(country.df,id=c("Country"))
      
      # remove the first two columns with meaningless values - repetions of the selected country name and code.
      country.df$Country = NULL 
      country.df$Code = NULL  
   
      names(country.df) = c("Country","NumMig")
      country.df = country.df %>% mutate(Country=as.character(Country),
                                         NumMig =as.numeric(NumMig))
      
      country.df = country.df %>% filter(NumMig>0) # filter out rows with no migration data
   
      country.df = country.df %>% mutate(Code=countrycode(Country,"country.name","iso3c"))
   
      # A much better result.  
      # It is still coded north korea incorrectly.  
      # No country codes for Other North and Other South.  This should be noted in the Shiny app.
   
      country.df = country.df %>% filter(!is.na(Code)) %>% # remove countries with no code
                                  mutate(Code=ifelse(grepl("^Korea,( +)Dem",Country),"PRK",Code)) %>% # fix north korea's code
                                  mutate(NumMigK = round(NumMig/1000,1)) %>%
                                  mutate(hoverpop = paste(as.character(round(NumMig/1000,1)),
                                                          "K moved to ",Country,sep=""))# compose the tooltip text
   })  # Mig2013DF - reactive
   
   # Sort the df in descending order based on the immigration number column
   # This df is used for Tab2
   
   Mig2013DF.desc = reactive ({
      
      dataTableDF = Mig2013DF() %>% arrange(desc(NumMig)) %>% select(NumMigK,Country)
      if (input$direction == "Immigrate to") {
         names(dataTableDF) = c("#Citizens in K","From")
      } else {
         names(dataTableDF) = c("#Citizens in K","To")
      }
      
      dataTableDF
   })  # Mig2013DF.desc - reactive
   
   
   # compose the note on tab1
   
   # max.df = reactive({
   #    
   #  
   # })
   
   # A note added to Tab2
   
   output$tab2_text  = renderUI({
      
      totalMig = formatC(sum(Mig2013DF()$NumMig), format="f", digits=0, big.mark=",")
      if (input$direction == "Immigrate to") {
         note = paste("Total Number of Immigrants: ",
                      as.character(totalMig,sep=""))
      } else {
         note = paste("Total Number of Emigrants ",
                      as.character(totalMig,sep=""))
      }
      
      HTML(
         paste("<B>",note,"</B>",sep="")
      )
   }) #tab1_text
   
  
   # The data table of top 20 countries on Tab2
   output$tab2_data = renderTable({
      
     head(Mig2013DF.desc(),n=20)
         
   })  # tab2_data
   
   output$Mig2013 <- renderPlotly({
      
      borders = list(color= toRGB("black"),width=1)
      map_options = list(
         scope="world",
         projection = list(type='Mercator'),
         showcoastlines = TRUE
      ) # borders
     
      selectedCountry = strsplit(input$country," - ")[[1]][2]  # extract the selected coutry name
      if (input$direction == "Immigrate to") {
         plotTitle = paste("Immigration to ",selectedCountry,sep="")
         plotColor = "Greens"
      } else {
         plotTitle = paste("Emigration from ",selectedCountry,sep="")
         plotColor = "Reds"
      }
      
      p3 = plot_ly(z   = ~Mig2013DF()$NumMig,
                   text = ~Mig2013DF()$hoverpop,
                   locations = ~Mig2013DF()$Code,
                   type ="choropleth",
                   locationmode='world',
                   color=~Mig2013DF()$NumMig,
                   colors=plotColor,
                   # hoverinfo="text",
                   marker = list(line=borders)) %>%
         
         colorbar(title = plotTitle) %>%
         
         plotly::layout(title=plotTitle, geo=map_options)
      
      p3
      
      
   }) # Mig2013
   
   
}) #shinyServer
