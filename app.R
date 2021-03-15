

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(readr)
library(readxl)

data2018 <- read_excel('./2018sort.xlsx')
data2000 <- read_excel('./2000sort.xlsx')
data2010 <- read_excel('./2010sort.xlsx')
stnm<-state.name
Illidata2018 <- data2018[data2018$PSTATABB =="Illinois",]
p = colorFactor(palette = c('Black','red','green','orange','yellow','blue','pink','brown','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),ordered = T)

d1<-Illidata2018[Illidata2018$PLGENACL>0,] #Coal
d2<-Illidata2018[Illidata2018$PLGENAOL>0,] #Oil
d3<-Illidata2018[Illidata2018$PLGENAGS>0,] #gas
d4<-Illidata2018[Illidata2018$PLGENANC>0,] #nuclear
d5<-Illidata2018[Illidata2018$PLGENAHY>0,] #hydro
d6<-Illidata2018[Illidata2018$PLGENABM>0,] #Biomass
d7<-Illidata2018[Illidata2018$PLGENAWI>0,] #Wind
d8<-Illidata2018[Illidata2018$PLGENASO>0,] #Solar
d9<-Illidata2018[Illidata2018$PLGENAGT>0,] #geothermal
d10<-Illidata2018[Illidata2018$PLGENAOO>0,] #others

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(collapsed = FALSE, disable = FALSE,
                   sidebarMenu(
                     menuItem("2018 Data Analysis", tabName = "2018_Data", icon = NULL),
                     menuItem("Three Year comparison", tabName = "3_year", icon = NULL),
                     menuItem("Entire US", tabName = "US_All", icon = NULL),
                     menuItem("Mine", tabName = "Mine", icon = NULL),
                     menuItem("About", tabName = "About", icon = NULL)
                   )
                   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "2018_Data",
              column(width=10, offset=1, 
                     "2018 Data of Illinois",
                    checkboxGroupInput('type_Source', 
                                       "Type of Source:", choices = c("All","Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others", "Renewables", "Nonrenewables"),selected = "All", inline=TRUE
                                       )
                    #checkboxInput('typesource', "check", value = FALSE, width = NULL)
                    ),
              fluidRow(box(leafletOutput(outputId = "illimap"))),
              
              actionButton("reset","reset")
      ),
      tabItem(tabName = "3_year",
              # somecode
              fluidRow(
                column(1,
                       selectInput("State1", "Choose the State for first graph:",
                                   choices = stnm, 
                                   selected = "Illinois"
                                   
                                   
                       ),
                       selectInput("year1", "Choose year for first graph:",
                                   choices = c("2000","2010","2018"), 
                                   selected = "2000"
                                   
                                   
                       ),
                       checkboxGroupInput('type_Source1', 
                                          "Type of Source:", choices = c("All","Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others", "Renewables", "Nonrenewables"),selected = "All"
                       )
                       ),
              column(5,
                     box(leafletOutput(outputId = "firstmap"))),
              column(1,
                     
                     selectInput("State2", "Choose the State for first graph:",
                                 choices = stnm, 
                                 selected = "Illinois"
                                 
                                 
                     ),
                     selectInput("year2", "Choose year for first graph:",
                                 choices = c("2000","2010","2018"), 
                                 selected = "2000"
                                 
                                 
                     ),
                     checkboxGroupInput('type_Source2', 
                                        "Type of Source:", choices = c("All","Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others", "Renewables", "Nonrenewables"),selected = "All"
                     )
                     ),
              column(5,
                     box(leafletOutput(outputId = "secondmap")))
              
      )),
      tabItem(tabName = "US_All"
               #somecode
      ),
      tabItem(tabName = "Mine"
               #somecode
      ),
      tabItem(tabName = "About",
              h2("About Page.\n This Project is done by Piyush Agrawal for Spring21. Data is taken from https://www.epa.gov/egrid/download-data for years 2000,2010,2018.")
              
      )
    )
  )
 
)
  
server <- function(input, output) {
    data<- reactiveValues(d1=Illidata2018[Illidata2018$PLGENACL>0,], d2=Illidata2018[Illidata2018$PLGENAOL>0,],d3=Illidata2018[Illidata2018$PLGENAGS>0,],d4 = Illidata2018[Illidata2018$PLGENANC>0,],d5=Illidata2018[Illidata2018$PLGENAHY>0,],d6=Illidata2018[Illidata2018$PLGENABM>0,],d7= Illidata2018[Illidata2018$PLGENAWI>0,],d8=Illidata2018[Illidata2018$PLGENASO>0,],d9=Illidata2018[Illidata2018$PLGENAGT>0,],d10=Illidata2018[Illidata2018$PLGENAOO>0,])
    datast20<-reactive({subset(data2000, data2000$PSTATABB == input$State1)})
    datast21<-reactive({subset(data2010, data2010$PSTATABB == input$State1)})
    data2st20<-reactive({subset(data2000, data2000$PSTATABB == input$State2)})
    data2st21<-reactive({subset(data2010, data2010$PSTATABB == input$State2)})
    datast28<-reactive({subset(data2018, data2018$PSTATABB == input$State1)})
    data2st28<-reactive({subset(data2018, data2018$PSTATABB == input$State2)})
    observeEvent(input$type_Source,{
     
  
        if("All" %in% input$type_Source){
          output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d1, color='Black')%>%
            addCircles(data = data$d2,color='red')%>%
            addCircles(data = data$d3,color='green')%>%
            addCircles(data = data$d4,color='orange')%>%
            addCircles(data = data$d5,color='yellow')%>%
            addCircles(data = data$d6,color='blue')%>%
            addCircles(data = data$d7,color='pink')%>%
            addCircles(data = data$d8,color='brown')%>%
            addCircles(data = data$d9,color='#0CA6AC')%>%
            addCircles(data = data$d10,color='purple')%>%
            addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
          })
        }
      else if("Renewables" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d5, color='yellow')%>%
            addCircles(data = data$d7,color='pink')%>%
            addCircles(data = data$d8,color='brown')%>%
            
          addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
        })}else if("Coal" %in% input$type_Source){
          
          output$illimap <- renderLeaflet({
            leaflet() %>%
              addTiles() %>%
              addCircles(data = data$d1, color='Black')%>%
              addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
          })}
      else if("Nonrenewables" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d1, color='Black')%>%
            addCircles(data = data$d2,color='red')%>%
            addCircles(data = data$d3,color='green')%>%
            addCircles(data = data$d4,color='orange')%>%
            addCircles(data = data$d6,color='blue')%>%
            addCircles(data = data$d9,color='#0CA6AC')%>%
            addCircles(data = data$d10,color='purple')%>%
          addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
        })}
      else if("Oil" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d2, color='red')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
        })}
      else if("Gas" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d3, color='green')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
        })}
      else if("Nuclear" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d4, color='orange')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
        })}
      else if("Hydro" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d5, color='yellow')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
        })}
      else if("Biomass" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d6, color='Blue')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
        })}
      else if("Wind" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d7, color='pink')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
        })}
      else if("Solar" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d8, color='brown')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
        })}
      else if("Geothermal" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d9, color='#0CA6AC')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
        })}
      else if("Others" %in% input$type_Source){
        
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d10, color='purple')%>%
            addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
        })}
    
      
      observeEvent(input$reset, {
      
        output$illimap <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addCircles(data = data$d1, color='Black')%>%
            addCircles(data = data$d2,color='red')%>%
            addCircles(data = data$d3,color='green')%>%
            addCircles(data = data$d4,color='orange')%>%
            addCircles(data = data$d5,color='yellow')%>%
            addCircles(data = data$d6,color='blue')%>%
            addCircles(data = data$d7,color='pink')%>%
            addCircles(data = data$d8,color='brown')%>%
            addCircles(data = data$d9,color='#0CA6AC')%>%
            addCircles(data = data$d10,color='purple')%>%
          addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
        })
      }) 
    },ignoreNULL = FALSE)
    
    
     observeEvent({
       input$State1
       input$year1
       input$type_Source1
       input$State2
       input$year2
       input$type_Source2},
       { 
         if (input$year1 == "2000"){
           datastr20<-datast20()
           if("All" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
           })
           }
           else if("Renewables" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
             })
           }
           
           else if("Nonrenewables" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
             })
           }
           else if("Coal" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
             })
           }
           
           else if("Oil" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
             })
           }
           
           else if("Gas" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
             })
           }
           else if("Nuclear" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
             })
           }
           
           else if("Hydro" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
             })
           }
           else if("Biomass" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
             })
           }
           else if("Wind" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
             })
           }
           else if("Solar" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
             })
           }
           else if("Geothermal" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
             })
           }
           else if("Others" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
             })
           }
           
           
         }
         else if (input$year1 == "2010"){
           datastr20<-datast21()
           if("All" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
             })
           }
           else if("Renewables" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
             })
           }
           
           else if("Nonrenewables" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
             })
           }
           else if("Coal" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
             })
           }
           
           else if("Oil" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
             })
           }
           
           else if("Gas" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
             })
           }
           else if("Nuclear" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
             })
           }
           
           else if("Hydro" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
             })
           }
           else if("Biomass" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
             })
           }
           else if("Wind" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
             })
           }
           else if("Solar" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
             })
           }
           else if("Geothermal" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
             })
           }
           else if("Others" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
             })
           }
           
           
         }
         else if (input$year1 == "2018"){
           datastr20<-datast28()
           if("All" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
             })
           }
           else if("Renewables" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
             })
           }
           
           else if("Nonrenewables" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
             })
           }
           else if("Coal" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
             })
           }
           
           else if("Oil" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
             })
           }
           
           else if("Gas" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
             })
           }
           else if("Nuclear" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
             })
           }
           
           else if("Hydro" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
             })
           }
           else if("Biomass" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
             })
           }
           else if("Wind" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
             })
           }
           else if("Solar" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
             })
           }
           else if("Geothermal" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
             })
           }
           else if("Others" %in% input$type_Source1){
             output$firstmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
             })
           }
           
           
         }
         if (input$year2 == "2000"){
           datastr20<-data2st20()
           if("All" %in% input$type_Source1){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
             })
           }
           else if("Renewables" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
             })
           }
           
           else if("Nonrenewables" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
             })
           }
           else if("Coal" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
             })
           }
           
           else if("Oil" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
             })
           }
           
           else if("Gas" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
             })
           }
           else if("Nuclear" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
             })
           }
           
           else if("Hydro" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
             })
           }
           else if("Biomass" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
             })
           }
           else if("Wind" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
             })
           }
           else if("Solar" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
             })
           }
           else if("Geothermal" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
             })
           }
           else if("Others" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
             })
           }
           
           
         }
         else if (input$year2 == "2010"){
           datastr20<-data2st21()
           if("All" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
             })
           }
           else if("Renewables" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
             })
           }
           
           else if("Nonrenewables" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
             })
           }
           else if("Coal" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
             })
           }
           
           else if("Oil" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
             })
           }
           
           else if("Gas" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
             })
           }
           else if("Nuclear" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
             })
           }
           
           else if("Hydro" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
             })
           }
           else if("Biomass" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
             })
           }
           else if("Wind" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
             })
           }
           else if("Solar" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
             })
           }
           else if("Geothermal" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
             })
           }
           else if("Others" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
             })
           }
           
           
         }
         else if (input$year2 == "2018"){
           datastr20<-data2st28()
           if("All" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal =p, values = c("Coal", "Oil", "Gas","Nuclear", "Hydro" ,"Biomass","Wind","Solar", "Geothermal", "Others"),title = "Sources")
             })
           }
           else if("Renewables" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow','pink','brown'),domain = c("Hydro","Wind","SOlar")),values = c("Hydro","Wind","Solar"),title="Sources")
             })
           }
           
           else if("Nonrenewables" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black','red','green','orange','blue','#0CA6AC','purple'),domain = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others")),values = c("Coal", "Oil", "Gas","Nuclear","Biomass","Geothermal", "Others"),title="Sources")
             })
           }
           else if("Coal" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENACL>0,],color='Black')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('Black'),domain = c("Coal")),values = c("Coal"),title="Sources")
             })
           }
           
           else if("Oil" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOL>0,],color='red')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('red'),domain = c("Oil")),values = c("Oil"),title="Sources")
             })
           }
           
           else if("Gas" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGS>0,],color='green')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('green'),domain = c("Gas")),values = c("Gas"),title="Sources")
             })
           }
           else if("Nuclear" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENANC>0,],color='orange')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('orange'),domain = c("Nuclear")),values = c("Nuclear"),title="Sources")
             })
           }
           
           else if("Hydro" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAHY>0,],color='yellow')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('yellow'),domain = c("Hydro")),values = c("Hydro"),title="Sources")
             })
           }
           else if("Biomass" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENABM>0,],color='blue')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('BLue'),domain = c("Biomass")),values = c("Biomass"),title="Sources")
             })
           }
           else if("Wind" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAWI>0,],color='pink')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('pink'),domain = c("Wind")),values = c("Wind"),title="Sources")
             })
           }
           else if("Solar" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENASO>0,],color='brown')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('brown'),domain = c("Solar")),values = c("Solar"),title="Sources")
             })
           }
           else if("Geothermal" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAGT>0,],color='#0CA6AC')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('#0CA6AC'),domain = c("Geothermal")),values = c("Geothermal"),title="Sources")
             })
           }
           else if("Others" %in% input$type_Source2){
             output$secondmap <- renderLeaflet({
               leaflet() %>%
                 addTiles() %>%
                 addCircles(data = datastr20[datastr20$PLGENAOO>0,],color='purple')%>%
                 addLegend(position = "topright",pal= colorFactor(palette = c('purple'),domain = c("Others")),values = c("Others"),title="Sources")
             })
           }
           
           
         }
         
         
         })
      
  
    
    
  }
shinyApp(ui = ui, server = server)

