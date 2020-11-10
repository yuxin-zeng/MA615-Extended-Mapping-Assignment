library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)

library(dplyr)
library(lubridate)
library(maps)
library(ggplot2)

#Read the data
dt <- read.csv(file="PublicAssistanceFundedProjectsDetails.csv",header=T)

#Filter the information from 2009 to 2018 and remove meaningless columns 
dt %>% 
    filter(2009<=year(declarationDate)&year(declarationDate)<=2018) %>%
    select(c(1,2,3,5,6,9,11,13,16)) -> dt1

#Focus on hurricane and tornado
unique(dt1$incidentType)
hurricane <- subset(dt1,incidentType=="Hurricane")
tornado <- subset(dt1,incidentType=="Tornado")

#Check for missing values
length(which(!is.na(hurricane)=="FALSE"))
length(which(!is.na(tornado)=="FALSE"))

#############################################################

#2009-2018 Annual Losses Due to Hurricane
aggregate(projectAmount~year(declarationDate),data=hurricane,FUN=sum) %>%
    rename("year"="year(declarationDate)","cost"="projectAmount") ->
    a1

#Figure
h_bar=ggplot(data=a1,aes(x=year,y=cost)) +
    geom_bar(stat="identity",fill="steelblue",color="black") +
    labs(title="2009-2018 Annual Costs of Hurricanes") +
    xlab("Year") +
    ylab("Estimated Total Cost") +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 13, face = "bold")) 

#############################################################

#Frequency of each damage type 
t1=table(hurricane$damageCategory)

#Cost of each damage type 
a2=aggregate(projectAmount~damageCategory,data=hurricane,FUN=sum)

#Pie chart data
cbind(a2,t1) %>%
    rename("damage"="damageCategory","cost"="projectAmount") %>%
    select(-3) ->
    h_piedata

#Proportion of Different Damage Categories
h_pie1 = ggplot(data=h_piedata, mapping=aes(x="",y=Freq,fill=damage)) +
    geom_bar(stat="identity",width=1) +
    coord_polar(theta="y") +
    labs(title="Different Damage Categories") +
    theme(axis.ticks=element_blank()) 

#Proportion of Losses in Different Damage Categories
h_pie2 = ggplot(data=h_piedata, mapping=aes(x="",y=cost,fill=damage)) +
    geom_bar(stat="identity",width=1) +
    coord_polar(theta="y") +
    labs(title="Costs of Different Damage Categories") +
    theme(axis.ticks=element_blank())

#############################################################

#2009-2018 Annual Losses Due to Tornado
aggregate(projectAmount~year(declarationDate),data=tornado,FUN=sum) %>%
    rename("year"="year(declarationDate)","cost"="projectAmount") ->
    a3

#Figure
t_bar = ggplot(data=a3,aes(x=year,y=cost)) +
    geom_bar(stat="identity",fill="steelblue",color="black") +
    labs(title="2009-2018 Annual Costs of Tornadoes") +
    xlab("Year") +
    ylab("Estimated Total Cost") +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 13, face = "bold"))  

#############################################################

#Frequency of each damage type 
t2=table(tornado$damageCategory)

#Cost of each damage type 
a4=aggregate(projectAmount~damageCategory,data=tornado,FUN=sum)

#Pie chart data
cbind(a4,t2) %>%
    rename("damage"="damageCategory","cost"="projectAmount") %>%
    select(-3) ->
    t_piedata

#Proportion of Different Damage Categories
t_pie1 = ggplot(data=t_piedata, mapping=aes(x="",y=Freq,fill=damage)) +
    geom_bar(stat="identity",width=1) +
    coord_polar(theta="y") +
    labs(title="Different Damage Categories") +
    theme(axis.ticks=element_blank()) 


#Proportion of Losses in Different Damage Categories
t_pie2 = ggplot(data=t_piedata, mapping=aes(x="",y=cost,fill=damage)) +
    geom_bar(stat="identity",width=1) +
    coord_polar(theta="y") +
    labs(title="Costs of Different Damage Categories") +
    theme(axis.ticks=element_blank())

#############################################################

#Total cost of each county
a5=aggregate(projectAmount~county,data=hurricane,FUN=sum)
#Unique applicant ID of each county
a6=aggregate(applicantId~county,data=hurricane,FUN=unique)
#Number of unique applicants of each county
a6$number=lengths(a6$applicantId)
#Join the tables (a5 & a6)
h_data=merge(a5,a6)
#Average cost per applicant of each county
h_data$average=h_data$projectAmount/h_data$number

#Organize the data
for (i in 1:nrow(h_data)){
    tolower(h_data$county[i]) -> h_data$subregion[i]
}

h_data %>% 
    select(-c(1,3)) %>% 
    rename("total"="projectAmount") -> 
    h_data

#Add long&lat information
map_data("county") -> geo 
geo %>% right_join(h_data,by=c('subregion'='subregion')) ->
    h_mapdata

#Total Cost Due to Hurricane from 2009 to 2018 
h_map1 = ggplot() + 
    geom_polygon(data=geo,aes(x=long,y=lat,group=group),colour="grey",fill="white") + 
    geom_polygon(data=h_mapdata,aes(x=long,y=lat,group=group,fill=total),colour="transparent") +
    labs(title="Total Costs of Hurricanes from 2009 to 2018",x="Longitude",y="Latitude",fill="Total Costs")


#Average Cost Due to Hurricane from 2009 to 2018
h_map2 = ggplot() + 
    geom_polygon(data=geo,aes(x=long,y=lat,group=group),colour="grey",fill="white") + 
    geom_polygon(data=h_mapdata,aes(x=long,y=lat,group=group,fill=average),colour="transparent") +
    labs(title="Average Costs of Hurricanes from 2009 to 2018",x="Longitude",y="Latitude",fill="Average Costs") 

#############################################################

#Total cost of each county
a7=aggregate(projectAmount~county,data=tornado,FUN=sum)
#Unique applicant ID of each county
a8=aggregate(applicantId~county,data=tornado,FUN=unique)
#Number of unique applicants of each county
a8$number=lengths(a8$applicantId)
#Join the tables (a7 & a8)
t_data=merge(a7,a8)
#Average cost per applicant of each county
t_data$average=t_data$projectAmount/t_data$number

#Organize the data
for (i in 1:nrow(t_data)){
    tolower(t_data$county[i]) -> t_data$subregion[i]
}

t_data %>% 
    select(-c(1,3)) %>% 
    rename("total"="projectAmount") -> 
    t_data

#Add long&lat information
map_data("county") -> geo 
geo %>% right_join(t_data,by=c('subregion'='subregion')) ->
    t_mapdata

#Total Cost Due to Tornado from 2009 to 2018 
t_map1 = ggplot() + 
    geom_polygon(data=geo,aes(x=long,y=lat,group=group),colour="grey",fill="white") + 
    geom_polygon(data=t_mapdata,aes(x=long,y=lat,group=group,fill=total),colour="transparent") +
    labs(title="Total Costs of Tornadoes from 2009 to 2018",x="Longitude",y="Latitude",fill="Total Costs") 

#Average Cost Due to Tornado from 2009 to 2018
t_map2 = ggplot() + 
    geom_polygon(data=geo,aes(x=long,y=lat,group=group),colour="grey",fill="white") + 
    geom_polygon(data=t_mapdata,aes(x=long,y=lat,group=group,fill=average),colour="transparent") +
    labs(title="Average Costs of Tornadoes from 2009 to 2018",x="Longitude",y="Latitude",fill="Average Costs") 

#############################################################



ui <- dashboardPage(
    dashboardHeader(title = "Disaster"),
    
    dashboardSidebar(  
        sidebarMenu(
            menuItem("Data", tabName = "Table", icon = icon("dashboard")),
            menuItem("Hurricane", tabName = "Hur", icon = icon("dashboard")),
            menuItem("Tornado", tabName = "Tor", icon = icon("dashboard")))),
    
    dashboardBody(
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        tabItems(
            tabItem(tabName = "Table",
                    DTOutput("table")),
            tabItem(tabName = "Hur",
                    box(plotOutput("plot1")),
                    box(plotOutput("plot2")),
                    box(plotOutput("plot3")),
                    box(plotOutput("plot4")),
                    box(plotOutput("plot5"))),
            tabItem(tabName = "Tor",
                    box(plotOutput("plot6")),
                    box(plotOutput("plot7")),
                    box(plotOutput("plot8")),
                    box(plotOutput("plot9")),
                    box(plotOutput("plot10")))
            )
    )
)


server <- function(input, output) {
    output$table <-renderDT({datatable(dt1)})
    
    output$plot1 <- renderPlot({h_map1})
    output$plot2 <- renderPlot({h_map2})
    output$plot3 <- renderPlot({h_pie1})
    output$plot4 <- renderPlot({h_pie2})
    output$plot5 <- renderPlot({h_bar})
    
    output$plot6 <- renderPlot({t_map1})
    output$plot7 <- renderPlot({t_map2})
    output$plot8 <- renderPlot({t_pie1})
    output$plot9 <- renderPlot({t_pie2})
    output$plot10 <- renderPlot({t_bar})
}


shinyApp(ui, server)
