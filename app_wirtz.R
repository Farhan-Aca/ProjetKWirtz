library(shiny)
library(shinythemes)
library(bslib)
library(readxl)
library(shinydashboard)
library(DT)
library(dplyr)
library(stringr)
library(gridExtra)
library(reactable)
library(readxl)
library(plotly)
library(crosstalk)
library(readr)
library(leaflet)
library(htmltools)
library(tidyr)


#Import data----        

#https://www.rstudio.com/blog/rstudio-v1-4-preview-little-things/
export_dataframe <- read_csv("export_dataframe.csv")
stade <- read_excel("stade.xlsx")
coord <- read_excel("coord.xlsx")


coord<-coord%>%mutate(top = dense_rank(desc(Budget)))

#######################################
#        Traitement data----
######################################
dom_ext<-export_dataframe%>%separate(Score,c("Dom","Ext"),sep=(2))
dom_ext<-dom_ext%>%mutate(Dom=substr(Dom,1,nchar(Dom)-1))
dom_ext

dom_ext<-dom_ext%>%mutate(pts_dom=case_when(dom_ext$Dom>dom_ext$Ext~3,dom_ext$Dom==dom_ext$Ext~1,dom_ext$Dom<dom_ext$Ext~0))
dom_ext<-dom_ext%>%mutate(pts_ext=case_when(dom_ext$Ext>dom_ext$Dom~3,dom_ext$Ext==dom_ext$Dom~1,dom_ext$Ext<dom_ext$Dom~0))
dom_ext<-dom_ext%>%group_by(Domicile)%>%mutate(nb_match_dom=sum(length(Domicile)))
dom_ext<-dom_ext%>%group_by(Extérieur)%>%mutate(nb_match_ext=sum(length(Extérieur)))

ptsdom<-dom_ext%>%group_by(Domicile)%>%summarize(sum(pts_dom))

ptsext<-dom_ext%>%group_by(Extérieur)%>%summarize(sum(pts_ext))
ptsstade<-ptsdom%>%left_join(ptsext,by=c("Domicile"="Extérieur"))
ptsstade<-ptsstade%>%dplyr::rename("Equipe"="Domicile","Points_Domicile"=`sum(pts_dom)`, "Points_Exterieur" = `sum(pts_ext)` )

ppmdom<-dom_ext%>%group_by(Domicile)%>%summarize("Points_Domicile"=sum(pts_dom)/nb_match_dom)
ppmdom<-ppmdom%>%distinct(Domicile,Points_Domicile)
ppmext<-dom_ext%>%group_by(Extérieur)%>%summarize("Points_Exterieur"=sum(pts_ext)/nb_match_ext)
ppmext<-ppmext%>%distinct(Extérieur,Points_Exterieur)
ppm<-ppmext%>%left_join(ppmdom,by=c("Extérieur"="Domicile"))
ppm<-ppm%>%dplyr::rename("Equipe"="Extérieur")%>%dplyr::ungroup()

table(dom_ext$Domicile)

summary(coord)

coord$Lat<-as.numeric(coord$Lat)
coord$Long<-as.numeric(coord$Long)

coord<-coord%>%mutate(tier=case_when(top<5~"Top 1 Budget",top>4&top<10~"Top 2 Budget",top>9&top<15~"Top 3 Budget",
                                     top>14~"Top 4 Budget"))

coord$Budget<-ifelse(!grepl("\\D", coord$Budget), format(as.numeric(coord$Budget), big.mark = " ", trim = T), string)

getColor <- function(coord) {
  sapply(coord$top, function(top) {
    if(top< 5) {
      "purple"
    } else if(top>4&top<10) {
      "green"
    } else if(top>9&top<15){
      "orange"
    }
      else if(top>14){
      "red"
     } } )}



icons <- awesomeIcons(
  icon = 'home',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(coord)
)


coordmap<-coord %>% 
  leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(icon=icons)



#https://1texte.com/outils/editeur/html/

#######################################
###           Shiny                ###
######################################
ui <- dashboardPage(skin="purple",title="Premier League stats",
                     dashboardHeader(title ='PL stats',titleWidth = 300),
                      dashboardSidebar(
                        sidebarMenu(HTML(paste0(
                          "<br>",
                          "<a href='https://www.nps.gov/index.htm' target='_blank'><img style = 'display: block;
                          margin-left: auto; margin-right: auto;' src='https://scontent.fsxb1-1.fna.fbcdn.net/v/t1.15752-9/305591168_665265044534990_1443497876533106748_n.jpg?_nc_cat=101&ccb=1-7&_nc_sid=ae9488&_nc_ohc=X2plodk-ILkAX_iqyZY&_nc_ht=scontent.fsxb1-1.fna&oh=03_AVLofsJeK5WdJw0c4GyIrqAz_Kmv_15KX1ZG41jItqUTPQ&oe=634E4E46' 
                          width = '186'></a>",
                          "<br>",
                          "<p style = 'text-align: center;'><small><a 
                          href='https://www.nps.gov/subjects/hfc/arrowhead-artwork.htm' target='_blank'>Logo de la Premier League</a>
                          </small></p>",
                          "<br>")),
                        menuItem("Read",tabName="lire",icon=icon("house")),
                        menuItem("Infos",tabName="Tableau",icon = icon("house")),
                        menuItem("Clubs",tabName="Carte",icon = icon("house")),
                        menuItem("Stats equipe",tabName = "Stat",icon=icon("house"),startExpanded = FALSE,
                                menuSubItem("Domicile/Extérieur",tabName = "DE",icon=icon("house")))
                      )),
                     dashboardBody(
                       tabItems(
                        tabItem(tabName = "lire",fluidPage(includeHTML("wirtz.html")
                                  )),
                         tabItem(tabName = "Carte",fluidRow(column(6,leafletOutput("map",height=700)),
                                 column(6,tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')), 
                                        a(class="twitter-timeline","data-height"="700","data-width"="600",
                                          href="https://twitter.com/projetwirtz/lists/1573949714982211590"
                                        )))),
                       tabItem(tabName = "Tableau",h1("test"),reactableOutput("tab1")),
                       tabItem(tabName = "DE",fluidPage(
                                                          selectizeInput("team","Choix de l'equipe",choices = sort(unique(dom_ext$Domicile)),
                                                                         multiple=TRUE),
                               plotlyOutput("graph1"),plotlyOutput("graph2"))))))
     
labels <- paste(
  "<strong>Ville :", coord$Ville,
    "</strong><br>Club :", coord$Club) %>%
  lapply(htmltools::HTML)

coord<-coord%>%mutate(tag=paste0("<a href=", Effectif,">", Effectif, "</a>"))

qpal <- colorFactor(c("purple","green","orange","red"), domain = c("LDC","Europe","Ventre mou","Galere"),ordered = T)

server<-function(input,output){
  output$tab1<-renderReactable({reactable(export_dataframe,
                                   filterable = TRUE,defaultPageSize = 10 )})
  output$map<-renderLeaflet({leaflet(coord) %>% 
                              addTiles() %>%
                              addAwesomeMarkers(lng = ~Long,lat=~Lat, icon=icons,label =~labels,
                              popup = ~paste0("Stade : ", htmlEscape(Stade),"<br>","Capacite : ", htmlEscape(Capacité),
                              "<br>","Budget salaire : ",htmlEscape(Budget),"<br>",
                              "Effectif : ", tag))%>%addLegend(pal=qpal,values =c("LDC","Europe","Ventre mou","Galere"),
                                                                      opacity = 1)})
  output$graph1<-renderPlotly({
    ptsstade%>%filter(Equipe %in% input$team)%>%
                                group_by(Equipe)%>%plot_ly(type = 'bar', name="Points Domicile",x=~Equipe,y=~Points_Domicile)%>%
                                add_trace(y=~Points_Exterieur,name = "Points Exterieur",marker = list(color='rgb(58,200,225)'))%>%
                      layout(title="Points cumulés sur les 3 dernières saisons")})
  output$graph2<-renderPlotly({
    ppm%>%filter(Equipe %in% input$team)%>%
      group_by(Equipe)%>%plot_ly(type = 'bar', name="Points Domicile",x=~Equipe,y=~Points_Domicile)%>%
      add_trace(y=~Points_Exterieur,name = "Points Exterieur",marker = list(color='rgb(58,200,225)'))%>%
      layout(title="Points par match sur les 3 dernières saisons")})
  }
  
shinyApp(ui,server)
  

#https://community.rstudio.com/t/add-percentile-each-value-is-and-put-it-into-new-column/103711