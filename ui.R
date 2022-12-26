ui <- dashboardPage(skin="purple",title="Premier League stats",
                    dashboardHeader(title ='PL stats',titleWidth = 300),
                    dashboardSidebar(
                      sidebarMenu(HTML(paste0(
                        "<br>",
                        "<a href='https://github.com/Flojea/ProjetKWirtz' target='_blank'><img style = 'display: block;
                          margin-left: auto; margin-right: auto;' src='https://static.onzemondial.com/article/img_evenement/img-176993.jpg' 
                          width = '186'></a>",
                        "<br>",
                        "<p style = 'text-align: center;'><small><a 
                          href='https://fr.wikipedia.org/wiki/Championnat_d%27Angleterre_de_football' target='_blank'>Logo de la Premier League</a>
                          </small></p>",
                        "<br>")),
                        menuItem("Read",tabName="lire",icon=icon("house")),
                        menuItem("Clubs",tabName="Carte",icon = icon("house")),
                        menuItem("Stats equipe",tabName = "Stat",icon=icon("house"),startExpanded = FALSE,
                                 menuSubItem("Domicile/Extérieur",tabName = "DE",icon=icon("house")),
                                 menuSubItem("Composition",tabName = "Cpe",icon=icon("house"))),
                        menuItem("Stats individuelle",tabName ="Statsi",icon = icon("house"),startExpanded = FALSE,
                                 menuSubItem("Graphique des aptitudes",tabName = "G",icon=icon("house")),
                                 menuSubItem("Evolution",tabName = "Evol",icon=icon("house")),
                                 menuSubItem("Compo",tabName = "compo_j",icon=icon("house"))),
                        menuItem("Liens",tabName = "Li",icon=icon("house")),
                        menuItem("Tableaux",tabName = "Tablo",icon=icon("house"),startExpanded = FALSE,
                                 menuSubItem("General",tabName = "Tablo1",icon=icon("house")),
                                 menuSubItem("Individuel par match",tabName = "Tablo2",icon=icon("house")),
                                 menuSubItem("Individuel par minute",tabName = "Tablo3",icon=icon("house")),
                                 menuSubItem("Compo",tabName = "Tablo4",icon=icon("house")),
                                 menuSubItem("Points par match",tabName = "Tablo5",icon=icon("house"))
                        )
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "lire",fluidPage(includeHTML("wirtz.html")
                        )),
                        tabItem(tabName = "Li",fluidPage(includeHTML("liens.html")
                        )),
                        tabItem(tabName = "Carte",fluidRow(column(6,leafletOutput("map",height=700)),
                                                           column(6,tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')), 
                                                                  a(class="twitter-timeline","data-height"="700","data-width"="600",
                                                                    href="https://twitter.com/projetwirtz/lists/1573949714982211590"
                                                                  )))),
                        tabItem(tabName = "DE",fluidPage(
                          selectizeInput("team","Choix de l'equipe",choices = sort(unique(dom_ext$Domicile)),
                                         multiple=TRUE),
                          plotlyOutput("graph1"),plotlyOutput("graph2"))),
                        tabItem(tabName = "Cpe",h1("Gauche = Domicile, Droite = Extérieur"),
                                fluidPage(selectizeInput("cp","Choix de l'equipe",choices = sort(unique(wsh$Club_dom)),
                                                         multiple=FALSE)),radioButtons(inputId = "datase","Choisis pour domicile",
                                                                                       choices = c("Victoire","Nul","Defaite"),
                                                                                       inline=TRUE),
                                radioButtons(inputId = "datas","Choisis pour exterieur",
                                             choices = c("Victoire","Nul","Defaite"),
                                             inline=TRUE)
                                ,fluidRow(column(6,plotlyOutput("graph5")),
                                          column(6,plotlyOutput("graph6"))),
                                fluidRow(column(6,style='padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:0px',
                                                plotlyOutput("graph7")),
                                         column(6,style='padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:0px',
                                                plotlyOutput("graph8")))),
                        tabItem(tabName = "G",fluidRow(selectizeInput("joueur","Choix du joueur",choices = sort(unique(indiv_bon$Equipe)),
                                                                      multiple=TRUE),radioButtons(inputId = "dataset","Choisis ton type chacal",
                                                                                                  choices = c("Global","Par poste"),inline=TRUE),
                                                       plotlyOutput("graph3"),
                                                       reactableOutput("Tab1"))),
                        tabItem(tabName = "Evol",fluidPage(selectizeInput("player","Choix du joueur",choices = sort(unique(indiv_bon$Equipe)),
                                                                          multiple=TRUE),
                                                           selectInput('y', 'Choisis', choices = colnames(test)),
                                                           plotlyOutput("graph4"), reactableOutput("Tab2"))),
                        tabItem(tabName = "compo_j",h1("Gauche = Domicile, Droite = Extérieur"),
                                fluidPage(selectizeInput("cpin","Choix de l'equipe",choices = sort(unique(essai$Equipe)),
                                                         multiple=FALSE)),radioButtons(inputId = "da","Choisis pour domicile",
                                                                                       choices = c("Victoire","Nul","Defaite"),
                                                                                       inline=TRUE),
                                radioButtons(inputId = "dat","Choisis pour exterieur",
                                             choices = c("Victoire","Nul","Defaite"),
                                             inline=TRUE)
                                ,fluidRow(column(6,plotlyOutput("graph10")),
                                          column(6,plotlyOutput("graph11"))),
                                fluidRow(column(6,style='padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:0px',
                                                plotlyOutput("graph12")),
                                         column(6,style='padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:0px',
                                                plotlyOutput("graph13")))),
                        tabItem(tabName = "Tablo1",reactableOutput("Tabloo1")),
                        tabItem(tabName = "Tablo2",reactableOutput("Tabloo2")),
                        tabItem(tabName = "Tablo3",reactableOutput("Tabloo3")),
                        tabItem(tabName = "Tablo4",reactableOutput("Tabloo4")),
                        tabItem(tabName = "Tablo5",reactableOutput("Tabloo5"))
                      )))
