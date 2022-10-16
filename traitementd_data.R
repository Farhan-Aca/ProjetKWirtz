#Import data----        
#https://www.rstudio.com/blog/rstudio-v1-4-preview-little-things/
export_dataframe <- read_csv("export_dataframe.csv")
stade <- read_excel("stade.xlsx")
coord <- read_excel("coord.xlsx")
indiv_<-read_excel("data_tfoot.xlsx")
indiv_1<-read_excel("data_saison2020_21.xlsx")
indiv_2<-read_excel("data_saison2021_22.xlsx")
indiv_3<-read_excel("data_saison2021_22_part2.xlsx")
comp_<-read_excel("data_compo_saison2019.xlsx")
comp1<-read_excel("data_compo_saison2020.xlsx")
comp2<-read_excel("data_compo_saison2020_part2.xlsx")
comp3<-read_excel("data_compo_saison2021_2022.xlsx")

indiv<-rbind(indiv_,indiv_1,indiv_2,indiv_3)
comp<-rbind(comp_,comp1,comp2,comp3)


#######################################
#        Traitement data----
######################################

#on separe la colonne score en deux pour avoir les buts dom ext et on fait une colonne adequate
dom_ext<-export_dataframe%>%separate(Score,c("Dom","Ext"),sep=(2))
dom_ext<-dom_ext%>%mutate(Dom=substr(Dom,1,nchar(Dom)-1))
dom_ext

#on calcule le nb de points
dom_ext<-dom_ext%>%mutate(pts_dom=case_when(dom_ext$Dom>dom_ext$Ext~3,dom_ext$Dom==dom_ext$Ext~1,dom_ext$Dom<dom_ext$Ext~0))
dom_ext<-dom_ext%>%mutate(pts_ext=case_when(dom_ext$Ext>dom_ext$Dom~3,dom_ext$Ext==dom_ext$Dom~1,dom_ext$Ext<dom_ext$Dom~0))

#on compte le nombre de match effectué par equipe
dom_ext<-dom_ext%>%group_by(Domicile)%>%mutate(nb_match_dom=sum(length(Domicile)))
dom_ext<-dom_ext%>%group_by(Extérieur)%>%mutate(nb_match_ext=sum(length(Extérieur)))
dom_ext$id<-1:nrow(dom_ext)

#on compte le nb de pts dom et ext
ptsdom<-dom_ext%>%group_by(Domicile)%>%summarize(sum(pts_dom))
ptsext<-dom_ext%>%group_by(Extérieur)%>%summarize(sum(pts_ext))
#jointure
ptsstade<-ptsdom%>%left_join(ptsext,by=c("Domicile"="Extérieur"))
#bon nom
ptsstade<-ptsstade%>%dplyr::rename("Equipe"="Domicile","Points_Domicile"=`sum(pts_dom)`, "Points_Exterieur" = `sum(pts_ext)` )

#agregat par match
ppmdom<-dom_ext%>%group_by(Domicile)%>%summarize("Points_Domicile"=sum(pts_dom)/nb_match_dom)
ppmdom<-ppmdom%>%distinct(Domicile,Points_Domicile)
ppmext<-dom_ext%>%group_by(Extérieur)%>%summarize("Points_Exterieur"=sum(pts_ext)/nb_match_ext)
ppmext<-ppmext%>%distinct(Extérieur,Points_Exterieur)
ppm<-ppmext%>%left_join(ppmdom,by=c("Extérieur"="Domicile"))
ppm<-ppm%>%dplyr::rename("Equipe"="Extérieur")%>%dplyr::ungroup()

table(dom_ext$Domicile)

summary(coord)

#Coordonne au bon format

coord$Lat<-as.numeric(coord$Lat)
coord$Long<-as.numeric(coord$Long)

#categorisation des budgets
coord<-coord%>%mutate(top = dense_rank(desc(Budget)))
coord<-coord%>%mutate(tier=case_when(top<5~"Top 1 Budget",top>4&top<10~"Top 2 Budget",top>9&top<15~"Top 3 Budget",
                                     top>14~"Top 4 Budget"))

#Espace tous les 3 chiffres pour plus de lisibilité 
coord$Budget<-ifelse(!grepl("\\D", coord$Budget), format(as.numeric(coord$Budget), big.mark = " ", trim = T), string)
coord$Capacité<-ifelse(!grepl("\\D", coord$Capacité), format(as.numeric(coord$Capacité), big.mark = " ", trim = T), string)

#Couleur des markers
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


#data scrappee sur fbref, on va selectionner les bonnes lignes (il y a des lignes vides) et on renomme les colonnes 
indiv<-indiv%>%filter(!is.na(id))%>%rename(Equipe=`Equipe à domicile`)
indiv_prep<-indiv%>%select(id,`Equipe à l'exterieur`,Equipe)
#on cree une colonne où on sait qui est à domicile ou exterieur et une colonne où on n'a plus de vide
indiv_prep<-indiv_prep%>%mutate(lieu=if_else(!is.na(Equipe),"D","E"),
                                Equipe=case_when(is.na(Equipe)~`Equipe à l'exterieur`,!is.na(Equipe)~Equipe))
#jointure 
indiv_bon<-indiv_prep%>%cbind(indiv)
indiv_bon<-indiv_bon[,-c(2,5,7,36)]
#bon format
indiv_bon[,c(6:32)]<-sapply(indiv_bon[,c(6:32)],as.numeric)
#stats indiv, combien de match le joueur a joué ?
nb_match<-indiv_bon%>%count(Equipe)%>%rename(nb_match=n)
#jointure
indiv_bon<-indiv_bon%>%left_join(nb_match,by="Equipe")
#on fait le choix de selectionner seulement la postion numéro 1
indiv_bon<-indiv_bon%>%mutate(position = substr(position,1,2))
#agregat de plusieurs variables qui seront utilisées dans les graphiques.
indiv_bon <- indiv_bon %>% group_by(Equipe) %>%
  mutate(
    minutes_joues = sum(`Minute joué`),
    buts_marques = sum(buts),
    passe_de = sum(`passe décisive`),
    peno = sum(`penalty marqué`),
    peno_tire = sum(`penalty tiré`),
    tirs_total = sum(tirs),
    tirs_cadres = sum(`tirs cadrés`),
    cartons_jaunes = sum(cj),
    cartons_rouges = sum(cr),
    ballons_touches = sum(touches),
    nb_press=sum(press),
    nb_tacle=sum(tcl),
    nb_intercep=sum(int),
    ballons_contrees=sum(`balles contrées`),
    xG_total=sum(xG),
    npxG_total=sum(npxG),
    xA_total=sum(xA),
    action_menant_tir=sum(amt),
    action_menant_but=sum(amb),
    passes_reussies=sum(cmp),
    passe_tentees=sum(att),
    passe_avant=sum(`passe avant`),
    balle_au_pied=sum(balle_pied),
    drible_tentee=sum(`Dribble tenter`),
    drible_reussi=sum(`Dribble reussi`)
    
  )%>%ungroup()%>%group_by(Equipe)%>%mutate(nb_match_cumule=row_number(),nb_match_joue=max(nb_match))

indiv_par_match<-indiv_bon%>%select(
  id,
  Equipe,
  lieu,
  position,
  minutes_joues,
  buts_marques,
  passe_de,
  peno,
  peno_tire,
  tirs_total,
  tirs_cadres,
  cartons_jaunes,
  cartons_rouges,
  nb_press,
  nb_tacle,
  nb_intercep,
  ballons_contrees,
  xG_total,
  npxG_total,
  xA_total,
  action_menant_tir,
  action_menant_but,
  passes_reussies,
  passe_tentees,
  passe_avant,
  balle_au_pied,
  drible_tentee,
  drible_reussi,
  nb_match_joue)%>%mutate(buts_marques_match=buts_marques/nb_match_joue,
                          passe_de_match=passe_de/nb_match_joue,
                          peno_match=peno/nb_match_joue,
                          peno_tire_match=peno_tire/nb_match_joue,
                          tirs_total_match=tirs_total/nb_match_joue,
                          tirs_cadres_match=tirs_cadres/nb_match_joue,
                          cartons_jaunes_match=cartons_jaunes/nb_match_joue,
                          cartons_rouges_match=cartons_rouges/nb_match_joue,
                          nb_press_match=nb_press/nb_match_joue,
                          nb_tacle_match=nb_tacle/nb_match_joue,
                          nb_intercep_match=nb_intercep/nb_match_joue,
                          ballons_contrees_match=ballons_contrees/nb_match_joue,
                          xG_total_match=xG_total/nb_match_joue,
                          npxG_total_match=npxG_total/nb_match_joue,
                          xA_total_match=xA_total/nb_match_joue,
                          action_menant_tir_match=action_menant_tir/nb_match_joue,
                          action_menant_but_match=action_menant_but/nb_match_joue,
                          passes_reussies_match=passes_reussies/nb_match_joue,
                          passe_tentees_match=passe_tentees/nb_match_joue,
                          passe_avant_match=passe_avant/nb_match_joue,
                          balle_au_pied_match=balle_au_pied/nb_match_joue,
                          drible_tentee_match=drible_tentee/nb_match_joue,
                          drible_reussi_match=drible_reussi/nb_match_joue
  )

indiv_par_minute<-indiv_bon%>%select(
  id,
  Equipe,
  lieu,
  position,
  minutes_joues,
  buts_marques,
  passe_de,
  peno,
  peno_tire,
  tirs_total,
  tirs_cadres,
  cartons_jaunes,
  cartons_rouges,
  nb_press,
  nb_tacle,
  nb_intercep,
  ballons_contrees,
  xG_total,
  npxG_total,
  xA_total,
  action_menant_tir,
  action_menant_but,
  passes_reussies,
  passe_tentees,
  passe_avant,
  balle_au_pied,
  drible_tentee,
  drible_reussi,
  minutes_joues)%>%mutate(buts_marques_minute=buts_marques/minutes_joues,
                          passe_de_minute=passe_de/minutes_joues,
                          peno_minute=peno/minutes_joues,
                          peno_tire_minute=peno_tire/minutes_joues,
                          tirs_total_minute=tirs_total/minutes_joues,
                          tirs_cadres_minute=tirs_cadres/minutes_joues,
                          cartons_jaunes_minute=cartons_jaunes/minutes_joues,
                          cartons_rouges_minute=cartons_rouges/minutes_joues,
                          nb_press_minute=nb_press/minutes_joues,
                          nb_tacle_minute=nb_tacle/minutes_joues,
                          nb_intercep_minute=nb_intercep/minutes_joues,
                          ballons_contrees_minute=ballons_contrees/minutes_joues,
                          xG_total_minute=xG_total/minutes_joues,
                          npxG_total_minute=npxG_total/minutes_joues,
                          xA_total_minute=xA_total/minutes_joues,
                          action_menant_tir_minute=action_menant_tir/minutes_joues,
                          action_menant_but_minute=action_menant_but/minutes_joues,
                          passes_reussies_minute=passes_reussies/minutes_joues,
                          passe_tentees_minute=passe_tentees/minutes_joues,
                          passe_avant_minute=passe_avant/minutes_joues,
                          balle_au_pied_minute=balle_au_pied/minutes_joues,
                          drible_tentee_minute=drible_tentee/minutes_joues,
                          drible_reussi_minute=drible_reussi/minutes_joues
  )

indiv_ok<-indiv_bon[,c(2,34:60)]
indiv_ok<-indiv_ok%>%distinct(Equipe,.keep_all = TRUE)
df <- tidyr::pivot_longer(indiv_ok, !Equipe)

dff<-df%>%group_by(name)%>%mutate(percentile=cume_dist(value))

indiv_ok_post<-indiv_bon[,c(2,34:60)]
indiv_ok_post<-indiv_ok_post%>%distinct(Equipe,.keep_all = TRUE)
df_post <- tidyr::pivot_longer(indiv_ok_post, !Equipe)
indiv_ouais<-indiv_bon[,c(2,5)]
indiv_ouais<-indiv_ouais%>%distinct(Equipe,.keep_all = TRUE)
df_post<-df_post%>%left_join(indiv_ouais,by="Equipe")
dff_post<-df_post%>%group_by(name,position)%>%mutate(percentile=cume_dist(value))
dff_post<-dff_post[,-4]

sisi<-rep(1:114,each=10)
sisi<-as.data.frame(sisi)

oui<-data.frame(id=1:1140,journee=sisi)%>%rename(journee=sisi)
test<-indiv_bon%>%group_by(Equipe)%>%mutate(minutes_cumul=cumsum(`Minute joué`),
                                            buts_cumul=cumsum(buts),
                                            passe_de_cumul=cumsum(`passe décisive`),
                                            peno_cumul=cumsum(`penalty marqué`),
                                            peno_tire_cumul=cumsum(`penalty tiré`),
                                            tirs_total_cumul=cumsum(tirs),
                                            tirs_cadres_cumul=cumsum(`tirs cadrés`),
                                            cartons_jaunes_cumul=cumsum(cj),
                                            cartons_rouges_cumul=cumsum(cr),
                                            nb_press_cumul=cumsum(press),
                                            nb_tacle_cumul=cumsum(tcl),
                                            nb_intercep_cumul=cumsum(int),
                                            ballons_contrees_cumul=cumsum(`balles contrées`),
                                            xG_total_cumul=cumsum(xG),
                                            npxG_total_cumul=cumsum(npxG),
                                            xA_total_cumul=cumsum(xA),
                                            action_menant_tir_cumul=cumsum(amt),
                                            action_menant_but_cumul=cumsum(amb),
                                            passes_reussies_cumul=(cmp),
                                            passe_tentees_cumul=cumsum(att),
                                            passe_avant_cumul=cumsum(`passe avant`),
                                            balle_au_pied_cumul=cumsum(balle_pied),
                                            drible_tentee_cumul=cumsum(`Dribble tenter`),
                                            drible_reussi_cumul=cumsum(`Dribble reussi`))
test<-test%>%left_join(oui,by="id")
test<-test[,c(2,4,59:85)]


comp<-comp%>%filter(!is.na(id))
comp_lieu<-rep(c("D","E"))
compo<-comp%>%cbind(comp_lieu)
compo<-compo%>%mutate(composition=str_extract(`composition du match`, "(?<=\\().*(?=\\))"))
compo_ext<-compo%>%filter(comp_lieu=="E")%>%rename(compo_exterieur=composition,Club_ext=Club)
compo_dom<-compo%>%filter(comp_lieu=="D")%>%rename(compo_domicile=composition,Club_dom=Club)
composition<-compo_dom%>%left_join(compo_ext,by="id")%>%select(id,Club_dom,compo_domicile,Club_ext,compo_exterieur)
wsh<-dom_ext%>%left_join(composition,by="id")
wsh<-wsh%>%group_by(Club_dom,compo_domicile)%>%mutate(nb_dom_comp=1)%>%group_by(Club_dom,compo_domicile)%>%mutate(nb_dom_compo=max(nb_dom_comp))

wsh<-wsh%>%group_by(Club_ext,compo_exterieur)%>%mutate(nb_ext_comp=1)%>%group_by(Club_ext)%>%mutate(nb_ext_compo=max(nb_ext_comp))

wsh_dom_v<-wsh%>%filter(pts_dom==3)%>%group_by(Club_dom,compo_domicile,pts_dom)%>%mutate(nb_v_dom=1) %>%group_by(Club_dom,compo_domicile,pts_dom)%>%
  mutate(nb_dom_v=sum(nb_v_dom))
wsh_dom_n<-wsh%>%filter(pts_dom==1)%>%group_by(Club_dom,compo_domicile,pts_dom)%>%mutate(nb_v_dom=1) %>%group_by(Club_dom,compo_domicile,pts_dom)%>%
  mutate(nb_dom_v=sum(nb_v_dom))
wsh_dom_d<-wsh%>%filter(pts_dom==0)%>%group_by(Club_dom,compo_domicile,pts_dom)%>%mutate(nb_v_dom=1) %>%group_by(Club_dom,compo_domicile,pts_dom)%>%
  mutate(nb_dom_v=sum(nb_v_dom))

wsh_ext_v<-wsh%>%filter(pts_ext==3)%>%group_by(Club_ext,compo_exterieur,pts_ext)%>%mutate(nb_v_ext=1) %>%group_by(Club_ext,compo_exterieur,pts_ext)%>%
  mutate(nb_ext_v=sum(nb_v_ext))
wsh_ext_n<-wsh%>%filter(pts_ext==1)%>%group_by(Club_ext,compo_exterieur,pts_ext)%>%mutate(nb_v_ext=1) %>%group_by(Club_ext,compo_exterieur,pts_ext)%>%
  mutate(nb_ext_v=sum(nb_v_ext))
wsh_ext_d<-wsh%>%filter(pts_ext==0)%>%group_by(Club_ext,compo_exterieur,pts_ext)%>%mutate(nb_v_ext=1) %>%group_by(Club_ext,compo_exterieur,pts_ext)%>%
  mutate(nb_ext_v=sum(nb_v_ext))


essai<-indiv_bon[,c(1:2,4)]
essai<-essai%>%left_join(wsh,by="id")
essai_dom_v<-essai%>%filter(pts_dom==3)%>%group_by(Club_dom,compo_domicile,pts_dom,Equipe
)%>%mutate(nb_v_dom=1) %>%group_by(Club_dom,compo_domicile,pts_dom,Equipe)%>%
  mutate(nb_dom_v=sum(nb_v_dom))
essai_dom_n<-essai%>%filter(pts_dom==1)%>%group_by(Club_dom,compo_domicile,pts_dom,Equipe)%>%
  mutate(nb_v_dom=1) %>%group_by(Club_dom,compo_domicile,pts_dom,Equipe)%>%
  mutate(nb_dom_v=sum(nb_v_dom))
essai_dom_d<-essai%>%filter(pts_dom==0)%>%group_by(Club_dom,compo_domicile,pts_dom,Equipe)%>%
  mutate(nb_v_dom=1) %>%group_by(Club_dom,compo_domicile,pts_dom,Equipe)%>%
  mutate(nb_dom_v=sum(nb_v_dom))

essai_ext_v<-essai%>%filter(pts_ext==3)%>%group_by(Club_ext,compo_exterieur,pts_ext,Equipe
)%>%mutate(nb_v_ext=1) %>%group_by(Club_ext,compo_exterieur,pts_ext,Equipe)%>%
  mutate(nb_ext_v=sum(nb_v_ext))
essai_ext_n<-essai%>%filter(pts_ext==1)%>%group_by(Club_ext,compo_exterieur,pts_ext,Equipe
)%>%mutate(nb_v_ext=1) %>%group_by(Club_ext,compo_exterieur,pts_ext,Equipe)%>%
  mutate(nb_ext_v=sum(nb_v_ext))
essai_ext_d<-essai%>%filter(pts_ext==0)%>%group_by(Club_ext,compo_exterieur,pts_ext,Equipe
)%>%mutate(nb_v_ext=1) %>%group_by(Club_ext,compo_exterieur,pts_ext,Equipe)%>%
  mutate(nb_ext_v=sum(nb_v_ext))

tableau_poste<-indiv_bon%>%select(Club,Equipe,position)%>%rename(Joueur=Equipe)%>%distinct(Joueur,.keep_all = TRUE)%>%
  mutate(position = substr(position,1,2))
#icone style 
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

labels <- paste(
  "<strong>Ville :", coord$Ville,
  "</strong><br>Club :", coord$Club) %>%
  lapply(htmltools::HTML)

coord<-coord%>%mutate(tag=paste0("<a href=", Effectif,">", Effectif, "</a>"))

qpal <- colorFactor(c("purple","green","orange","red"), domain = c("LDC","Europe","Ventre mou","Galere"),ordered = T)