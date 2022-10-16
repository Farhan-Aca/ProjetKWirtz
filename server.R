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
  curdata <- reactive({
    switch(input$dataset, "Global" = dff, "Par poste"= dff_post)})
  output$Tab1<-renderReactable({reactable(tableau_poste,filterable = TRUE,defaultPageSize= 5,striped = TRUE)})
  
  output$graph3<-renderPlotly({
    datase<-curdata()
    datase%>%filter(Equipe%in%input$joueur)%>%
      plot_ly(
        type = 'scatterpolar',
        mode = "closest",
        fill = 'toself',color = ~Equipe,fillcolor=~Equipe
      ) %>%add_trace(r=~percentile,
                     theta=~name,mode='markers',name~Equipe)})
  output$graph4<-renderPlotly({
    ah<-test%>%filter(Equipe %in% input$player)
    plot_ly(ah,x=~journee
            ,y=~get(input$y),frame=~journee,
            text=~Equipe,mode='markers')%>%
      add_text(text=~Equipe)%>%
      
      animation_opts(
        500, transition = 500, redraw = FALSE)})
  output$Tab2<-renderReactable({reactable(tableau_poste,filterable = TRUE,defaultPageSize= 5,striped = TRUE)})
  output$graph5<-renderPlotly({wsh%>%filter(Club_dom%in%input$cp)%>%group_by(Club_dom,compo_domicile)%>%plot_ly(
    type='pie', labels=~compo_domicile,values=~nb_dom_compo, 
    textinfo='label+percent',
    insidetextorientation='radial')
  })
  output$graph6<-renderPlotly({wsh%>%filter(Club_ext%in%input$cp)%>%group_by(Club_ext,compo_exterieur)%>%plot_ly(
    type='pie', labels=~compo_exterieur,values=~nb_ext_compo, 
    textinfo='label+percent',
    insidetextorientation='radial')
  })
  curdat <- reactive({
    switch(input$datase, "Victoire" = wsh_dom_v, "Nul"= wsh_dom_n,"Defaite"=wsh_dom_d)})
  output$graph7<-renderPlotly({data<-curdat()
  data%>%group_by(compo_domicile,nb_v_dom) %>%plot_ly(x = ~Club_dom, y = ~nb_dom_v, type = 'bar',color=~compo_domicile)%>% 
    layout(barmode = 'stack')
  })
  curda <- reactive({
    switch(input$datas, "Victoire" = wsh_ext_v, "Nul"= wsh_ext_n,"Defaite"=wsh_ext_d)})
  output$graph8<-renderPlotly({dataa<-curda()
  dataa %>%group_by(compo_exterieur,nb_v_ext) %>%plot_ly(x = ~Club_ext, y = ~nb_ext_v, type = 'bar',color=~compo_exterieur)%>% 
    layout(barmode = 'stack')
  })
  output$graph10<-renderPlotly({essai%>%filter(Equipe%in%input$cpin)%>%group_by(Club_dom,compo_domicile,Equipe)%>%plot_ly(
    type='pie', labels=~compo_domicile,values=~nb_dom_compo, 
    textinfo='label+percent',
    insidetextorientation='radial')
  })
  output$graph11<-renderPlotly({essai%>%filter(Equipe%in%input$cpin)%>%group_by(Club_ext,compo_exterieur,Equipe)%>%plot_ly(
    type='pie', labels=~compo_exterieur,values=~nb_ext_compo, 
    textinfo='label+percent',
    insidetextorientation='radial')
  })
  curd<- reactive({
    switch(input$da, "Victoire" = essai_dom_v, "Nul"= essai_dom_n,"Defaite"=essai_dom_d)})
  output$graph12<-renderPlotly({dataaa<-curd()
  dataaa%>%filter(Equipe%in%input$cpin)%>%
    group_by(compo_domicile,nb_v_dom,Equipe) %>%plot_ly(x = ~Equipe, y = ~nb_dom_v, type = 'bar',color=~compo_domicile)%>% 
    layout(barmode = 'stack')
  })
  cur<- reactive({
    switch(input$dat, "Victoire" = essai_ext_v, "Nul"= essai_ext_n,"Defaite"=essai_ext_d)})
  output$graph13<-renderPlotly({dataaaa<-cur()
  dataaaa %>%filter(Equipe%in%input$cpin)%>%
    group_by(compo_exterieur,nb_v_ext,Equipe) %>%plot_ly(x = ~Equipe, y = ~nb_ext_v, type = 'bar',color=~compo_exterieur)%>% 
    layout(barmode = 'stack')
  }) 
  output$Tabloo1<-renderReactable({reactable(indiv_bon,filterable = TRUE,defaultPageSize = 20)}
  )
  output$Tabloo2<-renderReactable({reactable(indiv_par_match,filterable = TRUE,defaultPageSize = 20)}
  )
  output$Tabloo3<-renderReactable({reactable(indiv_par_minute,filterable = TRUE,defaultPageSize = 20)}
  )
  output$Tabloo4<-renderReactable({reactable(wsh,filterable = TRUE,defaultPageSize = 20)}
  )
  output$Tabloo5<-renderReactable({reactable(ppm,filterable = TRUE,defaultPageSize = 20)}
  )
}