server <- function(input, output,session) {

  options(shiny.maxRequestSize=60*1024^2)

  ############
  ####DATA####
  ############

  metrics=eventReactive(input$file_prop,{
    temp_metrics=read.Chloe.properties(input$file_prop$datapath,"Metrics")
    temp_metrics[rev(order(temp_metrics))]
  })

  dist=eventReactive(input$file_prop,{
    read.Chloe.properties(input$file_prop$datapath,"Distance")

  })

  chloe=eventReactive(input$file_chloe,{
    read.csv(input$file_chloe$datapath, h=T, sep=";",check.names=F)
  })

  mfa <- reactiveVal()
  metrics_cor <- reactiveVal()
  volumes <- getVolumes()
  #dist <- reactiveValues(data = NULL)
  #chloe <- reactiveValues(data = NULL)
  Varia_paysage_model <- reactiveValues(data = NULL)
  shinyDirChoose(input, 'directory', roots=volumes, session=session)
  dir <- reactive({
    req(input$directory)
    paste(as.character(parseDirPath(volumes,input$directory)),"/",sep = "")
  })
  carab=reactive({
    read.csv(input$file_carab$datapath)[,c(2,3,4,5)]
  })

  pass_dir_map=reactiveVal(NULL)
  shinyDirChoose(input, 'directory_map', roots=volumes, session=session)
  dir_map <- reactive({
    req(input$directory_map)
    pass_dir_map(1)
    paste(as.character(parseDirPath(volumes,input$directory_map)),"/",sep = "")
  })


  points <- reactive({
    req(input$file_points)
    print(input$file_points$datapath)
    read.csv(input$file_points$datapath, sep = ";")

  })

  r <- reactive({
    req(input$file_ascii)
    req(pass_dir_map)
    raster(paste0(dir_map(),basename(input$file_ascii$name)))

  })

  temp <- reactive({
    req(points())
    req(r())
    temp=SpatialPoints(coords = points()[,c(2,3)], proj4string =crs(r()))
    spTransform(temp, CRS("+proj=longlat +datum=WGS84"))
  })

  # cor_spa=reactive({
  #   req(input$file_prop)
  #   req(input$directory)
  #   metrics<-read.Chloe.properties(input$file_prop$datapath,"Metrics")
  #   #dist<-read.Chloe.properties(input$file_prop$datapath,"Distance")
  #   dist=c(5,9,13)
  #   withProgress(message = "Application loading", value = 0, {
  #   cor_spatial(dir(),metrics,dist)
  #   })
  # })


  ##############
  ####Models####
  ##############

  tab_for_model=reactive({
    req(input$file_prop)
    req(input$file_chloe)




    formodel(chloe(),dist(),metrics())
  })


  output$plot1 <- renderPlotly({
    req(input$file_prop)
    req(input$file_chloe)
    req(input$metric)
    req(input$file_carab)
    req(input$species)
    req(input$scales)


    carab=read.csv(input$file_carab$datapath)[,c(2,3,4,5)]



    temp=plot_obs(tab_for_model(),dist(),input$metric,carab,input$species,paste("w",input$scales,sep = ""))



    ggplotly(eval(parse(text=(paste("temp$`",input$metric,"`$",paste("w",input$scales,sep = ""), sep = ""))))) %>%
      layout(autosize=TRUE)
  })

  output$plot2 <- renderPlotly({
    req(input$file_prop)
    req(input$file_chloe)
    req(input$metric)


    #chloe=read.csv(input$file_chloe$datapath, h=T, sep=";")
    #Varia_paysage_model=formodel(chloe,dist,metrics,plots = T)


    temp=plot_obs(tab_for_model(),dist(),input$metric)



    ggplotly(eval(parse(text=(paste("temp$`",input$metric,"`", sep = ""))))) %>%
      layout(autosize=TRUE)
  })


  output$plot3 <- renderPlotly({
    req(input$file_prop)
    req(input$file_chloe)
    req(input$metric)
    req(input$file_carab)
    req(input$species)




    carab=read.csv(input$file_carab$datapath)[,c(2,3,4,5)]
    Varia_paysage_model=merge(tab_for_model(),carab,by.x = "id", by.y="Id")
    radj=result_MARS(Varia_paysage_model, dist(),input$metric,input$species)

    mod_rsq=result_MARS(Varia_paysage_model, dist(),input$metric,input$species,tab = T)


    rsq_model=earth(radj~Distance,degree = 1,data = mod_rsq,nfold=10)
    test=format(rsq_model, style="pmax")
    test=gsub("\n","",test)
    Distance=as.numeric(dist())

    temp=cbind(dist(),eval(parse(text=test)))
    temp=as.data.frame(temp)

    colnames(temp)=c("Distance","Predict")
    temp$Distance=as.numeric(as.character(temp$Distance))
    temp$Predict=as.numeric(as.character(temp$Predict))



    col=gg_color_hue(1)

    p1=radj[[1]]+geom_line(aes(x=temp$Distance,y=temp$Predict),col=col)
    ggplotly(p1) %>%
      layout(autosize=TRUE)

  })


  output$plot4 <- renderPlotly({
    req(input$file_prop)
    req(input$file_chloe)
    req(input$metric)
    req(input$file_carab)
    req(input$species)




    carab=read.csv(input$file_carab$datapath)[,c(2,3,4,5)]
    Varia_paysage_model=merge(tab_for_model(),carab,by.x = "id", by.y="Id")
    radj=result_ALL(Varia_paysage_model, dist(),input$metric,input$species)

    col=gg_color_hue(1)


    ggplotly(radj[[1]]) %>%
      layout(autosize=TRUE)

  })
  #
  #   r <- reactive({
  #     #raster(input$file_ascii$datapath)
  #     raster("C:/Users/pglem/Documents/Master/Stage M2/Données/ZAAR_dataSIG/cover2016/cover_bois_2016.asc")
  #   })

  ###########
  ####MAP####
  ###########



  output$table_carab=renderDataTable({
    req(input$file_carab)

    DT::datatable(carab(), selection = "single",options=list(stateSave = TRUE))
  })


  observeEvent(input$mymap_shape_click, {
    clickId <- input$mymap_shape_click$id
    clickIdNum <- which(carab()$Id == clickId)
    dataTableProxy("table_carab") %>%
      selectRows(which(carab()$Id == clickId)) %>%
      selectPage(min(which(input$table_carab_rows_all == clickIdNum)) %/% input$table_carab_state$length + 1)
  })




  observeEvent(input$table_carab_rows_selected,{
    id=input$table_carab_rows_selected
    id=carab()[id,1]
    id=as.character(id)
    temp=SpatialPoints(coords = points()[points()$ID==id,c(2,3)], proj4string =crs(r()))
    temp=spTransform(temp, CRS("+proj=longlat +datum=WGS84"))

    if(!is.null(prev_id()))
    {
      proxy = leafletProxy('mymap')
      proxy %>%
        clearMarkers()
    }

    #color_pal <- colorNumeric(palette = "red", domain = id, reverse = F)
    proxy = leafletProxy('mymap')
    proxy %>%
      addMarkers(popup = id,lng = temp@coords[,1],lat = temp@coords[,2])

    # set new value to reactiveVal
    prev_id(id)
  })

  output$selected=renderText({
    req(input$table_rows_selected)
    as.character(input$table_rows_selected)})





  observeEvent(temp(),{

    pal <- colorNumeric(c("#ffb3ba","#ffdfba","#ffffba","#baffc9","#bae1ff"), values(r()),
                        na.color = "transparent")

    print("OK5")

    output$mymap <- renderLeaflet({
      # req(input$file_ascii)
      # req(input$file_points)
      # req(temp)
      # ras=r()
      leaflet(temp()) %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(r(), opacity = 0.5, colors = pal) %>%
        addCircles(popup = as.character(points()$ID),radius=rep(1000,30), color="#6495ed",
                   opacity = 0.5, fill = TRUE, fillColor = "#6495ed",
                   fillOpacity = 0.2,layerId=as.character(points()$ID)) %>%
        addLegend(pal = pal, values = values(r()),
                  title = "Occupation du sol")

    })
  })






  ######################
  ####REACTIVE MENUS####
  ######################

  prev_id <- reactiveVal()
  prev_points <- reactiveVal()

  choices_dropdown_scales = reactive({
    choices<-read.Chloe.properties(input$file_prop$datapath,"Distance")
    return(as.list(choices))
  })


  observeEvent(input$file_prop,{
    updateSelectInput(session,"scales",choices = choices_dropdown_scales())
  })

  choices_dropdown_metrics = reactive({
    if(is.null(metrics_cor())){
      choices<-metrics()
    }
    else{choices=metrics_cor()}
    # choices<-read.Chloe.properties(input$file_prop$datapath,"Metrics")
    return(as.list(choices))
  })


  observeEvent(input$file_prop,{
    updateSelectInput(session,"metric",choices = choices_dropdown_metrics())
  })

  observeEvent(metrics_cor(),{
    updateSelectInput(session,"metric",choices = as.list(metrics_cor()))
  })

  choices_dropdown_species = reactive({
    choices<-colnames(carab()[,2:ncol(carab())])
    return(as.list(choices))
  })


  observeEvent(input$file_carab,{
    updateSelectInput(session,"species",choices = choices_dropdown_species())
  })

  #####################
  #####CORRELATION#####
  #####################

  observeEvent(input$analysis, {

    #dist<-read.Chloe.properties(input$file_prop$datapath,"Distance")
    dist=c(5,9,13)
    withProgress(message = "Computed landscape variable correlation", value = 0, {
      toto(cor_spatial(dir(),metrics(),dist))

      typegroupe=rep("s",length(metrics())) #Les groupes de métriques sont des variables quatitative non-normalisé
      ngroup=rep(length(dist),length(metrics()))

      mfa(MFA(toto(),group=ngroup, #MFA
              type = typegroupe,name.group=metrics(),graph = FALSE,axes = c(1,2)))



      output$cor_plot1 <- renderPlot({
        ggcorrplot(mfa()$group$RV, type = "upper",lab = FALSE,method = "circle",title = "Sliding Analysis")
      })

    })
  })



  output$cor_plot2 <- renderPlot({
    req(input$file_prop)
    req(input$random_points)
    print("1")


    random_pts=read.csv(input$random_points$datapath,check.names=F)
    random_pts=cbind(seq(1,nrow(random_pts),1),random_pts)
    colnames(random_pts)[1]="id"
    print("2")

    Varia_paysage_multi=formultivariate(random_pts,dist(),metrics())
    print("3")

    withProgress(message = "Computed RV coefficients", value = 0, {
    test=RV_COR_shiny(Varia_paysage_multi,metrics(),dist())
    })
    print("4")

    temp_cor=findCorrelation(test[[1]], cutoff = 0.7, exact = TRUE)

    temp=test[[1]][-temp_cor,-temp_cor]
    temp_p=test[[2]][-temp_cor,-temp_cor]

    metrics_cor(colnames(temp))

    ggcorrplot(temp, type = "upper",lab = FALSE,method = "circle",p.mat = temp_p, sig.level = 0.05,
               hc.order = FALSE)
  })

  #outputOptions(output, "mymap", suspendWhenHidden = FALSE)
}
