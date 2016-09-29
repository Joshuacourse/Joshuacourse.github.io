


shinyServer(function(input,output){
  
  
  selected_df= reactive({
    
    df = yelpBusiness1
    if(input$RestaurantType != "All") { 
      df = df %>%
        filter(NewCategory == input$RestaurantType)
    }
    
    if(input$PriceRange != "All") {
      df = df %>%
        filter(PriceCategory == input$PriceRange)
    }
    
    if(input$RatingType != "All") {
      df = df %>%
        filter(stars_of_business == input$RatingType)
    }
    return (df)
    
  })  
  
  
  ################################### Leaflet ######################################################333333
  
  

  output$map =renderLeaflet({
    palColor = colorFactor(c("blue", "red"),
                           domain = c("Above Average", "Below Average"))
    
    YelpIcons <- icons(
      iconUrl = ifelse(selected_df()$NewRating == "Above Average",
                       "https://camo.githubusercontent.com/afa9cd3e3fde5e3768f0061f4a1d330d0cb25383/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d626c75652e706e673f7261773d74727565",
                       "https://camo.githubusercontent.com/70c53b19fb9ec32c09ff59b4aebe6bb8058dfb8b/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d7265642e706e673f7261773d74727565"
      ),
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = 10, iconAnchorY = 10
    )

    leaflet(selected_df()) %>%
      addTiles() %>%  
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addMarkers(icon = YelpIcons,
                 clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15,
                                                       # animateAddingMarkers= TRUE,
                                                       animate = TRUE),
                popup = ~ paste( sep = "<br/>",
                                 "<b>Description</b>",
                                 paste('<b>business_id : </b>', business_id,sep = ' '  ),
                                 paste('<b>stars :</b>', stars_of_business, sep = ' '),
                                 paste('<b>category:</b>',  categories, sep = ' '),
                                 paste('<b>Avg Category star:</b>',  AVG_stars_category, sep = ' ')
                )) %>%
      setView(lng = -112,
              lat = 33.5,
              zoom = 12)   %>%
      addLegend("topleft", pal = palColor,
                values = ~ selected_df()$NewRating,
                title = "Type of Restaurants",
                opacity = 0.5   )
  })
  
  
 
 #######################################3 # plot inside Leaflet #######################################################3
  
  datatable22 = reactive({
    datatable2 = as.data.table( selected_df(), rownames=FALSE)

    if (is.null(input$map_bounds))
      return(datatable2[FALSE,])
    bounds = input$map_bounds
    latRng = range(bounds$north, bounds$south)
    lngRng = range(bounds$east, bounds$west)

    datatable2[latitude >= latRng[1] & latitude <= latRng[2] &
                 longitude >= lngRng[1] & longitude <= lngRng[2]]

  })


  output$barplot1 = renderPlot({
   # library(ggplot2)

    df2 = as.data.frame(datatable22())
    df_plot2 = df2  %>%
            group_by(NewRating) %>%
            summarise(Total = n())

    color1 = c("blue","red")
    my_func = function(x){
      if(x=='Above Average') return ('1')
      else  return ('2')
    }

    names(color1) = c('1','2')
    color1 = color1[sapply(unique(df_plot2$NewRating), my_func)]
    
    g = ggplot(data=df_plot2,
                aes(x=factor(NewRating),y=Total))
    g = g + geom_bar(stat='identity', fill=color1)
    g = g + xlab('Type of Rating')
    g = g + ylab('Frequency')
    g = g + theme_minimal()
    #g = g + ylim(0,2000)
    g
  })
  
  
  
  
  #-#####################################3Datatable ############################################################3
  
  # show data using DataTable
  output$table = DT::renderDataTable({
    
    datatable( 
      yelpBusiness1,  rownames=FALSE) %>%
              formatStyle(input$selected,
                           background="skyblue", fontWeight='bold')
    
  })
  
  
  
  
  
  #######################################3 LDA Selectbox ###############################################################3
  
  LDAType= reactive({
    return (input$LDAid)
  })
  
  output$myChartLDASelect <- renderVis({
    if (LDAType() == "1"){
      json_American_Positive
    }
    else if (LDAType() == "2"){
      json_Chinese_Positive
    }
    else if (LDAType() == "3"){
      json_Italian_Positive
    }
    else if (LDAType() == "4"){
      json_Thai_Positive
    }
    else if (LDAType() == "5"){
      json_Mexican_Positive
    }
    else if (LDAType() == "6"){
      json_Greek_Positive
    }
    else if (LDAType() == "7"){
      json_Japanese_Positive
    }
    else if (LDAType() == "8"){
      json_Cafes_Positive
    }
    else if (LDAType() == "9"){
      json_Asian_Fusion_Positive
    }
    else if (LDAType() == "10"){
      json_Barbeque_Positive
    }
    else if (LDAType() == "11"){
      json_Breakfast_Brunch_Positive
    }
    else if (LDAType() == "12"){
      json_Buffets_Positive
    }
    else if (LDAType() == "13"){
      json_Chicken_Wings_Positive
    }
    else if (LDAType() == "14"){
      json_Delis_Positive
    }
    else if (LDAType() == "15"){
      json_Fast_Food_Positive
    }
    else if (LDAType() == "16"){
      json_Pizza_Positive
    }
    else{
      json_Sandwiches_Positive
    }
    
  })
  
  
  ################################################### Barplots ###############################################3
  
  output$plothour = renderGvis({
    gvisColumnChart(data = checkingtime_time,
                    xvar = "hours",
                    yvar = "checkingTotal",
                    options=list(
                      chartArea=  "{left:110,top:50,width:'81%',height:'55%'}",
                      seriesType="bars",
                      legend ="top",
                      title="Total number of check-in by time of the day ",
                      height = 600,
                      width = 1200,
                      hAxis= "{title: 'Hours'}",
                      vAxis= "{title: 'Total Count of Check-in '}"
                    ))
    
  })
  
  output$plotdays = renderGvis({
    gvisColumnChart(data = checkingtime_days,
                    xvar = "days",
                    yvar = "checkingTotal",
                    options=list(
                      chartArea=  "{left:110,top:50,width:'75%',height:'55%'}",
                      seriesType="bars",
                      legend ="top",
                      bar="{groupWidth:'40%'}",
                      title="Total number of check-in by days of the week ",
                      height = 600,
                      width = 1180,
                      hAxis= "{title: 'Days of the week'}",
                      vAxis= "{title: 'Total Count of Check-in '}"
                    ))
    
  })
  
  
  #dayhours
  checkingtime_dayhours = checkingTime %>%
    arrange(desc(check_in_number)) %>%
    top_n(15)
  checkingtime_dayhours['dayHour'] = paste(checkingtime_dayhours$days, checkingtime_dayhours$hours)
  checkingtime_dayhours$dayHour <- factor(checkingtime_dayhours$dayHour, 
                                          levels=unique(checkingtime_dayhours$dayHour))
  
  
  checkingtime_dayhours %>% 
    ggvis(~dayHour, ~check_in_number, fill=~days) %>%
    layer_bars(width = 0.5) %>%
    set_options(height = 600, width = 1100) %>%
  bind_shiny("plotdaytime")
  
  
  
  ############################## Image Selection  #########################################################3333333
  
  ImageType= reactive({
    return (input$Imageid)
  })
  
  output$ImageSelect <- renderUI({
    
    
    if (ImageType() == "1"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false,
          
          after: function() {
                  $("#name").text(this.alt);
          }



           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           <p><b>1 Baked Macaroni and Cheese</b><br /><img src="img/American/baked-mac-and-cheese.jpg"></p>
           <p><b>2 Pulled Pork</b><br /><img src="img/American/pulledpork.jpg"  alt="Pulled Pork"></p>
           <p><b>3 French Toast</b><br /><img src="img/American/frenchtoast.jpg" ></p>
           <p><b>4 Sweet Potato Fries</b><br /><img src="img/American/sweet_potato_fries.jpg" ></p>
           <p><b>5 Prime Rib</b><br /><img src="img/American/primerib.jpg" ></p>
           <p><b>6 Beer</b><br /><img src="img/American/beer.jpg" ></p>
           <p><b>7 Mashed Potatoes</b><br /><img src="img/American/mashedpotatoes.jpg" ></p>
           <p><b>8 Short Ribs</b><br /><img src="img/American/shortribs.jpg" ></p>
           <p><b>9 Eggs Benedict</b><br /><img src="img/American/eggsbenedict.jpg" ></p>
           <p><b>10 Pork Belly</b><br /><img src="img/American/porkbelly.jpg" ></p>
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           ')
    }
    
    else if (ImageType() == "2"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Fried Rice</b><br /><img src="img/Chinese/friedrice.jpg" ></p>
           <p><b>2 Orange Chicken</b><br /><img src="img/Chinese/orangechicken.jpg" ></p>
           <p><b>3 Sweet & Sour Pork</b><br /><img src="img/Chinese/sweetsourpork.jpg" ></p>
           <p><b>4 Lo Mein</b><br /><img src="img/Chinese/lomein.jpg" ></p>
           <p><b>5 Crab Puffs</b><br /><img src="img/Chinese/crabpuffs.jpg" ></p>
           <p><b>6 Egg Drop Soup</b><br /><img src="img/Chinese/eggdropsoup.jpg" ></p>
           <p><b>7 Hot Sour Soup</b><br /><img src="img/Chinese/hotsoursoup.jpg" ></p>
           <p><b>8 Egg Roll</b><br /><img src="img/Chinese/eggroll.jpg" ></p>
           <p><b>9 Dim Sum</b><br /><img src="img/Chinese/dimsum.jpg" ></p>
           <p><b>10 Teriyaki Chicken</b><br /><img src="img/Chinese/TeriyakiChicken.jpg" ></p>
           
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else if (ImageType() == "3"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Thin Crust Pizza</b><br /><img src="img/Italian/thincrustpizza.jpg" ></p>
           <p><b>2 Garlic Knots</b><br /><img src="img/Italian/garlicknots.jpg" ></p>
           <p><b>3 Marinara Sauce</b><br /><img src="img/Italian/marinarasauce.jpg" ></p>
           <p><b>4 Chicken Parmesan</b><br /><img src="img/Italian/chickenparm.jpg" ></p>
           <p><b>5 Olive Oil</b><br /><img src="img/Italian/oliveoil.jpg" ></p>
           <p><b>6 Chicken Marsala</b><br /><img src="img/Italian/chickenmarsala.jpg" ></p>
           <p><b>7 Margherita Pizza</b><br /><img src="img/Italian/margheritapizza.jpg" ></p>
           <p><b>8 Goat Cheese</b><br /><img src="img/Italian/goatcheese.jpg" ></p>
           <p><b>9 Caprese Salad</b><br /><img src="img/Italian/capresesalad.jpg" ></p>
           <p><b>10 Wood Fired Pizza</b><br /><img src="img/Italian/woodfiredpizza.jpg" ></p>
           
           
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else if (ImageType() == "4"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Pad Thai</b><br /><img src="img/Thai/padthai.jpg" ></p>
           <p><b>2 Fried Rice</b><br /><img src="img/Thai/FriedRice.jpg" ></p>
           <p><b>3 Red Curry</b><br /><img src="img/Thai/RedCurry.jpg" ></p>
           <p><b>4 Green Curry</b><br /><img src="img/Thai/GreenCurry.jpg" ></p>
           <p><b>5 Thai Basil</b><br /><img src="img/Thai/thaibasil.jpg" ></p>
           <p><b>6 Panang Curry</b><br /><img src="img/Thai/panangcurry.jpg" ></p>
           <p><b>7 Sticky Rice</b><br /><img src="img/Thai/ThaiStickyRice.jpg" ></p>
           <p><b>8 Drunken Noodle</b><br /><img src="img/Thai/Drunkennoodles.jpg" ></p>
           <p><b>9 Spring Rolls</b><br /><img src="img/Thai/springrolls.jpg" ></p>
           <p><b>10 Thai Iced Tea</b><br /><img src="img/Thai/thaiicedtea.jpg" ></p>
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    
    
    else if (ImageType() == "5"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Carne Asada Burrito</b><br /><img src="img/Mexican/carneasadaburrito.jpg" ></p>
           <p><b>2 Salsa & Chips</b><br /><img src="img/Mexican/salsachips.jpg" ></p>
           <p><b>3 Fish Tacos</b><br /><img src="img/Mexican/fishtacos.jpg" ></p>
           <p><b>4 Street Tacos</b><br /><img src="img/Mexican/streettacos.jpg" ></p>
           <p><b>5 Salsa Bar</b><br /><img src="img/Mexican/salsabar.jpg" ></p>
           <p><b>6 Al Pastor</b><br /><img src="img/Mexican/alpastor.jpg" ></p>
           <p><b>7 Green Chilis</b><br /><img src="img/Mexican/greenchilis.jpg" ></p>
           <p><b>8 Flour Tortilla</b><br /><img src="img/Mexican/flourtortillas.jpg" ></p>
           <p><b>9 Red Salsa</b><br /><img src="img/Mexican/redsalsa.jpg" ></p>
           <p><b>10 Refried Beans</b><br /><img src="img/Mexican/refriedbeans.jpg" ></p>
           
           
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    
    else if (ImageType() == "6"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
       	    <p><b>1 greek salad</b><br /><img src="img/Greek/1greek_salad.jpg" ></p>
           <p><b>2 pita bread</b><br /><img src="img/Greek/2pitabread.jpg"></p>
           <p><b>3 greek fries</b><br /><img src="img/Greek/3greekfries.jpg" ></p>
           <p><b>4 gyromeat</b><br /><img src="img/Greek/4gyromeat.jpg" ></p>
           <p><b>5 chicken shawarma</b><br /><img src="img/Greek/5chickenshawarma.jpg" ></p>
           <p><b>6 chicken souvlaki</b><br /><img src="img/Greek/6chickensouvlaki.jpg"></p>
           <p><b>7 chicken kabob</b><br /><img src="img/Greek/7chickenkabob.jpg" ></p>
           <p><b>8 feta cheese.</b><br /><img src="img/Greek/8fetacheese.jpg" ></p>
           <p><b>9 lamb gyro</b><br /><img src="img/Greek/9lambgyro.jpg" ></p>
           <p><b>10 lentil soup</b><br /><img src="img/Greek/10lentilsoup.jpg"></p>
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    
    
    
    else if (ImageType() == "7"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Spicy tuna roll</b><br /><img src="img/japanese/1spicytunaroll.jpg" ></p>
           <p><b>2 Bento box</b><br /><img src="img/japanese/2bentobox.jpg"></p>
           <p><b>3 Pork belly</b><br /><img src="img/japanese/3porkbelly.jpg" ></p>
           <p><b>4 Miso soup</b><br /><img src="img/japanese/4misosoup.jpg"></p>
           <p><b>5 Tuna roll</b><br /><img src="img/japanese/5tunaroll.jpg" ></p>
           <p><b>6 California roll</b><br /><img src="img/japanese/6californiaroll.jpg"></p>
           <p><b>7 Blue fin</b><br /><img src="img/japanese/7bluefin.jpg"></p>
           <p><b>8 Teriyaki Chicken.</b><br /><img src="img/japanese/8TeriyakiChicken.jpg" ></p>
           <p><b>9 Fried rice</b><br /><img src="img/japanese/9friedrice.jpg"></p>
           <p><b>10 Ice cream</b><br /><img src="img/japanese/10icecream.jpg"></p>

           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    
    else if (ImageType() == "8"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Chips and salsa</b><br /><img src="img/Cafes/1chipsandsalsa.jpg" ></p>
           <p><b>2 French toast</b><br /><img src="img/Cafes/2frenchtoast.jpg"></p>
           <p><b>3 Grilled cheese</b><br /><img src="img/Cafes/3grilledcheese.jpg"></p>
           <p><b>4 Breakfast sandwich</b><br /><img src="img/Cafes/4breakfastsandwich.jpg"></p>
           <p><b>5 Chicken salad</b><br /><img src="img/Cafes/5chickensalad.jpg" ></p>
           <p><b>6 Baked goods</b><br /><img src="img/Cafes/6bakedgoods.jpg"></p>
           <p><b>7 Breakfast burrito</b><br /><img src="img/Cafes/7breakfastburrito.jpg"></p>
           <p><b>8 Cream cheese</b><br /><img src="img/Cafes/8creamcheese.jpg"></p>
           <p><b>9 Chocolate chip cookies</b><br /><img src="img/Cafes/9chocolatechipcookies.jpg"></p>
           <p><b>10 Chicken pot pie.</b><br /><img src="img/Cafes/10chickenpotpie.jpg"></p>
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    
    else if (ImageType() == "9"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Orange chicken</b><br /><img src="img/AsianFusion/1orangechicken.jpg"></p>
           <p><b>2 Spring rolls</b><br /><img src="img/AsianFusion/2springrolls.jpg"></p>
           <p><b>3 Jadered chicken</b><br /><img src="img/AsianFusion/3jaderedchicken.JPG"></p>
           <p><b>4 Black beans</b><br /><img src="img/AsianFusion/4blackbeans.jpg"></p>
           <p><b>5 Jerk fried rice</b><br /><img src="img/AsianFusion/5jerkfriedrice.JPG" ></p>
           <p><b>6 Mongolian beef</b><br /><img src="img/AsianFusion/6mongolianbeef.JPG"></p>
           <p><b>7 Sushi</b><br /><img src="img/AsianFusion/7Sushi.png"></p>
           <p><b>8 Pad thai</b><br /><img src="img/AsianFusion/8padthai.JPG"></p>
           <p><b>9 Hot pot</b><br /><img src="img/AsianFusion/9hotpot.jpg"></p>
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    else if (ImageType() == "10"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Baked mac and cheese</b><br /><img src="img/Barbeque/1bakedmacandcheese.jpg"></p>
           <p><b>2 BBQ sauce</b><br /><img src="img/Barbeque/2bbqsauce.jpg"></p>
           <p><b>3 Potato salad</b><br /><img src="img/Barbeque/3potatosalad.jpg"></p>
           <p><b>4 Fatty brisket</b><br /><img src="img/Barbeque/4fattybrisket.jpg"></p>
           <p><b>5 Pork ribs</b><br /><img src="img/Barbeque/5porkribs.jpg"></p>
           <p><b>6 Jalapeno</b><br /><img src="img/Barbeque/6jalapeno.jpg"></p>
           <p><b>7 Pulled pork sandwich</b><br /><img src="img/Barbeque/7pulledporksandwich.jpg"></p>
           <p><b>8 Pecanpie</b><br /><img src="img/Barbeque/8pecanpie.png"></p>
           <p><b>9 Brisketpulledpork</b><br /><img src="img/Barbeque/9brisketpulledpork.jpg"></p>
           <p><b>10 Macaroni salad</b><br /><img src="img/Barbeque/10macaronisalad.JPG"></p>
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else if (ImageType() == "11"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 French toast</b><br /><img src="img/BreakfastandBrunch/1frenchtoast.jpg"></p>
           <p><b>2 Cream cheese</b><br /><img src="img/BreakfastandBrunch/2creamcheese.jpg"></p>
           <p><b>3 Hashbrowns</b><br /><img src= "img/BreakfastandBrunch/3hashbrowns.jpg"></p>
           <p><b>4 Breakfast burrito</b><br /><img src= "img/BreakfastandBrunch/4breakfastburrito.jpg"></p>
           <p><b>5 Egg benedict</b><br /><img src="img/BreakfastandBrunch/5eggsbenedict.jpg"></p>
           <p><b>6 Goat cheese</b><br /><img src=  "img/BreakfastandBrunch/6goatcheese.jpg"></p>
           <p><b>7 Breakfast sandwich</b><br /><img src="img/BreakfastandBrunch/7breakfastsandwich.jpg"></p>
           <p><b>8 Orange juice</b><br /><img src="img/BreakfastandBrunch/8orangejuice.jpg"></p>
           <p><b>9 Corned beef</b><br /><img src="img/BreakfastandBrunch/9cornedbeef.jpg"></p>
           <p><b>10 Bruschettaboard</b><br /><img src="img/BreakfastandBrunch/10bruschettaboard.jpg"></p>
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    else if (ImageType() == "12"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Garlic naan</b><br /><img src="img/Buffet/1garlicnaan.jpg"></p>
           <p><b>2 Chicken tikka masala</b><br /><img src="img/Buffet/2chickentikkamasala.jpg"></p>
           <p><b>3 Tandoori chicken</b><br /><img src= "img/Buffet/3tandoorichicken.jpg"></p>
           <p><b>4 Crab legs</b><br /><img src= "img/Buffet/4crablegs.jpg"></p>
           <p><b>5 Mango lassi</b><br /><img src="img/Buffet/5mangolassi.jpg"></p>
           <p><b>6 Salad bar</b><br /><img src=  "img/Buffet/6saladbar.jpg"></p>
           <p><b>7 Palak paneer</b><br /><img src="img/Buffet/7palakpaneer.JPG"></p>
           <p><b>8 Dimsum</b><br /><img src="img/Buffet/8dimsum.JPG"></p>
           <p><b>9 Chicken curry</b><br /><img src="img/Buffet/9chickencurry.png"></p>
           <p><b>10 Lambvindaloo</b><br /><img src="img/Buffet/10lambvindaloo.jpg"></p>

           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    else if (ImageType() == "13"){
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b>1 Chicken Sandwich</b><br /><img src="img/FastFood/1ChickenSandwich.JPG"></p>
           <p><b>2 Aaffle fries</b><br /><img src="img/FastFood/2wafflefries.jpg"></p>
           <p><b>3 Carneasada</b><br /><img src= "img/FastFood/3carneasada.jpg"></p>
           <p><b>4 Burrito bowl</b><br /><img src= "img/FastFood/4burritobowl.jpg"></p>
           <p><b>5 Chicken bowl</b><br /><img src="img/FastFood/5chickenbowl.jpg"></p>
           <p><b>6 Fried chicken</b><br /><img src=  "img/FastFood/6friedchicken.jpg"></p>
           <p><b>7 Grilled chicken</b><br /><img src="img/FastFood/7grilledchicken.jpg"></p>
           <p><b>8 Teriyaki chicken</b><br /><img src="img/FastFood/8teriyakichicken.jpg"></p>
           <p><b>9 Hot dog</b><br /><img src="img/FastFood/9hotdog.jpg"></p>
           <p><b>10 Pizza</b><br /><img src="img/FastFood/10pizza.jpg"></p>

           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else{
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <p><b> 1 Thin crust pizza</b><br /><img src="img/pizza/1thincrustpizza.jpg" ></p>
           <p><b> 2 Deep dish pizza</b><br /><img src="img/pizza/2deepdishpizza.jpg" ></p>
           <p><b> 3 Pizza wings</b><br /><img src="img/pizza/3pizzawings.jpg" ></p>
           <p><b> 4 Garlic Knots</b><br /><img src="img/pizza/4garlicknots.jpg" ></p>
           <p><b> 5 White pizza</b><br /><img src="img/pizza/5whitepizza.jpg" ></p>
           <p><b> 6 Wood fired pizza</b><br /><img src="img/pizza/6woodfiredpizza.jpg"></p>
           <p><b> 7 Margherita pizza</b><br /><img src="img/pizza/7margheritapizza.jpg" ></p>
           <p><b> 8 Caesar salad</b><br /><img src="img/pizza/8caesarsalad.jpg" ></p>
           <p><b> 9 Craft beer</b><br /><img src="img/pizza/9craftbeer.jpg" ></p>
           <p><b> 10 Ice cream</b><br /><img src="img/pizza/10icecream.jpg"></p>
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    })
  
   
})