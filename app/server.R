if(!require(shiny)){install.packages("shiny")}
if(!require(shinyWidgets)){install.packages("shinyWidgets")}
if(!require(rvest)){install.packages("rvest")}
if(!require(bslib)){install.packages("bslib")}
if(!require(jsonlite)){install.packages("jsonlite")}
if(!require(httr)){install.packages("httr")}
if(!require(kableExtra)){install.packages("kableExtra")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggmap)){install.packages("ggmap")}
if(!require(sf)){install.packages("sf")}
if(!require(rnaturalearth)){install.packages("rnaturalearth")}
if(!require(rnaturalearthdata)){install.packages("rnaturalearthdata")}
if(!require(devtools)){install.packages("devtools")}
if(!require(rnaturalearthhires)){devtools::install_github("ropensci/rnaturalearthhires")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(tmap)){install.packages("tmap")}

serverEnv = new.env()

assign("list_of_files", list.files("./Data"), envir = serverEnv)

server = function(input, output, session) {
  # create users enviroment
  session$userData = new.env() # This is executed every time a user connects to it

  # Only used for optimising testing
  #assign("set_fr_crime_commune",read.table("Data/set_fr_crime_commune.csv", sep = ";", header = TRUE),envir = session$userData)
  #session$userData$set_fr_crime_commune = readRDS("Data/set_fr_crime_commune.rds")
  # session$userData = read.table("Data/test.csv", sep = ",", header = TRUE)

  #assign("set_fr_crime_commune",readRDS("Data/set_fr_crime_commune.rds"),envir = .GlobalEnv)

  # Importing the list of files from the server
  assign("list_of_files", get("list_of_files", envir = serverEnv), envir = session$userData)

  # Importing the default data table
  assign("data_default", get("data_default", envir = serverEnv), envir = session$userData)

  # Load all base datasets into users enviroment from server enviroment
  # {
  #   data = get("data_default", envir = serverEnv)
  #   for(i in 1:nrow(data)){
  #     assign(data[i,2], get(data[i,2], envir = serverEnv), envir = session$userData) # Reactive enables updating of all elements that rely on that varaible
  #   }
  #   rm(data) # just making sure
  # }


  #assign("set_fr_crime_commune",get("set_fr_crime_commune",envir = serverEnv),envir = session$userData)

  for(i in 1:nrow(data_default)){
    assign(data_default[i,2], read.csv(paste("Data/",data_default[i,1],sep = "")), envir = serverEnv) # Reactive enables updating of all elements that rely on that varaible
  }

  # Static map rendering
  output$fr_map = renderPlot(
    ggplot(data = ne_countries(scale = 10, returnclass = "sf")) +
      geom_sf() +
      #geom_point(data = test[!is.na(test$latitude),], aes(x = longitude, y = latitude), size = 0.5, shape = 23, fill = "darkred") +
      coord_sf(xlim = c(-5.142,10), ylim = c(40,51.5), expand = FALSE)
  )

  # The reactive data selector function
  # assign("leaf_fr_data",
  #        reactive({
  #          select_set_fr_crime_commune(data = get("set_fr_crime_commune", envir = session$userData),
  #                                      metric = c("CODGEO_2023"), # By what variable the number fo counts is to be calculated: vector of cols in char format
  #                                      ndiff = FALSE, # Takes the output from the checkbox
  #                                      annee = input$leafmap_fr_annee,
  #                                      classe = input$leafmap_fr_classe,
  #                                      faits = input$leafmap_fr_faits,
  #                                      tauxpourmille = NULL,
  #                                      pop = input$leafmap_fr_POP, # No idea the data table returned in the data selection function hase 0 rows
  #                                      code_postal = multi_string_input(input$leafmap_fr_code_postal),
  #                                      nom_commune = multi_string_input(input$leafmap_fr_nom_commune),
  #                                      code_departement = input$leafmap_fr_code_departement,
  #                                      nom_departement = input$leafmap_fr_nom_departement,
  #                                      code_region = input$leafmap_fr_code_region,
  #                                      nom_region = input$leafmap_fr_nom_region,
  #                                      use_dom_tom = input$leafmap_fr_use_dom_tom)
  #          }),
  #        envir = session$userData)

  session$userData$leaf_fr_data <-reactive({
    select_set_fr_crime_commune(data = get("set_fr_crime_commune", envir = .GlobalEnv),
                                metric = c("CODGEO_2023"), # By what variable the number fo counts is to be calculated: vector of cols in char format
                                ndiff = FALSE, # Takes the output from the checkbox
                                annee = input$leafmap_fr_annee,
                                classe = input$leafmap_fr_classe,
                                faits = input$leafmap_fr_faits,
                                tauxpourmille = NULL,
                                pop = input$leafmap_fr_POP, # No idea the data table returned in the data selection function hase 0 rows
                                code_postal = multi_string_input(input$leafmap_fr_code_postal),
                                nom_commune = multi_string_input(input$leafmap_fr_nom_commune),
                                code_departement = input$leafmap_fr_code_departement,
                                nom_departement = input$leafmap_fr_nom_departement,
                                code_region = input$leafmap_fr_code_region,
                                nom_region = input$leafmap_fr_nom_region,
                                use_dom_tom = input$leafmap_fr_use_dom_tom)
  })



  # Leaflet map rendering output
  output$leaf_fr_map = renderLeaflet(
    render_leafmap(data = get("leaf_fr_data", envir = session$userData)(),       # Data (!!! the reactive values must have parenthisees behind them !!!)
                   color = get("leaf_fr_data",envir = session$userData)()[,"faits"], # Colour
                   popup = paste("Nom: ", get("leaf_fr_data",envir = session$userData)()[,"nom_commune"],"<br>",
                                 "Nb:"  , get("leaf_fr_data",envir = session$userData)()[,"faits"],"<br>",
                                 "Pupulation: ", get("leaf_fr_data",envir = session$userData)()[,"POP"],"<br>"
                   ),
                   labels = paste(get("leaf_fr_data",envir = session$userData)()[,"nom_commune"], get("leaf_fr_data", envir = session$userData)()[,"faits"], sep = " | "),
                   radius = log(get("leaf_fr_data", envir = session$userData)()[,"faits"]),
                   opacity = get("leaf_fr_data", envir = session$userData)()[,"faits"],
                   map = input$leafmap_fr_light_theme
    )
  )

  # Render the fait slider
  output$faits_input_slider = renderUI({
    sliderInput(inputId = "leafmap_fr_faits",
                label = "Number of counts:",
                min = 1,
                max = max(get("set_fr_crime_commune", envir = session$userData)[,"faits"], na.rm = TRUE),
                value = c(1, max(get("set_fr_crime_commune", envir = session$userData)[,"faits"], na.rm = TRUE))
    )
  })

  # Render the POP slider
  output$POP_input_slider = renderUI({
    sliderInput(
      inputId = "leafmap_fr_POP",
      label = "Population:",
      min = min(get("set_fr_crime_commune", envir = session$userData)[,"POP"],na.rm = TRUE),
      max = max(get("set_fr_crime_commune", envir = session$userData)[,"POP"],na.rm = TRUE),
      value = c(min(get("set_fr_crime_commune", envir = session$userData)[,"POP"],na.rm = TRUE),max(get("set_fr_crime_commune", envir = session$userData)[,"POP"],na.rm = TRUE))
    )
  })


  # Output tne number of comunes rendered on the map
  output$output_leaf_fr_nb_commune = renderText(paste("Number of comunes:",length(unique(get("leaf_fr_data",envir = session$userData)()[,"CODGEO_2023"])), sep = " "))

  # Prints list of files in data folder
  output$server_file_list = renderText(paste(get("list_of_files", envir = session$userData),
                                             collapse = " | "))

  # Output used for testing
  output$test = renderText(input$leafmap_fr_faits)

  # Render reference table
  output$data_default = renderTable(sapply(get("data_default", envir = session$userData)[,c(1,3,5,7,11)], as.character))

  # Checks for updates in the default data
  observeEvent(input$update_fr_crime_comune_data,{
    update_dataset(session$userData)
  })

  # Error message when failed to update data
  observeEvent(input$theInputThatSetsOffTheWindow, {
    shinyalert("Failed to update data", "What whent wrong", type = "error")
  })

}
