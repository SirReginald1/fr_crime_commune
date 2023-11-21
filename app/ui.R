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

ui = navbarPage(header = includeCSS("Styles/main.css"),
                #collapsible = TRUE,
                #useShinyalert(),
                windowTitle = "My shiny app",
                title = "Navbar title",
                #theme = bs_theme(bg="black",
                #         fg="white",
                #         #primary = "green",
                #         secondary = "blue",
                #         danger = "red"),
                # Tab1
                tabPanel(title = "Interactive map",
                         # Sidebar layouot and panel
                         sidebarLayout(sidebarPanel = sidebarPanel(width = 3,
                                                                   ################# Leafmap var selectors ###########################
                                                                   # Metric input
                                                                   # pickerInput(
                                                                   #   inputId = "leafmap_fr_metric",
                                                                   #   label = "What to measur by",
                                                                   #   choices = c("CODGEO_2023","annee","classe"), # The output value
                                                                   #   multiple = TRUE,
                                                                   #   selected = NULL,
                                                                   #   options = list(`actions-box` = FALSE, # Adds the select all
                                                                   #                  `selected-text-format` = "count > 3", # Above 1 element only  shows the number of elements selected
                                                                   #                  `hideDisabled` = "false",
                                                                   #                  `title` = "All"), # If nothing selected
                                                                   #   choicesOpt = list(
                                                                   #     content = sprintf("<div class='label label-%s'>%s</div>", # What is actualy shown in the HTML element
                                                                   #                       as.character(seq(1,length(c("Commune","Year","Offence")))), # Individual label class for css styling
                                                                   #                       c("Commune","Year","Offence")), # The shown value
                                                                   #     disabled = rep(F,length(unique(set_fr_crime_commune$annee[!is.na(set_fr_crime_commune$annee)])))) # vector indicating if option is to be disabled
                                                                   # ),
                                                                   # Annee input
                                                                   pickerInput(
                                                                     inputId = "leafmap_fr_annee",
                                                                     label = "Year:",
                                                                     choices = sort(unique(set_fr_crime_commune$annee[!is.na(set_fr_crime_commune$annee)])), # The output value
                                                                     multiple = TRUE,
                                                                     selected = NULL,
                                                                     options = list(`actions-box` = TRUE, # Adds the select all
                                                                                    `selected-text-format` = "count > 1", # Above 1 element only  shows the number of elements selected
                                                                                    `hideDisabled` = "false",
                                                                                    `title` = "All"), # If nothing selected
                                                                     choicesOpt = list(
                                                                       content = sprintf("<div class='label label-%s'>%s</div>", # What is actualy shown in the HTML element
                                                                                         as.character(seq(1,length(unique(set_fr_crime_commune$annee[!is.na(set_fr_crime_commune$annee)])))), # Individual label class for css styling
                                                                                         sort(unique(set_fr_crime_commune$annee[!is.na(set_fr_crime_commune$annee)]))), # The shown value
                                                                       disabled = rep(F,length(unique(set_fr_crime_commune$annee[!is.na(set_fr_crime_commune$annee)])))) # vector indicating if option is to be disabled
                                                                   ),
                                                                   # Classe input
                                                                   pickerInput(
                                                                     inputId = "leafmap_fr_classe",
                                                                     label = "Offence:",
                                                                     choices = unique(set_fr_crime_commune$classe[!is.na(set_fr_crime_commune$classe)]), # The output value
                                                                     multiple = TRUE,
                                                                     selected = NULL,
                                                                     options = list(`actions-box` = TRUE,
                                                                                    `selected-text-format` = "count > 1",
                                                                                    `hideDisabled` = "false",
                                                                                    `title` = "All"),
                                                                     choicesOpt = list(
                                                                       content = sprintf("<div class='label label-%s'>%s</div>",
                                                                                         as.character(seq(1,length(unique(set_fr_crime_commune$classe[!is.na(set_fr_crime_commune$classe)])))), # Individual class names for css styling
                                                                                         unique(set_fr_crime_commune$classe[!is.na(set_fr_crime_commune$classe)])), # The shown value
                                                                       disabled = rep(F,length(unique(set_fr_crime_commune$classe[!is.na(set_fr_crime_commune$classe)]))))
                                                                   ),
                                                                   # pop input
                                                                   uiOutput("POP_input_slider"),
                                                                   # faits input
                                                                   uiOutput("faits_input_slider"),
                                                                   # nom_commune input
                                                                   searchInput(
                                                                     inputId = "leafmap_fr_nom_commune",
                                                                     label = "Nom de la comune:",
                                                                     placeholder = "Use \",\" and space to seperate several towns",
                                                                     btnSearch = icon("search"),
                                                                     btnReset = icon("remove"),
                                                                     width = "100%"
                                                                   ),
                                                                   # code_postal input
                                                                   searchInput(
                                                                     inputId = "leafmap_fr_code_postal",
                                                                     label = "Postal code:",
                                                                     placeholder = "Use \",\" and space to seperate several postal codes",
                                                                     btnSearch = icon("search"),
                                                                     btnReset = icon("remove"),
                                                                     width = "100%"
                                                                   ),

                                                                   # code_region input
                                                                   pickerInput(
                                                                     inputId = "leafmap_fr_code_region",
                                                                     label = "Code région",
                                                                     choices = sort(unique(set_fr_crime_commune$code_region[!is.na(set_fr_crime_commune$code_region)])), # The output value
                                                                     multiple = TRUE,
                                                                     selected = NULL,
                                                                     options = list(`actions-box` = TRUE,
                                                                                    `selected-text-format` = "count > 1",
                                                                                    `hideDisabled` = "false",
                                                                                    `title` = "All"),
                                                                     choicesOpt = list(
                                                                       content = sprintf("<div class='label label-%s'>%s</div>",
                                                                                         as.character(seq(1,length(unique(set_fr_crime_commune$code_region[!is.na(set_fr_crime_commune$code_region)])))), # Individual class names for css styling
                                                                                         sort(unique(set_fr_crime_commune$code_region[!is.na(set_fr_crime_commune$code_region)]))), # The shown value
                                                                       disabled = rep(F,length(unique(set_fr_crime_commune$code_region[!is.na(set_fr_crime_commune$code_region)]))))
                                                                   ),
                                                                   # nom_region input
                                                                   pickerInput(
                                                                     inputId = "leafmap_fr_nom_region",
                                                                     label = "Nom région",
                                                                     choices = sort(unique(set_fr_crime_commune$nom_region[!is.na(set_fr_crime_commune$nom_region)])), # The output value
                                                                     multiple = TRUE,
                                                                     selected = NULL,
                                                                     options = list(`actions-box` = TRUE,
                                                                                    `selected-text-format` = "count > 1",
                                                                                    `hideDisabled` = "false",
                                                                                    `title` = "All"),
                                                                     choicesOpt = list(
                                                                       content = sprintf("<div class='label label-%s'>%s</div>",
                                                                                         as.character(seq(1,length(unique(set_fr_crime_commune$nom_region[!is.na(set_fr_crime_commune$nom_region)])))), # Individual class names for css styling
                                                                                         sort(unique(set_fr_crime_commune$nom_region[!is.na(set_fr_crime_commune$nom_region)]))), # The shown value
                                                                       disabled = rep(F,length(unique(set_fr_crime_commune$nom_region[!is.na(set_fr_crime_commune$nom_region)]))))
                                                                   ),
                                                                   # nom_departement input
                                                                   pickerInput(
                                                                     inputId = "leafmap_fr_nom_departement",
                                                                     label = "Nom département",
                                                                     choices = sort(unique(set_fr_crime_commune$nom_departement[!is.na(set_fr_crime_commune$nom_departement)])), # The output value
                                                                     multiple = TRUE,
                                                                     selected = NULL,
                                                                     options = list(`actions-box` = TRUE,
                                                                                    `selected-text-format` = "count > 1",
                                                                                    `hideDisabled` = "false",
                                                                                    `title` = "All"),
                                                                     choicesOpt = list(
                                                                       content = sprintf("<div class='label label-%s'>%s</div>",
                                                                                         as.character(seq(1,length(unique(set_fr_crime_commune$nom_departement[!is.na(set_fr_crime_commune$nom_departement)])))), # Individual class names for css styling
                                                                                         sort(unique(set_fr_crime_commune$nom_departement[!is.na(set_fr_crime_commune$nom_departement)]))), # The shown value
                                                                       disabled = rep(F,length(unique(set_fr_crime_commune$nom_departement[!is.na(set_fr_crime_commune$nom_departement)]))))
                                                                   ),
                                                                   # code_departement input
                                                                   pickerInput(
                                                                     inputId = "leafmap_fr_code_departement",
                                                                     label = "Code département",
                                                                     choices = sort(unique(set_fr_crime_commune$code_departement[!is.na(set_fr_crime_commune$code_departement)])), # The output value
                                                                     multiple = TRUE,
                                                                     selected = NULL,
                                                                     options = list(`actions-box` = TRUE,
                                                                                    `selected-text-format` = "count > 1",
                                                                                    `hideDisabled` = "false",
                                                                                    `title` = "All"),
                                                                     choicesOpt = list(
                                                                       content = sprintf("<div class='label label-%s'>%s</div>",
                                                                                         as.character(seq(1,length(unique(set_fr_crime_commune$code_departement[!is.na(set_fr_crime_commune$code_departement)])))), # Individual class names for css styling
                                                                                         sort(unique(set_fr_crime_commune$code_departement[!is.na(set_fr_crime_commune$code_departement)]))), # The shown value
                                                                       disabled = rep(F,length(unique(set_fr_crime_commune$code_departement[!is.na(set_fr_crime_commune$code_departement)]))))
                                                                   ),
                                                                   # Output value test
                                                                   textOutput("test"), # If the input is null it will collapse. Use it as download feedback
                                                                   # Include values without
                                                                   # prettyCheckbox(inputId = "include_ndiff",
                                                                   #                label = "Include non",
                                                                   #                value = FALSE,
                                                                   #                icon = icon("check"),
                                                                   #                status = "success",
                                                                   #                animation = "rotate"),
                                                                   # Include dom tom
                                                                   prettyCheckbox(
                                                                     inputId = "leafmap_fr_use_dom_tom",
                                                                     label = "Use Dom Tom:",
                                                                     value = FALSE,
                                                                     icon = icon("check"),
                                                                     status = "success",
                                                                     animation = "rotate"
                                                                   ),
                                                                   # Output total number of comunes redered
                                                                   textOutput("output_leaf_fr_nb_commune"),
                                                                   # Change map to light theme
                                                                   prettyCheckbox(
                                                                     inputId = "leafmap_fr_light_theme",
                                                                     label = "Ligth theme",
                                                                     value = FALSE,
                                                                     icon = icon("check"),
                                                                     status = "success",
                                                                     animation = "rotate")
                         ),

                         ############# Main Panel ########################### input$leafmap_fr_POP
                         mainPanel = mainPanel(width = 9,
                                               h1("French crime",align="center"),
                                               leafletOutput("leaf_fr_map",height = 800)
                         )
                         )
                ),
                ####################################### Tab2 ###################################################"
                tabPanel(title = "The data",
                         # Sidebar layouot and panel
                         sidebarLayout(sidebarPanel = sidebarPanel(width = 2,# Button to render map with current selection
                                                                   actionButton("update_fr_crime_comune_data","Update french crime data"),
                         ),
                         mainPanel = mainPanel(align="center",
                                               h3("Composit data info"),
                                               tableOutput("data_default"),
                                               h3("List of files on the server"),
                                               textOutput("server_file_list")
                         )
                         )
                ),
                # Tab test
                tabPanel(title = "Test tab",
                         # Sidebar layouot and panel
                         #leafletOutput("leaf_fr_map",height = 900)
                )

)
