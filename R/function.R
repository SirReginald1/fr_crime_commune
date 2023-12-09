
######### CUSTOM ERRORS ###############################
#' Creates a custom condition to be raised during errors.
#'
#' @author Hadley Wickham
#' @title condition
#' @param subclass Description The class of the new condition.
#' @param message The error message to be shown to the user.
#' @param call ???
#' @return A custom condition that can be used
#' @examples
#' condition("customErrorName", "Custom message")
#' @export
condition = function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}


error_failed_connection = condition(c("failed_connection", "error"), "Faild to connect to file location!")

invalid_input = condition(c("invalid_input", "error"), "The input of the function is invalid!")

######### FUNCTIONS ##########################################

#' Fetches the gz compressed file at specified url.
#'
#' @title get_url_file.gz
#' @param url Description The class of the new condition.
#' @param csv_type The csv function to be used to read the file. Defaults to "default".
#' @returns The file outputed by the read.csv functions.
#' @examples
#' get_url_file.gz("https::www.data_url.com", csv_url = "csv2")
#' get_url_file.gz("https::www.data_url.com", csv_url = "csv")
#' @export
get_url_file.gz = function(url,csv_type="default"){
  test = readLines(gzcon(url(url)))

  if(csv_type == "default"){
    # Test to see if there are more comas or semi colones
    semi_colon_count = lengths(regmatches(test[1], gregexpr(";", test[1])))
    coma_count = lengths(regmatches(test[1], gregexpr(",", test[1])))
    # choose which function based on coma and semi colon count
    if(semi_colon_count > coma_count){
      csv_type = "csv2"
    }
    else{
      csv_type = "cvs"
    }
  }

  out = textConnection(test)

  if(csv_type == "csv"){
    return(read.csv(out))
  }
  else if(csv_type == "csv2"){
    return(read.csv2(out))
  }
  stop("Accepted csv arguments are: \"csv\" ou \"csv2\"")
}



#' Returns true if the patern is in the string, else false.
#'
#' @title is_in_string
#' @param str The string in which to look for the patern
#' @param patern The patern to be found within the string
#' @param ignore.case Boolean value indicating if patern recognition should be case sensitive
#' @returns The boolean value indicating
#' @examples
#' if(is_in_string("Hello","ll",ignore.case = TRUE)){
#'   print("Yes it's in the string")
#' }
#' @export
is_in_string = function(str,patern,ignore.case = TRUE){
  if(ignore.case == TRUE){
    str = tolower(str)
    patern = tolower(patern)
  }

  for(i in 1:(nchar(str)-1)){
    if(substr(str,i,nchar(patern)+i-1) == patern){
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Trimes both sides of the given string of all characters that are not numbers or letters. Stops trimming once it reaches the first occurrence
#' of a number or letter.
#'
#' @title trim_string
#' @param text The text to be trimed
#' @returns The file outputed by the read.csv functions
#' @examples
#' out = trim_string("(  a (--_Hello\"(-(-'-( '-))   )))")
#' print(out)
#' # "a (--_Hello"
#' @export
trim_string = function(text){
  start = 0
  idx = 1
  remove_char = c("!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "-", "+", "=", "{", "}", "[", "]", "|", "\\", ":", ";", "'", "\"", "<", ">", ",", ".", "?", "/", "~", "`", " ", "\n")
  while(substr(text,idx,idx) %in% remove_char){
    idx = idx + 1
  }

  start = idx
  idx = nchar(text)
  while(substr(text,idx,idx) %in% remove_char){
    idx = idx - 1
  }

  return(substr(text, start, idx))
}

#' Function used to split single string search inputs into appropriate input for data selection function.
#'
#' @title trim_string
#' @param str The input string to be split into vector of substrings.
#' @returns A vector of strings resulting from the split of the input string.
#' @export
multi_string_input = function(str){
  if(str == ""){
    return(NULL)
  }

  if(unlist(gregexpr(', ', str)) < 0){
    return(str)
  }

  return(unlist(strsplit(str,", ")))
}

#' Formats dates from raw untidy string into usable date format.
#' Can format dates with months as text.
#' (Function designed to have more features)
#'
#' @title format_date_scrape
#' @param text The text to be formated as a date
#' @param sep The chacter used to seperate the different elements of the date. (Not yet implemented)
#' @returns A usable date.
#' @examples
#' out = format_date_scrape("#      07 juillet 2023    #")
#' print(out)
#' # 2023-07-07
#' class(out)
#' # "Date"
#' @export
format_date_scrape = function(text, sep = "default"){
  min_char_month =c("jan" = "01", "feb" = "02", "fév" = "02", "mar" = "03", "ap" = "04", "avr" = "04", "may" = "05", "mai" = "05", "jun" = "06", "juin" = "06", "jul" = "07", "juil" = "07", "aug" = "08", "août" = "08", "sep" = "09", "oct" = "10", "nov" = "11", "dec" = "12", "déc" = "12")
  out = trim_string(text)
  tab = "error"

  if(sep == "default"){
    tab = strsplit(out," ")
  }

  else{
    tab = strsplit(out,sep)
  }

  if(tab == "error"){
    stop("There was an error in spliting the text!")
  }

  # if month as text
  for(i in 1:length(min_char_month)){
    if(is_in_string(text,names(min_char_month[i]),ignore.case = TRUE)){
      tab[[1]][2] = min_char_month[i]
    }
  }
  date = gsub(" ","",paste(tab[[1]][1], "/", tab[[1]][2], "/", tab[[1]][3]))

  return(as.Date(date,format = "%d/%m/%y"))
}

#' Formats dates from raw untidy string into usable date format.
#' Can format dates with months as text.
#' (Not sure whet the difference is with the scrape varient)
#'
#' @title format_date
#' @param text The text to be formated as a date
#' @param sep The chacter used to seperate the different elements of the date. (Not yet implemented)
#' @returns A usable date.
#' @examples
#' out = format_date("#      07 juillet 2023    #")
#' print(out)
#' # 2023-07-07
#' class(out)
#' # "Date"
#' @export
format_date = function(text, sep = "default"){
  min_char_month =c("jan" = "01", "feb" = "02", "fév" = "02", "mar" = "03", "ap" = "04", "avr" = "04", "may" = "05", "mai" = "05", "jun" = "06", "juin" = "06", "jul" = "07", "juil" = "07", "aug" = "08", "août" = "08", "sep" = "09", "oct" = "10", "nov" = "11", "dec" = "12", "déc" = "12")
  out = trim_string(text)
  tab = "error"

  if(sep == "default"){
    tab = strsplit(out," ")
  }

  else{
    tab = strsplit(out,sep)
  }

  if(tab == "error"){
    stop("There was an error in spliting the text!")
  }

  # if month as text
  for(i in 1:length(min_char_month)){
    if(is_in_string(text,names(min_char_month[i]),ignore.case = TRUE)){
      tab[[1]][2] = min_char_month[i]
    }
  }
  return(as.Date(gsub(" ","",paste(tab[[1]][1], "/", tab[[1]][2], "/", tab[[1]][3])),format = "%d/%m/%y"))
}

#' Scrapes the given web page for the contense of the element identified by the CSS selector.
#' Formates the result as a date.
#' @title get_date_scrape
#' @param url The url of the site the date is to be retreaved from
#' @param selector The CSS selector used to identefy the date on the given page
#' @returns A usable date.
#' @example
#' out = get_date_scrape("url", "fr-text--sm fr-mt-0 fr-mb-3v")
#' class(out)
#' # "Date"
#' @export
get_date_scrape = function(url, selector){
  page = read_html(url)
  element = html_element(page,selector)
  return(format_date_scrape(trim_string(html_text(element))))
}


#' Retrieves the date from httr request header.
#' Header must have a `last-modified` tag.
#'
#' @title get_date_head
#' @param url The url of the site the date is to be retreaved from
#' @returns A usable date.
#' @example
#' out = get_date_head("url")
#' class(out)
#' # "Date"
#' @export
get_date_head = function(url){
  date = httr::HEAD(url)$headers$`last-modified`
  return(format_date(substr(date,6,16)))
}


#' Downloads the data at the given url and saves it as the given file name to the Data directory in the current working directory.
#' There must be a Data directory present in the current working directory for the file to be saved.
#' Saves the file in rds format.
#'
#' @title get_data
#' @param url The url from which the data is to be downloaded.
#' @param file_name The name for the file to be saved as. Will be saved as rds file. Will be placed in "Data" file in current directory.
#' File name must end in ".rds".
#' @param API_function The name of the function used to make the appropriate API request. Function name in string format without parentheses.
#' The function must exist and return a list of with user message in first place and the data as second place. Defaults to "no".
#' @param token The token used to access the API. !!! not implemented for any API other than INSEE API yet !!!
#' @returns List containing [[1]] user message indicating download status and [[2]] data.
#' @example
#' out = get_data("https:://www.data_url.com", "my_file.rds", "my_API_call_function", token = "75ab388c-e0cf-3a96-a00e-323d383f27b4")
#' out[[1]]
#' # [[1]] "Download successful"
#' class(out[[2]])
#' # "data.frame"
#' @export
get_data = function(url, file_name, API_function = "no", token = "no"){
  # If data dosn't have an API connection
  if(token == "no"){
    # Returns custom error if failed to connect
    tryCatch({lines = readLines(con = gzcon(url(url)), n=4)
    user_message = "Download successful"},
    error = function(e){
      stop(error_failed_connection)
    })

    lines = paste(lines,collapse = "")
    semi_colon_count = lengths(regmatches(lines, gregexpr(";", lines)))
    coma_count = lengths(regmatches(lines, gregexpr(",", lines)))


    if(coma_count>semi_colon_count){
      data = read.table(textConnection(readLines(gzcon(url(url)))), sep = ",", header = TRUE)
    }
    else{
      data = read.table(textConnection(readLines(gzcon(url(url)))), sep = ";", header = TRUE)
    }
  }



  # If the data has a API connection
  else{
    switch(API_function,
           "INSEE_Commune_Data" = {out = get_INSEE_Commune_Data_API(get_INSEE_token())})

    data = out[[2]]
    user_message = out[[1]]
  }

  saveRDS(data,file = paste("data/",file_name,sep = ""),compress = T)
  return(list(user_message, data))
}



#' Check for updates. Returns true if needs updating, can take dataset name as param.
#'
#' @title check_for_updates
#' @param line_nb The line number of the data to be checked for updates on the data_default reference table.
#' @param envi The environment in which to look for the reference table.
#' @returns The date the dataset was last updated in Date formate. (For APIs always returns the current date + 1 day)
#' @example
#' out = check_for_updates(1)
#' class(out)
#' # "Date"
#' @export
check_for_updates = function(line_nb, envi = .GlobalEnv){
  # If API not used
  if(get("data_default", envir = envi)[line_nb,8] == "no"){
    # Try to get date from HTTP header
    out = get_date_head(get("data_default", envir = envi)[line_nb,4])>get("data_default", envir = envi)[line_nb,7]
    # Try to get date from web scraping
    if(is.null(out)){
      out = get_date_scrape(get("data_default", envir = envi)[line_nb,4], get("data_default", envir = envi)[line_nb,5])
    }
    # If it works
    if(!is.null(out)){
      return(out)
    }
    # Otherwise return a date later than today to force update
    else{
      return(Sys.Date()+1)
    }
  }
  # If API used
  else{
    return(Sys.Date()+1)
  }

}


#' Check default_data reference table for updates, can take dataset name as parameter.
#' If data already up to date on server will load server data to user session.
#' Returns the message to inform user of download status.
#' Throws an error if error different than not able to connect to website.
#'
#' @title update_data
#' @param line_nb The line number of the data to be checked for updates on the data_default reference table.
#' @param session_env The user session environment.
#' @param server_env The server environment.
#' @returns The user message informing the user on the status of the data update in character format.
#' @example
#' out = update_data(1)
#' print(out)
#' # "Download successful"
#' @export
update_data = function(line_nb, session_env = .GlobalEnv, server_env = .GlobalEnv){

  # If if there is a more recent version
  if(check_for_updates(line_nb)){
    tryCatch({data = get_data(get("data_default", envir = server_env)["data_url", line_nb],
                              get("data_default", envir = server_env)["file_name", line_nb],
                              get("data_default", envir = server_env)["API_function", line_nb],
                              get("data_default", envir = server_env)["API_token", line_nb])
    print(paste("Got data:",data[[1]]))},
    error = function(e){
      if("failed_connection" %in% class(e)){
        return(message(e))
      }
      else {
        stop("Unknown error has ocured!")
      }
    })
  }

  else{
    # Replace the data stored in the user session with the data from the server
    assign(get("data_default", envir = server_env)["var_names", line_nb], get(data_default$var_names[line_nb], envir = serverEnv), envir = session_env)
    return("Session data updated")
  }
  assign(get("data_default", envir = server_env)[line_nb,2], data[[2]], envir = session_env) # Assigns variable to user session environment
  assign(get("data_default", envir = server_env)["date_last_update", line_nb], Sys.Date(), envir =  session_env)# Update the date in server table
  return(data[[1]])
}


#' Updates all data in the data_default table using the \code{update_data} function.
#'
#' @title update_all_data
#' @param session_env The user session environment.
#' @param server_env The server environment.
#' @returns Vector of user messages informing the user on the status of the data update in character format.
#' @example
#' out = update_all_data(session_env, server_env)
#' print(out)
#' # "Update successful"
#' @export
update_all_data = function(session_env = .GlobalEnv, server_env = .GlobalEnv){
  out = c()
  # check last update date
  for(i in 1:nrow(get("data_default", envir = server_env))){
    out[length(out)+1] = update_data(i, session_env, server_env )
  }
  return(out)
}

update_dataset = function(envir = .GlobalEnv){
  res = update_all_data(envir, serverEnv)
  print(paste("update all data res:", res))#####################################################################################
  for(i in 1:length(res)){
    if(res[i] != "Session data updated" & res[i] != "Download successful"){
      return("Update failed!")
    }
  }
  for(i in 1:nrow(get("data_default", envir = envir))){
    format_col(i,envir)
  }
  for(i in 1:nrow(get("data_sets", envir = envir))){
    merge_data_set(i, envir)
  }
  print("update_dataset sucsses")#############################################################################################
  return("Update successful")
}


#' Tests to see if the given file type is a gzfile.
#'
#' @title is.gzfile
#' @param file An object of file class.
#' @returns Boolean indicating if file is a gzfile.
#' @example
#' is.gzfile(file)
#' # TRUE
#' @export
is.gzfile = function(file){
  return(summary(file)$class == "gzfile")
}



#' Gets a new accsses token for the INSEE API.
#'
#' @title get_INSEE_token
#' @returns The API token in character format.
#' @example
#' get_INSEE_token()
#' # "75ab388c-e0cf-3a96-a10e-323d383f27b4"
#' @export
get_INSEE_token = function(){

  headers = c(
    `Authorization` = 'Basic TTdiTm1vUU1hNE92cVJMS2VBelNpSk5tS1pZYTp0Y2V2YWtKRVdvVGZhdGZCaERHalFSbVRSYUVh',
    `Content-Type` = 'application/x-www-form-urlencoded'
  )

  data = list(
    `grant_type` = 'client_credentials'
  )

  res = httr::POST(url = 'https://api.insee.fr/token', httr::add_headers(.headers=headers), body = data, encode = 'form', config = httr::config(ssl_verifypeer = FALSE))


  return(substr(rawToChar(res$content), 18,53))
}



#' Downloads the commune data from INSEE using there API.
#' Returns the message to be shown to the user at [[1]] and the dataset at [[2]]
#' Throws error_failed_connection error if can't connect to website
#'
#' @title get_INSEE_Commune_Data_API
#' @param token Token used to access the INSEE API. !!! Not yet implemented function will automatically call the \code{get_INSEE_token}
#' function. Will be used if future versions with stored token to reduce internet trafic. !!!
#' @returns List containing in [[1]] the user message returned by the API and in [[2]] the data.
#' @example
#' out = get_INSEE_Commune_Data_API("75ab388c-e0cf-3a96-a10e-323d383f27b4")
#' out
#' #[[1]]
#' # "Authentication requiered!"
#' #[[2]]
#' # NULL
#' @export
get_INSEE_Commune_Data_API = function(token = ""){

  headers = c(
    `Accept` = 'application/json',
    `Authorization` = paste('Bearer', token,sep = " "))

  params = list(
    `com` = 'true')
  tryCatch({temp =  httr::GET(url = 'https://api.insee.fr/metadonnees/V1/geo/communes', httr::add_headers(.headers=headers), query = params)},
           error = function(e){
             stop(error_failed_connection)
           })


  switch(as.character(temp$status_code),
         "200" = {user_message = "Download successful"},
         "401" = {user_message = "Authentication requiered!"},
         "404" = {user_message = "Resource not found! Resource may no longer exist!"},
         "500" = {user_message = "The server experienced an error!"})

  return(list(user_message,fromJSON(rawToChar(temp$content))))
}


#' Given the line number of the corresponding data in data_default and the environment in which the data must be changed
#' will set the data types to data types found data_default$var_type. (!!! The data frame to be changed must already be present in the given environment !!!)
#' Meant to be used for automatic formatting when data is downloaded.
#'
#' @title format_col
#' @param line_nb The line number of the data to be checked for updates on the data_default reference table.
#' @returns Does not return anything transform s the data inplace.
#' @export
format_col = function(line_nb, envir = .GlobalEnv){
  data = get(data_default$var_names[line_nb],envir = envir)
  for(i in 1:ncol(data)){
    switch(substr(data_default$var_type[line_nb],i,i),
           "1" = {data[,i] = as.numeric(data[,i])},
           "2" = {data[,i] = as.character(data[,i])},
           "3" = {data[,i] = as.Date(data[,i])})
  }
  assign(data_default$var_names[line_nb], data, envir = envir)
}

#' Will set the data types to data types found in given vector. (!!! The data frame to be changed must already be present in the given environment !!!)
#'
#' @title format_col_vect
#' @param data The data to be formatted.
#' @param var_name The name of the variable to be assigned to the environment. Default is set to the name of the inputed data.
#' @param vector The vector indicating the type the columns will be formatted to. 1 = numeric, 2 = character, 3 = Date, 4 = factor.
#' @param envir The environment in which the outputed data is to be placed. Default is set to \code{.GlobalEnv}.
#' @param ... The parameters to ba passed to the factor function.
#' @returns Nothing. Assigns the data directly to the specified environment.
#' @export
format_col_vect = function(data, var_name = deparse(substitute(data)), vector, envir=.GlobalEnv, ...){
  for(i in 1:ncol(data)){
    switch(as.character(vector[i]),
           "1" = {data[,i] = as.numeric(data[,i])},
           "2" = {data[,i] = as.character(data[,i])},
           "3" = {data[,i] = as.Date(data[,i])},
           "4" = {data[,i] = factor(data[,i])}, ...)
  }
  assign(var_name, data, envir = envir)
}


#' Given the line number in the data_sets table will reconstruct the data set in question from it's constituent data sets and assign it to it's variable name
#' all within
#'
#' @title merge_data_set
#' @param line_nb The line number of the data to be checked for updates on the data_default reference table.
#' @param envi The environment in which the data_default reference table is present and in which the outputed data will be place. (Yes I know. And no I don't know what I was thinking when I wrote this).
#' @returns Nothing. Assigns the data directly to the specified environment.
#' @export
merge_data_set = function(line_nb, envi = .GlobalEnv){
  ord = c() # First run through table an find all data subsets
  for(i in 1:nrow(data_default)){
    if(data_sets$data_set[line_nb] == data_default$data_set[i]){
      ord[as.character(data_default$merge_order[i])] = i
    }
  }
  ord = ord[as.character(order(as.numeric(names(ord))))]
  m.col = data_default$merg_col[ord[1]]
  out = merge(get(data_default$var_names[ord[1]], envir = envi),
              get(data_default$var_names[ord[2]],envir = envi),
              by.x = data_default$merg_col[ord[1]],
              by.y = data_default$merg_col[ord[2]],
              all = TRUE,
              suffixes = c(data_default$merg_sufix[ord[1]], data_default$merg_sufix[ord[2]]))
  mer_sufix = data_default[ord[2]]
  for(i in ord[c(-1,-2)]){
    out = merge(out,
                get(data_default$var_names[i],envir = envi),
                by.x = m.col,
                by.y = data_default$merg_col[i],
                all = TRUE,
                suffixes = c(mer_sufix, data_default$merg_sufix[i]))
    mer_sufix = data_default$merg_sufix[i]
  }
  assign(data_sets$var_names[line_nb], out, envir = envi)
}


#' Selects the data.frame to be used for ploting selects data.
#' Takes vector of selected modalities as parameters.
#' Use NULL as the default select all as it is the value returned by selecting widget when nothing is selected.
#' This function was designed to be able to be used for any data representation of the set_fr_crime_commune dataset, not just the leflet map.
#' This function was designed to be used with a shiny interface.
#'
#' @title select_set_fr_crime_commune
#' @param data The data on which the ploting will be based. !!! Will only take the formatted data that comes with the package. !!!
#' @param metric By what variable the number of counts is to be calculated: vector of column names in char format.
#' @param ndiff Boolean indicates if the communes with non disclosed figures should be present in the outputted data. !!! Should generally be set to FALSE. !!
#' @param annee Vector of the last 2 digits of the year in character format.
#' @param classe Vector containing the type of infraction in character format.
#' @param faits Vector of length 2 indicating the \code{c(minimum, maximum)} number of infraction counts to be included in the outputted data.
#' @param tauxpourmille Not prperly implemented yet. Leave as NULL.
#' @param pop Vector of length 2 indicating the \code{c(minimum, maximum)} population counts of communes to be included in the outputted data.
#' @param code_postal Vector of postal codes that is to be included in the outputted data.
#' @param nom_commune Vector of commune names that is to be included in the outputted data.
#' @param code_departement Vector of department codes that is to be included in the outputted data.
#' @param nom_departement Vector of department names that is to be included in the outputted data.
#' @param code_region Vector of region codes that is to be included in the outputted data.
#' @param nom_region Vector of region names that is to be included in the outputted data.
#' @param use_dom_tom Boolean value indicating if the "département et territoire d'outre mer" are to be included in the outputted data.
#' @returns Nothing. Assigns the data directly to the specified environment.
#' @export
select_set_fr_crime_commune = function(data,
                                       metric = NULL,
                                       ndiff = FALSE, # Takes the output from the checkbox
                                       annee = NULL,
                                       classe = NULL,
                                       faits = NULL,
                                       tauxpourmille = NULL,
                                       pop = NULL,
                                       code_postal = NULL,
                                       nom_commune = NULL,
                                       code_departement = NULL,
                                       nom_departement = NULL,
                                       code_region = NULL,
                                       nom_region = NULL,
                                       use_dom_tom = FALSE){ # Filters out dom tom's on FALSE
  #data = get("set_fr_crime_commune", envir = envir)

  # assign("data_test",data,envir = .GlobalEnv) # For testing ###############################################################

  # Never want to include NA latitude or Longitude
  cond_func = (!is.na(data$latitude) & !is.na(data$longitude) & !is.na(data$faits))

  if(!use_dom_tom){
    cond_func = (cond_func & !(data$code_departement %in% c(971,972,973,974,976)))
  }

  if(!ndiff){
    cond_func = (cond_func & data$valeur.publiée == "diff")
  }
  if(!is.null(annee)){
    cond_func = (cond_func & data$annee %in% annee & !is.na(data$annee))
  }
  if(!is.null(classe)){
    cond_func = (cond_func & data$classe %in% classe & !is.na(data$classe))
  }

  if(!is.null(tauxpourmille)){
    cond_func = (cond_func & data$tauxpourmille %in% tauxpourmille & !is.na(data$tauxpourmille))
  }
  if(!is.null(pop)){
    cond_func = (cond_func & !is.na(data$POP) & data$POP >= pop[1] & data$POP <= pop[2])
  }
  if(!is.null(code_postal)){
    cond_func = (cond_func & data$code_postal %in% code_postal & !is.na(data$code_postal))
  }
  if(!is.null(nom_commune)){
    cond_func = (cond_func & data$nom_commune %in% nom_commune & !is.na(data$nom_commune))
  }
  if(!is.null(code_departement)){
    cond_func = (cond_func & data$code_departement %in% code_departement & !is.na(data$code_departement))
  }
  if(!is.null(nom_departement)){
    cond_func = (cond_func & data$nom_departement %in% nom_departement & !is.na(data$nom_departement))
  }
  if(!is.null(code_region)){
    cond_func = (cond_func & data$code_region %in% code_region & !is.na(data$code_region))
  }
  if(!is.null(nom_region)){
    cond_func = (cond_func & data$nom_region %in% nom_region & !is.na(data$nom_region))
  }
  # Makes sure it can always render somting
  if(!exists("cond_func")){
    cond_func = TRUE
  }

  data = data[cond_func & data$faits > 0,]


  # Used for other sumary statistics if used for map must be set to: c("CODGEO_2023")
  if(!is.null(metric) & nrow(data) != 0){
    # Converting the char input vector to list of vectors
    metric_list = list()
    for(i in metric){
      metric_list[[length(metric_list)+1]] = data[,i]
    }

    agreg_data = aggregate(data$faits,by = metric_list, FUN = function(x){sum(x,na.rm = TRUE)})



    # Call the results column counts
    names(agreg_data) = c(metric,"counts")



    data = merge(
      data,
      agreg_data,
      all.x = F,
      all.y = T,
      by.x = metric,
      by.y = metric
    )
    data[,"faits"] = data[,"counts"]
  }




  # For testing
  #assign("test_data1", data, envir = .GlobalEnv)

  # Moved here so that selection is made on aggregated data
  if(!is.null(faits)){
    cond_func = (data$faits >= faits[1] & data$faits <= faits[2] & !is.na(data$faits))
  }
  data = data[cond_func & data$faits > 0,]


  rm(cond_func) # just to make sure
  return(data)
}


#' Render leaflet map enable the selection of which elements (colour, size, etc) governs what graphical element.
#' Remember to select only those that have infraction counts over 0 and not NULL.
#'
#' @title render_leafmap
#' @param data The data to be ploted on the leaflet map.
#' @param color The vector that is to be used to choose colour gradient.
#' @param popup The vector of characters that will be used in the popups. Can put raw html in the text such as "<br>".
#' @param labels The vector of characters that will be used in the labels that appear during hover.
#' @param radius The numerical vector to be used to determine the radius of the circles on the map.
#' @param opacity The numerical vector to be used to determine the opacity of the circles on the map.
#' @param map Can take boolean value such as outputted by a checkbox to indicate if the light theme map should be used.
#' Can also take a character string indicating the map distripution to be used for ploting.
#' @returns A leaflet map.
#' @export
render_leafmap = function(data, color, popup, labels, radius, opacity, map = "CartoDB.DarkMatter"){

  # Change between light an dark theme map
  if(map == TRUE){
    map = "CartoDB.Positron"
  }
  else if(map == FALSE){
    map = "CartoDB.DarkMatter"
  }

  # Outputs an empty map if there is provided data has no observations
  if(nrow(data) == 0){
    return(addProviderTiles(setView(leaflet(),lng = 2, lat = 46.7, zoom = 6), map))
  }

  color = color[!is.na(color)]

  # Produces hex colour vector if colour vector is a factor
  if(is.factor(color)){
    pal = colorFactor(heat.colors(max(log(color),na.rm = TRUE)),# Log used to reduce effect of extream values on data
                      domain = c(min(log(color),na.rm = TRUE),max(log(color),na.rm = TRUE)))
  }

  # Produces hex colour vector if colour vector is numeric
  if(is.numeric(color)){
    assign("color",color, envir = .GlobalEnv)
    assign("max",max(color,na.rm = TRUE), envir = .GlobalEnv)
    pal = colorNumeric(heat.colors(max(color,na.rm = TRUE)),domain = c(min(color), max(color)))
  }

  # Creates the leaflet map to be outputted
  out = addLegend(addCircleMarkers(addProviderTiles(setView(leaflet(data = data),
                                                            lng = 2,
                                                            lat = 46.7,
                                                            zoom = 6),
                                                    map),# The type of map: CartoDB.Positron
                                    # map graphic params
                                    lng = ~longitude,
                                    lat = ~latitude,
                                    popup = popup,
                                    label = labels,
                                    radius = radius,
                                    color = pal(color),
                                    stroke = FALSE,
                                    fillOpacity = log(sapply((opacity/max(opacity)),function(x){ifelse(x>0.1,x,0.1)})*11)
                                  ),
                  # Legend params
                  "bottomright", pal = pal, values = color,
                  title = "Number of<br>instances",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.8
                )

  return(out)
}

if(!require(shiny)){install.packages("shiny")} # Needed to run the run_fr_crime_app function

#' Runs the French crime by commune app
#'
#' @title run_fr_crime_app
#' @export
run_fr_crime_app = function(){
  library(shiny)

  assign("set_fr_crime_commune",readRDS("./app/Data/set_fr_crime_commune.rds"),envir = .GlobalEnv)

  runApp("./app")
}


#' The reference data.frame used in some of the functions. Contains relevant information to automate data prepossessing in order to
#' simplify data management in a shiny application.
#'
#' @title data_default
#' @export
data_default = data.frame("file_name" = c("french_crime_commune.rds",
                                                  "liste_commune_fr.rds",
                                                  "commune_GPS_fr.rds"),
                                  "var_names" = c("fr_crime_commune",
                                                  "INSEE_commune_list",
                                                  "commune_GPS_fr"),
                                  "user_data_name" = c("French crime by commune base",
                                                       "INSEE commune list",
                                                       "List des communes avec coordonée GPS"),
                                  "data_url" = c("https://www.data.gouv.fr/fr/datasets/r/3f51212c-f7d2-4aec-b899-06be6cdd1030",
                                                 "https://www.insee.fr/fr/statistiques/fichier/6800675/v_commune_2023.csv",
                                                 "https://www.data.gouv.fr/fr/datasets/r/dbe8a621-a9c4-4bc3-9cae-be1699c5ff25"),
                                  "web_page" = c("https://www.data.gouv.fr/fr/datasets/bases-statistiques-communale-et-departementale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/#/resources",
                                                 "https://www.insee.fr/fr/information/2560452",
                                                 "https://www.data.gouv.fr/fr/datasets/communes-de-france-base-des-codes-postaux/"),
                                  "date_selector" = c("p.fr-text--sm.fr-mt-0.fr-mb-3v", # CSS selector for element containing date
                                                      "no",
                                                      "fr-text--sm fr-mt-0 fr-mb-3v"),
                                  "date_last_update" = as.Date(c("2023-10-12",
                                                                 "2023-10-12",
                                                                 "2023-10-26")),
                                  "API_token" = c("no",
                                                  "75ab388c-e0cf-3a96-a00e-323d383f27b4",
                                                  "no"),
                                  "API_function" = c("no",
                                                     "INSEE_Commune_Data",
                                                     "no"),
                                  "token_time_stamp" = c("no",
                                                         "2023-10-17 08:26:03 CEST",
                                                         "no"),
                                  "data_set" = c("set_fr_crime_commune", # The data set that is
                                                 "set_fr_crime_commune",
                                                 "set_fr_crime_commune"),
                                  "merge_order" = c(3,
                                                    2,
                                                    1),
                                  "merg_sufix" = c(".fr_crime",
                                                   ".INSEE",
                                                   ".GPS"),
                                  "merg_col" = c("CODGEO_2023",           # The name of the columns used to merge them into a dataset
                                                 "code",
                                                 "code_commune_INSEE"),
                                  "var_type" = c("11122211111111", # The type of each variable 1 = numeric, 2 = char, 3 = date
                                                 "11223212",
                                                 "121221112221212")
                                  )

data(data_default)

#' The table containing the list of all data sets compiled from the servers base datasets
#'
#' @title data_sets
#' @export
data_sets = data.frame("file_name" = c("set_fr_crime_commune.csv"),
                              "var_names" = c("set_fr_crime_commune"),
                              "user_data_name" = c("French crime by commune"),
                              "date_last_update" = c("1999-10-12"),
                              "data_set" = c("set_fr_crime_commune")
                      )

data(data_sets)

