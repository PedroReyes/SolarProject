# =======================
# Data types
# =======================
data_types.IPS = "IPS"
data_types.CME = "CME"
data_types.FLR = "FLR"

# =======================
# Getting data from DONKI
# =======================
#' @title
#' Return the raw data downloaded from DONKI as treated data
#'
#' @description
#' Given the data downloaded directly from DONKI, this method will
#' treat such data and return it
treating_ips_raw_data <- function(data) {
  # Removing columns
  data = data[, !(names(data) %in% c("catalog", "activityID", "instruments"))]

  # Reordering columns
  data = data[, c("eventTime", "location")]

  # Treating data (if necessary)
  # ---------------------------

  return (data)
}

#' @title
#' Return the raw data downloaded from DONKI as treated data
#'
#' @description
#' Given the data downloaded directly from DONKI, this method will
#' treat such data and return it
treating_cme_raw_data <- function(data) {
  # Removing columns
  # data = data[, !(names(data) %in% c("catalog", "activityID", "instruments"))]

  # Reordering columns
  # data = data[, c("eventTime", "location")]

  # 1- Treating data (columns lat and lon will be converted to sourceLocation)
  remove_rows <- c()
  counter <- 1

  for(row_index in 1:nrow(data)){
    # 2- Treating data (when lat or lon are null, the entire row will be removed for the sake of data)
    if(stri_detect_fixed(data[row_index, "lat"],c("null"))
       || stri_detect_fixed(data[row_index, "lon"],c("null"))){
      remove_rows[counter] <- -row_index
      counter <- counter + 1
      data[row_index,"sourceLocaiton"] = ""
      next
    }

    # 1- Getting latitude
    lat = as.double(data[row_index, "lat"])
    lat = ifelse(lat>0, paste("N", lat, sep = ""), paste("S", abs(lat), sep = ""))

    # 1- Getting longitude
    lon = as.double(data[row_index, "lon"])
    lon = ifelse(lon>0, paste("E", lon, sep = ""), paste("W", abs(lon), sep = ""))

    # 1- Saving helio-location
    data[row_index,"sourceLocaiton"] = paste(lat, lon, sep = "")
  }

  # Removing rows that didn't pass the screen
  data <- data[remove_rows,]


  return (data)
}

#' @title
#' Return the raw data downloaded from DONKI as treated data
#'
#' @description
#' Given the data downloaded directly from DONKI, this method will
#' treat such data and return it
treating_flr_raw_data <- function(data) {
  # Removing columns
  data = data[, !(names(data) %in% c("flrID", "instruments", "linkedEvents", "activeRegionNum"))]

  # 1 - Treating data
  # (source_location converted to two additional columns: lat and lon)
  # (solar flare energetic classification converted to doubles [A=1, B=2, C=3, M=4, X=5])
  source_location = as.vector(data["sourceLocation"])

  # New column names
  lat_column_name = "lat"
  lon_column_name = "lon"
  class_type_conversion_column_name = "classTypeNumber"

  for(row_index in 1:nrow(data)){
    # Location
    location = as.character(source_location[row_index,1])

    # Getting latitude
    lat = unlist(strsplit(location, split="S|N|W|E"))[2]
    lat = ifelse(stri_detect_fixed(location,c("N")), paste("+", lat, sep = ""), paste("-", lat, sep = ""))

    # Getting longitude
    lon = unlist(strsplit(location, split="S|N|W|E"))[3]
    lon = ifelse(stri_detect_fixed(location,c("E")), paste("+", lon, sep = ""), paste("-", lon, sep = ""))

    # Getting conversion of class type
    class_type = as.character(data[row_index,"classType"])
    if(stri_detect_fixed(class_type,c("A"))){
      class_type =  as.double(unlist(strsplit(class_type, split="A|B|C|M|X"))[2])+0
    } else if(stri_detect_fixed(class_type,c("B"))){
      class_type =  as.double(unlist(strsplit(class_type, split="A|B|C|M|X"))[2])+10
    } else if(stri_detect_fixed(class_type,c("C"))){
      class_type =  as.double(unlist(strsplit(class_type, split="A|B|C|M|X"))[2])+20
    } else if(stri_detect_fixed(class_type,c("M"))){
      class_type =  as.double(unlist(strsplit(class_type, split="A|B|C|M|X"))[2])+30
    } else if(stri_detect_fixed(class_type,c("X"))){
      class_type =  as.double(unlist(strsplit(class_type, split="A|B|C|M|X"))[2])+40
    }else {
      class_type = 0.0
    }

    # The most powerful solar flare up to now measured has been X45. Nevetheless this is an exception.
    # With the purpose of normalizing as much as possible most of the data we divide the number by 60
    # class_type = class_type/50.0

    # Saving helio-location
    data[row_index,"lat"] = lat
    data[row_index,"lon"] = lon
    data[row_index,"classTypeNumber"] = class_type
  }

  # 2 - Treating data
  # There is an outlier with a longitude equals to -634, which flares is dated in 2013-10-28T02:12Z.
  # This flare is clearly an outlier that cannot be useful for the development of a model and that is why
  # it will be set a constraints which says that flares which latitude or longitude greater than 230 will be removed.
  # We establish this threshold since we initially think that is more than enough for a flare which producs a CME
  # to reach the Earth in form of an IPS.
  data = data[(abs(as.double(data$lon))<230),]


  # Reordering columns
  data = data[, c("beginTime", "peakTime", "endTime",
                  "classType", class_type_conversion_column_name,
                  "sourceLocation", lat_column_name, lon_column_name)]

  return (data)
}

#' @title
#' Return the joining of two dataframes without repeating any row. Taking into account
#' that the joining will depend on the array by.
#'
#' @examples
#' data_backup <- data.frame(a = 1:3, b=letters[1:3])
#' data_new <- data.frame(a = 3:6, b=letters[3:6])
#' join_datasets_by(data_backup, data_new, by = c("a"))
join_datasets_by <- function(df1, df2, by){
  # Auxiliar variable to store the joining
  df1_joined_to_df2 = NULL

  # Nothing to join if df2 is null
  if (!is.null(df1) && !is.null(df2)) {
    # Package needed
    require(dplyr)

    # Getting difference
    diff_between_dataframes = anti_join(x = df1, y = df2, by = by)

    # Joining difference rows to dataframes if there is any difference
    if (nrow(diff_between_dataframes) > 0) {
      df1_joined_to_df2 = rbind(df2, diff_between_dataframes)
    } else{
      df1_joined_to_df2 = df2
    }
  } else if(is.null(df1)){
    # Returning the dataframe that is not null
    return (df2)
  } else if(is.null(df2)){
    # Returning the dataframe that is not null
    return (df1)
  }

  return (df1_joined_to_df2)
}

#' @title
#' Download IPS/CME/SolarFlare data coming from DONKI
#'
#' @description
#' It will download the IPS/CME/SolarFlare data from DONKI in a single file
#' in the path indicated and in the range of dates indicated.
#' The data stored and returned is treated previously.
#'
#' @details
#' \itemize{
#'   \item The type_data could be "IPS", "CME", "SolarFlare"
#'   \item The path could be something like this "C://data/ipsData.csv"
#'   \item The start date could be someting like this "2010-12-31"
#'   \item The end date could be someting like this "2011-12-31"
#'   \item If return_data_treated = TRUE,
#'   then the data returned is treated
#'   else it is the raw data coming from the server. (get_data_from_local must be TRUE)
#'   \item If get_data_from_local = TRUE,
#'   then the data returned will [have to be/be searched] locally
#'   else the data will be searched in the server and append to the local file.
#' }
#'
#' The data saved locally is always treated
#'
#' @examples
#' download_IPS_data(type_data = "IPS",
#' datapath = "/Users/pedro/Google Drive/Desarrollo/ResearchBook/Solar_Dashboard/Data/",
#' start_date = "2008-01-01",
#' end_date = "2010-05-01",
#' return_data_treated = TRUE,
#' get_data_from_local = FALSE)
get_solar_data <- function(type_data,
                           datapath,
                           start_date,
                           end_date,
                           return_data_treated = TRUE,
                           get_data_from_local = TRUE) {
  # Library for detecting chars in strings
  if(!require(stringi)){
    install.packages("stringi")
    library(stringi)
  }

  # Library for parsing json data as dataframe
  if(!require(stringi)){
    install.packages("jsonlite")
    library(jsonlite)
  }

  # Name of the files that will be saved by this method
  ips_filename = "IPS_data_treated.csv"
  cme_filename = "CME_data_treated.csv"
  solar_flare_filename = "SolarFlare_data_treated.csv"

  # Create the directory where the data
  # will be saved if it does not exist
  if (!file.exists(datapath)){
    dir.create(datapath)
  }

  # Composing filepath & checking the type_data is right & Getting the URL to the data
  if (type_data == data_types.IPS) {
    filepath = paste(datapath, ips_filename, sep = "")
    URL <- paste("https://kauai.ccmc.gsfc.nasa.gov/DONKI/WS/get/IPS?startDate=", start_date, "&endDate=", end_date, sep = "")
  } else if (type_data == data_types.CME) {
    filepath = paste(datapath, cme_filename, sep = "")
    URL <- paste("https://kauai.ccmc.gsfc.nasa.gov/DONKI/WS/get/CMEAnalysis.txt?startDate=", start_date, "&endDate=", end_date, "&mostAccurateOnly=true", sep = "")
  } else if (type_data == data_types.FLR) {
    filepath = paste(datapath, solar_flare_filename, sep = "")
    URL <- paste("https://kauai.ccmc.gsfc.nasa.gov/DONKI/WS/get/FLR?startDate=", start_date, "&endDate=", end_date, sep = "")
  } else{
    return (NULL)
  }
  print(URL)

  # Checking data issues
  if (!get_data_from_local) { # Checking if there is some data in URL specified
    data_server = readLines(URL, warn = FALSE)
    dataSize = length(data_server)

    # Getting backup
    if(file.exists(filepath)){
      data_backup = read.csv2(filepath)
    }else{
      data_backup = NULL
    }

    # If we have data, we start treating and saving data
    if (dataSize == 0) {
      return (NULL)
    }
  } else if (get_data_from_local && !file.exists(filepath)) { # no file exist
    return (NULL)
  } else if (get_data_from_local && file.exists(filepath)) { # everything right
    data_backup = read.csv2(filepath)
  } else{
    stop("Something went wrong in DataManager.R. It is related to data backups.")
    return (NULL)
  }

  # Getting data either from local or from the server, depending on what you specified
  if (get_data_from_local) {
    # Getting the specified data from babakcup
    d_format <- "%Y-%m-%dT%H:%MZ"

    # Formatting start and end date that does not indicate time, just date
    start_date_format_conversion <- paste(start_date, "T00:00Z",sep = "")
    end_date_format_conversion <- paste(end_date, "T00:00Z",sep = "")

    # Selecting the rows that are in range
    if (type_data == data_types.IPS) {
      rows_in_range = (compare_dates(as.character(data_backup$eventTime), d_format, start_date_format_conversion, d_format)>=0 &
                         compare_dates(as.character(data_backup$eventTime), d_format, end_date_format_conversion, d_format)<=0)
    } else if (type_data == data_types.CME) {
      rows_in_range = (compare_dates(as.character(data_backup$C2), d_format, start_date_format_conversion, d_format)>=0 &
                         compare_dates(as.character(data_backup$C2), d_format, end_date_format_conversion, d_format)<=0)
    } else if (type_data == data_types.FLR) {
      rows_in_range = (compare_dates(as.character(data_backup$beginTime), d_format, start_date_format_conversion, d_format)>=0 &
                         compare_dates(as.character(data_backup$beginTime), d_format, end_date_format_conversion, d_format)<=0)
    }

    # Selecting rows specified from backup
    data_backup_in_range = data_backup[rows_in_range, ]

    # Treating the data (this initially wouldn't be necessary but since we could introduce
    # more data treatment in the future we would have to download the all dataset for treating it again, in
    # this way we do not have to.)
    data_backup_treated = NULL
    if (type_data == data_types.IPS){
      data_backup_treated = treating_ips_raw_data(data = data_backup_in_range)
    } else if (type_data == data_types.FLR){
      data_backup_treated = treating_flr_raw_data(data = data_backup_in_range)
    } else if (type_data == data_types.CME){
      data_backup_treated = treating_cme_raw_data(data = data_backup_in_range)
    }

    # Returning the data in the period specified
    return (data_backup_treated)
  } else {
    # Treating the data
    if(!is.null(data_server)){
      if (type_data == data_types.IPS || type_data == data_types.FLR) {
        # Parsing data as dataframe
        data_server = jsonlite::fromJSON(data_server) #(URL)

        # Data treated for IPS data
        if (type_data == data_types.IPS){
          data_server_treated = treating_ips_raw_data(data = data_server)
        } else if (type_data == data_types.FLR){
          data_server_treated = treating_flr_raw_data(data = data_server)
        }

        # Data to be used for joingin the backup and the new data
        column_name_for_joining_datasets = ifelse(type_data == data_types.IPS, "eventTime", "beginTime")

        # The data finally saved in the file will be a mix
        # of the backup and the new data
        data_server_treated_joined_to_data_backup = join_datasets_by(df1 = data_server_treated,
                                                                     df2 = data_backup,
                                                                     by = c(column_name_for_joining_datasets))
      } else if (type_data == data_types.CME) {
        # Parsing data as dataframe
        df <- data.frame(C21.5=character(),
                         lat=character(),
                         lon=character(),
                         rad=character(),
                         vel=character(),
                         C2=character(),
                         stringsAsFactors=FALSE)

        for(i in 1:length(data_server)){
          row_data <- unlist(strsplit(data_server[i],
                                      split="lat=|lon=|rad=|vel=|#|-CME-01|-CME-001|-CME-002|-CME-003"))
          row_data[6] <- substr(row_data[6], start = 1, stop = nchar(row_data[6])-3)
          row_data[6] <- paste(row_data[6], "Z", sep = "")
          df[i,] <- row_data
        }

        data_server <- df

        # Data to be used for joingin the backup and the new data
        column_name_for_joining_datasets = "C21.5"

        # Data treated for CME data
        data_server_treated = treating_cme_raw_data(data = data_server)

        # The data finally saved in the file will be a mix
        # of the backup and the new data
        data_server_treated_joined_to_data_backup = join_datasets_by(df1 = data_server_treated,
                                                                     df2 = data_backup,
                                                                     by = c(column_name_for_joining_datasets))
      }
    }

    # Saving the data
    if (!is.null(data_server_treated_joined_to_data_backup)) {
      write.csv2(x = data_server_treated_joined_to_data_backup,
                 file = filepath,
                 row.names = FALSE)
    }else{
      stop("Both dataframes treated (new and backup data) were NULL and this caused and error.")
    }

    # Returning the data in the period specified
    if (return_data_treated) {
      return (data_server_treated)
    } else{
      return (data_server)
    }
  }
}

#' @title
#' Return time difference between two dates
#'
#' @description
#' Given two dates, the method will return in the unit specified
#' the difference in time between that two datse
#'
#' @details
#' The units could be "secs", "mins", "hours"
#' The start date could be someting like this "2010-12-31" so the format will be "Y-%m-%d"
#' The end date could be someting like this "2011-12-31T23:45Z" so the format will be "Y-%m-%dT%RZ" or "%Y-%m-%dT%H:%MZ"
#'
#' @examples
#' d_format <- "%Y-%m-%dT%H:%MZ"
#' start_date = "2010-11-21T00:00Z"
#' end_date = "2010-11-21T02:00Z"
#' print(time_difference(start_date = start_date, start_date_format = d_format,
#'                           end_date = end_date, end_date_format = d_format,
#'                           units = "mins"))
time_difference <-
  function(start_date, start_date_format, end_date, end_date_format, units) {
    td <-
      as.numeric(strptime(start_date, start_date_format) - strptime(end_date, end_date_format), units = units)
    return (td)
  }

#' @title
#' Return (1,0,-1) if the date 1 (start_date) is (greater, equals, less) than date 2 (end_date)
#'
#' @examples
#' d_format <- "%Y-%m-%dT%H:%MZ"
#' start_date = "2010-11-21T00:00Z"
#' end_date = "2010-11-21T02:00Z"
#' print(compare_dates(start_date = start_date, start_date_format = d_format, end_date = end_date, end_date_format = d_format))
compare_dates <- function(dates1, start_date_format, dates2, end_date_format) {
  # Units to measure time
  units = "secs"

  # Checking if both arrays
  if(length(dates1)!=length(dates2)){
    if(length(dates1)==1){
      dates1 = rep(dates1, length(dates2))
    }else if(length(dates2)==1){
      dates2 = rep(dates2, length(dates1))
    }else{
      return (NULL)
    }
  }

  # Comparison between dates
  compares = c()

  # Comparing dates
  for(i in 1:length(dates1)){
    start_date = dates1[i]
    end_date = dates2[i]

    if(time_difference(start_date, start_date_format, end_date, end_date_format, units)<0){
      # return (-1)
      compares[i] = -1
    } else if(time_difference(start_date, start_date_format, end_date, end_date_format, units)>0){
      # return (1)
      compares[i] = 1
    } else if(time_difference(start_date, start_date_format, end_date, end_date_format, units)==0){
      # return (0)
      compares[i] = 0
    }
  }

  return (compares)
}
