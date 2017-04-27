# ============================================
# ====================TESTING=================
# ============================================
if(FALSE){
  library(SolarProject)
  print(getwd())
  print(paste(getwd(), "/Data/", sep = ""))
  solar_data = get_solar_data(
    type_data = data_types.IPS,
    # datapath = "/Users/pedro/Google Drive/Desarrollo/ResearchBook/Solar_Dashboard/SolarProject/Data/",
    datapath = paste(getwd(), "/Data/", sep = ""),
    start_date = "2017-01-01",
    end_date = "2017-06-01",
    return_data_treated = TRUE,
    get_data_from_local = FALSE
  )
  print(solar_data)
}

# print(length(solar_data$classType[stri_detect_fixed(solar_data$classType,c("M"))]))

# ============================================
# ====================TESTING=================
# ============================================
# d_format <- "%Y-%m-%dT%H:%MZ"
# start_date = c("2010-11-21T00:00Z", "2010-11-21T08:00Z", "2010-11-21T06:00Z")
# end_date = c("2010-11-21T02:00Z", "2010-11-21T06:00Z", "2010-11-21T06:00Z")
# start_date = "2010-11-21T06:00Z"
# end_date = "2010-11-21T08:00Z"
# print(compare_dates(start_date, start_date_format = d_format,
#                     end_date, end_date_format = d_format))

# ============================================
# ====================TESTING=================
# ============================================
# print(time_difference(start_date = start_date, start_date_format = d_format,
#                       end_date = end_date, end_date_format = d_format,
#                       units = "hours"))
