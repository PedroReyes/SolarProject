# library(plotly)
# library(SolarProject)
#
# # Colours that are used for the charts (try to set here as much as you can)
# chart_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#e377c2", "#9467bd","#8c564b", "#ffbb78",
#                   "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#aec7e8", "#f7b6d2", "#7f7f7f", "#c7c7c7",
#                   "#bcbd22", "#dbdb8d", "#17becf", "#9edae5")
#
# solar_data = get_solar_data(
#   type_data = data_types.FLR,
#   datapath = "/Users/pedro/Google Drive/Desarrollo/ResearchBook/Solar_Dashboard/SolarProject/Data/",
#   # datapath = paste(getwd(), "/Data/", sep = ""),
#   start_date = "2017-01-01",
#   end_date = "2018-01-01",
#   return_data_treated = TRUE,
#   get_data_from_local = TRUE
# )
#
# solar_data = solar_data[,c("classType", "lon", "lat")]
# solar_data[, "classType"] <- substr(solar_data[,"classType"], 1, 1)
#
# p <- plot_ly(solar_data, x = ~lon, y = ~classType, z = ~lat, color = ~classType, colors = chart_colors[1:5]) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'x'),
#                       yaxis = list(title = 'flare class'),
#                       zaxis = list(title = 'y'),
#                       camera = list(eye = list(x = 1.25, y = -1.0, z = 0.25))))
# p
#
