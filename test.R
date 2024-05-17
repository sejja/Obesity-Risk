library(httr)
library(dplyr)
library(purrr)  
library(leaflet)  # Load the leaflet package

# Function to geocode place name and obtain latitude and longitude using OpenStreetMap Nominatim API
geocode_place <- function(place_name) {
  # Construct the geocoding request URL
  base_url <- "https://nominatim.openstreetmap.org/search"
  url <- modify_url(base_url, query = list(q = place_name, format = "json", limit = 1))
  
  # Make the request
  response <- GET(url)
  
  # Extract latitude and longitude from the response
  if (status_code(response) == 200) {
    geocode_data <- content(response, "parsed")
    if (length(geocode_data) > 0) {
      latitude <- geocode_data[[1]]$lat
      longitude <- geocode_data[[1]]$lon
      return(c(Latitude = latitude, Longitude = longitude))
    } else {
      return(c(Latitude = NA, Longitude = NA))
    }
  } else {
    return(c(Latitude = NA, Longitude = NA))
  }
}

# Read the CSV file containing the data into a data frame named 'bizkaia_data'
bizkaia_data <- read.csv("Foral_Bizkaia_General_2023.csv")

# Assuming you have a dataset named 'top_10_areas' with the top 10 areas in Bizkaia
top_10_areas <- bizkaia_data %>%
  filter(CONCEPT == "VOTES" & AREA != "BIZKAIA") %>% #Remove the PERCENTAGE Rows
  group_by(AREA) %>% #Group by the area
  summarise(Total_Votes = sum(EAJ.PNV, EH.BILDU, EB.AZ, ELKARREKIN.POD., ENK.EXI, PH, PP, PSE.EE, STOP, VOX)) %>% #Sum all the columns from the main parties, and add the votes altogether
  arrange(desc(Total_Votes)) %>% #Sort by the number of votes
  top_n(10) #And pick the top 10

# Apply the geocode_place function to each area name in the 'top_10_areas' dataset
top_10_areas <- top_10_areas %>%
  mutate(Geocode = map(.$AREA, ~geocode_place(.x))) %>%
  mutate(Latitude = sapply(Geocode, function(x) as.numeric(x[1])),
         Longitude = sapply(Geocode, function(x) as.numeric(x[2])))

# Create a map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -2.97, lat = 43.25, zoom = 10)  # Set initial view to Bizkaia

# Add markers for the top 10 areas
for (i in 1:nrow(top_10_areas)) {
  map <- addMarkers(map,
                    lng = top_10_areas$Longitude[i],
                    lat = top_10_areas$Latitude[i],
                    popup = paste("Area:", top_10_areas$AREA[i], "<br>",
                                  "Total Voters:", top_10_areas$Total_Votes[i]))
}

# Display the map
map
