# Create a dataframe of USA monthly temperatures, plus population density
# for use with the app
# this will rely on some downloads from tyrell/clean-data copied into our /data/ folder:
# population-density-states.RDS
# worldclim-states.RDS
# gadm-states.RDS
library("zeallot")
library("tidyr")

setwd("~/Documents/cov-env-app/code/")

# will use this a couple of times later
month_list <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# read pop density data
pop_density <- as.data.frame(readRDS("../data/population-density-states.RDS"))
pop_density$state <- row.names(pop_density)

temperature <- as.data.frame(readRDS("../data/worldclim-states.RDS"))
temperature <- temperature[,c(1:12)]
names(temperature) <- month_list
temperature$state <- row.names(temperature)

# funtion to subset the data down to US only, and give them
# correct state names.
# need to load the states data for this
c(states, states_data) %<-% readRDS("../data/gadm-states.RDS")
US_data <- states_data[states_data$GID_0 == "USA",]

get_US_data <- function(state_data){
  # bring the states climate df to USA only
  USA_only <- state_data[with(state_data, grepl("USA", state)),]
  # check if the climate data and GADM data are in the same order of states
  if(identical(as.character(USA_only$state), US_data$GID_1)){
    # TRUE - so we can take the state codes in the GADM data ($HASC_1) and add them to our climate dataframe
    USA_only$state <- gsub("US.", "", US_data$HASC_1)
    return(USA_only)
  }
  else{ # if FALSE
    return("Error - state numbers don't match up")
  }
}

USA_temperature_df <- get_US_data(temperature)
USA_popdensity_df <- get_US_data(pop_density)

# add pop density to the temperature data
USA_temperature_df$Pop_density <- USA_popdensity_df$Pop_density

# make temperature df long and take only mean

climate_df <- pivot_longer(USA_temperature_df,
                           cols = 1:12,
                           names_to = "Month",
                           values_to = "Temperature")

# save the dataset as a csv
write.csv(climate_df, "../data/USA-env-data.csv", row.names = FALSE)
