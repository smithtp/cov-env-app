library("shiny")
library("usmap")
library("ggplot2")

# remove this when deploying app
setwd("~/Documents/cov-env-app/")

# get environmenal dataset
env_data <- read.csv("data/USA-env-data.csv")

# list of months
month_list <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# now use our regression coefficients to calculate R0 for each temperature/pop_density combination
# Intercept = 1.553588
# Temperature = -0.041482
# log(Pop_density) = 0.276289
calc_R0 <- function(temperature, pop_density){
  return(1.553588 -0.041482*temperature + 0.276289*log(pop_density))
}

# ui
ui <- fluidPage(
  selectInput(inputId = "month", 
              label = "Select Base Monthly Temperature", 
              choices = month_list),
  plotOutput("usa_map")
)

# server
server <- function(input, output) {
  output$usa_map <- renderPlot({
    # subset data to selected month
    monthly_data <- env_data[env_data$Month == input$month,]
    # calculate R0 for temperature/pop density selection
    monthly_data$R0 <- calc_R0(temperature = monthly_data$Temperature, pop_density = monthly_data$Pop_density)
    # render the output map
    plot_usmap(data = monthly_data, values = "R0") + 
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 2.25, name = expression(R[0]),
                                  limits = c(0, 5),
                           guide = guide_colourbar(barwidth = 25, barheight = 1,
                                                   #put legend title on top of legend
                                                   title.position = "top", direction = "horizontal")) +
      theme(legend.position = "bottom",
            legend.title=element_text(size=12), 
            legend.text=element_text(size=10))
  })
}

# run app
shinyApp(ui = ui, server = server)