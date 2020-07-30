library("shiny")
library("shinyWidgets")
library("usmap")
library("ggplot2")

# remove this when deploying app
setwd("~/Documents/cov-env-app/")

# get environmenal dataset
env_data <- read.csv("data/USA-env-data.csv")

# list of months
month_list <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# use our regression coefficients to calculate R0 for each temperature/pop_density combination
# Intercept = 1.553588
# Temperature = -0.041482
# log(Pop_density) = 0.276289
calc_R0 <- function(temperature, pop_density){
  return(1.553588 -0.041482*temperature + 0.276289*log(pop_density))
}

# from the Rt vs lockdown regression
calc_R0_lockdown <- function(temperature, pop_density){
  return(0.973268 + 0.006269*temperature - 0.023906*log(pop_density))
}

# ui
ui <- fluidPage(
  # App title ----
  titlePanel("Simple COVID-19 Forecasting"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # month selection boxes
      selectInput(inputId = "month", 
                  label = "Select Base Monthly Temperature", 
                  choices = month_list),
      # lockdown yes/no selection
      selectInput(inputId = "lockdown", 
                  label = "Are lockdown measures in place?", 
                  choices = c("Yes", "No"),
                  selected = "No"),
      # temperature slider
      sliderInput(inputId = "temperature", 
                  label = "Choose temperature increase/decrease", 
                  value = 0, min = -10, max = 10),
      # pop density multiplier, a bit more complicated
      # as we want it in a weird scale
      shinyWidgets::sliderTextInput(inputId = "popdensity",
                                    label = "Choose a population density multiplier:",
                                    choices=c(0.01, 0.1, 1, 10, 100),
                                    selected=1, grid = T)
      ),
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("usa_map", height = 800, width = 800)
    )
  ) 
)

# server
server <- function(input, output) {
  # first get the R0 values created by the inputs
  reactive_data <- reactive({
    # subset data to selected month
    monthly_data <- env_data[env_data$Month == input$month,]
    # check if lockdown is in place first
    if(input$lockdown == "No"){
      # calculate R0 for temperature/pop density selection
      monthly_data$R0 <- calc_R0(temperature = monthly_data$Temperature+input$temperature, 
                                 pop_density = monthly_data$Pop_density*input$popdensity)
    }
    else if(input$lockdown == "Yes"){
      # calculate R0 using lockdown version of calculation
      monthly_data$R0 <- calc_R0_lockdown(temperature = monthly_data$Temperature+input$temperature, 
                                          pop_density = monthly_data$Pop_density*input$popdensity)
      
    }
    # hacky way of keeping values inside our scale limits
    if(nrow(monthly_data[monthly_data$R0 < 0,]) > 0){
      monthly_data[monthly_data$R0 < 0,]$R0 <- 0 
    }
    return(monthly_data)
  })
  # then make the plot using that reactive data
  output$usa_map <- renderPlot({
    # render the output map
    plot_usmap(data = reactive_data(), values = "R0") + 
      scale_fill_gradientn(colours = c("green", "orange", "purple"),
                           values = c(0, 0.3, 1),
                           limits = c(0, 5),
                           name = expression(R[0]),
                           guide = guide_colourbar(barwidth = 25, barheight = 1,
                                                   title.position = "top", direction = "horizontal")) + 
      theme(legend.position = "bottom",
            legend.title=element_text(size=12), 
            legend.text=element_text(size=10))
  })
}

# run app
shinyApp(ui = ui, server = server)