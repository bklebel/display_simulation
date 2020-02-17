library(shiny)
library(tidyverse)
library(MASS)


# set up functions ------
get_data <- function() {
    tibble(
        a = 1,
        b = 5, 
        c = 10
    ) %>% 
        pivot_longer(everything()) %>% 
        mutate(sim = map(value, ~rnorm(n = 100, mean = .))) %>% 
        unnest_longer(sim)
}


filter_data <- function(df, my_value) {
    df %>% 
        filter(value == my_value)
}


make_plot <- function(filtered_data) {
    ggplot(filtered_data, aes(value, sim)) +
        geom_jitter() +
        scale_x_continuous(limits = c(0, 12)) +
        scale_y_continuous(limits = c(-2, 13)) +
        theme_bw()
}


# get data ------
set.seed(298374)
the_data <- get_data()



# Define UI for application ------
ui <- fluidPage(

    # Application title
    titlePanel("Display Simulation results"),

    # Sidebar with input
    sidebarLayout(
        sidebarPanel(
            selectInput("user_value",
                        "Selected value",
                        choices = c(1, 5, 10),
                        selected = 1)
        ),

        # show plot
        mainPanel(
           plotOutput("plot")
        )
    )
)

# define server -----
server <- function(input, output) {
    output$plot <- renderPlot({
        data_filtered <- filter_data(the_data, input$user_value)
        
        make_plot(data_filtered)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
