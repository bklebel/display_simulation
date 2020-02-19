library(shiny)
library(tidyverse)
library(DBI)


con <- dbConnect(RSQLite::SQLite(), dbname = "data.db")
db_table <- tbl(con, "data")
on.exit(dbDisconnect(con))

# set up functions ------
filter_data <- function(df, gap1_usr = 0, gap2_usr = 0, stdpercent_usr = 0,
                        T_usr = 0) {
    df %>% 
        filter(gap1 == gap1_usr,
               gap2 == gap2_usr,
               stdpercent == stdpercent_usr,
               T == T_usr)
}


make_plot <- function(filtered_data, x_lim) {
    filtered_data %>%
        collect() %>% 
        select(contains("bias"), q, samegap1, samegap2) %>%
        pivot_longer(contains("bias"), names_to = "x_name", values_to = "x_val") %>% 
        pivot_longer(q:samegap2, names_to = "y_name", values_to = "y_val") %>% 
        distinct() %>% 
        ggplot(aes(x_val, y_val, colour = y_name)) +
        geom_point(alpha = .6) +
        labs(x = "bias") +
        coord_cartesian(xlim = c(x_lim * -1, x_lim))
}


# get input parameters
bounds <- db_table %>% 
    summarise(max_gap1 = max(gap1),
              min_gap1 = min(gap1),
              max_gap2 = max(gap2),
              min_gap2 = min(gap2),
              max_std = max(stdpercent),
              min_std = min(stdpercent),
              max_t = max(T),
              min_t = min(T)) %>% 
    collect()




# Define UI for application ------
ui <- fluidPage(
    
    # Application title
    titlePanel("Display Simulation results"),
    
    # Sidebar with input
    sidebarLayout(
        sidebarPanel(
            sliderInput("gap1_usr",
                        "gap1",
                        min = bounds$min_gap1,
                        max = bounds$max_gap1,
                        value = bounds$min_gap1),
            sliderInput("gap2_usr",
                        "gap2",
                        min = bounds$min_gap2,
                        max = bounds$max_gap2,
                        value = bounds$min_gap2),
            sliderInput("stdpercent_usr",
                        "stdpercent",
                        min = bounds$min_std,
                        max = bounds$max_std,
                        value = bounds$min_std),
            sliderInput("T_usr",
                        "T",
                        min = bounds$min_t,
                        max = bounds$max_t,
                        value = bounds$min_t)
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
        # set x_lim
        x_lim <- max(input$gap1_usr, input$gap2_usr) * 3
        
        db_table %>% 
            filter_data(gap1_usr = input$gap1_usr,
                        gap2_usr = input$gap2_usr,
                        stdpercent_usr = input$stdpercent_usr,
                        T = input$T_usr) %>% 
            make_plot(x_lim)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
