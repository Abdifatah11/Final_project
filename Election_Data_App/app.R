#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

all_election_data



library(shiny)

ui <- fluidPage(checkboxGroupInput("checkGroup", label = h3("Select States to compare"), 
                                   choices = list("Alabama"="Alabama", "Alaska" ="Alaska", 
                                                  "Arizona" ="Arizona","Arkansas"="Arkansas", 
                                                  "California" ="California", "Colorado"="Colorado", 
                                                  "Connecticut"="Connecticut","Delaware"="Delaware",
                                                  "Florida"="Florida", "Georgia"="Georgia", 
                                                  "Hawaii"="Hawaii","Idaho"="Idaho", "Illinois"="Illinois",
                                                  "Indiana"="Indiana", "Iowa"="Iowa",
                                                  "Kansas"="Kansas", "Kentucky"="Kentucky",
                                                  "Louisiana"="Louisiana", "Maine"="Maine",
                                                  "Maryland"="Maryland", "Massachusetts"="Massachusetts",
                                                  "Michigan"="Michigan", "Minnesota"="Minnesota",
                                                  "Mississippi"="Mississippi", "Missouri"="Missouri",
                                                  "Montana"="Montana", "Nebraska"="Nebraska",
                                                  "Nevada"="Nevada", "New Hampshire"="New Hampshire",
                                                  "New Mexico"="New Mexico","New York"="New York", 
                                                  "North Carolina"="North Carolina", 
                                                  "North Dakota"="North Dakota","Ohio"="Ohio",
                                                  "Oklahoma"="Oklahoma", "Oregon"="Oregon", "Pennsylvania"="Pennsylvania",
                                                  "Rhode Island"="Rhode Island","South Carolina"="South Carolina",
                                                  "South Dakota"="South Dakota", "Tennessee"="Tennessee",
                                                  "Texas"="Texas", "Utah"="Utah",
                                                  "Vermont"="Vermont", "Virginia"="Virginia",
                                                  "Washington"="Washington", "West Virginia"="West Virginia",
                                                  "Wisconsin"="Wisconsin", "Wyoming"="Wyoming"),
                                   selected = 1),
                submitButton(text = "Compare Election Results!"),
                plotOutput(outputId = "timeplot"),
                sliderInput(inputId = "years", 
                            label = "Year Range",
                            min = 1974, 
                            max = 2020, 
                            value = c(1974,2020))
                        

    
)
# Define server logic required to draw a histogram
server <-function(input, output) {
    output$timeplot <- renderPlot({
        all_election_data %>% 
            group_by(state_name, year, democrat, republican) %>%
            group_by(state) %>% 
            
            filter( state_name %in% c(input$checkGroup)) %>% 
            arrange(desc(democrat, republican))%>%
            ggplot() +
            geom_line(aes(x=year, y= percent_rank(democrat), color=state_name)) +
    
            scale_x_continuous(breaks = seq(1972,2016,4)) +
            theme(legend.position = "right")+
            labs(x = "",
                 y = "",
                 title= "percentage of democratic votes since 1972")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
