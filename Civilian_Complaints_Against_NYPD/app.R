library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

#** LOAD DATA **#

complaints_data <- read.csv("civilian complaints against NYPD.csv")
nyc_pop <- read.csv("NYC Pop by Age and Sex.csv")

#** Define UI **#

ui <- fluidPage(
  titlePanel("Complaints Against NYPD Analysis"),
  tabsetPanel(
    
    # FIRST PAGE
    tabPanel("Complaints Over Time",
             titlePanel("How have complaints against NYPD change over time?"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("fado_types", "Select FADO Types:",
                                    choices = c("Force", "Abuse of Authority", "Discourtesy", "Offensive Language"),
                                    selected = c("Force", "Abuse of Authority", "Discourtesy", "Offensive Language")),
                 selectInput("selected_year", "Select Year:", 
                             choices = c("All Time Data", as.character(2020:1985)),
                             selected = "All Time Data"),
                 radioButtons("top_n", "Select Top N Allegations:",
                              choices = c(5, 10, 15),
                              selected = 10)
               ),
               mainPanel(
                 plotlyOutput("bar_chart"),
                 dataTableOutput("raw_data"),
                 tableOutput("top_allegations_table")
               )
             )
    ),
    
    # SECOND PAGE
    tabPanel("Complainant Demographics",
             titlePanel("Are certain demographics more likely to file a complaint?"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("chartType", "Select Complainant Demographic:",
                              choices = c("Age", "Gender", "Ethnicity"),
                              selected = "Age"),
                 radioButtons("NYC_Pop", "Select NYC Population Demographic:",
                              choices = c("Age", "Gender", "Ethnicity"),
                              selected = "Age")
               ),
               mainPanel(
                 plotlyOutput("pieChart"),
                 plotlyOutput("NYC_pieChart")
               )
             )
    ),
    
    # THIRD PAGE
    tabPanel("Blank Page 2", 
             fluidPage(
               h1("This is another blank page.")
             )
    )
  )
)

#** DEFINE SERVER **#

server <- function(input, output, session) {
  
#** DATA CLEANING **#
  
  #* removed rows with "" values in complainant_ethnicity column
  #* removed rows with -4301, -1, 0 or NA values in complainant_age_incident column
  #* removed rows with "" values in complainant_gender column
  cleaned_data <- subset(complaints_data, 
                         !complainant_ethnicity == "" &
                           !complainant_age_incident %in% c(-4301, -1, 0, NA) &
                           !complainant_gender == "")
  
#** FIRST PAGE CONTENTS **#
  
  # reactive func. to filter data based on selected FADO types and year
  filtered_data <- reactive({
    if (input$selected_year == "All Time Data") {
      complaints_data %>%
        filter(fado_type %in% input$fado_types)
    } else {
      complaints_data %>%
        filter(fado_type %in% input$fado_types) %>%
        filter(year_received == input$selected_year)
    }
  })
  
  # reactive func. to update raw data based on user mouse action
  selected_data <- reactive({
    event_data("plotly_click", source = "bar_chart")
  })
  
  output$bar_chart <- renderPlotly({
    if (input$selected_year == "All Time Data") {
      
      # calculate the count of complaints for each FADO type and year
      complaints_count <- complaints_data %>%
        filter(fado_type %in% input$fado_types) %>%
        group_by(year_received, fado_type) %>%
        summarise(complaint_count = n())
      
      # create the stacked bar chart using plotly
      gg <- ggplot(complaints_count, aes(x = year_received, y = complaint_count, fill = fado_type)) +
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "Complaint Count", fill = "FADO Type") +
        ggtitle("Complaint Count by FADO Type Over the Years") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # convert ggplot to plotly for interactivity
      ggplotly(gg)
      
    } else {
      # calculate the count of complaints for each FADO type and month of the selected year
      complaints_count <- filtered_data() %>%
        group_by(month_received, fado_type) %>%
        summarise(complaint_count = n())
      
      # create the stacked bar chart using plotly
      gg <- ggplot(complaints_count, aes(x = month_received, y = complaint_count, fill = fado_type)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = "Month", y = "Complaint Count", fill = "FADO Type") +
        ggtitle(paste("Complaint Count by FADO Type in", input$selected_year)) +
        scale_x_continuous(breaks = 1:12, labels = month.abb[1:12]) +
        theme_minimal()
      
      # convert ggplot to plotly for interactivity
      ggplotly(gg)
    }
  })
  
  output$raw_data <- renderDataTable({
    selected_data()
  })
  
  output$top_allegations_table <- renderTable({
    if (input$selected_year == "All Time Data") {
      # filter data based on selected FADO types
      filtered <- complaints_data %>%
        filter(fado_type %in% input$fado_types)
      
    } else {
      # filter data based on selected FADO types and year
      filtered <- complaints_data %>%
        filter(fado_type %in% input$fado_types) %>%
        filter(year_received == input$selected_year)
    }
    
    # count occurrences of each allegation and get the top N as selected by the user
    top_n <- as.numeric(input$top_n)
    top_allegations <- filtered %>%
      group_by(allegation) %>%
      summarise(allegation_count = n()) %>%
      arrange(desc(allegation_count)) %>%
      head(top_n)
    
    top_allegations
  })
  
#** SECOND PAGE CONTENTS **#
  
  output$NYC_pieChart <- renderPlotly({
    if (input$NYC_Pop == "Age") {
      # define age groups based on the age ranges
      age_groups <- c("Under 15", "15-19", "20-24", "25-39", "40-59", "Above 59")
      
      # calculate the total pop for each age group
      total_population <- c(
        sum(nyc_pop$`under.5`, nyc_pop$`age.5.to.9`),
        nyc_pop$`age.10.to.14`,
        nyc_pop$`age.15.to.19`,
        sum(nyc_pop$`age.20.to.24`),
        sum(nyc_pop$`age.25.to.29`, nyc_pop$`age.30.to.34`, nyc_pop$`age.35.to.39`),
        sum(nyc_pop$`age.40.to.44`, nyc_pop$`age.45.to.49`, nyc_pop$`age.50.to.54`, 
            nyc_pop$`age.55.to.59`),
        sum(nyc_pop$`age.60.to.64`, nyc_pop$`age.65.to.69`, nyc_pop$`age.70.to.74`,
            nyc_pop$`age.75.to.79`, nyc_pop$`age.80.to.84`, nyc_pop$`age.85.and.over`)
      )
      
      pie_chart <- plot_ly(labels = age_groups, values = total_population, type = 'pie')
    } else if (input$NYC_Pop == "Gender") {
      
      total_male <- sum(nyc_population$NYC.Male.Total.Pop)
      total_female <- sum(nyc_population$NYC.Female.Total.Pop)
      
      sex_distribution <- data.frame(
        Sex = c("Male", "Female"),
        Population = c(total_male, total_female)
      )
      
      pie_chart <- plot_ly(
        data = sex_distribution,
        labels = ~Sex,
        values = ~Population,
        type = 'pie'
      )
      
    } else if (input$NYC_Pop == "Ethnicity") {
      
      nyc_ethnicity <- c("White", "Hispanic", "Black", "Asian", "American Indian", "Mixed", "Other Race")
      population_percentages <- c(30.9, 28.3, 20.2, 15.6, 0.2, 3.4, 1.4)
      
      nyc_ethnicity_data <- data.frame(nyc_ethnicity, population_percentages)
      
      pie_chart <- plot_ly(nyc_ethnicity_data, labels = ~nyc_ethnicity, values = ~population_percentages, type = 'pie')
    }
    
  })

  output$pieChart <- renderPlotly({
    if (input$chartType == "Age") {
      
      # define age groups
      age_groups <- cut(cleaned_data$complainant_age_incident,
                        breaks = c(0, 14, 19, 24, 39, 59, Inf),
                        labels = c("Under 15", "15-19", "20-24", "25-39", "40-59", "Above 59"))
      
      # count occurrences of each age group
      age_group_counts <- table(age_groups)
      
      pie_chart <- plot_ly(labels = names(age_group_counts), values = age_group_counts, type = 'pie')
      
    } else if (input$chartType == "Gender") {
      
      gender_counts <- cleaned_data %>% 
        group_by(complainant_gender) %>% 
        summarise(count = n())
      
      pie_chart <- plot_ly(gender_counts, labels = ~complainant_gender, values = ~count, type = 'pie')
      
    } else if (input$chartType == "Ethnicity") {
      
      ethnicity_counts <- cleaned_data %>% 
        group_by(complainant_ethnicity) %>% 
        summarise(count = n())
      
      pie_chart <- plot_ly(ethnicity_counts, labels = ~complainant_ethnicity, values = ~count, type = 'pie')
      
    }
  })

}

shinyApp(ui = ui, server = server)