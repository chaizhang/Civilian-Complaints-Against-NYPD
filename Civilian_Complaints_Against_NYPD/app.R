library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)

#** LOAD DATA **#

complaints_data <- read.csv("civilian complaints against NYPD.csv")
nyc_pop <- read.csv("NYC Pop by Age and Sex.csv")

#** Define UI **#

ui <- page_fluid(
  # for adding top & bottom padding for h2 titles
  tags$head(tags$style(
    HTML(
      "
      .custom-h2 {
        margin-top: 15px;
        margin-bottom: 25px;
      }
      .custom-titlePanel {
        padding-top: 10px;
        padding-bottom: 15px;
      }
    "
    )
  )),
  
  # titlePanel have to be wrapped inside a `div` element
  div(class = "custom-titlePanel", titlePanel("Complaints Against NYPD Analysis")),
  tabsetPanel(
    
    # INTRO PAGE
    tabPanel(
      "Intro",
      card(htmlOutput("text")),
      card(
        "This dataset is useful for exploring the prevalence of actual or alleged
        malpractice by police and associated complaints in the most populous city
        in the US. Specifically, we will investigate the change in types and number
        of complaints over time, factors in the outcomes of these complaints, and
        demographics of complainants.'"
      )
    ),
    
    
    # FIRST PAGE
    tabPanel(
      "Complaints Over Time",
      h2(class = "custom-h2", "How have complaints against NYPD change over time?"),
      card(layout_columns(
        card(
          checkboxGroupInput(
            "fado_types",
            "Select FADO Types:",
            choices = c(
              "Force",
              "Abuse of Authority",
              "Discourtesy",
              "Offensive Language"
            ),
            selected = c(
              "Force",
              "Abuse of Authority",
              "Discourtesy",
              "Offensive Language"
            )
          )
        ),
        card(
          selectInput(
            "selected_year",
            "Select Year:",
            choices = c("All Time Data", as.character(2020:1985)),
            selected = "All Time Data"
          )
        ),
        card(
          radioButtons(
            "top_n",
            "Select Top N Allegations:",
            choices = c(5, 10, 15),
            selected = 10
          )
        ),
      )),
      layout_columns(card(plotlyOutput("bar_chart")),
                     card(
                       dataTableOutput("raw_data"),
                       tableOutput("top_allegations_table")
                     ),)
    ),
    
    # SECOND PAGE
    tabPanel(
      "Complainant Demographics",
      h2(class = "custom-h2" , "Are certain demographics more likely to file a complaint?"),
      layout_columns(
        card(
          radioButtons(
            "chartType",
            "Select Complainant Demographic:",
            choices = c("Age", "Gender", "Ethnicity"),
            selected = "Age"
          ),
          radioButtons(
            "NYC_Pop",
            "Select NYC Population Demographic:",
            choices = c("Age", "Gender", "Ethnicity"),
            selected = "Age"
          )
        ),
        card(
          card_title("Complainant Demographic"),
          plotlyOutput("pieChart"),
        ),
        card(
          card_title("NYC Population (2020 US Census Data)"),
          plotlyOutput("NYC_pieChart")
        ),
        col_widths = c(2, 5, 5)
      ),
      card(layout_sidebar(
        sidebar = sidebar(
          selectInput(
            "ethnicity",
            "Select your ethnicity:",
            choices = c(
              "Black",
              "Hispanic",
              "White",
              "Unknown",
              "Other",
              "Asian",
              "Refused",
              "American Indian"
            )
          ),
          radioButtons(
            "age",
            "Select your age group:",
            choices = c("Under 15", "15 to 19", "20 to 24", "25 to 39", "40 to 59", "Above 59")
          ),
          radioButtons(
            "gender",
            "Select your gender:",
            choices = c("Male", "Female", "Other")
          ),
          actionButton("calculate", "Calculate")
        ),
        mainPanel(card(
          textOutput("result"),
          plotOutput("pie_chart")
        ))
      ))
    ),
    
    # THIRD PAGE
    tabPanel(
      "Outcomes of Complaints",
      h2(
        class = "custom-h2",
        "How do the outcomes of investigated complaints vary by officer rank, race, and age?"
      ),
      card(layout_sidebar(
        sidebar = sidebar(
          selectInput("rank", "Select Officer Rank:",
                      choices = c(
                        "All", unique(complaints_data$rank_incident)
                      )),
          selectInput("race", "Select Officer Race:",
                      choices = c(
                        "All", unique(complaints_data$mos_ethnicity)
                      )),
          sliderInput(
            "ageRange",
            "Select Officer Age Range:",
            min = min(complaints_data$mos_age_incident),
            max = max(complaints_data$mos_age_incident),
            value = c(20, 50)
          )
        ),
        card(plotlyOutput("stackedBarChart")),
        card(plotlyOutput("stackedBarChartByCount"))
      ),)
    ),
    
    # INTRO PAGE
    tabPanel(
      "Conclusion",
      card(htmlOutput("text2")),
    )
  )
)

#** DEFINE SERVER **#

server <- function(input, output, session) {
  #** DATA CLEANING **#
  
  # removed rows with "" values in complainant_ethnicity column
  # removed rows with -4301, -1, 0 or NA values in complainant_age_incident column
  # removed rows with "" values in complainant_gender column
  cleaned_data <- subset(
    complaints_data,!complainant_ethnicity == "" &
      !complainant_age_incident %in% c(-4301,-1, 0, NA) &
      !complainant_gender == ""
  )
  
  # replace values that contain "Substantiated" with just "Substantiated"
  board_dispos_cleaned_data <- complaints_data %>%
    mutate(board_disposition = case_when(
      grepl("Substantiated", board_disposition) ~ "Substantiated",
      TRUE ~ board_disposition
    ))
  
  #** INTRO PAGE CONTENTS **#
  
  output$text <- renderText({
    HTML(
      "Group 1: Chai Zhang, Kendle Schooler, Atharv Gupta, Colette D'Costa, & Siobhan Loughney
      <br>
      <br>
      Our Dataset: Civilian Complaints Against New York City Police Officers
         <br>
         Source: NYC Civilian Complaint Review Board (ProPublica)
         <br>
         Released 7/2020
         <br>
         Time period: 1985-2020"
    )
  })
  
  
  output$text2 <- renderText({
    HTML(
      "For our first question, “What are the most common complaints or allegations made against NYPD officers, we were able to visualize the significant growth of “Abuse of Authority” complaints over time, while also obsevring a general upward trend in the volume of complaints filed across all FADO types.
      <br>
      <br>
      For our second question, examining which demographics file the most compaints, we see that the demographics of citizens filing complaints is disproportionately skewed towards specific age groups, races, and genders, inconsistent with the distribution of NYC’s population.
      <br>
      <br>
      Finally, by questioning the outcome of complaints by officer rank, we discovered some variation through our visualizations. It appears that the proportion of exonerated outcomes grows slightly in the higher ranks. An additional discovery made through these visualizations is the number of complaints made against officers of a given rank by race. You will find that some selections yield few bars on the chart, simply because there is no data of complaints filed against an officer of certain rank in some of the racial demographics. "
    )
  })
  
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
      gg <-
        ggplot(complaints_count,
               aes(x = year_received, y = complaint_count, fill = fado_type)) +
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
      gg <-
        ggplot(complaints_count,
               aes(x = month_received, y = complaint_count, fill = fado_type)) +
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
      age_groups <-
        c("Under 15", "15-19", "20-24", "25-39", "40-59", "Above 59")
      
      # calculate the total pop for each age group
      total_population <- c(
        sum(nyc_pop$`under.5`, nyc_pop$`age.5.to.9`),
        nyc_pop$`age.10.to.14`,
        nyc_pop$`age.15.to.19`,
        sum(nyc_pop$`age.20.to.24`),
        sum(
          nyc_pop$`age.25.to.29`,
          nyc_pop$`age.30.to.34`,
          nyc_pop$`age.35.to.39`
        ),
        sum(
          nyc_pop$`age.40.to.44`,
          nyc_pop$`age.45.to.49`,
          nyc_pop$`age.50.to.54`,
          nyc_pop$`age.55.to.59`
        ),
        sum(
          nyc_pop$`age.60.to.64`,
          nyc_pop$`age.65.to.69`,
          nyc_pop$`age.70.to.74`,
          nyc_pop$`age.75.to.79`,
          nyc_pop$`age.80.to.84`,
          nyc_pop$`age.85.and.over`
        )
      )
      
      pie_chart <-
        plot_ly(labels = age_groups,
                values = total_population,
                type = 'pie')
    } else if (input$NYC_Pop == "Gender") {
      total_male <- sum(nyc_pop$NYC.Male.Total.Pop)
      total_female <- sum(nyc_pop$NYC.Female.Total.Pop)
      
      sex_distribution <- data.frame(
        Sex = c("Male", "Female"),
        Population = c(total_male, total_female)
      )
      
      pie_chart <- plot_ly(
        data = sex_distribution,
        labels = ~ Sex,
        values = ~ Population,
        type = 'pie'
      )
      
    } else if (input$NYC_Pop == "Ethnicity") {
      nyc_ethnicity <-
        c("White",
          "Hispanic",
          "Black",
          "Asian",
          "American Indian",
          "Mixed",
          "Other Race")
      population_percentages <-
        c(30.9, 28.3, 20.2, 15.6, 0.2, 3.4, 1.4)
      
      nyc_ethnicity_data <-
        data.frame(nyc_ethnicity, population_percentages)
      
      pie_chart <-
        plot_ly(
          nyc_ethnicity_data,
          labels = ~ nyc_ethnicity,
          values = ~ population_percentages,
          type = 'pie'
        )
    }
    
  })
  
  output$pieChart <- renderPlotly({
    if (input$chartType == "Age") {
      # define age groups
      age_groups <- cut(
        cleaned_data$complainant_age_incident,
        breaks = c(0, 14, 19, 24, 39, 59, Inf),
        labels = c("Under 15", "15-19", "20-24", "25-39", "40-59", "Above 59")
      )
      
      # count occurrences of each age group
      age_group_counts <- table(age_groups)
      
      pie_chart <-
        plot_ly(
          labels = names(age_group_counts),
          values = age_group_counts,
          type = 'pie'
        )
      
    } else if (input$chartType == "Gender") {
      gender_counts <- cleaned_data %>%
        group_by(complainant_gender) %>%
        summarise(count = n())
      
      pie_chart <-
        plot_ly(
          gender_counts,
          labels = ~ complainant_gender,
          values = ~ count,
          type = 'pie'
        )
      
    } else if (input$chartType == "Ethnicity") {
      ethnicity_counts <- cleaned_data %>%
        group_by(complainant_ethnicity) %>%
        summarise(count = n())
      
      pie_chart <-
        plot_ly(
          ethnicity_counts,
          labels = ~ complainant_ethnicity,
          values = ~ count,
          type = 'pie'
        )
      
    }
  })
  
  #** SECOND PAGE: COMPLAINANT DEMOGRAPHIC BASED ON USER INPUT **#
  
  # reactive expressions to capture input values
  age <- reactive(input$age)
  gender <- reactive(input$gender)
  ethnicity <- reactive(input$ethnicity)
  
  # calculate percentage based on input demographics
  observeEvent(input$calculate, {
    age_percent <- switch(
      age(),
      "Under 15" = 1.67,
      "15 to 19" = 10.8,
      "20 to 24" = 17.1,
      "25 to 39" = 43.4,
      "40 to 59" = 24.3,
      "Above 59" = 2.72
    )
    
    gender_percent <- switch(
      gender(),
      "Male" = 82.7,
      "Female" = 17.1,
      "Other" = 0.02
    )
    
    ethnicity_percent <- switch(
      ethnicity(),
      "Black" = 59.6,
      "Hispanic" = 22.4,
      "White" = 9.71,
      "Unknown" = 2.95,
      "Other" = 2.38,
      "Asian" = 1.85,
      "Refused" = 0.89,
      "American Indian" = 0.226
    )
    
    total_percent <-
      (age_percent / 100) * (gender_percent / 100) * (ethnicity_percent / 100)
    other_percent <- 1 - total_percent
    
    output$result <- renderText({
      paste(
        "Your specific demographic makes up approximately",
        sprintf("%.2f", total_percent * 100),
        "% of the overall complainant pool."
      )
    })
    
    # create pie chart
    output$pie_chart <- renderPlot({
      data <- c("Your Demographic", "Other")
      values <- c(total_percent, other_percent)
      colors <- c("blue", "orange")
      pie(values,
          labels = data,
          col = colors,
          main = "Demographic Breakdown")
    })
  })
  
  #** THIRD PAGE CONTENTS **#
  
  custom_order <- c(
    "Police Officer",
    "Sergeant",
    "Detective",
    "Lieutenant",
    "Captain",
    "Deputy Inspector",
    "Inspector",
    "Chiefs and other ranks"
  )
  
  # reorder rank_incident column factor levels
  board_dispos_cleaned_data$rank_incident <-
    factor(board_dispos_cleaned_data$rank_incident, levels = custom_order)
  
  # reactive: filter the data based on user inputs
  filtered_data <- reactive({
    data <- board_dispos_cleaned_data
    
    if (input$rank != "All") {
      data <- data[data$rank_incident == input$rank, ]
    }
    if (input$race != "All") {
      data <- data[data$mos_ethnicity == input$race, ]
    }
    data <-
      data[data$mos_age_incident >= input$ageRange[1] &
             data$mos_age_incident <= input$ageRange[2], ]
  })
  
  # render the stacked bar chart
  output$stackedBarChart <- renderPlotly({
    data <- filtered_data()
    
    # prepare the data for plotting
    plot_data <- data %>%
      group_by(rank_incident, board_disposition) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = count / sum(count) * 100)
    
    p <-
      ggplot(plot_data,
             aes(x = rank_incident, y = percentage, fill = board_disposition)) +
      geom_bar(stat = "identity", position = "fill") +
      labs(x = "Officer Rank at Incident", y = "Percentage (%)", fill = "Complaint Outcome") +
      theme_minimal() +
      ggtitle("Proportion of Complaint Outcomes by Rank, Race, and Age")
    
    ggplotly(p)
    
  })
  
  #** THIRD PAGE SECOND CHART: BY COUNT **#
  
  output$stackedBarChartByCount <- renderPlotly({
    data <- filtered_data()
    
    # prepare the data for plotting
    count_data <- data %>%
      count(rank_incident, board_disposition)
    
    # Plotting the stacked bar chart
    stacked_bar_chart <-
      ggplot(count_data,
             aes(x = rank_incident, y = n, fill = board_disposition)) +
      geom_bar(stat = "identity") +
      labs(x = "Officer Rank at Incident", y = "Complaint Outcome Count", fill = "Complaint Outcome") +
      theme_minimal()
    
  })
  
}

shinyApp(ui = ui, server = server)
