#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)





#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(broom)  # For tidying model outputs

# Load the cleaned dataset
cleaned_data <- readr::read_csv("Cleaned_Burnout_Modified2.csv")

# Define UI for the app
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  shiny::titlePanel("Burnout Levels Visualization"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("xaxis", "Select X-axis Variable:", 
                         choices = c("age", "gender", "marital_status", "years_in_profession", "work_life_balance", "job_satisfaction")),
      shiny::selectInput("year", "Select Year:", 
                         choices = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "All")),
      shiny::checkboxInput("regression", "Show Regression Line", value = FALSE),
      width = 3
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Home",
                        shiny::h3("Burnout Levels Visualization Dashboard"),
                        shiny::p("This dashboard provides insights into burnout levels across various medical professions based on different factors such as age, gender, marital status, years in the profession, work-life balance, and job satisfaction."),
                        shiny::p("Key insights include identifying the highest and lowest burnout levels among professions, understanding the impact of demographic factors on burnout, and exploring the correlation between work-life balance or job satisfaction with burnout levels."),
                        shiny::p("Use the sidebar to select different variables and years to visualize the data. Toggle the regression line option to see the trend between variables.")
        ),
        
        shiny::tabPanel("Introduction",
                        shiny::h3("Brief introdution to the burden of Burnout"),
                        shiny::p("Burnout is a significant public health issue characterized by chronic workplace stress, leading to emotional exhaustion, depersonalization, and reduced personal accomplishment. It affects numerous professions globally, with severe consequences for health and productivity. In the United States, burnout-related healthcare spending ranges from $125 to $190 billion annually, while in Germany, burnout costs the economy approximately €9 billion each year. According to the APA’s 2021 Work and Well-being Survey, 79% of employees experienced work-related stress in the month before the survey. Nearly 3 in 5 employees reported negative impacts, including lack of interest, motivation, or energy (26%) and lack of effort at work (19%). Additionally, 36% reported cognitive weariness, 32% reported emotional exhaustion, and 44% reported physical fatigue, marking a 38% increase since 2019."),
                        shiny::p("Burnout leads to severe physical and psychological consequences, including obesity, type 2 diabetes, cardiovascular diseases, musculoskeletal pain, insomnia, depressive symptoms, and psychological ill-health symptoms. Employees who frequently experience burnout are 63% more likely to take sick days, 23% more likely to visit the emergency room, and 2.6 times more likely to leave their current employer. They are also 13% less confident in their performance."),
        ),
        shiny::tabPanel("Plot",
                        plotly::plotlyOutput("plot"),
                        shiny::textOutput("description")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- shiny::reactive({
    if(input$year == "All") {
      data <- cleaned_data
    } else {
      data <- dplyr::filter(cleaned_data, year == as.numeric(input$year))
    }
    data
  })
  
  output$plot <- plotly::renderPlotly({
    data <- filtered_data()
    x_var <- input$xaxis
    
    if(x_var %in% c("work_life_balance", "job_satisfaction")) {
      plot_data <- data %>% dplyr::group_by(profession) %>% 
        dplyr::summarize(avg_value = mean(!!rlang::sym(x_var), na.rm = TRUE),
                         burnout_level_avg = mean(burnout_level, na.rm = TRUE),
                         count = n(), .groups = 'drop')
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = avg_value, y = burnout_level_avg, size = count, color = profession)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::labs(x = paste("Average", gsub("_", " ", x_var)), y = "Average Burnout Level (0-10)", title = "Burnout Level by Profession") +
        ggplot2::theme_minimal() +
        ggplot2::scale_y_continuous(limits = c(0, 10))
      
      if (input$regression) {
        lm_model <- lm(burnout_level_avg ~ avg_value, data = plot_data)
        r_squared <- summary(lm_model)$r.squared
        interpretation <- ifelse(r_squared >= 0.5, "strong", "weak")
        p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
          ggplot2::annotate("text", x = Inf, y = Inf, label = paste("R-squared:", round(r_squared, 2), "-", interpretation, "correlation"), 
                            hjust = 1.1, vjust = 2, size = 4, color = "blue")
      }
    } else {
      if (x_var == "age") {
        plot_data <- data %>% dplyr::mutate(age_group = cut(age, breaks = seq(20, 65, by = 5))) %>%
          dplyr::group_by(profession, age_group) %>% 
          dplyr::summarize(burnout_level_avg = mean(burnout_level, na.rm = TRUE), count = n(), .groups = 'drop')
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = age_group, y = burnout_level_avg, fill = profession)) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::labs(x = "Age Group", y = "Average Burnout Level (0-10)", title = "Burnout Level by Age Group and Profession") +
          ggplot2::theme_minimal() +
          ggplot2::scale_y_continuous(limits = c(0, 10))
      } else if (x_var == "gender") {
        plot_data <- data %>% dplyr::group_by(profession, gender) %>% 
          dplyr::summarize(burnout_level_avg = mean(burnout_level, na.rm = TRUE), count = n(), .groups = 'drop')
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = profession, y = burnout_level_avg, fill = gender)) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::labs(x = "Profession", y = "Average Burnout Level (0-10)", title = "Burnout Level by Gender and Profession") +
          ggplot2::theme_minimal() +
          ggplot2::scale_y_continuous(limits = c(0, 10)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
      } else if (x_var == "marital_status") {
        plot_data <- data %>% dplyr::group_by(profession, marital_status) %>% 
          dplyr::summarize(burnout_level_avg = mean(burnout_level, na.rm = TRUE), count = n(), .groups = 'drop')
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = profession, y = burnout_level_avg, fill = marital_status)) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::labs(x = "Profession", y = "Average Burnout Level (0-10)", title = "Burnout Level by Marital Status and Profession") +
          ggplot2::theme_minimal() +
          ggplot2::scale_y_continuous(limits = c(0, 10)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
      } else if (x_var == "years_in_profession") {
        plot_data <- data %>% dplyr::mutate(years_group = cut(years_in_profession, breaks = seq(0, max(data$years_in_profession, na.rm = TRUE), by = 5))) %>%
          dplyr::group_by(profession, years_group) %>% 
          dplyr::summarize(burnout_level_avg = mean(burnout_level, na.rm = TRUE), count = n(), .groups = 'drop')
        
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = years_group, y = burnout_level_avg, fill = profession)) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::labs(x = "Years in Profession", y = "Average Burnout Level (0-10)", title = "Burnout Level by Years in Profession and Profession") +
          ggplot2::theme_minimal() +
          ggplot2::scale_y_continuous(limits = c(0, 10))
      }
    }
    
    plotly::ggplotly(p) %>% plotly::config(displayModeBar = TRUE)
  })
  
  # Adding text description based on the selected x-axis variable
  output$description <- shiny::renderText({
    x_var <- input$xaxis
    description <- switch(x_var,
                          "age" = "This visualization shows the burnout levels across different age groups and professions. Key observations include:
- The highest burnout in Neurology for the age group 30-35.
- Burnout levels are generally high across all age groups, with notable peaks in specific professions.
- Younger to middle-aged physicians (30-40) tend to report higher burnout, particularly in high-stress specialties like Neurology and Emergency Medicine.
- This data suggests a need for targeted interventions to manage burnout, especially among younger physicians in high-stress fields.",
                          
                          "gender" = "This bar chart compares the average burnout levels among various medical professions, segmented by gender. Key observations include:
- Highest burnout levels in Obstetrics/Gynecology for both genders, with females showing higher burnout levels.
- Lower burnout levels in Opthalmology and Radiology , particularly for males. There is least gender gap in ENT and Dermatology.
- Noticeable differences in certain fields like Dermatology, General Surgery, and Orthopedic Surgery, which show significant gender differences in burnout levels.",
                          
                          "marital_status" = "This chart illustrates the relationship between marital status and burnout levels across different professions. Key observations include:
- Higher burnout in Emergency Medicine and General Surgery.
- Pathology and Ophthalmology have the lowest burnout levels.
- Single physicians generally report higher burnout levels compared to their divorced and married counterparts.
- Divorced physicians also show higher burnout levels but less than single physicians.
- Married physicians have the lowest burnout levels across most professions.
- Interpretation: Single physicians experience the highest burnout, indicating a potential lack of personal support. Emergency Medicine and Surgery are particularly high-stress fields, exacerbating burnout regardless of marital status.",
                          
                          "years_in_profession" = "This chart illustrates the relationship between years in profession and burnout levels across different medical professions. Key observations include:
- Generally higher burnout levels in the initial years (0-5) across most professions.
- Mid to late career trends show varied burnout levels among professions, with some showing a decrease and others remaining high or increasing slightly.
- Emergency Medicine and Obstetrics/Gynecology consistently show higher burnout levels across all experience ranges.",
                          
                          "work_life_balance" = "This scatter plot shows the correlation between average work-life balance and average burnout levels across various professions. Key observations include:
- A clear negative correlation, indicating that higher work-life balance is associated with lower burnout levels.
- High burnout and low work-life balance in professions such as Emergency Medicine, Obstetrics/Gynecology, and Family Medicine.
- Lower burnout and higher work-life balance in professions like Radiology and Pathology.",
                          
                          "job_satisfaction" = "This scatter plot shows the correlation between job satisfaction and burnout levels across various professions. Key observations include:
- A negative correlation, indicating that higher job satisfaction is associated with lower burnout levels.
- High burnout levels in professions such as Emergency Medicine and Anesthesiology, which also have lower job satisfaction.
- Lower burnout levels in professions like Dermatology and ENT, which also have higher job satisfaction."
    )
    description
  })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)






