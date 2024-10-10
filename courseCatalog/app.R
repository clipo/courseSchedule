library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Read the Excel files
courses <- read_excel("course_list.xlsx")
course_descriptions <- read_excel("Harpur_Course_Descriptions.xlsx")

# Create unique keys for both dataframes
courses <- courses %>%
  mutate(Course_Key = paste(Dept, Crse, sep = "-"))

course_descriptions <- course_descriptions %>%
  mutate(Course_Key = paste(Dept, Crs, sep = "-"))

# Function to find the matching description
find_match <- function(key, descriptions) {
  match <- descriptions %>%
    filter(Course_Key == key) %>%
    slice(1)  # Take only the first match if multiple exist
  
  if (nrow(match) == 0) {
    return(c(NA_character_, NA_character_))
  } else {
    return(c(match$`Catalog Title`, match$`Catalog Description`))
  }
}

# Apply the matching function to each course
courses <- courses %>%
  rowwise() %>%
  mutate(
    match_result = list(find_match(Course_Key, course_descriptions)),
    `Catalog Title` = match_result[1],
    `Catalog Description` = match_result[2]
  ) %>%
  ungroup() %>%
  select(-match_result)  # Remove the temporary list column

# Handle missing descriptions and create location links
courses <- courses %>%
  mutate(
    `Catalog Description` = ifelse(is.na(`Catalog Description`) | `Catalog Description` == "", 
                                   "No description is currently available.", 
                                   `Catalog Description`),
    `Catalog Title` = ifelse(is.na(`Catalog Title`) | `Catalog Title` == "",
                             Title,  # Use the Title from courses if Catalog Title is not available
                             `Catalog Title`),
    Location = ifelse(is.na(Location) | Location == "", 
                      Location, 
                      sprintf('<a href="https://www.binghamton.edu/maps/index.html?q=%s&btn_site=search" target="_blank">%s</a>', 
                              substr(Location, 1, 2), 
                              Location))
  )

# Define UI
ui <- fluidPage(
  titlePanel("Harpur College Courses"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("term", "Select Term:",
                  choices = c("All", sort(unique(courses$Term)))),
      
      selectInput("dept", "Select Department:",
                  choices = c("All", sort(unique(courses$Dept)))),
      
      selectInput("level", "Course Level:",
                  choices = c("All", "100", "200", "300", "400", "500", "600", "700")),
      
      selectInput("rows", "Rows to display:",
                  choices = c("10" = 10, "50" = 50, "100" = 100, "All" = nrow(courses)),
                  selected = nrow(courses)),
      
      selectInput("fontSize", "Select Font Size:",
                  choices = c("Ultra Small" = "6px", "Smallest" = "8px", "Small" = "10px", "Medium" = "14px", "Large" = "18px"),
                  selected = "10px"),
      
      checkboxGroupInput("columns", "Select columns to display:",
                         choices = c(setdiff(colnames(courses), "Catalog Description"), "Catalog Description"),
                         selected = setdiff(colnames(courses), 
                                            c("Catalog Description", "Course_Key", "Term", "Select", 
                                              "Resp College", "Resp Dept", "Rst", "XL Cap", "XL Act", 
                                              "XL Rem", "Fee", "Instr Method", "Spec Crse Attr", 
                                              "Term Code", "CRN", "Resp Coll", "Wait Cap", "Wait Act", 
                                              "Wait Rem", "Dept", "Date", "Gen Ed", "Catalog Title")))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Course List", DTOutput("coursesTable")),
        tabPanel("Mock Schedule", 
                 h3("Selected Courses"),
                 DTOutput("selectedCoursesTable"),
                 verbatimTextOutput("conflictOutput")
        ),
        tabPanel("Dashboard",
                 fluidRow(
                   column(6, plotOutput("coursesPerDept")),
                   column(6, plotOutput("levelDistribution"))
                 ),
                 fluidRow(
                   column(6, plotOutput("coursesPerDay")),
                   column(6, plotOutput("coursesPerHour"))
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive filtered data
  # Reactive filtered data
  filtered_data <- reactive({
    data <- courses
    
    if (input$term != "All") {
      data <- data %>% filter(Term == input$term)
    }
    
    if (input$dept != "All") {
      data <- data %>% filter(Dept == input$dept)
    }
    
    if (input$level != "All") {
      level_start <- as.numeric(input$level)
      level_end <- level_start + 99
      data <- data %>% 
        filter(as.numeric(str_sub(Crse, 1, 3)) >= level_start,
               as.numeric(str_sub(Crse, 1, 3)) <= level_end)
    }
    
    # Sort by Subj, then by the numeric part of Crse, then by Sec
    data %>%
      mutate(
        Crse_Num = as.numeric(str_extract(Crse, "^\\d+")),
        Sec_Num = as.numeric(str_extract(Sec, "^\\d+"))
      ) %>%
      arrange(Subj, Crse_Num, Sec_Num, Sec) %>%
      select(-Crse_Num, -Sec_Num)
  })
  
  # Render the interactive table
  output$coursesTable <- renderDT({
    data <- filtered_data()
    
    # Ensure Catalog Description is the last column if selected
    selected_columns <- input$columns
    if ("Catalog Description" %in% selected_columns) {
      selected_columns <- c(setdiff(selected_columns, "Catalog Description"), "Catalog Description")
    }
    
    datatable(
      data[, selected_columns, drop = FALSE],
      options = list(
        pageLength = if(input$rows == "All") nrow(data) else as.numeric(input$rows),
        lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(
          targets = which(names(data[, selected_columns, drop = FALSE]) == "Time") - 1,
          width = "200px"
        ))
      ),
      style = 'auto',
      class = 'cell-border stripe',
      rownames = FALSE,
      escape = FALSE,  # Allow HTML in the table cells
      selection = 'single'
    ) %>% 
      formatStyle(
        columns = names(data[, selected_columns, drop = FALSE]),
        fontSize = input$fontSize
      )
  })
  
  # Handle course selection
  observeEvent(input$coursesTable_rows_selected, {
    selected_row <- filtered_data()[input$coursesTable_rows_selected, ]
    current_selected <- selected_courses()
    
    if (!any(current_selected$CRN == selected_row$CRN)) {
      selected_courses(rbind(current_selected, selected_row))
    }
  })
  
  # Render selected courses table
  output$selectedCoursesTable <- renderDT({
    selected_data <- selected_courses()
    if (nrow(selected_data) > 0) {
      selected_data <- selected_data %>%
        select(Subj, Crse, Sec, Title, Days, Time, Date)
    }
    
    datatable(
      selected_data,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      style = 'auto',
      class = 'cell-border stripe',
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  # Handle course deselection
  observeEvent(input$selectedCoursesTable_rows_selected, {
    selected_row <- input$selectedCoursesTable_rows_selected
    current_selected <- selected_courses()
    
    if (length(selected_row) > 0) {
      current_selected <- current_selected[-selected_row, ]
      selected_courses(current_selected)
    }
  })
  
  # Parse time string
  parse_time <- function(time_str) {
    # Remove any extra spaces
    time_str <- trimws(time_str)
    
    # Split start and end times
    times <- strsplit(time_str, " - ")[[1]]
    
    # Parse times
    start_time <- parse_date_time(times[1], "I:M p")
    end_time <- parse_date_time(times[2], "I:M p")
    
    # If parsing failed, return NULL
    if (is.na(start_time) || is.na(end_time)) {
      return(NULL)
    }
    
    list(start = start_time, 
         end = end_time,
         original = time_str)
  }
  
  # Check for time conflicts
  check_conflicts <- reactive({
    courses_data <- selected_courses()
    conflicts <- character(0)
    
    if (nrow(courses_data) > 1) {
      for (i in 1:(nrow(courses_data) - 1)) {
        for (j in (i + 1):nrow(courses_data)) {
          course1 <- courses_data[i, ]
          course2 <- courses_data[j, ]
          
          if (!is.na(course1$Time) && !is.na(course2$Time) && 
              course1$Time != "" && course2$Time != "" &&
              !is.na(course1$Days) && !is.na(course2$Days) &&
              course1$Days != "" && course2$Days != "") {
            
            time1 <- parse_time(course1$Time)
            time2 <- parse_time(course2$Time)
            days1 <- strsplit(course1$Days, "")[[1]]
            days2 <- strsplit(course2$Days, "")[[1]]
            
            if (!is.null(time1) && !is.null(time2)) {
              common_days <- intersect(days1, days2)
              
              if (length(common_days) > 0) {
                if ((time1$start <= time2$start && time2$start < time1$end) || 
                    (time2$start <= time1$start && time1$start < time2$end)) {
                  conflicts <- c(conflicts, 
                                 sprintf("Time conflict between %s (%s) and %s (%s) on %s", 
                                         course1$Course_Key, time1$original,
                                         course2$Course_Key, time2$original,
                                         paste(common_days, collapse = ", ")))
                }
              }
            }
          }
        }
      }
    }
    conflicts
  })
  
  # Display conflicts
  output$conflictOutput <- renderText({
    conflicts <- check_conflicts()
    if (length(conflicts) > 0) {
      paste("Conflicts detected:\n", paste(conflicts, collapse = "\n"))
    } else {
      "No time conflicts detected."
    }
  })
  
  # Reactive for dashboard data
  dashboard_data <- reactive({
    data <- filtered_data()  # Use filtered_data which is based only on courses
    
    data %>%
      mutate(
        Level = case_when(
          as.numeric(substr(Crse, 1, 1)) == 1 ~ "100",
          as.numeric(substr(Crse, 1, 1)) == 2 ~ "200",
          as.numeric(substr(Crse, 1, 1)) == 3 ~ "300",
          as.numeric(substr(Crse, 1, 1)) == 4 ~ "400",
          as.numeric(substr(Crse, 1, 1)) >= 5 ~ "500+",
          TRUE ~ "Other"
        ),
        Hour = as.numeric(substr(str_extract(Time, "^\\d{2}"), 1, 2))
      ) %>%
      filter(!is.na(Hour))  # Remove rows with NA hours
  })
  
  # Courses per Department
  output$coursesPerDept <- renderPlot({
    ggplot(dashboard_data(), aes(x = reorder(Dept, Dept, function(x) length(x)), fill = Dept)) +
      geom_bar() +
      coord_flip() +
      labs(title = "Courses per Department", x = "Department", y = "Number of Courses") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Level Distribution
  output$levelDistribution <- renderPlot({
    ggplot(dashboard_data(), aes(x = Level, fill = Level)) +
      geom_bar() +
      labs(title = "Course Level Distribution", x = "Level", y = "Number of Courses") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Courses per Day
  output$coursesPerDay <- renderPlot({
    dashboard_data() %>%
      separate_rows(Days, sep = "") %>%
      filter(Days != "") %>%
      ggplot(aes(x = Days, fill = Days)) +
      geom_bar() +
      labs(title = "Courses per Day", x = "Day", y = "Number of Courses") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Courses per Hour
  output$coursesPerHour <- renderPlot({
    ggplot(dashboard_data(), aes(x = Hour)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = "Course Offerings per Hour", x = "Hour of Day", y = "Number of Courses") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 23, by = 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)