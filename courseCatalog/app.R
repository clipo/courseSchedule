library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(stringr)

# Read the Excel files
courses <- read_excel("sasExportKUKC.xlsx")
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

# Handle missing descriptions
courses <- courses %>%
  mutate(
    `Catalog Description` = ifelse(is.na(`Catalog Description`) | `Catalog Description` == "", 
                                   "No description is currently available.", 
                                   `Catalog Description`),
    `Catalog Title` = ifelse(is.na(`Catalog Title`) | `Catalog Title` == "",
                             Title,  # Use the Title from courses if Catalog Title is not available
                             `Catalog Title`)
  )

# Define UI
ui <- fluidPage(
  titlePanel("Harpur College Courses"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dept", "Select Department:",
                  choices = c("All", sort(unique(courses$Dept)))),
      
      selectInput("level", "Select Course Level:",
                  choices = c("All", "100", "200", "300", "400", "500", "600", "700")),
      
      selectInput("rows", "Rows to display:",
                  choices = c("10" = 10, "50" = 50, "100" = 100, "All" = -1),
                  selected = -1),
      
      selectInput("fontSize", "Select Font Size:",
                  choices = c("Small" = "10px", "Medium" = "14px", "Large" = "18px"),
                  selected = "14px"),
      
      checkboxGroupInput("columns", "Select columns to display:",
                         choices = c(setdiff(colnames(courses), "Catalog Description"), "Catalog Description"),
                         selected = setdiff(colnames(courses), 
                                            c("Catalog Description", "Course_Key", "Term", "Select", 
                                              "Resp College", "Resp Dept", "Rst", "XL Cap", "XL Act", 
                                              "XL Rem", "Fee", "Instr Method", "Spec Crse Attr", 
                                              "Term Code", "CRN", "Resp Coll", "Wait Cap", "Wait Act", 
                                              "Wait Rem", "Dept")))
    ),
    
    mainPanel(
      DTOutput("coursesTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- courses
    
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
    
    data
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
        pageLength = if(input$rows == -1) nrow(data) else as.numeric(input$rows),
        lengthMenu = c(10, 50, 100, -1),
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      style = 'auto',
      class = 'cell-border stripe',
      rownames = FALSE
    ) %>% 
      formatStyle(
        columns = names(data[, selected_columns, drop = FALSE]),
        fontSize = input$fontSize
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)