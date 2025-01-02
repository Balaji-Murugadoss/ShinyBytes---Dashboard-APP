# Load required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(shinyjs)
library(httr)
library(stringr)
library(jsonlite)
library(DBI)
library(RMariaDB)
library(rsconnect)

rsconnect::setAccountInfo(name='balajimurugadoss', token='AEE0EA5538048168134CD3C056E61B84', secret='noWLxzHWTu89WCb0EXmHavMilyP/0/ix63FF2h1Y')

# UI
ui <- dashboardPage(
  skin = "blue",  # Changed skin to 'blue' for a different theme color
  dashboardHeader(title = "ITOM6265-HW1"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("HW Summary", tabName = "HWSummary", icon = icon("dashboard")),
      menuItem("Q1-DB Query", tabName = "dbquery", icon = icon("database")),
      menuItem("Q2-Maps", tabName = "leaflet", icon = icon("map")),
      menuItem("ChatGPT Clone", tabName = "chatgpt", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap"),
      tags$style(HTML("
        body, .content-wrapper, .right-side {
          font-family: 'Poppins', sans-serif;
          background-color: #f4f6f9;
        }
        .box {
          border-top: 3px solid #1E90FF;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .btn-primary {
          background-color: #1E90FF;
          border-color: #1C86EE;
        }
        .btn-primary:hover {
          background-color: #1C86EE;
        }
        #chat_history {
          max-height: 400px;
          overflow-y: auto;
          border: 1px solid #ddd;
          padding: 10px;
          border-radius: 8px;
          background-color: #fff;
        }
        .alert-primary {
          background-color: #e8e6ff;
          border-color: #d4d0ff;
          color: #4a47a3;
        }
        .alert-success {
          background-color: #e6ffe8;
          border-color: #d0ffd4;
          color: #47a34a;
        }
        .chat-message {
          margin-bottom: 10px;
          padding: 10px;
          border-radius: 8px;
        }
        .user-message {
          background-color: #e8e6ff;
          margin-left: 20px;
        }
        .bot-message {
          background-color: #e6ffe8;
          margin-right: 20px;
        }
        .skin-blue .main-header .navbar {
    background-color: #000000;
        }
.skin-blue .main-header .logo {
    background-color: #000000;
    color: #fff;
    border-bottom: 0 solid transparent;
}


      "))
    ),
    tabItems(
      # HW Summary Tab
      tabItem(
        tabName = "HWSummary",
        h3("This HW was submitted by Balaji Murugadoss of ITOM6265"),
        p("Approach and Enhancements:"),
        tags$ul(
          tags$li("Used shinydashboard for an intuitive and responsive layout."),
          tags$li("Implemented RMariaDB for efficient database queries on Zomato restaurant data."),
          tags$li("Utilized Leaflet for interactive mapping of restaurant locations."),
          tags$li("Developed a ChatGPT clone using OpenAI's API for natural language interactions."),
          tags$li("Enhanced UI with a custom teal theme and Poppins font for improved aesthetics."),
          tags$li("Improved chat interface with distinct styling for user and bot messages."),
          tags$li("Added download functionality for chat history."),
          tags$li("Implemented responsive design for better usability across devices."),
          tags$li("Ensured code modularity and readability for easy maintenance and future enhancements.")
        ),
        p("The application showcases integration of multiple R libraries to create a comprehensive dashboard with database querying, geospatial visualization, and AI-powered chat capabilities.")
      ),
      
      # Q1-DB Query Tab
      tabItem(
        tabName = "dbquery",
        fluidRow(
          box(
            title = "Search Parameters", width = 4, status = "primary",
            textInput("name_pattern", "Name Pattern:", placeholder = "Enter pattern (e.g., %Pizza%)"),
            sliderInput("votes_range", "Votes Range:", min = 0, max = 1000, value = c(0, 1000)),
            actionButton("run_query", "Get Results", class = "btn-primary")
          ),
          box(
            title = "Query Results", width = 8, status = "info",
            DTOutput("query_results")
          )
        )
      ),
      
      # Q2-Maps Tab
      tabItem(
        tabName = "leaflet",
        fluidRow(
          box(
            title = "Map Controls", width = 4, status = "primary",
            actionButton("show_map", "Display Map", class = "btn-primary")
          ),
          box(
            title = "Map Output", width = 8, status = "info",
            leafletOutput("map_output", height = 500)
          )
        )
      ),
      
      # ChatGPT Clone Tab
      tabItem(tabName = "chatgpt",
              fluidPage(
                div(
                  titlePanel("ChatGPT Clone with Shiny"),
                  style = "color: #ffffff; background-color: #000000; padding: 15px; border-radius: 10px;"
                ),
                sidebarLayout(
                  sidebarPanel(
                    style = "background-color: #000000; color: #ffffff; padding: 15px; border-radius: 10px;",
                    h3("OpenAI Playground - ChatGPT Clone"),
                    p("Chat with an OpenAI GPT model. Add your API key below."),
                    textInput("api_key", "API Key", "sk-PLACEYOUROWNAPIKEYHERE"),
                    tags$p("Get your API key: ", 
                           tags$a(href = "https://platform.openai.com/account/api-keys", target="_blank", "OpenAI API Keys", style = "color: #1e90ff; text-decoration: none;")
                    ),
                    tags$hr(),
                    selectInput("model_name", "Model Name",
                                choices = c("gpt-4", "gpt-4-turbo", "gpt-3.5-turbo-0301", "gpt-3.5-turbo"), selected = "gpt-3.5-turbo"),
                    sliderInput("temperature", "Temperature", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
                    sliderInput("max_length", "Maximum Length", min = 1, max = 2048, value = 512, step = 1),
                    tags$hr(),
                    textAreaInput("sysprompt", "SYSTEM PROMPT", height = "100px", placeholder = "You are a helpful assistant."),
                    actionButton("clear_history", "Clear Chat History", icon = icon("trash"), style = "margin-top: 10px; background-color: #1e90ff; color: white; border: none;"),
                    downloadButton("download_chat", "Download Chat History", icon = icon("download"), style = "margin-top: 10px; background-color: #1e90ff; color: white; border: none;")
                  ),
                  mainPanel(
                    style = "background-color: #222222; padding: 15px; color: #ffffff; border-radius: 10px;",
                    tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}"),
                    tags$style(type = "text/css", ".shiny-output-error:before {content: 'Check your inputs or API key';}"),
                    fluidRow(
                      column(12, tags$h3("Chat History", style = "color: #ffffff;"), tags$hr(), uiOutput("chat_history"), tags$hr())
                    ),
                    fluidRow(
                      column(11, textAreaInput("user_message", "USER PROMPT", placeholder = "Enter your message:", width = "100%")),
                      column(1, actionButton("send_message", "Send", icon = icon("paper-plane"), style = "background-color: #1e90ff; color: white; border: none; margin-top: 10px;"))
                    )
                  )
                )
              )
      )
      
      
    )
  )
)

# Server
server <- function(input, output, session) {
  # DB Connection
  dbConnector1 <- reactive({
    DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = "itom6265-db.c1e6oi6e06on.us-east-2.rds.amazonaws.com",
      port = 3306,
      dbname = "zomato",
      user = "root",
      password = "mysql_local_pass"
    )
  })
  
  # Q1: DB Query
  observeEvent(input$run_query, {
    conn <- dbConnector1()
    on.exit(dbDisconnect(conn), add = TRUE)
    
    name_filter <- if (input$name_pattern != "") paste0("name LIKE '", input$name_pattern, "'") else "1=1"
    votes_filter <- paste0("votes BETWEEN ", input$votes_range[1], " AND ", input$votes_range[2])
    
    query <- paste0(
      "SELECT * FROM zomato_rest_char WHERE ", name_filter, " AND ", votes_filter
    )
    result <- dbGetQuery(conn, query)
    output$query_results <- renderDT({datatable(result)})
  })
  
  # Q2: Maps
  observeEvent(input$show_map, {
    # Connect to the database
    conn <- dbConnector1()
    on.exit(dbDisconnect(conn), add = TRUE)  # Ensure the connection closes after use
    
    # Query the data for valid latitude and longitude
    query <- "SELECT name, Latitude, longitude FROM zomato_rest_char WHERE Latitude IS NOT NULL AND longitude IS NOT NULL"
    locations <- dbGetQuery(conn, query)
    
    # Convert latitude and longitude to numeric (in case they are stored as characters)
    locations$Latitude <- as.numeric(locations$Latitude)
    locations$longitude <- as.numeric(locations$longitude)
    
    # Debugging: Print the fetched locations to verify
    print(locations)
    
    # Render the map with markers
    output$map_output <- renderLeaflet({
      if (nrow(locations) > 0) {
        leaflet(data = locations) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%  # Use a modern and lightweight tile provider
          addMarkers(
            ~longitude, ~Latitude,
            popup = ~name,  # Display restaurant name in the popup
            label = ~name,  # Add a label for each marker
            clusterOptions = markerClusterOptions()  # Cluster markers if they overlap
          )
      } else {
        # Handle empty data gracefully
        leaflet() %>%
          addTiles() %>%
          addPopups(
            lng = 0, lat = 0,
            popup = "No data available to display. Please check the database or query."
          )
      }
    })
  })
  
  
  # ChatGPT Clone
  chat_data <- reactiveVal(data.frame(source = character(0), message = character(0), stringsAsFactors = FALSE))
  
  observeEvent(input$send_message, {
    if (input$user_message != "" && input$api_key != "") {
      new_data <- data.frame(source = "User", message = input$user_message, stringsAsFactors = FALSE)
      chat_data(rbind(chat_data(), new_data))
      
      disable("send_message")
      gpt_res <- call_gpt_api(input$api_key, input$user_message, input$model_name, input$temperature, input$max_length, input$sysprompt)
      print(gpt_res)
      
      if (!is.null(gpt_res)) {
        gpt_data <- data.frame(source = "ChatGPT", message = gpt_res, stringsAsFactors = FALSE)
        chat_data(rbind(chat_data(), gpt_data))
      } else {
        showModal(modalDialog(
          title = "Error",
          "Failed to get response. Check your API key or input.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      enable("send_message")
      updateTextInput(session, "user_message", value = "")
    }
  })
  
  observeEvent(input$clear_history, {
    chat_data(data.frame(source = character(0), message = character(0), stringsAsFactors = FALSE))
  })
  
  call_gpt_api <- function(api_key, prompt, model_name, temperature, max_length, sysprompt) {
    tryCatch({
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions", 
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type("application/json"),
        encode = "json",
        body = list(
          model = model_name,
          messages = list(
            list(role = "system", content = sysprompt),
            list(role = "user", content = prompt)
          ),
          temperature = temperature,
          max_tokens = max_length
        )
      )
      if (status_code(response) == 200) {
        content(response)$choices[[1]]$message$content %>% str_trim()
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
  }
  
  output$chat_history <- renderUI({
    lapply(1:nrow(chat_data()), function(i) {
      div(class = ifelse(chat_data()[i, "source"] == "User", "alert alert-secondary", "alert alert-success"),
          HTML(paste0("<b>", chat_data()[i, "source"], ":</b> ", chat_data()[i, "message"])))
    }) %>% tagList()
  })
  
  output$download_chat <- downloadHandler(
    filename = function() { "chat_history.txt" },
    content = function(file) {
      chat_history <- paste(chat_data()$source, chat_data()$message, sep = ": ", collapse = "\n")
      writeLines(chat_history, file)
    }
  )
}

# Run the app
shinyApp(ui,server)