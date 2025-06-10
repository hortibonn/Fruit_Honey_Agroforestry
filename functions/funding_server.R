# dynamic function to create funding UI based on the structure defined in funding_data.xlsx
# function to be called in app.R

create_funding_ui <- function(id, data_path = "data/funding_data.xlsx", sheet = 1) {
  ns <- NS(id) # use namespacing instead of concatenating values - ask @Adrain if this is okay...?####
  funding_data <- readxl::read_excel(data_path, sheet = sheet)
  
  tagList(
    selectInput(ns("country"), "Select Country", choices = unique(funding_data$Country)),
    uiOutput(ns("state_ui")),
    # Government aids/subsidies/ financial support
    
    uiOutput(ns("funding_one_ui")),      # One-time funding dropdown
    uiOutput(ns("funding_annual_ui")),   # Annual funding dropdown
    # private financial support 
    
    numericInput(ns("onetime_private"), "One-time private support [€]",min = 1, max = 1000000, value = 500),
    numericInput(ns("annual_private"), "Annual private support [€/ha]", min = 1, max = 5000, value = 50)
  )
}


funding_server <- function(id, data_path = "data/funding_data.xlsx", sheet = 1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    funding_data <- readxl::read_excel(data_path, sheet = sheet)
    
    # Normalize Funding Type column
    funding_data$Funding_Type_Clean <- tolower(trimws(funding_data$`Funding Type`))
    
    # Country-based state filtering
    filtered_states <- reactive({
      req(input$country)
      funding_data %>%
        filter(Country == input$country) %>%
        pull(State) %>%
        unique() %>%
        sort()
    })
    
    output$state_ui <- renderUI({
      selectInput(ns("state"), "Select State", choices = filtered_states())
    })
    
    # Data filtered by country and state
    filtered_data <- reactive({
      req(input$country, input$state)
      funding_data %>%
        filter(Country == input$country, State == input$state)
    })
    
    # One-time GOV funding
    output$funding_one_ui <- renderUI({
      one_time_gov <- filtered_data() %>%
        filter(grepl("^one-time", Funding_Type_Clean) & !grepl("private", Funding_Type_Clean))
      
      selectInput(
        ns("one_schemes"),
        "Select Government One-time Funding Schemes",
        choices = one_time_gov$`UI display`,
        multiple = TRUE
      )
    })
    
    # Annual GOV funding
    output$funding_annual_ui <- renderUI({
      annual_gov <- filtered_data() %>%
        filter(grepl("^annual", Funding_Type_Clean) & !grepl("private", Funding_Type_Clean))
      
      selectInput(
        ns("annual_schemes"),
        "Select Government Annual Funding Schemes",
        choices = annual_gov$`UI display`,
        multiple = TRUE
      )
    })
    
    # # Category-wise funding total for selected GOV schemes
    # category_summary <- reactive({
    #   req(input$one_schemes, input$annual_schemes)
    #   
    #   selected <- c(input$one_schemes, input$annual_schemes)
    #   
    #   filtered_data() %>%
    #     filter(`UI display` %in% selected) %>%
    #     group_by(category_name) %>%
    #     summarise(total_value = sum(Value, na.rm = TRUE)) %>%
    #     { setNames(.$total_value, .$category_name) }
    # })
    
    category_summary <- reactive({
      # No schemes selected? Return a named numeric(0) vector.
      if (length(input$one_schemes) == 0 && length(input$annual_schemes) == 0) {
        return(setNames(numeric(0), character(0)))
      }
      
      selected <- c(input$one_schemes, input$annual_schemes)
      
      filtered_data() %>% 
        filter(`UI display` %in% selected) %>% 
        group_by(category_name) %>% 
        summarise(total_value = sum(Value, na.rm = TRUE)) %>% 
        { setNames(.$total_value, .$category_name) }
    })
    
    
    # Overall final totals (government + private)
    combined_totals <- reactive({
      cat_sum <- category_summary()
      
      # Add private numeric input to relevant categories
      cat_sum["onetime_external_support_c"] <- input$onetime_private
      cat_sum["annual_external_support_c"] <- input$annual_private
      
      cat_sum
    })
    
    return(
      list(
        country = reactive(input$country),
        state = reactive(input$state),
        selected_one_time_gov = reactive(input$one_schemes),
        selected_annual_gov = reactive(input$annual_schemes),
        onetime_private_input = reactive(input$onetime_private),
        annual_private_input = reactive(input$annual_private),
        category_totals = category_summary,
        total_funding_with_private = combined_totals
      )
    )
  })
}

