# # Install + load libraries ----
# packages <- c("shiny", "readxl", "bslib", "sortable", "shinythemes", "shinyWidgets",
#               "decisionSupport", "tidyverse", "ggridges", "here",
#               "dplyr", "ggplot2", "readr")
# 
# if (Sys.info()[["sysname"]] == "Windows") {
#   install_if_missing <- function(pkg) {
#     if (!requireNamespace(pkg, quietly = TRUE))
#       install.packages(pkg, dependencies = TRUE)
#   }
#   ## install what’s missing
#   invisible(lapply(packages, install_if_missing))
# }
# 
# ## then attach everything
# lapply(packages, library, character.only = TRUE)

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)

if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}
library(shinythemes)

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
library(shinyWidgets)

if (!requireNamespace("decisionSupport", quietly = TRUE)) {
  install.packages("decisionSupport")
}
library(decisionSupport)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)  # For reading and writing CSV files

if (!requireNamespace("ggridges", quietly = TRUE)) {
  install.packages("ggridges")
}
library(ggridges)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")
}
library(ggtext)


# source("functions/saveLoad-module.R")
source("functions/Hof_Luetke.R")
# source("functions/Walnut_grain_veg_tub_df.R")
source("functions/dynamic-helper.R")
source("functions/funding_server.R")

file_path_vars <- "data/Hof_luetke_input_table.xlsx"
sheet_meta <- readxl::read_excel(file_path_vars, sheet = "sheet_names",
                                 col_types = c("text", "text"))
sheet_names <- sheet_meta$sheet_names
sheet_icons <- setNames(sheet_meta$icon, sheet_meta$sheet_names)





# UI ----
ui <- fluidPage(
  
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  ## Title ----
  titlePanel(
    tags$div(
      style = "display:flex; align-items:center;justify-content:space-between;
      width: 100% !important; margin: 20px; padding: 0 15px;
      box-sizing: border-box; background-color: #f2f2f2;",
      
      # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
      tags$img(src = "UniBonnHortiBonn_logo_transparent.png", height = "100px",
               style = "margin-left: auto; max-width: 20%; height: auto; cursor: pointer;"),
      # ),
      
      tags$h2(tags$div("Decision Analysis:"),
              tags$div("Conversion of treeless cropland into Fruit and Honey Agroforestry System"),
              style = "text-align: center; flex-grow: 1;"),
      
      # tags$a(href = "https://www.uni-bonn.de", target = "_blank",
      tags$img(src = "ReFOREST_logo_horizontal_transparent.png", height = "100px",
               style = "margin-right: auto; max-width: 30%; height: auto; cursor: pointer;")
      # ),
    ),
  ),
  
  ## Sidebar ----
  sidebarLayout(
    sidebarPanel(width = 4,
                 style = "height: 100%; overflow-y: auto",
                 
                 accordion(
                   id = "collapseSidebar",
                   open = FALSE,
                   
                   div(
                     class = "text-center",
                     actionButton("run_simulation", "Run Model",
                                  icon = icon("play"), class = "btn-primary")
                   ),
                   br(),
                   
                   ### Save/Load functionality ----
                   # saveLoadUI("savemod"),
                   accordion_panel(
                     title = "Save / Load project", icon = icon("folder-open"),
                     tagList(
                       textInput("state_name", "Project name"),
                       actionButton("save_btn",  label = tagList(icon("floppy-disk"),  "Save"  ), class = "btn btn-dark"),
                       
                       br(), br(),
                       selectInput("state_picker", "Saved versions", choices = NULL),
                       
                       fluidRow(
                         column(6, actionButton("load_btn",   tagList(icon("rotate"),  "Load"  ), class = "btn btn-secondary")),
                         column(6, actionButton("delete_btn", tagList(icon("trash"),   "Delete"), class = "btn btn-secondary"))
                       ),
                       hr(),
                       downloadButton("download_csv", label = tagList(icon("download"), "Download current inputs (.csv)"))
                     )
                   ),
                   
                   
                   
                   ### Expertise filter ----
                   accordion_panel(
                     title = "Expertise categories",
                     icon = icon("clipboard-question"),
                     tagList(
                       tags$h5(
                         "Expertise categories",
                         tags$span(
                           icon("circle-question"),
                           title = "Select your main expertise to modify only the variables suited to such expertise. Ultimately, we are all experts to a higher or lower degree in all categories. You too :) 
                           
                           But we recommend that the first time you use this interface you select only one or two categories in order to get familiar with it. The simulations will still run with the default values for those expertise categories that you do not select.
                           
                           If you do not check any box, you will see all the variables of the model (i.e.: all expertise categories).
                           
                           If you are the decision maker, e.g.: you are a farmer thinking about implementing agroforestry in treeles land, please check the Decision Maker box to set basic variables that serve to quantify your interests and motivations",
                           style = "cursor: help; margin-left: 8px;"
                         )
                       ),
                       uiOutput("category_filter_ui")
                     )
                   ),
                   
                   # ### Crop selector and rotation ----
                   # accordion_panel(
                   #   title = "Crops",
                   #   icon = icon("clipboard-question"),
                   #   accordion_panel(
                   #     title = "Crop selector",
                   #     icon = icon("seedling"),
                   #     uiOutput("crop_rot_filter_ui")
                   #   ),
                   #   uiOutput("rotation_builder_ui"),   # rendered only when crops picked
                   #   verbatimTextOutput("rotation_vec") # convenient preview
                   # ),
                   
                   ### funding scheme ----
                   accordion_panel(
                     title = "Funding schemes", icon = icon("euro-sign"),
                     create_funding_ui("funding")
                   ),
                   br(),
                   uiOutput("dynamic_element_ui")
                   
                 )
                 
    ),
    
    ## Main Panel ----
    mainPanel(width = 8,
              # tags$p(
              #   tags$strong(
              #     tagList(
              #       "This app simulates the present value of converting a treeless arable field into alley cropping with fruit and/or high-value timber trees (hedgerows included).",
              #       # tags$br(),
              #       "Open the tabs on the left to adjust the value ranges of every model variable so they reflect local conditions.",
              #       # tags$br(),
              #       "If you do not feel confident about all inputs, use the ",
              #       tags$em("'Expertise categories'"),
              #       " tab to limit edits to those you know.",
              #       # tags$br(),
              #       "Press ", tags$em("Run the model"), " to launch multiple Monte-Carlo runs; each uses a random combination of input values drawn from your ranges.",
              #       # tags$br(),
              #       "After computation the results appear below.",
              #       # tags$br(),
              #       "Under ", tags$em("Funding schemes"), " you can choose the scheme for your region. Missing schemes? Let us know at mjimene1@uni-bonn.de."
              #     )
              #   )
              # ),
              tags$h5(
                "This app serves to simulate the present value of converting a treeless arable field into alley cropping with fruit and/or high-value timber trees. It also allows to account for hedgerows in the hedges of the alley cropping field.\n
              You can modify the value ranges of all the variables of the calculator by opening the tabs in left-hand side of the screen.\n
              By doing so, you can account for the locally-specific environment and socio-economic context.\n
              In the tab 'Expertise categories' you can select the categories for which you want to modify the default values. This is useful if you do not feel confident enough to provide estimates for all the variables of the model.\n
              When clicking 'Run the model', the app will perform multiple runs of the model, each with a unique combination of values of the input variables, always within the range defined by the bars of the left-hand side tabs.\n
              After the model is computed, the results will be displayed here below\n
              In the tab 'Funding schemes' you can select the funding scheme of your region of interest. If you do not find the funding schemes of your regions of interest, please contact us to include it (mjimene1@uni-bonn.de)"
              ),
              tags$a(
                "Click here for latest info on Sustainable Farming Incentive",
                href = "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024",
                target="_blank",
                class = "my-btn"
              ),
              br(), br(),
              plotOutput("plot1_ui", height = "550px"),
              br(),
              uiOutput("plot1_dl_ui"),
              br(), br(),br(), br(),
              
              plotOutput("plot2_ui", height = "550px"),
              br(),
              uiOutput("plot2_dl_ui"),
              br(), br(),br(), br(),
              
              # plotOutput("plot3_ui", height = "550px"),
              # br(),
              # uiOutput("plot3_dl_ui"),
              # br(), br(),br(), br(),
              
              plotOutput("plot4_ui", height = "550px"),
              br(),
              uiOutput("plot4_dl_ui"),
              br(), br(),br(), br(),
              
              plotOutput("plot5_ui", height = "550px"),
              br(),
              uiOutput("plot5_dl_ui"),
              br(), br(),br(), br(),
              
              # plotOutput("plot6_ui", height = "550px"),
              # br(),
              # uiOutput("plot6_dl_ui"),
              # br(), br(),br(), br(),
              # 
              # plotOutput("plot7_ui", height = "550px"),
              # br(),
              # uiOutput("plot7_dl_ui"),
              # br(), br(),br(), br(),
              
              plotOutput("plot8_ui", height = "550px"),
              br(),
              uiOutput("plot8_dl_ui"),
              br(), br(),br(), br(),
              
    )
  )
  
)


# Server ----
server <- function(input, output, session) {
  
  ## Dynamic funding module ----
  funding <- funding_server("funding")   # returns a list of reactives
  
  ## Helper for safe extraction from named vector
  safe_get <- function(vec, name) {
    if (is.null(vec) || length(vec) == 0 || is.na(vec[name])) return(0)
    if (! name %in% names(vec)) return(0)
    as.numeric(vec[name])
  }
  
  ## Wrapper -> *exact* variables the walnut model expects
  funding_variables <- reactive({
    sel <- funding$category_totals()        # named vector per category (gov)
    
    ## private inputs
    onetime_private <- funding$onetime_private_input()
    annual_private  <- funding$annual_private_input()
    
    # area / trees already entered elsewhere in UI
    AF1_area  <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF1_tree_row_area_c))
    AF2_area  <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF2_tree_row_area_c))
    AF1_trees <- as.numeric(input$AF1_num_trees_c)
    AF2_trees <- sum(as.numeric(input$num_oak_trees_c), as.numeric(input$num_birch_trees_c),
                     as.numeric(input$num_rowan_trees_c), as.numeric(input$num_hazel_trees_c),
                     as.numeric(input$num_damson_trees_c), as.numeric(input$num_bcherry_trees_c))
    
    # funding amounts per category
    per_ha      <- safe_get(sel, "funding_onetime_per_ha_schemes_c")
    per_tree    <- safe_get(sel, "funding_onetime_per_tree_schemes_c")
    annual_ha   <- safe_get(sel, "annual_funding_schemes_c")
    perc_incost <- safe_get(sel, "funding_onetime_percentage_incost_schemes_c")
    perc_cons   <- safe_get(sel, "funding_onetime_percentage_consult_schemes_c")
    
    # Compute totals
    AF1_one_time   <- per_ha   * AF1_area  + per_tree * AF1_trees + onetime_private
    AF2_one_time   <- per_ha   * AF2_area  + per_tree * AF2_trees + onetime_private
    AF1_annual     <- annual_ha * AF1_area + annual_private * AF1_area
    AF2_annual     <- annual_ha * AF2_area + annual_private * AF2_area
    
    AF1_perc <- perc_incost   # tie whichever category decided for AF1
    AF2_perc <- perc_cons     # … and AF2
    any_perc <- as.numeric((AF1_perc + AF2_perc) > 0)
    
    data.frame(
      variable = c("AF1_total_annual_funding_c", "AF2_total_annual_funding_c",
                   "AF1_total_one_time_funding_c", "AF2_total_one_time_funding_c",
                   "AF2_percentage_values_c", "AF1_percentage_values_c",
                   "selected_percentage_c"),
      lower = c(AF1_annual, AF2_annual, AF1_one_time, AF2_one_time,
                AF2_perc, AF1_perc, any_perc),
      upper = c(AF1_annual, AF2_annual, AF1_one_time, AF2_one_time,
                AF2_perc, AF1_perc, any_perc),
      distribution = "const",
      stringsAsFactors = FALSE
    )
  })
  
  
  ## Dynamic expertise-filter module ----
  # helper that sanitises category names into safe IDs
  sanitize <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  
  # all categories across every sheet
  categories <- reactive({
    cats <- unique(unlist(lapply(excelData(), function(df) df$Expertise)))
    cats <- cats[!is.na(cats) & cats != ""]
    trimws(unique(unlist(strsplit(cats, ";"))))
  })
  
  # filter bar UI
  output$category_filter_ui <- renderUI({
    if (length(categories()) == 0) return(NULL)
    tagList(
      lapply(categories(), function(cat){
        checkboxInput(
          paste0("cat_", sanitize_id(cat)), cat, value = FALSE)
      })
    )
  })
  
  
  # ## Dynamic Crop-selector module ----
  # #  all available crops for crop_rotations across every sheet
  # crop_rot <- reactive({
  #   crops <- unique(unlist(lapply(excelData(), \(df) df$Crop_rotation)))
  #   crops <- crops[!is.na(crops) & crops != "" &
  #                    tolower(crops) != "na"]
  #   trimws(unique(unlist(strsplit(crops, ";"))))
  # })
  # 
  # 
  # ### Crop rotation module ----
  # 
  # #### check-boxes ----
  # output$crop_rot_filter_ui <- renderUI({
  #   if (length(crop_rot()) == 0) return(NULL)
  #   tagList(
  #     lapply(crop_rot(), function(crop){
  #       checkboxInput(
  #         paste0("crop_", sanitize_id(crop)), crop, value = FALSE)
  #     })
  #   )
  # })
  # 
  # # output$crop_filter_ui <- renderUI({
  # #   tagList(lapply(crop_rot(), function(crop) {
  # #     checkboxInput(paste0("crop_", crop), crop)
  # #   }))
  # # })
  # 
  # # helper: which crops are ticked?
  # selected_crops <- reactive({
  #   keep <- vapply(
  #     crop_rot(),
  #     \(crop) isTRUE(input[[paste0("crop_", crop)]]),
  #     logical(1)
  #   )
  #   crop_rot()[keep]
  # })
  # 
  # # store the two buckets
  # rv <- reactiveValues(
  #   avail = character(0),   # left bucket (clonable copies)
  #   rot   = character(0)    # right bucket (user’s rotation)
  # )
  # 
  # # keep 'avail' in sync with current ticks
  # observe({
  #   rv$avail <- selected_crops()
  # })
  # 
  # #### Drag-and-drop builder ----
  # output$rotation_builder_ui <- renderUI({
  #   req(length(rv$avail) > 0)          # show only after at least one crop chosen
  #   
  #   sortable::bucket_list(
  #     header      = "Drag crops to the right, duplicate as needed, reorder freely",
  #     orientation = "horizontal",
  #     
  #     sortable::add_rank_list(
  #       input_id = "avail_bucket",
  #       text     = "Available crops",
  #       labels   = rv$avail,
  #       options  = sortable::sortable_options(
  #         group = list(name = "crops", pull = "clone", put = FALSE)
  #       )
  #     ),
  #     
  #     sortable::add_rank_list(
  #       input_id = "rot_bucket",
  #       text     = "Your rotation",
  #       labels   = rv$rot,
  #       options  = sortable::sortable_options(
  #         group = list(name = "crops", pull = FALSE, put = TRUE),
  #         multiDrag = TRUE,
  #         swap      = TRUE
  #       )
  #     )
  #   )
  # })
  # 
  # # keep rv$rot in sync with what user dragged
  # observeEvent(input$rot_bucket, {
  #   rv$rot <- input$rot_bucket
  # })
  # 
  # # expose the final vector
  # output$rotation_vec <- renderPrint(rv$rot)
  
  
  ## Dynamic UI inputs ----
  
  # read in input xlsx file
  excelData <- reactive({
    sheet_number <- seq_along(sheet_names)+1
    all_sheets <- lapply(sheet_number, function(sht) {
      readxl::read_excel(file_path_vars, sheet = sht,
                         col_types = c("text", "numeric", "numeric", "text", "text", "text", "text", "guess", "guess", "text", "text")
      )
    })
    names(all_sheets) <- sheet_names
    all_sheets
  })
  
  
  # util: turns a category vector into a JS condition 
  ### render but hide unchecked expertise categories - default show-all ----
  panel_condition <- function(cat_vec) {
    cat_vec <- trimws(cat_vec)
    cat_vec <- cat_vec[cat_vec != "" & !is.na(cat_vec)]
    if (length(cat_vec) == 0) return("true")
    
    cat_ids <- sprintf("input['cat_%s']", sanitize_id(cat_vec))
    
    cat_show_all <- paste0(
      "Object.keys(input).filter(k => k.startsWith('cat_')).",
      "every(k => input[k] === false)"
    )
    
    sprintf("(%s) || (%s)",            # show when *no* cat box ticked
            cat_show_all,              #…or any matching cat ticked
            paste(cat_ids, collapse = ' || '))
  }
  
  
  output$dynamic_element_ui <- renderUI({
    
    data_list   <- excelData()
    sheet_names <- names(data_list)
    
    # build one accordion panel per sheet
    # the elements are generated via the external function create_ui_element()
    panels <- lapply(seq_along(data_list), function(j) {
      
      sheet <- data_list[[j]]
      
      cats  <- unique(trimws(unlist(strsplit(sheet$Expertise %||% "", ";|,"))))
      cats  <- cats[cats != ""]
      
      ui_elems <- lapply(seq_len(nrow(sheet)), function(i) {
        create_ui_element(sheet[i, ])
      })
      
      conditionalPanel(
        condition = panel_condition(cats),   # hide panel is empty
        accordion_panel(
          title = sheet_names[j],
          icon  = icon(sheet_icons[[ sheet_names[j] ]] %||% "circle-dot"),
          tagList(ui_elems)
        )
      )
    })
    
    tagList(panels)   # render the list
  })
  
  
  ## Save, Load and Delete module
  all_inputs <- reactive({
    names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))]
  })
  
  current_input_table <- reactive({
    variables <- all_inputs()
    # # source("functions/Walnut_grain_veg_tub_mcsim-only.R", local = T)
    # 
    # message("Accessing user input estimates from the interface...")
    # 
    # # 1. Gather current widget values
    # exclude_inputs <- c("collapseSidebar", "save", "load", "delete",
    #                     "confirm_delete", "admin_selected_user",
    #                     "project_name", "version_select", "delete_version_select")
    # 
    # # variables <- setdiff(
    #   names(input)[grepl("(_c$|_p$|_t$|_n$|_cond$)", names(input))],
    #   exclude_inputs
    # )
    
    lower_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[1])
    })
    upper_values <- sapply(variables, function(v) {
      val <- input[[v]]
      if (length(val) == 1) as.numeric(val) else as.numeric(val[2])
    })
    
    # 2. Re-read Excel (keeps original bounds & distributions)
    all_sheets <- excelData()            # list of data-frames
    input_file <- bind_rows(all_sheets)  # one big table
    
    # Overwrite lower/upper with current UI inputs
    input_file <- input_file %>%
      left_join(
        tibble(variable = variables,
               lower    = lower_values,
               upper    = upper_values),
        by = "variable",
        suffix = c("", ".new")
      ) %>%
      mutate(
        lower = coalesce(lower.new, lower),
        upper = coalesce(upper.new, upper)
      ) %>%
      select(-ends_with(".new"))
    
    # 3. Append funding scalars
    fund_df <- funding_variables()
    fund_df$lower[is.na(fund_df$lower)] <- 0
    fund_df$upper[is.na(fund_df$upper)] <- 0
    
    input_file <- bind_rows(
      input_file %>% filter(!variable %in% fund_df$variable),
      fund_df
    )
    
    # # 4. Save UI snapshot (optional)
    # saveRDS(list(sheet_names, input_file), "data/Walnut_grain_veg_tub_ui_updated.RDS")
    
    # 5. clean-up: keep only numeric rows
    input_file <- input_file %>%
      filter(
        !is.na(lower), !is.na(upper),
        is.finite(lower), is.finite(upper)
      )
    
    # write.csv(input_file,"data/input_table.csv",row.names = F)
    
    input_file
  })
  
  ## Save/Load functionality ----
  # saveLoadServer("savemod", current_input_table)
  get_base_dir <- function() {
    if (Sys.info()[["sysname"]] == "Windows")
      "user-states/Germany2"
    else
      "/srv/shiny-app-data/user-states/Germany2"
  }
  
  get_user_dir <- function() {
    uid <- session$user
    safe_uid <- if (is.null(uid) || uid == "") "anon"
    else gsub("[^A-Za-z0-9_.-]", "_", uid)
    dir <- file.path(get_base_dir(), safe_uid)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
  }
  
  timestamp_name <- function(raw) {
    paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),"_",
           gsub("[^A-Za-z0-9_.-]", "_", raw), ".rds")
  }
  
  observeEvent(input$save_btn, {
    dir  <- get_user_dir()
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(files) >= 5) {
      showModal(modalDialog("You already have five versions. Delete one first.",
                            easyClose = TRUE))
      return()
    }
    
    req(nzchar(input$state_name))
    saveRDS(
      list(input_table = current_input_table(),
           raw_inputs  = reactiveValuesToList(input)),
      file.path(dir, timestamp_name(input$state_name))
    )
  })
  
  saved_files <- reactiveFileReader(
    2000, session, get_user_dir(),
    function(dir) sort(list.files(dir, pattern = "\\.rds$", full.names = TRUE),decreasing = T)
  )
  
  observe({
    updateSelectInput(session, "state_picker",
                      choices = basename(saved_files()))
  })
  
  observeEvent(input$load_btn, {
    req(input$state_picker)
    obj <- readRDS(file.path(get_user_dir(), input$state_picker))
    vals <- obj$raw_inputs
    
    restore_one <- function(id, val) {
      if (is.null(val)) return()
      switch(class(val)[1],
             numeric   = updateNumericInput(session, id, value = val),
             integer   = updateNumericInput(session, id, value = val),
             character = updateTextInput   (session, id, value = val),
             logical   = updateCheckboxInput(session, id, value = val),
             factor    = updateSelectInput (session, id, selected = as.character(val)),
             # length-2 numeric == slider
             { if (is.numeric(val) && length(val) == 2)
               updateSliderInput(session, id, value = val) }
      )
    }
    
    # ordinary widgets
    lapply(names(vals), \(id) try(restore_one(id, vals[[id]]), silent = TRUE))
    
    # funding module widgets  (country + state first, the rest after rebuild)
    ## doesn't work cleanly yet - load button needs to be pressed twice
    ns <- NS("funding")   # helper to prepend "funding-"
    
    # (a) push country and state immediately 
    try(updateSelectInput(session, ns("country"),
                          selected = vals[[ns("country")]]), silent = TRUE)
    try(updateSelectInput(session, ns("state"),
                          selected = vals[[ns("state")]]),   silent = TRUE)
    
    # (b) *once* the state really is set, restore the rest
    observeEvent(input[[ns("state")]], {
      if (!identical(input[[ns("state")]], vals[[ns("state")]])) return()
      
      try(updateSelectInput(session, ns("one_schemes"),
                            selected = vals[[ns("one_schemes")]]), silent = TRUE)
      try(updateSelectInput(session, ns("annual_schemes"),
                            selected = vals[[ns("annual_schemes")]]), silent = TRUE)
      try(updateNumericInput(session, ns("onetime_private"),
                             value = vals[[ns("onetime_private")]]),  silent = TRUE)
      try(updateNumericInput(session, ns("annual_private"),
                             value = vals[[ns("annual_private")]]),   silent = TRUE)
    }, once = TRUE, ignoreInit = FALSE)
  })
  
  observeEvent(input$delete_btn, {
    req(input$state_picker)
    unlink(file.path(get_user_dir(), input$state_picker))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("current_input_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(current_input_table(), file)
  )
  
  
  
  ## Monte Carlo Simulation ----
  mcSimulation_results <- eventReactive(input$run_simulation, {
    # sadly this still needs to be assigned globally at the moment - otherwise the Model function wont see the vector.
    # functionSyntax = plainNames assigns the model to another variable with its own environment (e) - this environment only holds the input table and the model (does not see rot_vec)
    # assigning to globalEnv will assign it to an environment shared by up to 20 people. people could overwrite their crop rotation vector.
    # rot_vec <- rv$rot
    # assign("crop_rotation", rot_vec, envir = .GlobalEnv)
    
    # model_function <- function(x) {
    #   list2env(as.list(x), envir = environment())
    #   crop_rotation <- rot_vec
    #   walnut_local <- Walnut_grain_veg_tub
    #   environment(walnut_local) <- environment()
    #   walnut_local()
    # }
    
    # walnut_local <- Walnut_grain_veg_tub
    # environment(walnut_local) <- list2env(list(crop_rotation = rot_vec),
    #                                       parent = environment(Walnut_grain_veg_tub))
    
    input_file <- current_input_table()
    
    # 6. Run Monte-Carlo
    decisionSupport::mcSimulation(
      estimate          = decisionSupport::as.estimate(input_file),
      model_function    = AF_benefit,
      numberOfModelRuns = input$num_simulations_c,
      functionSyntax    = "plainNames"
      # ,model_function    = model_function
      # ,model_function    = walnut_local
      # ,functionSyntax    = "data.frameNames"
      # ,crop_rotation     = rot_vec
    )
    
  })
  
  ## Generating plots ----
  # helper to add title subtile caption etc
  add_meta <- function(p, title, subtitle = NULL, caption = NULL,
                       legend = "bottom") {
    
    p +
      labs(title = title, subtitle = subtitle, caption = caption) +
      theme(
        plot.title = element_textbox_simple(
          size   = 24,
          face   = "bold",
          width  = unit(1, "npc"),  # full plot width
          halign = 0.5,              # centered
          margin = margin(b = 6)
        ),
        plot.subtitle = element_textbox_simple(
          size   = 18,
          width  = unit(1, "npc"),
          halign = 0.5,
          margin = margin(b = 10)
        ),
        plot.caption  = element_textbox_simple(
          size   = 13,
          width  = unit(1, "npc"),
          halign = 0,              # left-aligned
          margin = margin(t = 6),
          hjust = 0
        ),
        axis.title      = element_text(size = 14),
        legend.text     = element_text(size = 11, hjust = 0.5),
        legend.position = legend
      )  }
  
  # download helper
  make_download <- function(id, plot_obj, filename, width = 13, height = 5, dpi = 300) {
    output[[id]] <- downloadHandler(
      filename = function() filename,
      content  = function(file) {
        # device is inferred from file extension; here "png"
        ggsave(file, plot_obj, width = width, height = height, dpi = dpi)
      }
    )
  }
  
  
  
  observeEvent(mcSimulation_results(), {
    mc_data <- mcSimulation_results()
    
    plot1 <-
      decisionSupport::plot_distributions(mcSimulation_object = mc_data,
                                          vars = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                                          method = "smooth_simple_overlay",
                                          old_names = c("NPV_Treeless_System", "NPV_Agroforestry_System"),
                                          new_names = c("Cultivation without trees", "Agroforestry intervention"),
                                          x_axis_name = "NPV (€)",
                                          y_axis_name = "Probability") |>
      add_meta(
        title    = "Figure 1. Probabilistic distributions of Net Present Value",
        subtitle = "Agroforestry intervention (green) vs. conventional farming (blue)",
        caption  = "Figure 1 shows the Net Present Value (NPV) distributions of 
        the decision to establish the alley cropping system (green) and the 
        decision to continue farming without planting trees (blue) for the timescope
        of interest. The x-axis displays NPV values (i.e.: the sum of discounted
        annual cash flows). The y-axis displays the probability of each NPV amount
        to occur (i.e.: higer y-values indicate higher probability"
      )
    
    plot2 <- decisionSupport::plot_distributions(
      mc_data, "NPVtrade_off",
      method     = "smooth_simple_overlay",
      old_names  = "NPV_decision_AF1",
      new_names  = "Agroforestry – Treeless",
      x_axis_name= "NPV (€)",
      y_axis_name= "Probability") |>
      add_meta(
        title    = "Figure 2. Distribution of the *incremental* NPV",
        subtitle = "Difference between agroforestry and treeless farming under identical scenarios",
        caption  = "Figure 2 shows the Net Present Value (NPV) distributions of the decision to establish the alley cropping system (green) 
                as compared to the decision to continue farming without planting trees for the timescope of interest (i.e.: NPV agroforestry - NPV treeless under identical real-world scenarios).
                The x-axis displays NPV values (i.e.: the sum of discounted annual cash flows).
                The y-axis displays the probability of each NPV amount to occur (i.e.: higer y-values indicate higher probability"
        , legend = "none")
    
    # plot3 <- decisionSupport::plot_distributions(
    #   mc_data,
    #   vars      = c("planing_costs","planting_cost",
    #                 "strips_maintenance","application_cost"),
    #   method    = "boxplot",
    #   old_names = c("planing_costs","planting_cost",
    #                 "strips_maintenance","application_cost"),
    #   new_names = c("Planning & design","Planting",
    #                 "Maintenance","Bureaucratic work"),
    #   x_axis_name = "Sum over entire time-horizon (€)",
    #   y_axis_name = "Cost class") |>
    #   add_meta(
    #     title    = "Figure 3. Cost structure of the agroforestry project",
    #     caption  = 'Figure 3 shows the costs (expressed in €) associated with each cost category of the decision to develop an alley-cropping system.
    #              The middle line of each box shows the median of its probability distribution.
    #              The extremes of each boxes show the first and third quartile of the probability distribution. 
    #              The extremes of the lines show the 5th and 95th percentile of the probability distribution. Dots are outliers beyond these percentiles.
    #              Please note that the "Bureaucratic work" and the "Maintenance" boxes show the sum of the annual costs of every year over the timescope period, whereas the "Planning and design" and "Planting" boxes occur only in one year.'
    #   )
    
    plot4 <- decisionSupport::plot_cashflow(
      mc_data, "AFcashflow",
      x_axis_name = "",
      y_axis_name = "Annual cash-flow from Agroforestry (€)",
      facet_labels = "") |>
      add_meta(
        title   = "Figure 4. Annual cash-flow of the agroforestry intervention", 
        caption = 'Figure 4 shows the annual balance (expressed in €) of alley-cropping in the intervened field.'
      )
    
    plot5 <- decisionSupport::plot_cashflow(
      mc_data, "AFcumcashflow",
      x_axis_name = "",
      y_axis_name = "Cumulative cash-flow from Agroforestry (€)",
      facet_labels = "") |>
      add_meta(
        title   = "Figure 5. Cumulative cash-flow of the agroforestry intervention", 
        caption = "Figure 5 shows the cumulative annual balance (expressed in €) of alley-cropping in the intervened field."
      )
    
    # plot6 <- decisionSupport::plot_cashflow(
    #   mc_data, "Cashflow_AF1_decision",
    #   x_axis_name = "",
    #   y_axis_name = "Annual cash-flow (€)",
    #   facet_labels = "") |>
    #   add_meta(
    #     title   = "Figure 6. Incremental annual cash-flow",
    #     subtitle= "Agroforestry minus baseline farming",
    #     caption = "Figure 6 shows the difference (expressed in €) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.")
    # 
    # plot7 <- decisionSupport::plot_cashflow(
    #   mc_data, "Cum_Cashflow_AF1_decision",
    #   x_axis_name = "",
    #   y_axis_name = "Cumulative cash-flow (€)",
    #   facet_labels = "") |>
    #   add_meta(
    #     title   = "Figure 7. Incremental cumulative cash-flow",
    #     subtitle= "Agroforestry minus baseline farming",
    #     caption = 'Figure 7 shows the cumulative difference (expressed in €) between the annual balance of alley-cropping and continue farming without planting trees under identical real-world scenarios.')
    # 
    
    # Send plots to UI
    output$plot1_ui <- renderPlot({ plot1 })
    make_download("download_plot1", plot1, "Figure1_NPV.png")
    output$plot1_dl_ui <- renderUI({
      downloadButton("download_plot1", "Download Figure 1")
    })
    
    output$plot2_ui <- renderPlot({ plot2 })
    make_download("download_plot2", plot2, "Figure2_Incremental_NPV.png")
    output$plot2_dl_ui <- renderUI({
      downloadButton("download_plot2", "Download Figure 2")
    })
    
    # output$plot3_ui <- renderPlot({ plot3 })
    # make_download("download_plot3", plot3, "Figure3_Cost_Structure.png")
    # output$plot3_dl_ui <- renderUI({
    #   downloadButton("download_plot3", "Download Figure 3")
    # })
    
    output$plot4_ui <- renderPlot({ plot4 })
    make_download("download_plot4", plot4, "Figure4_Annual_Cashflow.png")
    output$plot4_dl_ui <- renderUI({
      downloadButton("download_plot4", "Download Figure 4")
    })
    
    output$plot5_ui <- renderPlot({ plot5 })
    make_download("download_plot5", plot5, "Figure5_Cumulative_Cashflow.png")
    output$plot5_dl_ui <- renderUI({
      downloadButton("download_plot5", "Download Figure 5")
    })
    
    # output$plot6_ui <- renderPlot({ plot6 })
    # make_download("download_plot6", plot6, "Figure6_Incremental_Annual_CF.png")
    # output$plot6_dl_ui <- renderUI({
    #   downloadButton("download_plot6", "Download Figure 6")
    # })
    # 
    # output$plot7_ui <- renderPlot({ plot7 })
    # make_download("download_plot7", plot7, "Figure7_Incremental_Cumulative_CF.png")
    # output$plot7_dl_ui <- renderUI({
    #   downloadButton("download_plot7", "Download Figure 7")
    # })
    

    # Ask user whether to run EVPI (takes time!)
    showModal(modalDialog(
      title = "Run EVPI analysis?",
      "Do you want to assess the Expected Value of Perfect Information (EVPI)?
      This step may take a while, but you can explore the other graphs while the EVPI is processed.
      The EVPI graph will appear at the bottom of the page, below the last of the other graphs.",
      footer = tagList(
        modalButton("No"),
        actionButton("confirm_evpi", "Yes, run EVPI")
      )
    ))
    
    # Handle user confirmation to run EVPI
    observeEvent(input$confirm_evpi, {
      
      removeModal()  # remove popup
      
      # Try running EVPI only if it can return meaningful values
      tryCatch({
        evpi_input <- as.data.frame(cbind(
          mc_data$x,
          NPV_decision_AF1 = mc_data$y$NPVtrade_off
        ))
        
        evpi_result <- decisionSupport::multi_EVPI(evpi_input,  "NPVtrade_off")
        
        # saveRDS(evpi_input, "evpi_input_test.rds")
        # evpi_input <- readRDS("evpi_input_test.rds")
        
        # saveRDS(evpi_result, "evpi_result_test.rds")
        # evpi_result <- readRDS("evpi_result_test.rds")
        
        var_lookup <- bind_rows(excelData()) %>%
          filter(!is.na(variable), !is.na(name)) %>%
          distinct(variable, name) %>%
          deframe()
        
        plot8 <- plot_evpi(evpi_result, decision_vars = "NPVtrade_off",
                           new_names = "") +
          scale_y_discrete(labels = var_lookup)
        
        plot8 <- plot8 |>
          add_meta(title = "Figure 8. EVPI for Each Variable",
                   subtitle = "Maximum amount worth paying for perfect information on each variable."
                   )
        
        output$plot8_ui <- renderPlot({ plot8 })
        
        make_download("download_plot8", plot8, "Figure8_EVPI.png")
        
        output$plot8_dl_ui <- renderUI({
          downloadButton("download_plot8", "Download Figure 8")
        })
        
      }, error = function(e) {
        warning("EVPI plot skipped due to error: ", e$message)
        output$plot8_ui <- renderPlot({
          plot.new()
          text(0.5, 0.5, "There are no variables with a positive EVPI.\nGetting better information will \nnot reduce the level of uncertainty of the decision.", cex = 1.2)
        })
      })
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
