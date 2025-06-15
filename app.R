#
# Effective Stress Calculator - Shiny application
# Version 0.1.0
#

library(shiny)
library(shinyMatrix)
library(shinyFeedback)
library(bslib)
library(shinyjs)

# Helper functions for stress calculations
effective_stress <- function(sigma, u) {
  #' Calculates the effective stress 
  #'
  #' @param sigma Total stress [kPa]
  #' @param u Pore water pressure [kPa]
  #' @return Effective stress [kPa]
  
  return(sigma - u)
}

total_stress_d <- function(soil_table, depth) {
  #' Calculates the total stress at a specified depth
  #'
  #' @param soil_table Data frame with soil properties
  #' @param depth Depth to calculate stress at [m]
  #' @return Total stress [kPa]
  
  total <- 0.0
  
  for (ii in 1:nrow(soil_table)) {
    if (soil_table$Bottom.depth[ii] <= depth) {
      total <- total + (soil_table$Bottom.depth[ii] - soil_table$Top.depth[ii]) * soil_table$Soil.Unit.Weight[ii]
    } else if (soil_table$Top.depth[ii] < depth) {
      total <- total + (depth - soil_table$Top.depth[ii]) * soil_table$Soil.Unit.Weight[ii]
    }
  }
  
  return(total)
}

pore_water_pressure <- function(GWL, depths) {
  #' Calculates the pore water pressure at a specified depth
  #'
  #' @param GWL Groundwater level [m below ground level]
  #' @param depths Depth to calculate pressure at [m]
  #' @return Pore water pressure [kPa]
  
  if (depths < GWL) {
    u <- 0
  } else {
    u <- (depths - GWL) * 9.81
  }
  
  return(u)
}

mattodt_convert <- function(matrix_input) {
  #' Converts matrix input to data frame for soil layers
  #'
  #' @param matrix_input Matrix of soil properties
  #' @return Data frame with soil properties
  
  m <- matrix_input
  m2 <- c(0, m[1:nrow(m)-1, 2])
  df1 <- data.frame(
    "Unit" = m[,1], 
    "Top.depth" = m2, 
    "Bottom.depth" = m[,2], 
    "Soil.Unit.Weight" = m[,3]
  )
  soil.data <- na.omit(df1)
  return(soil.data)
}

# Initial matrix for soil data
initial_matrix <- matrix(
  c(1, 2, 18,
    2, 5, 19), 
  nrow = 2, 
  ncol = 3, 
  byrow = TRUE
)
colnames(initial_matrix) <- c("Unit", "Bottom Depth (mbgl)", "Unit Weight (kN/m3)")

# Define UI
ui <- page_sidebar(
  title = "Effective Stress Calculator",
  theme = bs_theme(bootswatch = "flatly"),
  
  # Add shinyjs
  useShinyjs(),
  
  tags$head(
    tags$script(src = "matrixInput.js"),
    tags$style(HTML("
            /* Make the soil layer card larger */
            .matrix-input-container {
                min-height: 250px;
            }
            
            /* Style for the welcome modal */
            #welcome-modal .modal-dialog {
                max-width: 600px;
            }
            
            #welcome-modal .modal-header {
                background-color: #4582EC;
                color: white;
            }
            
            #welcome-modal .modal-body {
                padding: 20px;
            }
            
            #welcome-modal .feature-list {
                margin-top: 15px;
                margin-bottom: 15px;
            }
            
            #welcome-modal .feature-item {
                margin-bottom: 8px;
                display: flex;
                align-items: center;
            }
            
            #welcome-modal .feature-icon {
                margin-right: 10px;
                color: #4582EC;
            }
        "))
  ),
  
  sidebar = sidebar(
    title = "Input Parameters",
    numericInput("gwl", "Ground Water Level (mbgl):", 
                 value = 1, min = 0, step = 0.1),
    numericInput("depth", "Calculation Depth (m):", 
                 value = 3, min = 0, step = 0.1)
  ),
  
  card(
    id = "soil_layer_card",
    card_header("Soil Layer Properties"),
    card_body(
      style = "min-height: 300px;",
      p("Enter soil layers from top to bottom. Each row represents one soil layer. (Bulk copy and pasting / clipboard available on certain browsers)"),
      div(
        class = "matrix-input-container",
        matrixInput(
          inputId = "soildata",
          value = initial_matrix,
          class = "numeric",
          paste = TRUE,
          copy = TRUE,
          cols = list(
            names = TRUE,
            editableNames = FALSE
          ),
          rows = list(
            names = FALSE, 
            extend = TRUE, 
            delta = 1
          )
        )
      )
    )
  ),
  
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("Soil Profile"),
      tableOutput("soil_table")
    ),
    
    card(
      card_header("Calculation Results"),
      useShinyFeedback(),
      card_body(
        value_box(
          title = "Total Stress",
          value = textOutput("total_stress"),
          showcase = bsicons::bs_icon("arrow-down-square-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Pore Water Pressure",
          value = textOutput("pore_pressure"),
          showcase = bsicons::bs_icon("water"),
          theme = "info"
        ),
        value_box(
          title = "Effective Stress",
          value = textOutput("effective_stress_result"),
          showcase = bsicons::bs_icon("calculator-fill"),
          theme = "success"
        )
      )
    )
  ),
  
  # Added back the About and How to Use cards
  card(
    card_header("About"),
    card_body(
      layout_column_wrap(
        width = 1/2,
        card_body(
          h4("Effective Stress Calculator"),
          p("This tool calculates effective stress at depth based on soil properties and groundwater level."),
          p("Effective stress is a key parameter in geotechnical engineering that represents the stress carried by the soil skeleton."),
          p("Effective Stress = Total Stress - Pore Water Pressure")
        ),
        card_body(
          h4("How to Use"),
          p("1. Enter soil layers with unit number, bottom depth, and unit weight. More rows will appear the rows are filled."),
          p("2. Set the groundwater level (GWL)"),
          p("3. Specify the depth for calculation"),
          p("4. Results will update automatically in the Calculation Results section")
        )
      )
    ),
    footer = tags$div(
      style = "text-align: center; color: #666; font-size: 0.8rem;",
      "Version 1.3 | Developed with Shiny for R | © 2023"
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Show welcome modal on startup
  showModal(modalDialog(
    title = "Welcome to the Effective Stress Calculator",
    HTML("
            <div>
                <p>This application helps geotechnical engineers calculate effective stress at different depths based on soil properties and groundwater conditions.</p>
                
                <h5>Key Features:</h5>
                <p> version 0.1.0 <p>
                <div class='feature-list'>
                    <div class='feature-item'>
                        <span class='feature-icon'><i class='bi bi-layers-fill'></i></span>
                        <span>Define multiple soil layers with different properties</span>
                    </div>
                    <div class='feature-item'>
                        <span class='feature-icon'><i class='bi bi-water'></i></span>
                        <span>Specify groundwater level for pore water pressure calculations</span>
                    </div>
                    <div class='feature-item'>
                        <span class='feature-icon'><i class='bi bi-calculator-fill'></i></span>
                        <span>Automatic calculation of total stress, pore pressure, and effective stress</span>
                    </div>
                    <div class='feature-item'>
                        <span class='feature-icon'><i class='bi bi-table'></i></span>
                        <span>Visualize soil profile in tabular format</span>
                    </div>
                </div>
                
                <p>To get started, enter your soil layer properties in the table and set the groundwater level and calculation depth.</p>
                
                <div class='disclaimer' style='margin-top: 20px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #dc3545; font-size: 0.9rem;'>
                    <strong>Disclaimer:</strong> This application is provided for demonstration purposes only. 
                    The calculations are based on simplified models and should not be relied upon for 
                    critical engineering decisions without verification. No warranty or guarantee is provided 
                    regarding the accuracy of the results. Users should apply appropriate engineering judgment 
                    when interpreting and using the outputs.
                </div>
            </div>
        "),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Get Started")
    ),
    size = "m",
    id = "welcome-modal"
  ))
  
  # Rest of the server code remains unchanged
  soil_data <- reactive({
    mattodt_convert(input$soildata)
  })
  
  # Calculate stresses
  total_stress <- reactive({
    total_stress_d(soil_data(), input$depth)
  })
  
  pore_pressure <- reactive({
    pore_water_pressure(input$gwl, input$depth)
  })
  
  eff_stress <- reactive({
    effective_stress(total_stress(), pore_pressure())
  })
  
  # Render soil data table
  output$soil_table <- renderTable({
    soil_data()
  }, rownames = FALSE)
  
  # Render stress results
  output$total_stress <- renderText({
    paste(round(total_stress(), 2), "kPa")
  })
  
  output$pore_pressure <- renderText({
    paste(round(pore_pressure(), 2), "kPa")
  })
  
  output$effective_stress_result <- renderText({
    paste(round(eff_stress(), 2), "kPa")
  })
  
  # Show warning if depth exceeds maximum soil depth
  observeEvent(list(input$depth, input$soildata), {
    max_depth <- max(soil_data()$Bottom.depth)
    feedbackDanger(
      "depth", 
      max_depth < input$depth,
      paste("Warning: Calculation depth exceeds maximum soil depth (", max_depth, "m)!")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)