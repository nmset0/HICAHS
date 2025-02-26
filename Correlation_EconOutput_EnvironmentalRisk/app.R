library(shiny)
library(dplyr)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Environmental Risk Correlation Analysis"),

  tags$head(
    tags$style(HTML("
      .selectize-input {
        width: 100% !important;
      }
      .selectize-dropdown {
        width: auto !important;
        max-width: none !important;
      }
      .selectize-dropdown-content {
        max-width: none !important;
        width: auto !important;
        white-space: nowrap;
        overflow-x: auto;
      }
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset:",
                  choices = NULL),  # Choices will be populated in server

      selectInput("response_var", "Select Response Variable:",
                  choices = NULL),  # Choices will be populated in server

      checkboxInput("sort_corr", "Sort by correlation magnitude", TRUE)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Correlation Results",
                 br(),
                 dataTableOutput("corr_table")
        ),
        tabPanel("Dataset Summary",
                 verbatimTextOutput("data_summary"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # List of response variables
  response_variables <- c(
    "commodity_totals_sales_measured_in_$",
    "commodity_totals_sales_measured_in_$_operation",
    "crop_totals_sales_measured_in_$",
    "animal_totals_incl_products_sales_measured_in_$",
    "commodity_totals_operations_with_sales",
    "farm_sales_(less_than_2500_$)",
    "farm_sales_(2500_to_4999_$)",
    "farm_sales_(5000_to_9999_$)",
    "farm_sales_(10000_to_24999_$)",
    "farm_sales_(25000_to_49999_$)",
    "farm_sales_(50000_to_99999_$)",
    "farm_sales_(100000_or_more_$)",
    "income_farmrelated_operations_with_receipts",
    "income_farmrelated_receipts_measured_in_$",
    "income_net_cash_farm_of_operations_operations_with_net_income",
    "income_net_cash_farm_of_operations_net_income_measured_in_$",
    "income_net_cash_farm_of_operations_net_income_measured_in_$_operation"
  )

  # Load the preprocessed data
  output_environmentalRisk <- readRDS("output_EnvironmentalRisk.rds")

  # Define environmental risk variables
  environmental_risk_vars <- list(
    "All Environmental Risk Factors" = names(output_environmentalRisk),
    "Drought Only" = grep("drought", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Wildfire Only" = grep("wildfire", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Cold Wave Only" = grep("Cold.Wave", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Tornado Only" = grep("tornado", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Ice Storm Only" = grep("ice.storm", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Winter Weather Only" = grep("winter.weather", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Strong Wind Only" = grep("strong.wind", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Hail Only" = grep("hail", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Heat Wave Only" = grep("heat.wave", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Landslide Only" = grep("Landslide", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE),
    "Lightning Only" = grep("lightning", names(output_environmentalRisk), value = TRUE, ignore.case = TRUE)
  )

  # Update dataset selector choices
  updateSelectInput(session, "dataset", choices = names(environmental_risk_vars))

  # Update response variable selector choices
  updateSelectInput(session, "response_var", choices = response_variables)

  # Get the current dataset
  current_dataset <- reactive({
    req(input$dataset)
    selected_vars <- c(response_variables, environmental_risk_vars[[input$dataset]])
    output_environmentalRisk[, selected_vars, drop = FALSE]
  })

  # Calculate correlations
  correlations <- reactive({
    req(input$response_var)
    df <- current_dataset()
    response_var <- input$response_var

    if (!response_var %in% colnames(df)) {
      return(data.frame(
        Predictor = "Response variable not found in this dataset",
        Correlation = NA
      ))
    }

    all_predictors <- setdiff(colnames(df), response_variables)
    corr_data <- data.frame(
      Predictor = character(),
      Correlation = numeric()
    )

    for (predictor in all_predictors) {
      if (is.numeric(df[[predictor]]) && is.numeric(df[[response_var]]) &&
          var(df[[predictor]], na.rm = TRUE) > 0 && var(df[[response_var]], na.rm = TRUE) > 0) {
        cor_val <- cor(df[[response_var]], df[[predictor]], use = "pairwise.complete.obs")
        corr_data <- rbind(corr_data, data.frame(
          Predictor = predictor,
          Correlation = round(cor_val, 3)
        ))
      }
    }

    if (input$sort_corr) {
      corr_data <- corr_data[order(abs(corr_data$Correlation), decreasing = TRUE), ]
    }

    corr_data
  })

  # Render correlation table
  output$corr_table <- renderDataTable({
    req(correlations())
    datatable(correlations(), options = list(pageLength = 25, autoWidth = TRUE), rownames = FALSE)
  })

  # Dataset summary
  output$data_summary <- renderPrint({
    req(current_dataset())
    df <- current_dataset()
    cat("Dataset:", input$dataset, "\n\n")
    cat("Dimensions:", nrow(df), "rows and", ncol(df), "columns\n\n")
    cat("Response Variables:", sum(response_variables %in% colnames(df)), "\n")
    cat("Predictor Variables:", ncol(df) - sum(response_variables %in% colnames(df)), "\n\n")
    if (input$response_var %in% colnames(df)) {
      cat("Summary for selected response variable:", input$response_var, "\n")
      print(summary(df[[input$response_var]]))
    } else {
      cat("Selected response variable not found in this dataset.\n")
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)