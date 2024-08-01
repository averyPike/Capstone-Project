library(shiny)
library(tidyverse)
library(viridis)
library(shinydashboard)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "PCA Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("PCA Analysis", tabName = "pca_analysis", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "pca_analysis",
              fluidRow(
                box(
                  title = "PCA Analysis",
                  width = 12,
                  selectInput("contexts", "Select Contexts:", choices = NULL, multiple = TRUE, selectize = TRUE),
                  plotlyOutput("pcaPlot"),
                  div(id = "pcaTable", tableOutput("pcaLoadings"))
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load preprocessed data with specified column types
  cleaned_data <- read_csv("/CleanedData.csv", show_col_types = FALSE)
  non_numeric_cols <- read_csv("/NonNumericCols.csv", show_col_types = FALSE)
  standardized_data <- read_csv("/StandardizedData.csv", show_col_types = FALSE)
  
  # Perform PCA
  pca_result <- prcomp(standardized_data, scale. = TRUE)
  pca_components <- as.data.frame(predict(pca_result))
  pca_components_df <- cbind(non_numeric_cols, pca_components)
  
  # Calculate the proportion of variance explained
  variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  # Update context and sample dropdown choices, ordered from smallest to largest
  observe({
    context_choices <- sort(unique(pca_components_df$Context))
    sample_choices <- sort(unique(non_numeric_cols$Sample))
    element_choices <- colnames(standardized_data)
    max_impact_element <- element_choices[which.max(abs(pca_result$rotation[,1]))]
    updateSelectInput(session, "contexts", choices = context_choices, selected = NULL)
    updateSelectInput(session, "contextSelect", choices = sample_choices, selected = NULL)
    updateSelectInput(session, "elements", choices = element_choices, selected = max_impact_element)
  })
  
  # Render PCA plot with plotly for interactivity
  output$pcaPlot <- renderPlotly({
    selected_contexts <- input$contexts
    
    # Handle case where no contexts are selected
    if (length(selected_contexts) == 0) {
      selected_contexts <- unique(pca_components_df$Context) # Show all contexts if none are selected
    }
    
    # Generate a color palette for selected contexts
    color_palette <- viridis(length(selected_contexts))
    names(color_palette) <- selected_contexts
    
    # Create a color mapping for contexts
    color_mapping <- ifelse(pca_components_df$Context %in% selected_contexts, as.character(pca_components_df$Context), "Unselected")
    
    # Plot with dynamic colors based on selected contexts
    plot <- ggplot(pca_components_df, aes(x = PC1, y = PC2, text = paste("Sample:", Sample, "<br>Context:", Context, "<br>Square:", Square))) +
      geom_point(aes(color = factor(color_mapping), 
                     alpha = ifelse(Context %in% selected_contexts, 1, 0.3), 
                     size = ifelse(Context %in% selected_contexts, 3, 1)), 
                 show.legend = TRUE) +
      scale_color_manual(values = c(color_palette, "Unselected" = "grey")) +
      scale_alpha_identity() +
      scale_size_identity() +
      labs(
        x = paste("Principal Component 1 (", round(variance_explained[1] * 100), "%)", sep = ""),
        y = paste("Principal Component 2 (", round(variance_explained[2] * 100), "%)", sep = ""),
        title = "PCA on Cleaned Data"
      ) +
      guides(color = guide_legend(title = "Highlighted Contexts", override.aes = list(alpha = 1)),
             alpha = "none",
             size = "none") +
      theme_minimal() +
      theme(legend.text = element_text(face = "bold"))
    
    ggplotly(plot, tooltip = "text") %>%
      layout(clickmode = 'event+select') %>%
      config(displayModeBar = FALSE)  # Remove mode bar for cleaner UI
  })
  
  # Render PCA loadings table
  output$pcaLoadings <- renderTable({
    pca_loadings <- as.data.frame(pca_result$rotation) %>%
      select(PC1, PC2) %>%
      mutate(Element = rownames(pca_result$rotation)) %>%
      arrange(desc(abs(PC1)), desc(abs(PC2)))
    pca_loadings
  }, rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
