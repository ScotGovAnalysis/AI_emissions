
library(shiny)
library(shinyjs)
library(tidyverse)
library(shiny)
source("global.R")
ui <- fluidPage(
  
  h2("AI Environmental Impact Calculator"),
  
  numericInput("total_requests", "Total requests",
               value = 4000000, min = 0),
  
  hr(),
  h3("Message Length Distribution"),
  
  selectInput("auto_length", "Automatically compute:",
              choices = c("Long", "Medium", "Short"), selected = "Short"),
  
  numericInput("prop_long",   "Proportion Long",   value = 0.2, min = 0, max = 1),
  numericInput("prop_medium", "Proportion Medium", value = 0.4, min = 0, max = 1),
  
  h4("Proportion Short (auto-calculated):"),
  textOutput("prop_short_auto"),
  
  hr(),
  h3("Complexity Distribution Within Long Messages"),
  
  selectInput("auto_long_complexity", "Auto compute:",
              choices = c("Complex", "Medium", "Simple"), selected = "Simple"),
  
  numericInput("long_complex", "Complex", value = 0.7, min = 0, max = 1),
  numericInput("long_medium",  "Medium",  value = 0.3, min = 0, max = 1),
  
  h4("Simple (auto):"),
  textOutput("long_simple_auto"),
  
  hr(),
  h3("Complexity Distribution Within Medium Messages"),
  
  selectInput("auto_med_complexity", "Auto compute:",
              choices = c("Complex", "Medium", "Simple"), selected = "Simple"),
  
  numericInput("med_complex", "Complex", value = 0.2, min = 0, max = 1),
  numericInput("med_medium",  "Medium",  value = 0.6, min = 0, max = 1),
  
  h4("Simple (auto):"),
  textOutput("med_simple_auto"),
  
  hr(),
  h3("Complexity Distribution Within Short Messages"),
  
  selectInput("auto_short_complexity", "Auto compute:",
              choices = c("Complex", "Medium", "Simple"), selected = "Simple"),
  
  numericInput("short_complex", "Complex", value = 0, min = 0, max = 1),
  numericInput("short_medium",  "Medium",  value = 0.2, min = 0, max = 1),
  
  h4("Simple (auto):"),
  textOutput("short_simple_auto"),
  
  hr(),
  actionButton("run", "Run"),
  
  hr(),
  tableOutput("table")
)
server <- function(input, output, session) {
  
  compute_missing <- function(x, y) {
    return(1 - sum(c(x, y), na.rm = TRUE))
  }
  
  # ---- Auto-display computed values ----
  
  output$prop_short_auto <- renderText({
    if (input$auto_length == "Short") {
      return(compute_missing(input$prop_long, input$prop_medium))
    }
  })
  
  output$long_simple_auto <- renderText({
    if (input$auto_long_complexity == "Simple") {
      return(compute_missing(input$long_complex, input$long_medium))
    }
  })
  
  output$med_simple_auto <- renderText({
    if (input$auto_med_complexity == "Simple") {
      return(compute_missing(input$med_complex, input$med_medium))
    }
  })
  
  output$short_simple_auto <- renderText({
    if (input$auto_short_complexity == "Simple") {
      return(compute_missing(input$short_complex, input$short_medium))
    }
  })
  
  # ---- Compute final mixture ----
  final_proportions <- eventReactive(input$run, {
    
    # Length proportions
    long  <- input$prop_long
    med   <- input$prop_medium
    
    short <- compute_missing(long, med)
    
    # Complexity: long
    l_c <- input$long_complex
    l_m <- input$long_medium
    l_s <- compute_missing(l_c, l_m)
    
    # Complexity: medium
    m_c <- input$med_complex
    m_m <- input$med_medium
    m_s <- compute_missing(m_c, m_m)
    
    # Complexity: short
    s_c <- input$short_complex
    s_m <- input$short_medium
    s_s <- compute_missing(s_c, s_m)
   
    tibble::tibble(
      model = rep(c("GPT-5 (high)", "GPT-5 (medium)", "GPT-5 (low)"), times = 3),
    length = rep(c("Long", "Medium", "Short"), each = 3),
    proportion = c(
      long * l_c, long * l_m, long * l_s,
      med  * m_c, med  * m_m, med  * m_s,
      short * s_c, short * s_m, short * s_s
    )
    )
  })

dataset <- eventReactive(input$run, {

  dat %>%
    dplyr::left_join(final_proportions(), by = c("model", "length")) %>%
    dplyr::mutate(
      total = input$total_requests,
      energy = mean_combined_energy_wh * proportion * total,
      carbon = mean_combined_carbon_g_co2e * proportion * total,
      water  = mean_combined_water_site_source_m_l * proportion * total
    ) %>%
    dplyr::summarise(
      `energy (MWh)`      = sum(energy) / 1e6,
      `carbon (tonnes)`   = sum(carbon) / 1e6,
      `water (kiloliters)` = sum(water) / 1e6
    )
})

output$table <- renderTable(dataset())
}



shinyApp(ui, server)