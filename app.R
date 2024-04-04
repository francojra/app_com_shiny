library(shiny)
library(bslib)
library(tidyverse)
library(ggExtra)
library(dados)

df <- dados::dados_starwars

df <- df |>
  drop_na()

# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric)) 

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Bill Length (mm)"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Bill Depth (mm)"),
    checkboxGroupInput(
      "genero", "Filter by gênero",
      choices = unique(df$genero), 
      selected = unique(df$genero)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("por_genero", "Mostrar gêneros", TRUE),
    checkboxInput("mostrar_margins", "Mostrar marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$genero)
    df |> filter(genero %in% input$genero)
  })

  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$por_genero) aes(color = genero),
      geom_point(),
      if (input$smooth) geom_smooth()
    )

    if (input$show_margins) {
      margin_type <- if (input$por_genero) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
        size = 8, groupColour = input$por_genero, groupFill = input$por_genero)
    }

    p
  }, res = 100)
}

shinyApp(ui, server)