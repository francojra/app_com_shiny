library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(dados)

df <- dados::dados_starwars
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric)) 
df_num <- df_num[1:10 , ]

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Bill Length (mm)"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Bill Depth (mm)"),
    checkboxGroupInput(
      "nome", "Filter by nomes",
      choices = unique(df$nome), 
      selected = unique(df$nome)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("por_nomes", "Mostrar nomes", TRUE),
    checkboxInput("mostrar_margins", "Mostrar marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$nome)
    df |> filter(nome %in% input$nome)
  })

  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$por_nomes) aes(color = nome),
      geom_point(),
      if (input$smooth) geom_smooth()
    )

    if (input$show_margins) {
      margin_type <- if (input$por_nomes) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
        size = 8, groupColour = input$por_nomes, groupFill = input$por_nomes)
    }

    p
  }, res = 100)
}

shinyApp(ui, server)