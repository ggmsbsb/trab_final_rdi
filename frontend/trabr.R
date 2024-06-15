# Carregando bibliotecas necessárias
library(readr)
library(dplyr)
library(shiny)
library(ggplot2)
library(caret)
library(openxlsx)
library(shinydashboard)
library(shinyWidgets)

# Definindo os caminhos dos arquivos CSV
path_base <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league.csv"
path_finalbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_final.csv"
path_semifinalbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_semifinalist.csv"
path_quarterbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_quarterfinalist.csv"
path_octabase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_octafinalist.csv"

# Leitura dos arquivos CSV
data_base <- read_csv(path_base)
data_finalbase <- read_csv(path_finalbase)
data_semifinalbase <- read_csv(path_semifinalbase)
data_quarterbase <- read_csv(path_quarterbase)
data_octabase <- read_csv(path_octabase)

# Transformações necessárias
data_base <- data_base %>%
  mutate(across(c(gols_mandante, gols_visitante), as.numeric))

ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Futebol Analytics"),
      tags$button(id = "toggle_btn", class = "btn btn-default", "Toggle Sidebar")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("DataSet", tabName = "dataset", icon = icon("table")),
      menuItem("Dashboards", tabName = "dashboards", icon = icon("chart-bar")),
      menuItem("Predição de Rendimento", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$script('
        $(document).ready(function() {
          $("#toggle_btn").click(function(){
            $(".main-sidebar").toggleClass("sidebar-collapse");
          });
        });
      '),
      tags$style(HTML('
        body, .content-wrapper, .right-side {
          background-color: #303030;
          min-height: 100vh;
          height: auto;
        }
        .main-header .navbar {
          background-color: #333 !important;
        }
        .main-header .logo {
          background-color: #333 !important;
          color: #fff !important;
        }
        .main-sidebar {
          background-color: #222 !important;
        }
        .main-sidebar .sidebar-menu>li.active>a {
          border-left-color: #3c8dbc !important;
        }
        .table-responsive {
          overflow-x: auto;
        }
        .box {
          background-color: #080808 !important;
          border-color: #4a4a4a !important;
          color: #d7d8d9 !important;
        }
        .box-header {
          color: #d7d8d9 !important;
          background: #4a4a4a !important;
        }
      '))
    ),
    class = "skin-black",
    tabItems(
      tabItem(tabName = "dataset",
              fluidRow(
                box(
                  title = "Filtros",
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  selectInput("dataset_selector", "Selecione a Base de Dados:", 
                              choices = c("Base", "Final", "Semifinal", "Quarterfinal", "Octafinal")),
                ),
                box(
                  title = "Dados",
                  solidHeader = TRUE,
                  status = "primary",
                  div(class = "table-responsive", tableOutput("data")),
                  actionButton("full_df", "Ver mais")
                )
              )
      ),
      tabItem(tabName = "dashboards",
              fluidRow(
                box(
                  title = "Configuração do Dashboard",
                  solidHeader = TRUE,
                  status = "primary",
                  selectInput("plot_type", "Selecione o tipo de Dashboard:",
                              choices = c("Finalistas", "Valores")),
                  uiOutput("liga_selector_dashboard"),
                  actionButton("update_plot", "Atualizar Plot")
                ),
                box(
                  title = "Dashboard",
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("dashboard_plot"),
                  uiOutput("dashboard_insight")
                )
              )
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = "Configurações de Predição",
                  solidHeader = TRUE,
                  status = "primary",
                  uiOutput("liga_selector_pred"),
                  numericInput("gols", "Gols Marcados:", value = 0, min = 0),
                  numericInput("vitorias", "Vitórias:", value = 0, min = 0),
                  numericInput("derrotas", "Derrotas:", value = 0, min = 0),
                  numericInput("empates", "Empates:", value = 0, min = 0),
                  numericInput("posicao", "Posição Final:", value = 20, min = 1, max = 20),
                  selectInput("liga_cup", "Resultado no Campeonato:", choices = c("W", "L")),
                  actionButton("predict", "Predizer Rendimento")
                ),
                box(
                  title = "Resultado da Predição",
                  solidHeader = TRUE,
                  status = "primary",
                  verbatimTextOutput("prediction_result"),
                  uiOutput("model_performance")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {

  # Função para selecionar a base de dados
  get_data <- reactive({
    switch(input$dataset_selector,
           "Base" = data_base,
           "Final" = data_finalbase,
           "Semifinal" = data_semifinalbase,
           "Quarterfinal" = data_quarterbase,
           "Octafinal" = data_octabase)
  })

  # armazena o valor da liga para o dashboard
  output$liga_selector_dashboard <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "Finalistas") {
        return(NULL)  # Retorna NULL para remover o seletor de liga
    } else if (input$plot_type == "Valores") {
        return(NULL)
    }
    selectInput("liga_dashboard", "Selecione a Liga:", choices = unique(get_data()$liga))
  })

  # Seletor de ligas para a predição
  output$liga_selector_pred <- renderUI({
    selectInput("liga_pred", "Selecione a Liga:", choices = unique(get_data()$liga))
  })
  
  # Exibe os dados filtrados na tabela
  output$data <- renderTable({
    filtered_data <- get_data()
    
    if (!is.null(input$team) && input$team != "Todos" && !is.character(input$team)) {
      filtered_data <- filtered_data %>%
        filter(time_mandante == input$team | time_visitante == input$team)
    }
    
    filtered_data
  })

  # Renderiza o plot do dashboard
  output$dashboard_plot <- renderPlot({
    req(input$plot_type)
    switch(input$plot_type,
           "Finalistas" = {
             finais_data <- data_finalbase
             teams_in_finals <- c(finais_data$time_mandante, finais_data$time_visitante)
             team_counts <- table(teams_in_finals)
             plot_data <- data.frame(team = names(team_counts), count = as.integer(team_counts))
             ggplot(data = plot_data, aes(x = team, y = count)) +
               geom_bar(stat = "identity", fill = "blue") +
               labs(title = "Number of times each team made it to the Finals",
                    x = "Team", y = "Count")
           },
           "Valores" = {
             # Usar o data_finalbase diretamente
             data <- data_finalbase
             # Converter a coluna 'temporada' para o ano inicial
             data$ano <- as.numeric(substring(data$temporada, 1, 4))
             ggplot(data, aes(x = ano)) +
               geom_line(aes(y = valor_medio_equipe_titular_mandante, color = "Home Team")) +
               geom_line(aes(y = valor_medio_equipe_titular_visitante, color = "Away Team")) +
               scale_color_manual(values = c("Home Team" = "green", "Away Team" = "pink")) +
               scale_y_continuous(labels = scales::comma) +
               labs(title = "Comparison of Team Values Over Time",
                    x = "Season", y = "Average Team Value")
           },
           "Estatísticas de Time" = {
             req(input$liga_dashboard)
             data <- get_data() %>%
               filter(liga == input$liga_dashboard)
             ggplot(data, aes(x = time_mandante)) +
               geom_bar(fill = "blue") +
               labs(title = "Distribuição dos Jogos por Time Mandante", x = "Time Mandante", y = "Contagem")
           },
           "Clusters de Desempenho" = {
             req(input$relation_cluster)
             data <- get_data()
             ggplot(data, aes_string(x = "gols_mandante", y = "gols_visitante")) +
               geom_point(color = "blue") +
               labs(title = "Clusters de Desempenho", x = "Gols Mandante", y = "Gols Visitante")
           }
    )
  })

  observeEvent(input$full_df, {
    showModal(modalDialog(
      title = "Dados Completos",
      div(class = "table-responsive", tableOutput("full_data")),
      easyClose = TRUE,
      size = "l"
    ))
    
    output$full_data <- renderTable({
      get_data()
    })
  })

  observeEvent(input$update_plot, {
    output$dashboard_plot <- renderPlot({
      req(input$plot_type)
      switch(input$plot_type,
             "Finalistas" = {
               finais_data <- data_finalbase
               teams_in_finals <- c(finais_data$time_mandante, finais_data$time_visitante)
               team_counts <- table(teams_in_finals)
               plot_data <- data.frame(team = names(team_counts), count = as.integer(team_counts))
               ggplot(data = plot_data, aes(x = team, y = count)) +
                 geom_bar(stat = "identity", fill = "blue") +
                 labs(title = "Number of times each team made it to the Finals",
                      x = "Team", y = "Count")
             },
             "Valores" = {
               # Usar o data_finalbase diretamente
               data <- data_finalbase
               # Converter a coluna 'temporada' para o ano inicial
               data$ano <- as.numeric(substring(data$temporada, 1, 4))
               ggplot(data, aes(x = ano)) +
                 geom_line(aes(y = valor_medio_equipe_titular_mandante, color = "Mandante")) +
                 geom_line(aes(y = valor_medio_equipe_titular_visitante, color = "Visitante")) +
                 scale_color_manual(values = c("Mandante" = "green", "Visitante" = "pink")) +
                 scale_y_continuous(labels = scales::comma) +
                 labs(title = "Valor dos finalistas ao longo do tempo",
                      x = "Temporada", y = "Média dos valores")
             },
      )
    })
  })

  observeEvent(input$predict, {
    # Aqui deve-se implementar a lógica de predição e retornar o resultado
    output$prediction_result <- renderPrint({
      paste("Resultado da predição para a liga", input$liga_pred, ":")
    })
  })
}

shiny::runApp(shinyApp(ui = ui, server = server), host = "127.0.0.1", port = 3838)
