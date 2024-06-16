# Carregando bibliotecas necessárias
library(readr)
library(dplyr)
library(shiny)
library(ggplot2)
library(caret)
library(openxlsx)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(randomForest)
library(caret)

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
          color: #fff !iant;
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
                  width = 3,
                  collapsible = TRUE,
                  selectInput("dataset_selector", "Selecione a Base de Dados:", 
                              choices = c("Base", "Final", "Semifinal", "Quarterfinal", "Octafinal")),
                ),
                box(
                  title = "Dados",
                  solidHeader = TRUE,
                  status = "primary",
                  div(class = "table-responsive", tableOutput("data")),
                  actionButton("full_df", "Visualizar dados.")
                )
              )
      ),
      tabItem(tabName = "dashboards",
              fluidRow(
                box(
                  title = "Configuração do Dashboard",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 3,
                  selectInput("plot_type", "Selecione o tipo de Dashboard:",
                              choices = c("Finalistas", "Valores")),
                  uiOutput("liga_selector_dashboard"),
                  actionButton("update_plot", "Atualizar Plot")
                ),
                box(
                  title = "Dashboard",
                  solidHeader = TRUE,
                  status = "primary",
                  plotlyOutput("dashboard_plot"),
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
                  selectInput("time_mandante", "Time Mandante:", choices = NULL),
                  selectInput("time_visitante", "Time Visitante:", choices = NULL),
                  actionButton("predict", "Predizer Resultado")
                ),
                box(
                  title = "Resultado da Predição",
                  solidHeader = TRUE,
                  status = "primary",
                  verbatimTextOutput("prediction_result"),
                  verbatimTextOutput("predicted_goals"),
                  uiOutput("model_performance")
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  library(randomForest)
  library(caret)
  
  # Função para selecionar a base de dados
  get_data <- reactive({
    switch(input$dataset_selector,
           "Base" = data_base,
           "Final" = data_finalbase,
           "Semifinal" = data_semifinalbase,
           "Quarterfinal" = data_quarterbase,
           "Octafinal" = data_octabase)
  })

  # Atualiza os choices dos selects de time
  observe({
    updateSelectInput(session, "time_mandante", choices = unique(get_data()$time_mandante))
    updateSelectInput(session, "time_visitante", choices = unique(get_data()$time_visitante))
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
  output$dashboard_plot <- renderPlotly({
    req(input$plot_type)
    switch(input$plot_type,
          "Finalistas" = {
            finais_data <- data_finalbase
            teams_in_finals <- c(finais_data$time_mandante, finais_data$time_visitante)
            team_counts <- table(teams_in_finals)
            plot_data <- data.frame(team = names(team_counts), count = as.integer(team_counts))
            p <- ggplot(data = plot_data, aes(x = team, y = count)) +
              geom_bar(stat = "identity", fill = "blue") +
              labs(title = "Finalistas",
                   x = "Time", y = "Quantidade")
            ggplotly(p)
          },
          "Valores" = {
            # Usar o data_finalbase diretamente
            data <- data_finalbase
            # Converter a coluna 'temporada' para o ano inicial
            data$ano <- as.numeric(substring(data$temporada, 1, 4))
            p <- ggplot(data, aes(x = ano)) +
              geom_line(aes(y = valor_medio_equipe_titular_mandante, color = "Mandante")) +
              geom_line(aes(y = valor_medio_equipe_titular_visitante, color = "Visitante")) +
              scale_color_manual(values = c("Mandante" = "green", "Visitante" = "pink")) +
              scale_y_continuous(labels = scales::comma) +
              labs(title = "Valor médio dos times finalistas ao longo do tempo",
                   x = "Temporada", y = "Média dos valores")
            ggplotly(p)
          })
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
                 labs(title = "Finalistas",
                      x = "Time", y = "Quantidade")
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
                 labs(title = "Valor médio dos times finalistas ao longo do tempo",
                      x = "Temporada", y = "Média dos valores")
             },
      )
    })
  })

  # Função de predição com validação cruzada e grid search
  observeEvent(input$predict, {
    # Aqui deve-se implementar a lógica de predição e retornar o resultado
    data <- get_data()
    
    # Identificar colunas numéricas para preenchimento de valores ausentes
    numeric_columns <- sapply(data, is.numeric)
    
    # Preencher valores ausentes apenas nas colunas numéricas com a média da coluna
    data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
      ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    })
    
    # Selecionar variáveis preditoras e alvo
    predictors <- data %>%
      select(idade_tecnico_mandante, idade_tecnico_visitante, proporcao_sucesso_mandante, 
            proporcao_sucesso_visitante, valor_equipe_titular_mandante, valor_equipe_titular_visitante, 
            valor_medio_equipe_titular_mandante, valor_medio_equipe_titular_visitante, 
            convocacao_selecao_principal_mandante, convocacao_selecao_principal_visitante, 
            selecao_juniores_mandante, selecao_juniores_visitante, estrangeiros_mandante, 
            estrangeiros_visitante, socios_mandante, socios_visitante, idade_media_titular_mandante, 
            idade_media_titular_visitante)
    target <- data$gols_mandante

    # Verificar se há valores ausentes após a limpeza
    sum(is.na(predictors))  # Deve retornar 0 se não houver valores ausentes

    # Dividir os dados em treino e teste
    set.seed(123)
    trainIndex <- createDataPartition(target, p = .8, list = FALSE)
    dataTrain <- predictors[trainIndex, ]
    targetTrain <- target[trainIndex]
    dataTest <- predictors[-trainIndex, ]
    targetTest <- target[-trainIndex]

    # Treinar o modelo Random Forest
    model <- randomForest(dataTrain, targetTrain, ntree = 100)

    # Fazer predições
    predictions <- predict(model, dataTest)

    # Avaliar o modelo
    rmse <- sqrt(mean((predictions - targetTest)^2))

    output$prediction_result <- renderPrint({
      paste("A precisão do modelo (RMSE):", round(rmse, 2))
    })
  })
}

shiny::runApp(shinyApp(ui = ui, server = server), host = "127.0.0.1", port = 3838)