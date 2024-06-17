library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(randomForest)
library(caret)
library(readr)
library(dplyr)
library(openxlsx)
library(MLmetrics)

# Definindo os caminhos dos arquivos CSV
path_base <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league.csv"
path_finalbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_final.csv"
path_semifinalbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_semifinalist.csv"
path_quarterbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_quarterfinalist.csv"
path_octabase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_octafinalist.csv"
path_goalbase <- "D:\\CDMI\\rdifinal\\src\\main\\resources\\data\\champions_league_goals.csv"

# Leitura dos arquivos CSV
data_base <- read_csv(path_base)
data_finalbase <- read_csv(path_finalbase)
data_semifinalbase <- read_csv(path_semifinalbase)
data_quarterbase <- read_csv(path_quarterbase)
data_octabase <- read_csv(path_octabase)
data_goalbase <- read_csv(path_goalbase)

# Transformações necessárias
data_base <- data_base %>%
  mutate(across(c(gols_mandante, gols_visitante), as.numeric))
data_goalbase <- data_goalbase %>%
  mutate(across(c(gols_mandante, gols_visitante), as.numeric))

# Adicionar a coluna match_outcome
data_goalbase <- data_goalbase %>%
  mutate(
    match_outcome = case_when(
      gols_mandante > gols_visitante ~ "mandante",
      gols_mandante < gols_visitante ~ "visitante",
      TRUE ~ "empate"
    )
  )

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
                              choices = c("Base", "Final", "Semifinal", "Quarterfinal", "Octafinal", "Goals")),
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
  
  # Função para selecionar a base de dados
  get_data <- reactive({
    switch(input$dataset_selector,
           "Base" = data_base,
           "Final" = data_finalbase,
           "Semifinal" = data_semifinalbase,
           "Quarterfinal" = data_quarterbase,
           "Octafinal" = data_octabase,
           "Goals" = data_goalbase)
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

  observeEvent(input$predict, {
    # Prepare predictors and target
    predictors <- data_goalbase[, c('proporcao_sucesso_mandante', 'proporcao_sucesso_visitante', 'gols_mandante', 'gols_visitante')]
    target <- data_goalbase$match_outcome  # Nova coluna criada
    
    # Verificar se há pelo menos 2 classes no alvo
    if (length(unique(target)) < 2) {
      output$prediction_result <- renderPrint({
        "Erro: O alvo deve ter pelo menos duas classes diferentes."
      })
      return()
    }
    
    # Tratar NAs se houver
    na_indices <- which(apply(predictors, 1, function(x) any(is.na(x))))
    predictors <- predictors[-na_indices, ]
    target <- target[-na_indices]

    # Split the data into training and testing sets
    set.seed(123)
    trainIndex <- createDataPartition(target, p = .8, list = FALSE, times = 1)
    dataTrain <- predictors[trainIndex, ]
    targetTrain <- target[trainIndex]
    dataTest <- predictors[-trainIndex, ]
    targetTest <- target[-trainIndex]

    # Define the control
    control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = multiClassSummary)

    # Define the grid of parameters to consider
    tuneGrid <- expand.grid(.mtry = c(1:sqrt(ncol(dataTrain))))

    # Train the model
    model <- train(dataTrain, targetTrain, method = "rf", metric = "Accuracy", tuneGrid = tuneGrid, trControl = control)

    # Make predictions
    predictions <- predict(model, newdata = dataTest)

    # Calculate accuracy
    accuracy <- sum(predictions == targetTest) / length(targetTest)

    # Calculate probabilities for the selected teams
    selected_data <- data_goalbase %>%
      filter(time_mandante == input$time_mandante & time_visitante == input$time_visitante)

    # Prepare selected data for prediction
    selected_predictors <- selected_data[, c('proporcao_sucesso_mandante', 'proporcao_sucesso_visitante', 'gols_mandante', 'gols_visitante')]

    # Check for NA or NaN values and remove or replace them
    selected_predictors <- na.omit(selected_predictors)

    # Make predictions for the selected teams
    prob_predictions <- predict(model, newdata = selected_predictors, type = "prob")
    prob_mandante <- prob_predictions[, "mandante"]
    prob_visitante <- prob_predictions[, "visitante"]

    # Display results in the Shiny interface
    output$prediction_result <- renderPrint({
      paste("Probabilidade de Vitória - Mandante:", round(prob_mandante * 100, 2), "%\n",
            "Probabilidade de Vitória - Visitante:", round(prob_visitante * 100, 2), "%")
    })

    output$model_performance <- renderUI({
      tagList(
        h4("Desempenho do Modelo"),
        p(paste("Accuracy:", round(accuracy, 2)))
      )
    })
  })

}

shiny::runApp(shinyApp(ui = ui, server = server), host = "127.0.0.1", port = 3838)
