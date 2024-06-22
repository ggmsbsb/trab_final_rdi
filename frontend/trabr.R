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

# Lista de pacotes necessários
packages <- c("shiny", "shinydashboard", "shinyWidgets", "plotly", "randomForest", "caret", "readr", "dplyr", "openxlsx", "MLmetrics")

# Verifica se os pacotes estão instalados. Caso não, instalar. (Minha versão do pacman)
for(package in packages){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

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
                              choices = c("Finalistas", "Valores", "EDA")),
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
          },
           "EDA" = {
             # Análise exploratória de dados
             data <- data_base
             # Criar coluna 'ano' a partir de 'temporada'
             data$ano <- as.numeric(substring(data$temporada, 1, 4))
             # Definir o tamanho da amostra
             sample_size <- min(10, nrow(data))
             # Amostrar os dados para reduzir a quantidade de pontos
             set.seed(123) # Para reprodutibilidade
             data_sampled <- data[sample(nrow(data), sample_size), ]
             p <- ggplot(data_sampled, aes(x = ano, y = publico)) +
               geom_line(color = "blue") +
               labs(title = "Distribuição de Público ao Longo das Temporadas",
                    x = "Ano", y = "Público") +
               theme_minimal()
             # Converter para plotly para renderização interativa
             ggplotly(p)
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


observeEvent(input$predict, {
  
  # Função para determinar quem é "melhor" com base na quantidade de gols
  definir_melhor <- function(data, time1, time2) {
    confrontos <- data %>%
      filter((time_mandante == time1 & time_visitante == time2) | (time_mandante == time2 & time_visitante == time1))
    
    if (nrow(confrontos) == 0) {
      return(0)  # Não há confrontos diretos entre os times
    } else {
      gols_time1 <- sum(confrontos[confrontos$time_mandante == time1, "gols_mandante"]) + sum(confrontos[confrontos$time_visitante == time1, "gols_visitante"])
      gols_time2 <- sum(confrontos[confrontos$time_mandante == time2, "gols_mandante"]) + sum(confrontos[confrontos$time_visitante == time2, "gols_visitante"])
      
      return(sign(gols_time1 - gols_time2))  # retorna -1, 0 ou 1
    }
  }
  
  # Calcula a classificação ordinal dos times
  calcular_classificacao <- function(data, times) {
    classificacao <- numeric(length(times))
    
    for (i in seq_along(times)) {
      for (j in seq_along(times)[-i]) {
        confronto <- definir_melhor(data, times[i], times[j])
        classificacao[i] <- classificacao[i] + max(confronto, 0)
        classificacao[j] <- classificacao[j] + max(-confronto, 0)
      }
    }
    
    return(classificacao)
  }
  
  # Função para preparar os dados para treinamento do modelo de IA
  preparar_dados_modelo <- function(data, times_selecionados) {
    mandante_data <- data %>%
      filter(time_mandante == times_selecionados[1]) %>%
      summarise(
        proporcao_sucesso_mandante = mean(proporcao_sucesso_mandante, na.rm = TRUE),
        gols_mandante = mean(gols_mandante, na.rm = TRUE)
      )
    
    visitante_data <- data %>%
      filter(time_visitante == times_selecionados[2]) %>%
      summarise(
        proporcao_sucesso_visitante = mean(proporcao_sucesso_visitante, na.rm = TRUE),
        gols_visitante = mean(gols_visitante, na.rm = TRUE)
      )
    
    data.frame(
      proporcao_sucesso_mandante = mandante_data$proporcao_sucesso_mandante,
      gols_mandante = mandante_data$gols_mandante,
      proporcao_sucesso_visitante = visitante_data$proporcao_sucesso_visitante,
      gols_visitante = visitante_data$gols_visitante
    )
  }
  
  # Lista de times selecionados pelo usuário
  times_selecionados <- unique(c(input$time_mandante, input$time_visitante))
  
  # Classificação ordinal dos times
  classificacoes <- calcular_classificacao(data_goalbase, times_selecionados)
  
  # Preparação dos dados para treinamento do modelo
  predictors <- data_goalbase[, c('proporcao_sucesso_mandante', 'proporcao_sucesso_visitante', 'gols_mandante', 'gols_visitante')]
  target <- data_goalbase$match_outcome
  
  # Remover linhas com valores NA e substituir por média da coluna
  preProc <- preProcess(predictors, method = 'medianImpute')
  predictors <- predict(preProc, predictors)
  complete_cases <- complete.cases(predictors, target)
  predictors <- predictors[complete_cases, ]
  target <- target[complete_cases]
  
  # Dividir os dados em conjunto de treino e teste
  set.seed(123)
  trainIndex <- createDataPartition(target, p = .8, list = FALSE, times = 1)
  dataTreino <- predictors[trainIndex, ]
  targetTreino <- target[trainIndex]
  dataTestes <- predictors[-trainIndex, ]
  targetTestes <- target[-trainIndex]
  
  # Definir controle com validação cruzada de 10 folds
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = multiClassSummary)
  
  # Definir grid de parâmetros a considerar
  tuneGrid <- expand.grid(.mtry = 1:sqrt(ncol(dataTreino)))
  
  # Treinar o modelo
  model <- train(x = dataTreino, y = targetTreino, method = "rf", metric = "Accuracy", tuneGrid = tuneGrid, trControl = control)
  
  # Fazer previsões com o modelo treinado
  predictions <- predict(model, newdata = dataTestes)
  
  # Calcular a precisão do modelo
  accuracy <- mean(predictions == targetTestes)
  
  # Calcular a margem de erro da acurácia
  cv_results <- model$results$Accuracy
  mean_accuracy <- mean(cv_results)
  sd_accuracy <- sd(cv_results)
  margin_of_error <- qt(0.975, df = length(cv_results) - 1) * sd_accuracy / sqrt(length(cv_results))
  
  # Previsões com o modelo para os times selecionados
  selected_predictors <- preparar_dados_modelo(data_goalbase, times_selecionados)
  prob_predictions <- predict(model, newdata = selected_predictors, type = "prob")
  prob_mandante <- prob_predictions[, "mandante"] * 100
  prob_visitante <- prob_predictions[, "visitante"] * 100
  
  # Normalizar as probabilidades para somar 100%
  total_prob <- prob_mandante + prob_visitante
  prob_mandante_norm <- prob_mandante / total_prob * 100
  prob_visitante_norm <- prob_visitante / total_prob * 100
  
  # Exibição dos resultados na interface Shiny
  output$prediction_result <- renderPrint({
    paste("Probabilidade de Vitória - Mandante:", round(prob_mandante_norm, 2), "%\n",
          "Probabilidade de Vitória - Visitante:", round(prob_visitante_norm, 2), "%")
  })
  
  output$model_performance <- renderUI({
    tagList(
      h4("Desempenho do Modelo"),
      p(paste("Pico de precisão em %:", round(accuracy * 100, 2))),
      p(paste("Minimo de precisão em %:", round(accuracy * 100 - margin_of_error, 2))),
    )
  })
})


}

shiny::runApp(shinyApp(ui = ui, server = server), host = "127.0.0.1", port = 3838)