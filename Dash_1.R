install.packages("shiny", "DT", "ggplot2", "dplyr", "tidyr", "readxl")

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)

# Загрузка данных
data <- readxl::read_excel("/Users/stamp/Downloads/AD_1(23-24) Оценки_МЭО22-5 (1).xlsx", sheet = "Оценки")

# Функция для фильтрации данных по email
get_student_data <- function(email) {
  student_data <- data %>% 
    filter(`Адрес электронной почты` == email) %>%
    select(-`Адрес электронной почты`, -`Участник групп`) %>%
    t() %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("Тест/Задание") %>%
    rename("Оценка" = "V1") %>%
    mutate(Оценка = as.numeric(Оценка)) %>%
    filter(!is.na(Оценка))
  
  return(student_data)
}

# Функция для создания графика успеваемости
plot_grades <- function(student_data) {
  ggplot(student_data, aes(x = reorder(`Тест/Задание`, Оценка), y = Оценка)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Успеваемость студента",
         x = "Тест/Задание",
         y = "Оценка") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

# Функция для создания сводной статистики
get_summary_stats <- function(student_data) {
  stats <- student_data %>%
    summarise(
      Средний_балл = mean(Оценка, na.rm = TRUE),
      Максимальный_балл = max(Оценка, na.rm = TRUE),
      Минимальный_балл = min(Оценка, na.rm = TRUE),
      Количество_тестов = n()
    )
  
  return(stats)
}

# Создание Shiny приложения
ui <- fluidPage(
  titlePanel("Дашборд успеваемости студента"),
  sidebarLayout(
    sidebarPanel(
      textInput("email", "Введите адрес электронной почты студента:", value = ""),
      actionButton("submit", "Показать успеваемость")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("График успеваемости", plotOutput("grade_plot")),
        tabPanel("Таблица оценок", DTOutput("grade_table")),
        tabPanel("Сводная статистика", tableOutput("summary_stats"))
      )
    )
  )
)

server <- function(input, output) {
  student_data <- eventReactive(input$submit, {
    req(input$email)
    get_student_data(input$email)
  })
  
  output$grade_plot <- renderPlot({
    plot_grades(student_data())
  })
  
  output$grade_table <- renderDT({
    datatable(student_data(), options = list(pageLength = 10))
  })
  
  output$summary_stats <- renderTable({
    get_summary_stats(student_data())
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)
