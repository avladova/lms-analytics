# Загрузка необходимых библиотек
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Загрузка данных
data <- readxl::read_excel("/Users/stamp/Downloads/AD_1(23-24) Оценки_МЭО22-5 (1).xlsx", sheet = "Оценки")

# Очистка данных: удаление строк с NA в email и преобразование оценок в числовой формат
data_clean <- data %>%
  filter(!is.na(`Адрес электронной почты`)) %>%
  mutate(across(where(is.character), as.numeric))

# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(title = "Анализ успеваемости студентов"),
  dashboardSidebar(
    textInput("email", "Введите email студента:", placeholder = "например, 225704"),
    actionButton("submit", "Показать результаты")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Общая информация о студенте",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        textOutput("student_info"),
        br(),
        plotOutput("student_plot", height = "600px") # Увеличиваем высоту графика
      )
    ),
    fluidRow(
      box(
        title = "Детализация оценок",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        DTOutput("grades_table")
      )
    )
  )
)

# Server часть приложения
server <- function(input, output) {
  # Реактивное выражение для фильтрации данных по email
  student_data <- eventReactive(input$submit, {
    req(input$email)
    data_clean %>% 
      filter(`Адрес электронной почты` == as.numeric(input$email))
  })
  
  # Вывод общей информации о студенте
  output$student_info <- renderText({
    student <- student_data()
    if (nrow(student) == 0) return("Студент с таким email не найден.")
    
    paste0("Студент: ", input$email, 
           "\nГруппа(ы): ", student$`Участник групп`[1],
           "\nИтоговая оценка: ", student$`Итоговая оценка за курс (Значение)`[1])
  })
  
  # График успеваемости студента
  output$student_plot <- renderPlot({
    student <- student_data()
    if (nrow(student) == 0) return(NULL)
    
    # Преобразование данных для графика
    grades_long <- student %>%
      select(-`Адрес электронной почты`, -`Участник групп`, -`Итоговая оценка за курс (Значение)`) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      filter(!is.na(Оценка))
    
    ggplot(grades_long, aes(x = reorder(Тест, Оценка), y = Оценка)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = Оценка), hjust = -0.2, size = 5) + # Добавляем подписи значений
      coord_flip() +
      labs(title = "Успеваемость студента по тестам",
           x = "",
           y = "Оценка") +
      theme_minimal(base_size = 14) + # Увеличиваем базовый размер шрифта
      theme(
        axis.text.y = element_text(size = 12, face = "bold"), # Увеличиваем шрифт подписей осей
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none" # Убираем легенду
      ) +
      ylim(0, max(grades_long$Оценка) * 1.1) # Добавляем немного места справа для подписей
  })
  
  # Таблица с детализацией оценок (без цветового форматирования)
  output$grades_table <- renderDT({
    student <- student_data()
    if (nrow(student) == 0) return(NULL)
    
    grades_table <- student %>%
      select(-`Адрес электронной почты`, -`Участник групп`) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      mutate(Тип = ifelse(Тест == "Промежуточная аттестация", "Аттестация", "Тест")) %>%
      arrange(desc(Тип), Тест) %>%
      select(Тип, Тест, Оценка) %>%
      filter(!is.na(Оценка))
    
    datatable(grades_table, 
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.21/i18n/Russian.json')
              ),
              rownames = FALSE)
  })
}

# Запуск приложения
shinyApp(ui, server)
