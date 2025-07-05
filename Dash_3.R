library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Загрузка данных
data <- readxl::read_excel("/Users/stamp/Downloads/AD_1(20-21) Оценки_ГФК19-4.xlsx", sheet = "Оценки")

# Очистка данных
data_clean <- data %>%
  filter(!is.na(`Адрес электронной почты`)) %>%
  select(-last_col()) # Удаляем последний столбец

# Сохраняем название столбца с итоговой оценкой
final_grade_col <- "Итоговая оценка за курс (Значение)"

# Преобразуем только столбцы с оценками в числовой формат (кроме email и итоговой оценки)
grade_cols <- setdiff(names(data_clean), c("Адрес электронной почты", final_grade_col))
data_clean <- data_clean %>%
  mutate(across(all_of(grade_cols), as.numeric))

# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(title = "Анализ успеваемости студентов"),
  dashboardSidebar(
    textInput("email", "Введите ID студента:", placeholder = "например, 237829"),
    actionButton("submit", "Показать результаты")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Общая информация о студенте",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        h3(textOutput("student_final_grade")),
        br(),
        plotOutput("student_plot", height = "600px")
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
      filter(`Адрес электронной почты` == input$email)
  })
  
  # Вывод итоговой оценки крупным шрифтом
  output$student_final_grade <- renderText({
    student <- student_data()
    if (nrow(student) == 0) return("Студент с таким ID не найден.")
    
    final_grade <- student[[final_grade_col]][1]
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade))
  })
  
  # График успеваемости студента (без итоговой оценки)
  output$student_plot <- renderPlot({
    student <- student_data()
    if (nrow(student) == 0) return(NULL)
    
    # Преобразование данных для графика (исключаем итоговую оценку)
    grades_long <- student %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col)) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      filter(!is.na(Оценка))
    
    if (nrow(grades_long) == 0) return(NULL)
    
    ggplot(grades_long, aes(x = reorder(Тест, Оценка), y = Оценка)) +
      geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
      geom_text(aes(label = Оценка), hjust = -0.2, size = 5) +
      coord_flip() +
      labs(title = "Успеваемость студента",
           x = "",
           y = "Оценка") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none"
      ) +
      ylim(0, max(grades_long$Оценка, na.rm = TRUE) * 1.1)
  })
  
  # Таблица с детализацией оценок (без итоговой оценки и цветового форматирования)
  output$grades_table <- renderDT({
    student <- student_data()
    if (nrow(student) == 0) return(NULL)
    
    grades_table <- student %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col)) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      mutate(Тип = case_when(
        grepl("аттестация", Тест, ignore.case = TRUE) ~ "Аттестация",
        grepl("контрольная", Тест, ignore.case = TRUE) ~ "Контрольная работа",
        grepl("лекционный", Тест, ignore.case = TRUE) ~ "Лекционный опрос",
        TRUE ~ "Тест"
      )) %>%
      arrange(Тип, Тест) %>%
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