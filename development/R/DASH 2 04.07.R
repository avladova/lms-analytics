library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(DT)

# Функция для определения группы задания
assign_group <- function(col_name) {
  col_name <- as.character(col_name)
  if (str_detect(col_name, "Тренинг|самостоятельной")) {
    return("Тренинг")
  } else if (str_detect(col_name, "ДЗ")) {
    return("ДЗ")
  } else if (str_detect(col_name, "аттестация")) {
    if (str_detect(col_name, "Зачет")) return("Аттестация (Зачет)")
    if (str_detect(col_name, "Экзамен")) return("Аттестация (Экзамен)")
    return("Аттестация")
  } else if (str_detect(col_name, "Контрольная|контрольной")) {
    return("Контрольные")
  } else if (str_detect(col_name, "РАР")) {
    return("РАР")
  } else if (str_starts(col_name, "Тест")) {
    return("АСР")
  }
  return(NA)
}

# Загрузка данных
data <- read_excel("C:/НИР/1.2.Коммиссионники-Дашборд.xlsx", sheet = "Оценки")

# Обработка данных
processed_data <- data %>%
  # Удаление строк с отсутствующим email
  filter(!is.na(`Адрес электронной почты`)) %>%
  # Удаление столбца с файлами
  select(-matches("файлы|Файлы|Последние загруженные из этого курса", ignore.case = TRUE)) %>%
  # Преобразование всех столбцов оценок в числовой формат
  mutate(across(-c(`Адрес электронной почты`, `Итоговая оценка за курс (Значение)`), 
                ~ ifelse(. == "-", NA, as.numeric(.)))) %>%
  # Преобразование в длинный формат
  pivot_longer(
    cols = -c(`Адрес электронной почты`, `Итоговая оценка за курс (Значение)`),
    names_to = "Задание",
    values_to = "Оценка"
  ) %>%
  # Назначение групп заданиям
  mutate(Группа = sapply(Задание, assign_group)) %>%
  # Замена NA на отрицательные значения в зависимости от группы
  mutate(Оценка = case_when(
    is.na(Оценка) & Группа %in% c("Тренинг", "ДЗ", "АСР") ~ -1,
    is.na(Оценка) & Группа %in% c("Аттестация", "Аттестация (Зачет)", "Аттестация (Экзамен)") ~ -60,
    is.na(Оценка) & Группа %in% c("Контрольные", "РАР") ~ -5,
    TRUE ~ Оценка
  ))

# UI приложения
ui <- dashboardPage(
  dashboardHeader(title = "Аналитический дашборд"),
  dashboardSidebar(
    selectInput("email", "Выберите email студента:", 
                choices = unique(processed_data$`Адрес электронной почты`))
  ),
  dashboardBody(
    fluidRow(
      box(title = "Выполнение заданий", status = "primary", solidHeader = TRUE,
          plotOutput("task_completion_plot", height = "600px"), width = 12)
    ),
    fluidRow(
      box(title = "Сделанные задачи", status = "success", solidHeader = TRUE,
          plotOutput("task_pie_chart"), width = 6),
      box(title = "Баллы по видам заданий", status = "info", solidHeader = TRUE,
          plotOutput("group_scores_plot"), width = 6)
    )
  )
)

# Server логика
server <- function(input, output) {
  
  # Реактивные данные для выбранного студента
  student_data <- reactive({
    req(input$email)
    processed_data %>%
      filter(`Адрес электронной почты` == input$email)
  })
  
  # График выполнения заданий
  output$task_completion_plot <- renderPlot({
    s_data <- student_data()
    
    ggplot(s_data, aes(x = reorder(Задание, Оценка), y = Оценка, 
                       fill = ifelse(Оценка < 0, "red", "blue"))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = ifelse(Оценка < 0, "Не выполнено", round(Оценка, 1))), 
                hjust = -0.1, size = 3.5) +
      scale_fill_identity() +
      coord_flip() +
      labs(x = "", y = "Оценка", title = "Результаты по заданиям") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        panel.grid.major.y = element_blank()
      ) +
      ylim(min(s_data$Оценка, na.rm = TRUE) * 1.1, max(s_data$Оценка, na.rm = TRUE) * 1.2)
  })
  
  # Круговая диаграмма выполненных задач
  output$task_pie_chart <- renderPlot({
    s_data <- student_data()
    
    summary <- s_data %>%
      summarise(
        Сделано = sum(Оценка >= 0, na.rm = TRUE),
        Не_сделано = sum(Оценка < 0, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Статус", values_to = "Количество")
    
    # Check for empty or all-zero data
    if (nrow(summary) == 0 || sum(summary$Количество) == 0) {
      plot.new()
      text(0.5, 0.5, "Нет данных для отображения")
      return()
    }
    
    ggplot(summary, aes(x = "", y = Количество, fill = Статус)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(
        aes(label = ifelse(sum(Количество) == 0, "", 
                           paste0(Количество, "\n", round(Количество/sum(Количество)*100), "%"))),
        position = position_stack(vjust = 0.5)
      ) +
      labs(title = "Процент выполненных заданий", fill = "Статус") +
      scale_fill_manual(values = c("Сделано" = "#1f77b4", "Не_сделано" = "#ff7f0e")) +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
  # График баллов по группам заданий
  output$group_scores_plot <- renderPlot({
    s_data <- student_data()
    
    group_order <- c("Тренинг", "ДЗ", "Аттестация (Зачет)", "Аттестация (Экзамен)", 
                     "Контрольные", "РАР", "АСР")
    
    # For "Аттестация (Зачет)" and "Аттестация (Экзамен)" take only the last non-negative mark
    attest_groups <- c("Аттестация (Зачет)", "Аттестация (Экзамен)")
    
    # For these groups, get the last non-negative mark
    attest_scores <- s_data %>%
      filter(Группа %in% attest_groups, Оценка > 0) %>%
      group_by(Группа) %>%
      summarise(Баллы = dplyr::last(Оценка), .groups = "drop")
    
    # For other groups, sum as before
    other_scores <- s_data %>%
      filter(!(Группа %in% attest_groups), Оценка > 0) %>%
      group_by(Группа) %>%
      summarise(Баллы = sum(Оценка, na.rm = TRUE), .groups = "drop")
    
    # Combine
    group_scores <- bind_rows(attest_scores, other_scores) %>%
      complete(Группа = group_order, fill = list(Баллы = 0))
    
    group_scores$Группа <- factor(group_scores$Группа, levels = group_order)
    
    ggplot(group_scores, aes(x = Группа, y = Баллы, fill = Группа)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Баллы, 1)), vjust = -0.3, size = 4) +
      labs(x = "Категория заданий", y = "Сумма баллов", 
           title = "Накопленные баллы по категориям") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none"
      ) +
      scale_fill_brewer(palette = "Set3") +
      ylim(0, max(group_scores$Баллы, na.rm = TRUE) * 1.2)
  })
}

# Запуск приложения
shinyApp(ui, server)
