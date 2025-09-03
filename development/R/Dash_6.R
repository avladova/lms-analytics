library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(stringr)

# Функция для определения группы задания (из вашего кода)
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

# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(title = "Анализ успеваемости студентов"),
  dashboardSidebar(
    fileInput("file", "Выберите файл с оценками", 
              accept = c(".xlsx", ".xls"),
              buttonLabel = "Обзор...",
              placeholder = "Файл не выбран"),
    
    conditionalPanel(
      condition = "output.file_loaded",
      textInput("email", "Введите код студента:", placeholder = "например, 237829"),
      actionButton("submit", "Показать результаты"),
      tags$hr(),
      helpText("Введите код студента и нажмите кнопку, чтобы увидеть его результаты.")
    )
  ),
  dashboardBody(
    conditionalPanel(
      condition = "!output.file_loaded",
      box(
        width = 12,
        status = "info",
        h2("Добро пожаловать в Сервис управления обучением"),
        p("Пожалуйста, загрузите файл с оценками в формате Excel (.xlsx или .xls)."),
        p("После загрузки файла вы сможете просматривать результаты студентов.")
      )
    ),
    
    conditionalPanel(
      condition = "output.file_loaded && input.submit == 0",
      box(
        width = 12,
        status = "info",
        h3("Файл успешно загружен"),
        p("Теперь вы можете ввести код студента в поле слева и нажать кнопку 'Показать результаты'.")
      )
    ),
    
    conditionalPanel(
      condition = "output.file_loaded && input.submit > 0",
      fluidRow(
        box(
          title = "Общая информация о студенте",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h3(textOutput("student_final_grade")),
          br()
        )
      ),
      fluidRow(
        box(
          title = "Суммарные оценки по категориям",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          plotOutput("student_plot", height = "300px")
        ),
        box(
          title = "Сравнение с группой",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          plotOutput("group_comparison_plot", height = "300px")
        )
      ),
      fluidRow(
        box(
          title = "Выполнение заданий",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          uiOutput("task_completion_plot_ui")
        )
      )
    )
  )
)

# Server часть приложения
server <- function(input, output, session) {
  data_clean <- reactiveVal(NULL)
  final_grade_col <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      showModal(modalDialog("Загрузка файла...", footer = NULL))
      
      data <- readxl::read_excel(input$file$datapath, sheet = "Оценки")
      
      # Удаляем полностью пустые столбцы и столбец "Участник групп"
      clean_data <- data %>%
        select(where(~!all(is.na(.)))) %>%  # Удаляем полностью пустые столбцы
        select(-matches("Участник групп")) %>%  # Удаляем столбец "Участник групп"
        filter(!is.na(`Адрес электронной почты`)) %>%
        select(-last_col())  # Удаляем последний столбец (обычно технический)
      
      grade_col <- "Итоговая оценка за курс (Значение)"
      
      grade_cols <- setdiff(names(clean_data), c("Адрес электронной почты", grade_col))
      clean_data <- clean_data %>%
        mutate(across(all_of(grade_cols), ~ ifelse(. == "-", NA, as.numeric(.))))
      
      data_clean(clean_data)
      final_grade_col(grade_col)
      
      # Подготовка данных для вашей диаграммы
      processed <- clean_data %>%
        pivot_longer(
          cols = -c(`Адрес электронной почты`, all_of(grade_col)),
          names_to = "Задание",
          values_to = "Оценка"
        ) %>%
        mutate(Группа = sapply(Задание, assign_group)) %>%
        mutate(Оценка = case_when(
          is.na(Оценка) & Группа %in% c("Тренинг", "ДЗ", "АСР") ~ -1,
          is.na(Оценка) & Группа %in% c("Аттестация", "Аттестация (Зачет)", "Аттестация (Экзамен)") ~ -60,
          is.na(Оценка) & Группа %in% c("Контрольные", "РАР") ~ -5,
          TRUE ~ Оценка
        ))
      
      processed_data(processed)
      
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Ошибка при загрузке файла:", e$message), type = "error")
    })
  })
  
  output$file_loaded <- reactive({
    !is.null(data_clean())
  })
  outputOptions(output, "file_loaded", suspendWhenHidden = FALSE)
  
  student_data <- eventReactive(input$submit, {
    req(input$email, data_clean())
    data_clean() %>% 
      filter(`Адрес электронной почты` == input$email)
  })
  
  student_processed_data <- eventReactive(input$submit, {
    req(input$email, processed_data())
    processed_data() %>% 
      filter(`Адрес электронной почты` == input$email)
  })
  
  # Функция для подготовки данных по категориям
  prepare_category_data <- function(data) {
    data %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col())) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      filter(!is.na(Оценка)) %>%
      mutate(
        Категория = case_when(
          grepl("аттестация", Тест, ignore.case = TRUE) ~ "Аттестация",
          grepl("контрольная", Тест, ignore.case = TRUE) ~ "Контрольные работы",
          grepl("лекционный", Тест, ignore.case = TRUE) ~ "Лекционные опросы",
          grepl("ДСР", Тест) ~ "Домашние работы",
          grepl("АСР", Тест) ~ "Аудиторные работы",
          TRUE ~ "Другие задания"
        )
      ) %>%
      group_by(Категория) %>%
      summarise(
        Средняя_оценка = mean(Оценка, na.rm = TRUE),
        Количество_заданий = n(),
        Суммарная_оценка = sum(Оценка, na.rm = TRUE)
      ) %>%
      arrange(desc(Суммарная_оценка))
  }
  
  output$student_final_grade <- renderText({
    student <- student_data()
    if (nrow(student) == 0) return("Студент с таким кодом не найден.")
    
    final_grade <- student[[final_grade_col()]][1]
    group_avg <- mean(data_clean()[[final_grade_col()]], na.rm = TRUE)
    
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade),
           "")
  })
  
  output$student_plot <- renderPlot({
    student <- student_data()
    req(nrow(student) > 0)
    
    grades_summary <- prepare_category_data(student)
    
    req(nrow(grades_summary) > 0)
    
    ggplot(grades_summary, aes(x = Категория, y = Суммарная_оценка)) +
      geom_bar(stat = "identity", width = 0.6, fill = "steelblue") +
      geom_text(aes(label = round(Суммарная_оценка, 1)), vjust = -0.5, size = 4) +
      labs(
        title = "Суммарные оценки по категориям",
        x = "",
        y = "Сумма оценок"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$group_comparison_plot <- renderPlot({
    student <- student_data()
    req(nrow(student) > 0)
    
    # Подготовка данных студента
    student_categories <- prepare_category_data(student) %>%
      mutate(Тип = "Студент")
    
    # Подготовка данных группы
    group_categories <- data_clean() %>%
      prepare_category_data() %>%
      group_by(Категория) %>%
      summarise(Средняя_оценка = mean(Средняя_оценка, na.rm = TRUE),
                Количество_заданий = mean(Количество_заданий, na.rm = TRUE),
                Суммарная_оценка = mean(Суммарная_оценка, na.rm = TRUE)) %>%
      mutate(Тип = "Группа")
    
    # Объединение данных
    comparison_data <- bind_rows(student_categories, group_categories)
    
    req(nrow(comparison_data) > 0)
    
    ggplot(comparison_data, aes(x = Категория, y = Средняя_оценка, fill = Тип)) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
      geom_text(aes(label = round(Средняя_оценка, 1)), 
                position = position_dodge(width = 0.7), 
                vjust = -0.5, size = 4) +
      scale_fill_manual(values = c("Студент" = "#E41A1C", "Группа" = "#377EB8")) +
      labs(
        title = "Сравнение студента со средними показателями группы",
        x = "Категория",
        y = "Средняя оценка",
        fill = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.grid.major.x = element_blank()
      ) +
      scale_y_continuous(limits = c(0, max(comparison_data$Средняя_оценка) * 1.2))
  })
  
  # Динамическое определение высоты графика выполнения заданий
  output$task_completion_plot_ui <- renderUI({
    s_data <- student_processed_data()
    req(nrow(s_data) > 0)
    
    # Рассчитываем высоту графика на основе количества заданий
    n_tasks <- nrow(s_data)
    plot_height <- max(400, n_tasks * 20)  # Минимальная высота 400px, +20px на каждое задание
    
    plotOutput("task_completion_plot", height = paste0(plot_height, "px"))
  })
  
  # Ваша диаграмма выполнения заданий
  output$task_completion_plot <- renderPlot({
    s_data <- student_processed_data()
    req(nrow(s_data) > 0)
    
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
}

# Запуск приложения
shinyApp(ui, server)