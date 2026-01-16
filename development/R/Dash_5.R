library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)

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
      textInput("email", "Введите ID студента:", placeholder = "например, 237829"),
      actionButton("submit", "Показать результаты"),
      tags$hr(),
      helpText("Введите ID студента и нажмите кнопку, чтобы увидеть его результаты.")
    )
  ),
  dashboardBody(
    conditionalPanel(
      condition = "!output.file_loaded",
      box(
        width = 12,
        status = "info",
        h2("Добро пожаловать в систему анализа успеваемости"),
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
        p("Теперь вы можете ввести ID студента в поле слева и нажать кнопку 'Показать результаты'.")
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
      # Измененная часть - две диаграммы в одной строке
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
          title = "Детализация оценок",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          DTOutput("grades_table")
        )
      )
    )
  )
)

# Server часть приложения (остается без изменений)
server <- function(input, output, session) {
  data_clean <- reactiveVal(NULL)
  final_grade_col <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      showModal(modalDialog("Загрузка файла...", footer = NULL))
      
      data <- readxl::read_excel(input$file$datapath, sheet = "Оценки")
      
      clean_data <- data %>%
        filter(!is.na(`Адрес электронной почты`)) %>%
        select(-last_col())
      
      grade_col <- "Итоговая оценка за курс (Значение)"
      
      grade_cols <- setdiff(names(clean_data), c("Адрес электронной почты", grade_col))
      clean_data <- clean_data %>%
        mutate(across(all_of(grade_cols), as.numeric))
      
      data_clean(clean_data)
      final_grade_col(grade_col)
      
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
    if (nrow(student) == 0) return("Студент с таким ID не найден.")
    
    final_grade <- student[[final_grade_col()]][1]
    group_avg <- mean(data_clean()[[final_grade_col()]], na.rm = TRUE)
    
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade))
  })
  
  output$student_plot <- renderPlot({
    student <- student_data()
    req(nrow(student) > 0)
    
    grades_summary <- prepare_category_data(student)
    
    req(nrow(grades_summary) > 0)
    
    # Вертикальная версия диаграммы
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
    
    # Построение графика сравнения
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
  
  output$grades_table <- renderDT({
    student <- student_data()
    req(nrow(student) > 0)
    
    grades_table <- student %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col())) %>%
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
    
    req(nrow(grades_table) > 0)
    
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