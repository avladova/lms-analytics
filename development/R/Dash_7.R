library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(stringr)
library(httr)

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

# Функция для получения списка файлов из GitHub
get_github_files <- function() {
  base_url <- "https://api.github.com/repos/avladova/lms-analytics/contents/%D0%98%D1%82%D0%BE%D0%B3_1?ref=MYdata"
  
  tryCatch({
    response <- GET(base_url, add_headers(
      "User-Agent" = "R",
      "Accept" = "application/vnd.github.v3+json"
    ))
    
    if (http_error(response)) {
      stop(sprintf("GitHub API request failed [%s]", status_code(response)))
    }
    
    files <- content(response, "parsed")
    
    if (!is.list(files) || length(files) == 0) {
      return(data.frame(name = character(), url = character()))
    }
    
    data.frame(
      name = sapply(files, `[[`, "name"),
      url = sapply(files, function(x) {
        if (!is.null(x$download_url)) {
          x$download_url
        } else {
          paste0("https://raw.githubusercontent.com/avladova/lms-analytics/MYdata/",
                 "Итог_1/", 
                 x$name)
        }
      }),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Ошибка при получении файлов: ", e$message)
    data.frame(name = character(), url = character())
  })
}

# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(title = "Анализ успеваемости студентов"),
  dashboardSidebar(
    selectInput("github_file", "Выберите файл из репозитория", 
                choices = NULL, selectize = FALSE),
    
    conditionalPanel(
      condition = "output.file_loaded",
      textInput("email", "Введите хэш студента:", placeholder = "например, 237829"),
      actionButton("submit", "Показать результаты"),
      tags$hr(),
      helpText("Введите хэш студента и нажмите кнопку, чтобы увидеть его результаты.")
    )
  ),
  dashboardBody(
    conditionalPanel(
      condition = "!output.file_loaded",
      box(
        width = 12,
        status = "info",
        h2("Добро пожаловать в Сервис управления обучением"),
        p("Пожалуйста, выберите файл с оценками из репозитория."),
        p("После выбора файла вы сможете просматривать результаты студентов.")
      )
    ),
    
    conditionalPanel(
      condition = "output.file_loaded && input.submit == 0",
      box(
        width = 12,
        status = "info",
        h3("Файл успешно загружен"),
        p("Теперь вы можете ввести хэш студента в поле слева и нажать кнопку 'Показать результаты'.")
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
  assignments_with_grades <- reactiveVal(NULL)
  
  # Загружаем список файлов при запуске приложения
  github_files <- reactive({
    withProgress({
      setProgress(message = "Получение списка файлов...")
      get_github_files()
    }, message = "Загрузка данных")
  })
  
  # Обновляем выбор файлов
  observe({
    files <- github_files()
    if (nrow(files) > 0) {
      updateSelectInput(session, "github_file", 
                        choices = files$name,
                        selected = NULL)
    } else {
      showNotification("Не удалось загрузить список файлов или папка пуста", type = "error")
    }
  })
  
  # Обработка выбора файла
  observeEvent(input$github_file, {
    req(input$github_file)
    
    files <- github_files()
    selected_file <- files[files$name == input$github_file, ]
    req(nrow(selected_file) == 1)
    
    tryCatch({
      showModal(modalDialog("Загрузка файла...", footer = NULL))
      
      # Создаем временный файл
      temp_file <- tempfile(fileext = ".xlsx")
      
      # Улучшенная загрузка с обработкой ошибок
      download_result <- tryCatch({
        GET(
          URLencode(selected_file$url),
          write_disk(temp_file, overwrite = TRUE),
          add_headers("User-Agent" = "Mozilla/5.0"),
          progress()
        )
      }, error = function(e) {
        stop(sprintf("Ошибка загрузки файла: %s", e$message))
      })
      
      # Проверяем успешность загрузки
      if (!file.exists(temp_file) || file.size(temp_file) == 0) {
        stop("Файл не был загружен или пуст")
      }
      
      # Чтение файла с обработкой ошибок
      data <- tryCatch({
        readxl::read_excel(temp_file, sheet = "Оценки")
      }, error = function(e) {
        stop(sprintf("Ошибка чтения Excel файла: %s", e$message))
      })
      
      # Очистка данных
      clean_data <- data %>%
        select(where(~!all(is.na(.)))) %>%
        select(-matches("Участник групп")) %>%
        filter(!is.na(`Адрес электронной почты`)) %>%
        select(-last_col())
      
      grade_col <- "Итоговая оценка за курс (Значение)"
      
      grade_cols <- setdiff(names(clean_data), c("Адрес электронной почты", grade_col))
      clean_data <- clean_data %>%
        mutate(across(all_of(grade_cols), ~ ifelse(. == "-", NA, as.numeric(.))))
      
      # Определяем задания с оценками
      assignments_with_data <- grade_cols[sapply(grade_cols, function(col) {
        any(!is.na(clean_data[[col]]))
      })]
      
      assignments_with_grades(assignments_with_data)
      data_clean(clean_data)
      final_grade_col(grade_col)
      
      # Подготовка данных для визуализации
      processed <- clean_data %>%
        pivot_longer(
          cols = all_of(assignments_with_data),
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
      showNotification(paste("Ошибка:", e$message), type = "error", duration = 10)
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
      filter(`Адрес электронной почты` == input$email) %>%
      mutate(Задание = str_replace_all(Задание, "\\s*\\([^)]*\\)", "")) # Удаляем текст в скобках
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
    if (nrow(student) == 0) return("Студент с таким хэшем не найден.")
    
    final_grade <- student[[final_grade_col()]][1]
    group_avg <- mean(data_clean()[[final_grade_col()]], na.rm = TRUE)
    
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade),
           " (Среднее по группе: ", round(group_avg, 1), ")")
  })
  
  output$student_plot <- renderPlot({
    student <- student_data()
    req(nrow(student) > 0)
    
    grades_summary <- prepare_category_data(student)
    req(nrow(grades_summary) > 0)
    
    ggplot(grades_summary, aes(x = reorder(Категория, Суммарная_оценка), y = Суммарная_оценка)) +
      geom_col(fill = "#4E79A7", width = 0.7) +
      geom_text(aes(label = round(Суммарная_оценка, 1)), 
                hjust = -0.1, size = 5, color = "#4E79A7") +
      coord_flip() +
      labs(x = NULL, y = "Сумма оценок", title = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text = element_text(color = "black"),
        panel.grid.major.y = element_blank(),
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
      summarise(
        Средняя_оценка = mean(Средняя_оценка, na.rm = TRUE),
        Количество_заданий = mean(Количество_заданий, na.rm = TRUE),
        Суммарная_оценка = mean(Суммарная_оценка, na.rm = TRUE)
      ) %>%
      mutate(Тип = "Группа")
    
    # Объединение данных
    comparison_data <- bind_rows(student_categories, group_categories)
    req(nrow(comparison_data) > 0)
    
    ggplot(comparison_data, aes(x = Категория, y = Средняя_оценка, fill = Тип)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = round(Средняя_оценка, 1)), 
    position = position_dodge(width = 0.8),
    vjust = -0.5, size = 4
  ) +
    scale_fill_manual(values = c("Студент" = "#E15759", "Группа" = "#76B7B2")) +
    labs(
      x = NULL, y = "Средняя оценка", fill = NULL,
      title = "Сравнение с группой"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })

output$task_completion_plot_ui <- renderUI({
  s_data <- student_processed_data()
  req(nrow(s_data) > 0)
  
  n_tasks <- nrow(s_data)
  plot_height <- max(400, n_tasks * 20)
  plotOutput("task_completion_plot", height = paste0(plot_height, "px"))
})

output$task_completion_plot <- renderPlot({
  s_data <- student_processed_data()
  req(nrow(s_data) > 0)
  
  ggplot(s_data, aes(x = reorder(Задание, Оценка), y = Оценка, 
                     fill = ifelse(Оценка < 0, "#E15759", "#4E79A7"))) +
    geom_col() +
    geom_text(
      aes(label = ifelse(Оценка < 0, "Не выполнено", round(Оценка, 1))), 
      hjust = -0.1, size = 4
    ) +
    scale_fill_identity() +
    coord_flip() +
    labs(x = NULL, y = "Оценка", title = "Результаты по заданиям") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 12)
    ) +
    scale_y_continuous(
      limits = c(min(s_data$Оценка, na.rm = TRUE) * 1.1, 
                 max(s_data$Оценка, na.rm = TRUE) * 1.2)
    )
})
}

# Запуск приложения
shinyApp(ui, server)