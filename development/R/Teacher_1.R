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
  dashboardHeader(title = "Анализ успеваемости студентов (Преподаватель)"),
  dashboardSidebar(
    selectInput("github_file", "Выберите файл с оценками", 
                choices = NULL, selectize = FALSE),
    
    conditionalPanel(
      condition = "output.file_loaded",
      selectInput("view_type", "Тип анализа:",
                  choices = c("Общая статистика группы" = "group",
                              "Анализ отдельного студента" = "student"),
                  selected = "group"),
      
      conditionalPanel(
        condition = "input.view_type == 'student'",
        selectInput("student_email", "Выберите студента:", 
                    choices = NULL, selectize = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.view_type == 'group'",
        selectInput("group_metric", "Метрика для анализа:",
                    choices = c("Средние оценки" = "mean",
                                "Процент выполнения" = "completion",
                                "Распределение оценок" = "distribution"),
                    selected = "mean"),
        
        sliderInput("grade_range", "Диапазон оценок:",
                    min = 0, max = 100, value = c(0, 100))
      ),
      
      downloadButton("download_report", "Скачать отчет")
    )
  ),
  dashboardBody(
    conditionalPanel(
      condition = "!output.file_loaded",
      box(
        width = 12,
        status = "info",
        h2("Дашборд преподавателя"),
        p("Пожалуйста, выберите файл с оценками из репозитория."),
        p("После выбора файла вы сможете анализировать успеваемость студентов.")
      )
    ),
    
    conditionalPanel(
      condition = "output.file_loaded && input.view_type == 'group'",
      fluidRow(
        box(
          title = "Общая статистика по группе",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("group_summary_table")
        )
      ),
      fluidRow(
        box(
          title = "График успеваемости по категориям",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          plotOutput("group_category_plot", height = "500px")
        )
      ),
      fluidRow(
        box(
          title = "Распределение оценок",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          plotOutput("grade_distribution_plot", height = "400px")
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.file_loaded && input.view_type == 'student'",
      fluidRow(
        box(
          title = "Информация о студенте",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h3(textOutput("student_name")),
          h4(textOutput("student_final_grade")),
          DTOutput("student_grades_table")
        )
      ),
      fluidRow(
        box(
          title = "Прогресс по категориям",
          status = "success",
          solidHeader = TRUE,
          width = 6,
          plotOutput("student_category_plot", height = "300px")
        ),
        box(
          title = "Сравнение с группой",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          plotOutput("student_comparison_plot", height = "300px")
        )
      ),
      fluidRow(
        box(
          title = "История оценок",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotOutput("student_grades_history", height = "400px")
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
      
      # Обновляем список студентов
      updateSelectInput(session, "student_email",
                        choices = clean_data$`Адрес электронной почты`)
      
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
        Суммарная_оценка = sum(Оценка, na.rm = TRUE),
        Процент_выполнения = mean(Оценка >= 0, na.rm = TRUE) * 100
      ) %>%
      arrange(desc(Суммарная_оценка))
  }
  
  # Общая статистика по группе
  output$group_summary_table <- renderDT({
    req(data_clean())
    
    summary_data <- data_clean() %>%
      summarise(
        `Количество студентов` = n(),
        `Средняя итоговая оценка` = mean(.data[[final_grade_col()]], na.rm = TRUE),
        `Медианная итоговая оценка` = median(.data[[final_grade_col()]], na.rm = TRUE),
        `Минимальная оценка` = min(.data[[final_grade_col()]], na.rm = TRUE),
        `Максимальная оценка` = max(.data[[final_grade_col()]], na.rm = TRUE),
        `Процент сдачи` = mean(.data[[final_grade_col()]] >= 60, na.rm = TRUE) * 100
      ) %>%
      mutate(across(where(is.numeric), ~round(., 1)))
    
    datatable(summary_data, 
              options = list(dom = 't', scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle(names(summary_data), fontWeight = 'bold')
  })
  
  # График успеваемости по категориям
  output$group_category_plot <- renderPlot({
    req(processed_data())
    
    category_data <- processed_data() %>%
      filter(Оценка >= 0) %>% # Исключаем невыполненные задания
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка, na.rm = TRUE),
        Процент_выполнения = mean(!is.na(Оценка)) * 100,
        Количество_студентов = n_distinct(`Адрес электронной почты`)
      ) %>%
      filter(!is.na(Группа))
    
    ggplot(category_data, aes(x = reorder(Группа, Средняя_оценка), y = Средняя_оценка)) +
      geom_col(fill = "#4E79A7", alpha = 0.8) +
      geom_text(aes(label = round(Средняя_оценка, 1)), 
                hjust = -0.1, size = 5, color = "black") +
      coord_flip() +
      labs(x = NULL, y = "Средняя оценка", 
           title = "Успеваемость по категориям заданий") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  # Распределение оценок
  output$grade_distribution_plot <- renderPlot({
    req(data_clean())
    
    grades <- data_clean()[[final_grade_col()]]
    grades <- grades[!is.na(grades)]
    
    ggplot(data.frame(Оценка = grades), aes(x = Оценка)) +
      geom_histogram(binwidth = 5, fill = "#76B7B2", color = "white") +
      geom_vline(xintercept = mean(grades), color = "#E15759", linetype = "dashed", size = 1) +
      annotate("text", x = mean(grades), y = 0, 
               label = paste("Среднее:", round(mean(grades), 1)), 
               vjust = -1, hjust = -0.1, color = "#E15759") +
      labs(x = "Итоговая оценка", y = "Количество студентов",
           title = "Распределение итоговых оценок") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Данные выбранного студента
  student_data <- reactive({
    req(input$student_email, data_clean())
    data_clean() %>% 
      filter(`Адрес электронной почты` == input$student_email)
  })
  
  student_processed_data <- reactive({
    req(input$student_email, processed_data())
    processed_data() %>% 
      filter(`Адрес электронной почты` == input$student_email) %>%
      mutate(Задание = str_replace_all(Задание, "\\s*\\([^)]*\\)", "")) # Удаляем текст в скобках
  })
  
  # Информация о студенте
  output$student_name <- renderText({
    paste("Студент:", input$student_email)
  })
  
  output$student_final_grade <- renderText({
    student <- student_data()
    if (nrow(student) == 0) return("Данные не найдены")
    
    final_grade <- student[[final_grade_col()]][1]
    group_avg <- mean(data_clean()[[final_grade_col()]], na.rm = TRUE)
    
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade),
           " (Среднее по группе: ", round(group_avg, 1), ")")
  })
  
  # Таблица оценок студента
  output$student_grades_table <- renderDT({
    student <- student_data()
    req(nrow(student) > 0)
    
    grades_table <- student %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col())) %>%
      pivot_longer(everything(), names_to = "Задание", values_to = "Оценка") %>%
      mutate(
        Категория = sapply(Задание, assign_group),
        Задание = str_replace_all(Задание, "\\s*\\([^)]*\\)", "")
      ) %>%
      arrange(Категория, Задание)
    
    datatable(grades_table, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle("Оценка", 
                  backgroundColor = styleInterval(c(60, 80), c("#FF9999", "#FFFF99", "#99FF99")))
  })
  
  # График успеваемости по категориям для студента
  output$student_category_plot <- renderPlot({
    s_data <- student_processed_data()
    req(nrow(s_data) > 0)
    
    category_summary <- s_data %>%
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка[Оценка >= 0], na.rm = TRUE),
        Процент_выполнения = mean(Оценка >= 0, na.rm = TRUE) * 100
      ) %>%
      filter(!is.na(Группа))
    
    ggplot(category_summary, aes(x = reorder(Группа, Средняя_оценка), y = Средняя_оценка)) +
      geom_col(fill = "#E15759", alpha = 0.8) +
      geom_text(aes(label = paste0(round(Средняя_оценка, 1), "\n(", round(Процент_выполнения), "%)")), 
                hjust = -0.1, size = 4, color = "black") +
      coord_flip() +
      labs(x = NULL, y = "Средняя оценка", 
           title = "Успеваемость по категориям") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  })
  
  # Сравнение студента с группой
  output$student_comparison_plot <- renderPlot({
    s_data <- student_processed_data()
    req(nrow(s_data) > 0)
    
    # Данные студента
    student_categories <- s_data %>%
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка[Оценка >= 0], na.rm = TRUE),
        Тип = "Студент"
      ) %>%
      filter(!is.na(Группа))
    
    # Данные группы
    group_categories <- processed_data() %>%
      filter(Оценка >= 0) %>%
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка, na.rm = TRUE),
        Тип = "Группа"
      ) %>%
      filter(!is.na(Группа))
    
    # Объединение данных
    comparison_data <- bind_rows(student_categories, group_categories)
    
    ggplot(comparison_data, aes(x = Группа, y = Средняя_оценка, fill = Тип)) +
      geom_col(position = position_dodge(), width = 0.7) +
      geom_text(
        aes(label = round(Средняя_оценка, 1)),
        position = position_dodge(width = 0.7),
        vjust = -0.5, size = 4
      ) +
      scale_fill_manual(values = c("Студент" = "#E15759", "Группа" = "#76B7B2")) +
      labs(x = NULL, y = "Средняя оценка", fill = NULL,
           title = "Сравнение с группой") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  # История оценок студента
  output$student_grades_history <- renderPlot({
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
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(
        limits = c(min(s_data$Оценка, na.rm = TRUE) * 1.1, 
                   max(s_data$Оценка, na.rm = TRUE) * 1.2)
      )
  })
  
  # Скачивание отчета
  output$download_report <- downloadHandler(
    filename = function() {
      paste("student_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Создание PDF отчета
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        student_data = student_data(),
        student_processed = student_processed_data(),
        group_data = data_clean(),
        final_grade_col = final_grade_col()
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

# Запуск приложения
shinyApp(ui, server)