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
library(forcats)
library(ggrepel)
library(plotly)

# Функция для определения группы задания
assign_group <- function(col_name) {
  col_name <- as.character(col_name)
  case_when(
    str_detect(col_name, "Тренинг|самостоятельной") ~ "Тренинг",
    str_detect(col_name, "ДЗ") ~ "ДЗ",
    str_detect(col_name, "Зачет") ~ "Аттестация (Зачет)",
    str_detect(col_name, "Экзамен") ~ "Аттестация (Экзамен)",
    str_detect(col_name, "аттестация") ~ "Аттестация",
    str_detect(col_name, "Контрольная|контрольной") ~ "Контрольные",
    str_detect(col_name, "РАР") ~ "РАР",
    str_starts(col_name, "Тест") ~ "АСР",
    TRUE ~ "Другие задания"
  )
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
      return(data.frame(name = character(), url = character(), stringsAsFactors = FALSE))
    }
    
    data.frame(
      name = sapply(files, `[[`, "name"),
      url = sapply(files, function(x) {
        if (!is.null(x$download_url)) {
          x$download_url
        } else {
          paste0("https://raw.githubusercontent.com/avladova/lms-analytics/MYdata/",
                 "Итог_1/", 
                 URLencode(x$name))
        }
      }),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("Ошибка при получении файлов: ", e$message)
    data.frame(name = character(), url = character(), stringsAsFactors = FALSE)
  })
}

# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(
    title = "Анализ успеваемости студентов (Преподаватель)",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebar",
      menuItem("Загрузка данных", tabName = "data", icon = icon("database")),
      menuItem("Анализ группы", tabName = "group", icon = icon("users")),
      menuItem("Анализ студента", tabName = "student", icon = icon("user-graduate"))
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'data'",
      selectInput("github_file", "Выберите файл с оценками", 
                  choices = NULL, selectize = FALSE),
      actionButton("refresh_files", "Обновить список файлов", icon = icon("sync"))
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'group'",
      selectInput("group_metric", "Метрика для анализа:",
                  choices = c("Средние оценки" = "mean",
                              "Процент выполнения" = "completion",
                              "Распределение оценок" = "distribution"),
                  selected = "mean"),
      sliderInput("grade_range", "Диапазон оценок:",
                  min = 0, max = 100, value = c(0, 100)),
      checkboxInput("show_top_students", "Показать топ студентов", TRUE),
      checkboxInput("show_low_students", "Показать отстающих студентов", TRUE)
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'student'",
      selectInput("student_email", "Выберите студента:", 
                  choices = NULL, selectize = TRUE),
      checkboxInput("compare_to_group", "Сравнить с группой", TRUE),
      checkboxInput("show_missed", "Показать пропущенные задания", TRUE)
    ),
    
    conditionalPanel(
      condition = "output.file_loaded",
      downloadButton("download_report", "Скачать отчет", 
                     class = "btn-primary",
                     style = "width: 100%; margin-top: 20px;")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        box(
          width = 12,
          status = "info",
          h2("Дашборд преподавателя"),
          p("Пожалуйста, выберите файл с оценками из репозитория."),
          p("После выбора файла вы сможете анализировать успеваемость студентов."),
          DTOutput("file_preview")
        )
      ),
      
      tabItem(
        tabName = "group",
        fluidRow(
          valueBoxOutput("total_students", width = 3),
          valueBoxOutput("avg_grade", width = 3),
          valueBoxOutput("pass_rate", width = 3),
          valueBoxOutput("completion_rate", width = 3)
        ),
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
            width = 6,
            plotlyOutput("group_category_plot", height = "500px")
          ),
          box(
            title = "Топ студентов",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_students_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Распределение оценок",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("grade_distribution_plot", height = "400px")
          )
        )
      ),
      
      tabItem(
        tabName = "student",
        fluidRow(
          valueBoxOutput("student_final_grade", width = 4),
          valueBoxOutput("student_rank", width = 4),
          valueBoxOutput("student_completion", width = 4)
        ),
        fluidRow(
          box(
            title = "Информация о студенте",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3(textOutput("student_name")),
            DTOutput("student_grades_table")
          )
        ),
        fluidRow(
          box(
            title = "Прогресс по категориям",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("student_category_plot", height = "300px")
          ),
          box(
            title = "Сравнение с группой",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("student_comparison_plot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "История оценок",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("student_grades_history", height = "400px")
          )
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
    input$refresh_files
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
      showNotification("Не удалось загрузить список файлов или папка пуста", 
                       type = "error", duration = 10)
    }
  })
  
  # Обработка выбора файла
  observeEvent(input$github_file, {
    req(input$github_file)
    
    files <- github_files()
    selected_file <- files[files$name == input$github_file, ]
    req(nrow(selected_file) == 1)
    
    tryCatch({
      showModal(modalDialog("Загрузка и обработка файла...", footer = NULL))
      
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
        mutate(
          Группа = sapply(Задание, assign_group),
          Задание_короткое = str_replace_all(Задание, "\\s*\\([^)]*\\)", ""),
          Задание_короткое = str_trunc(Задание_короткое, 30, "right")
        ) %>%
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
  
  # Предпросмотр файла
  output$file_preview <- renderDT({
    req(data_clean())
    datatable(
      head(data_clean(), 10),
      options = list(scrollX = TRUE, dom = 't'),
      rownames = FALSE
    )
  })
  
  # Информационные блоки для группы
  output$total_students <- renderValueBox({
    req(data_clean())
    valueBox(
      nrow(data_clean()), 
      "Студентов в группе",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_grade <- renderValueBox({
    req(data_clean(), final_grade_col())
    avg <- mean(data_clean()[[final_grade_col()]], na.rm = TRUE)
    valueBox(
      round(avg, 1), 
      "Средняя итоговая оценка",
      icon = icon("calculator"),
      color = ifelse(avg >= 60, "green", "red")
    )
  })
  
  output$pass_rate <- renderValueBox({
    req(data_clean(), final_grade_col())
    rate <- mean(data_clean()[[final_grade_col()]] >= 60, na.rm = TRUE) * 100
    valueBox(
      paste0(round(rate, 1), "%"), 
      "Процент сдачи курса",
      icon = icon("percent"),
      color = ifelse(rate >= 50, "green", "red")
    )
  })
  
  output$completion_rate <- renderValueBox({
    req(processed_data())
    rate <- mean(processed_data()$Оценка >= 0, na.rm = TRUE) * 100
    valueBox(
      paste0(round(rate, 1), "%"), 
      "Процент выполненных заданий",
      icon = icon("check-circle"),
      color = ifelse(rate >= 70, "green", "orange")
    )
  })
  
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
        `Процент сдачи` = mean(.data[[final_grade_col()]] >= 60, na.rm = TRUE) * 100,
        `Стандартное отклонение` = sd(.data[[final_grade_col()]], na.rm = TRUE)
      ) %>%
      mutate(across(where(is.numeric), ~round(., 1)))
    
    datatable(summary_data, 
              options = list(dom = 't', scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle(names(summary_data), fontWeight = 'bold')
  })
  
  # График успеваемости по категориям
  output$group_category_plot <- renderPlotly({
    req(processed_data())
    
    category_data <- processed_data() %>%
      filter(Оценка >= 0) %>% # Исключаем невыполненные задания
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка, na.rm = TRUE),
        Процент_выполнения = mean(!is.na(Оценка)) * 100,
        Количество_заданий = n()
      ) %>%
      filter(!is.na(Группа))
    
    p <- ggplot(category_data, aes(x = reorder(Группа, Средняя_оценка), y = Средняя_оценка,
                                   text = paste0("Категория: ", Группа, "\n",
                                                 "Средняя оценка: ", round(Средняя_оценка, 1), "\n",
                                                 "Процент выполнения: ", round(Процент_выполнения, 1), "%"))) +
      geom_col(aes(fill = Средняя_оценка), alpha = 0.8) +
      geom_text(aes(label = round(Средняя_оценка, 1)), 
                hjust = -0.1, size = 4) +
      coord_flip() +
      labs(x = NULL, y = "Средняя оценка", 
           title = "Успеваемость по категориям заданий") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      ) +
      scale_fill_gradient(low = "#E15759", high = "#59A14F") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Топ студентов
  output$top_students_plot <- renderPlotly({
    req(data_clean(), final_grade_col())
    
    top_students <- data_clean() %>%
      arrange(desc(.data[[final_grade_col()]])) %>%
      slice_head(n = 10) %>%
      mutate(Студент = str_extract(`Адрес электронной почты`, "^[^@]+"),
             Студент = fct_reorder(Студент, .data[[final_grade_col()]]))
    
    p <- ggplot(top_students, aes(x = Студент, y = .data[[final_grade_col()]],
                                  text = paste0("Студент: ", Студент, "\n",
                                                "Оценка: ", round(.data[[final_grade_col()]], 1)))) +
      geom_col(aes(fill = .data[[final_grade_col()]])) +
      coord_flip() +
      labs(x = NULL, y = "Итоговая оценка", title = "Топ студентов") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#F28E2B", high = "#59A14F")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Распределение оценок
  output$grade_distribution_plot <- renderPlotly({
    req(data_clean(), final_grade_col())
    
    grades <- data_clean()[[final_grade_col()]]
    grades <- grades[!is.na(grades)]
    
    p <- ggplot(data.frame(Оценка = grades), aes(x = Оценка)) +
      geom_histogram(binwidth = 5, fill = "#76B7B2", color = "white") +
      geom_vline(xintercept = mean(grades), color = "#E15759", linetype = "dashed", size = 1) +
      annotate("text", x = mean(grades), y = 0, 
               label = paste("Среднее:", round(mean(grades), 1)), 
               vjust = -1, hjust = -0.1, color = "#E15759") +
      labs(x = "Итоговая оценка", y = "Количество студентов",
           title = "Распределение итоговых оценок") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
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
      filter(`Адрес электронной почты` == input$student_email)
  })
  
  # Информационные блоки для студента
  output$student_final_grade <- renderValueBox({
    req(student_data(), final_grade_col())
    final_grade <- student_data()[[final_grade_col()]][1]
    valueBox(
      ifelse(is.na(final_grade), "нет данных", final_grade),
      "Итоговая оценка",
      icon = icon("star"),
      color = ifelse(final_grade >= 60, "green", "red")
    )
  })
  
  output$student_rank <- renderValueBox({
    req(data_clean(), student_data(), final_grade_col())
    grade <- student_data()[[final_grade_col()]][1]
    if (is.na(grade)) {
      return(valueBox("N/A", "Ранг в группе", icon = icon("sort"), color = "blue"))
    }
    
    ranks <- data_clean() %>%
      mutate(Rank = rank(-.data[[final_grade_col()]], ties.method = "min", na.last = "keep")) %>%
      filter(!is.na(.data[[final_grade_col()]]))
    
    student_rank <- ranks %>% 
      filter(`Адрес электронной почты` == input$student_email) %>% 
      pull(Rank)
    
    total <- nrow(ranks)
    
    valueBox(
      paste0(student_rank, " из ", total),
      "Ранг в группе",
      icon = icon("trophy"),
      color = ifelse(student_rank <= total * 0.2, "green", 
                     ifelse(student_rank <= total * 0.5, "yellow", "red"))
    )
  })
  
  output$student_completion <- renderValueBox({
    req(student_processed_data())
    rate <- mean(student_processed_data()$Оценка >= 0, na.rm = TRUE) * 100
    valueBox(
      paste0(round(rate, 1), "%"), 
      "Выполнено заданий",
      icon = icon("tasks"),
      color = ifelse(rate >= 80, "green", 
                     ifelse(rate >= 50, "yellow", "red"))
    )
  })
  
  # Информация о студенте
  output$student_name <- renderText({
    email <- input$student_email
    name <- str_extract(email, "^[^@]+")
    paste("Студент:", name)
  })
  
  # Таблица оценок студента
  output$student_grades_table <- renderDT({
    student <- student_processed_data()
    req(nrow(student) > 0)
    
    grades_table <- student %>%
      select(Задание, Оценка, Группа) %>%
      mutate(
        Статус = case_when(
          Оценка >= 80 ~ "Отлично",
          Оценка >= 60 ~ "Хорошо",
          Оценка >= 0 ~ "Удовлетворительно",
          TRUE ~ "Не выполнено"
        )
      ) %>%
      arrange(Группа, desc(Оценка))
    
    datatable(
      grades_table,
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      extensions = 'Buttons'
    ) %>%
      formatStyle("Оценка", 
                  backgroundColor = styleInterval(c(60, 80), c("#FF9999", "#FFFF99", "#99FF99"))) %>%
      formatStyle("Статус",
                  backgroundColor = styleEqual(
                    c("Не выполнено", "Удовлетворительно", "Хорошо", "Отлично"),
                    c("#FF9999", "#FFFF99", "#99FF99", "#339933")
                  ))
  })
  
  # График успеваемости по категориям для студента
  output$student_category_plot <- renderPlotly({
    s_data <- student_processed_data()
    req(nrow(s_data) > 0)
    
    category_summary <- s_data %>%
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка[Оценка >= 0], na.rm = TRUE),
        Процент_выполнения = mean(Оценка >= 0, na.rm = TRUE) * 100,
        .groups = 'drop'
      ) %>%
      filter(!is.na(Группа))
    
    p <- ggplot(category_summary, aes(x = reorder(Группа, Средняя_оценка), y = Средняя_оценка,
                                      text = paste0("Категория: ", Группа, "\n",
                                                    "Средняя оценка: ", round(Средняя_оценка, 1), "\n",
                                                    "Выполнено: ", round(Процент_выполнения, 1), "%"))) +
      geom_col(aes(fill = Средняя_оценка), alpha = 0.8) +
      geom_text(aes(label = paste0(round(Средняя_оценка, 1))), 
                hjust = -0.1, size = 4) +
      coord_flip() +
      labs(x = NULL, y = "Средняя оценка", 
           title = "Успеваемость по категориям") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      ) +
      scale_fill_gradient(low = "#F28E2B", high = "#59A14F") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Сравнение студента с группой
  output$student_comparison_plot <- renderPlotly({
    req(input$compare_to_group, student_processed_data(), processed_data())
    
    # Данные студента
    student_categories <- student_processed_data() %>%
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка[Оценка >= 0], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Группа)) %>%
      mutate(Тип = "Студент")
    
    # Данные группы
    group_categories <- processed_data() %>%
      filter(Оценка >= 0) %>%
      group_by(Группа) %>%
      summarise(
        Средняя_оценка = mean(Оценка, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Группа)) %>%
      mutate(Тип = "Группа")
    
    # Объединение данных
    comparison_data <- bind_rows(student_categories, group_categories)
    
    p <- ggplot(comparison_data, aes(x = Группа, y = Средняя_оценка, fill = Тип,
                                     text = paste0("Категория: ", Группа, "\n",
                                                   "Тип: ", Тип, "\n",
                                                   "Оценка: ", round(Средняя_оценка, 1)))) +
      geom_col(position = position_dodge(), width = 0.7) +
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
    
    ggplotly(p, tooltip = "text")
  })
  
  # История оценок студента
  output$student_grades_history <- renderPlotly({
    s_data <- student_processed_data()
    req(nrow(s_data) > 0)
    
    # Добавляем порядковый номер задания для оси X
    s_data <- s_data %>%
      mutate(Номер_задания = row_number())
    
    p <- ggplot(s_data, aes(x = Номер_задания, y = Оценка, 
                            color = Группа,
                            text = paste0("Задание: ", Задание_короткое, "\n",
                                          "Оценка: ", ifelse(Оценка < 0, "Не выполнено", Оценка), "\n",
                                          "Категория: ", Группа))) +
      geom_point(size = 3) +
      geom_line(aes(group = 1), color = "gray", linetype = "dashed") +
      geom_hline(yintercept = 60, color = "#59A14F", linetype = "dashed") +
      geom_hline(yintercept = 80, color = "#4E79A7", linetype = "dashed") +
      labs(x = "Порядковый номер задания", y = "Оценка", 
           title = "История оценок студента") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_color_brewer(palette = "Set2") +
      scale_y_continuous(limits = c(min(s_data$Оценка, na.rm = TRUE) * 1.1, 
                                    max(s_data$Оценка, na.rm = TRUE) * 1.1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Скачивание отчета
  output$download_report <- downloadHandler(
    filename = function() {
      if (input$sidebar == "group") {
        paste("group_report_", Sys.Date(), ".pdf", sep = "")
      } else {
        student_name <- str_extract(input$student_email, "^[^@]+")
        paste("student_report_", student_name, "_", Sys.Date(), ".pdf", sep = "")
      }
    },
    content = function(file) {
      # Создание PDF отчета
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      if (input$sidebar == "group") {
        file.copy("group_report.Rmd", tempReport, overwrite = TRUE)
        params <- list(
          group_data = data_clean(),
          processed_data = processed_data(),
          final_grade_col = final_grade_col()
        )
      } else {
        file.copy("student_report.Rmd", tempReport, overwrite = TRUE)
        params <- list(
          student_data = student_data(),
          student_processed = student_processed_data(),
          group_data = data_clean(),
          final_grade_col = final_grade_col(),
          student_email = input$student_email
        )
      }
      
      withProgress({
        setProgress(message = "Генерация отчета...")
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
      })
    }
  )
}

# Запуск приложения
shinyApp(ui, server)