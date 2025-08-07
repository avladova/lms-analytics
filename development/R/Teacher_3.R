library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(stringr)
library(httr)
library(tibble)

# Функция для определения группы задания
assign_group <- function(col_name) {
  col_name <- as.character(col_name)
  if (str_detect(col_name, "Тренинг|самостоятельной")) {
    return("Тренинг")
  } else if (str_detect(col_name, "ДЗ")) {
    return("ДЗ")
  } else if (str_detect(col_name, "ДСР")) {
    return("ДСР")
  } else if (str_detect(col_name, "аттестация|экзамен|зачет")) {
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
                choices = NULL, selectize = FALSE)
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Анализ группы",
               fluidRow(
                 box(
                   width = 12,
                   status = "info",
                   h2("Анализ группы"),
                   p("Здесь отображается анализ успеваемости всей группы.")
                 )
               ),
               fluidRow(
                 box(
                   title = "Распределение итоговых оценок",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 6,
                   plotOutput("final_grades_distribution")
                 ),
                 box(
                   title = "Статистика по категориям оценок",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 6,
                   tableOutput("grades_summary_table")
                 )
               ),
               fluidRow(
                 box(
                   title = "Тепловая карта корреляций",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   plotOutput("correlation_heatmap", height = "600px"),
                   p("Корреляции между группами заданий (без повторений и самокорреляций)")
                 )
               )
      ),
      tabPanel("Анализ отдельного студента",
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
                 condition = "output.file_loaded",
                 fluidRow(
                   box(
                     width = 12,
                     status = "info",
                     h3("Файл успешно загружен"),
                     selectInput("student_email", "Выберите студента:", 
                                 choices = NULL, selectize = FALSE),
                     tags$hr(),
                     helpText("Выберите студента из списка, чтобы увидеть его результаты.")
                   )
                 )
               ),
               conditionalPanel(
                 condition = "output.file_loaded && input.student_email != ''",
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
  )
)

# Server часть приложения
server <- function(input, output, session) {
  data_clean <- reactiveVal(NULL)
  final_grade_col <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  assignments_with_grades <- reactiveVal(NULL)
  
  # Загружаем список файлов
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
      
      temp_file <- tempfile(fileext = ".xlsx")
      download_result <- GET(
        URLencode(selected_file$url),
        write_disk(temp_file, overwrite = TRUE),
        add_headers("User-Agent" = "Mozilla/5.0"),
        progress()
      )
      
      data <- readxl::read_excel(temp_file, sheet = "Оценки")
      
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
      
      assignments_with_data <- grade_cols[sapply(grade_cols, function(col) {
        any(!is.na(clean_data[[col]]))
      })]
      
      assignments_with_grades(assignments_with_data)
      data_clean(clean_data)
      final_grade_col(grade_col)
      
      updateSelectInput(session, "student_email", 
                        choices = c("", clean_data$`Адрес электронной почты`),
                        selected = NULL)
      
      processed <- clean_data %>%
        pivot_longer(
          cols = all_of(assignments_with_data),
          names_to = "Задание",
          values_to = "Оценка"
        ) %>%
        mutate(Группа = sapply(Задание, assign_group)) %>%
        mutate(Оценка = case_when(
          is.na(Оценка) & Группа %in% c("Тренинг", "ДЗ", "ДСР", "АСР") ~ -1,
          is.na(Оценка) & Группа == "Аттестация" ~ -60,
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
  
  categorize_final_grade <- function(grade) {
    case_when(
      is.na(grade) ~ "Нет данных",
      grade < 50 ~ "Не сдано",
      grade >= 50 & grade < 70 ~ "Удовлетворительно",
      grade >= 70 & grade < 86 ~ "Хорошо",
      grade >= 86 ~ "Отлично"
    )
  }
  
  output$final_grades_distribution <- renderPlot({
    req(data_clean())
    
    grades_data <- data_clean() %>%
      mutate(
        FinalGrade = as.numeric(get(final_grade_col())),
        GradeCategory = factor(
          categorize_final_grade(FinalGrade),
          levels = c("Нет данных", "Не сдано", "Удовлетворительно", "Хорошо", "Отлично"),
          ordered = TRUE
        )
      ) %>%
      count(GradeCategory) %>%
      mutate(Percentage = n / sum(n) * 100)
    
    ggplot(grades_data, aes(x = GradeCategory, y = n, fill = GradeCategory)) +
      geom_col() +
      geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
                vjust = -0.5, size = 5) +
      scale_fill_manual(values = c(
        "Отлично" = "#2E8B57",
        "Хорошо" = "#3A7CA5",
        "Удовлетворительно" = "#F39C12",
        "Не сдано" = "#E74C3C",
        "Нет данных" = "#95A5A6"
      )) +
      labs(x = "Категория оценки", y = "Количество студентов") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$grades_summary_table <- renderTable({
    req(data_clean())
    
    data_clean() %>%
      summarise(
        `Средний балл` = round(mean(as.numeric(get(final_grade_col())), na.rm = TRUE), 1),
        `Медианный балл` = round(median(as.numeric(get(final_grade_col())), na.rm = TRUE), 1),
        `Минимальный балл` = min(as.numeric(get(final_grade_col())), na.rm = TRUE),
        `Максимальный балл` = max(as.numeric(get(final_grade_col())), na.rm = TRUE),
        `Стандартное отклонение` = round(sd(as.numeric(get(final_grade_col())), na.rm = TRUE), 1),
        `Количество студентов` = n()
      ) %>%
      pivot_longer(everything(), names_to = "Показатель", values_to = "Значение")
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "lr")
  
  prepare_correlation_data <- function(data) {
    req(data)
    
    grouped_data <- data %>%
      pivot_longer(
        cols = -c(`Адрес электронной почты`, final_grade_col()),
        names_to = "Задание",
        values_to = "Оценка"
      ) %>%
      mutate(
        Группа = sapply(Задание, assign_group),
        Оценка = as.numeric(Оценка)
      ) %>%
      filter(!is.na(Группа), !is.na(Оценка)) %>%
      group_by(`Адрес электронной почты`, Группа) %>%
      summarise(Средняя_оценка = mean(Оценка, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(
        names_from = Группа,
        values_from = Средняя_оценка
      )
    
    grouped_data <- grouped_data %>%
      select(where(~sum(!is.na(.)) > 0))
    
    cor_matrix <- cor(grouped_data %>% select(-`Адрес электронной почты`), 
                      use = "pairwise.complete.obs")
    
    # Преобразуем в длинный формат и удаляем лишние корреляции
    cor_df <- as.data.frame(cor_matrix) %>%
      rownames_to_column("Var1") %>%
      pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>%
      # Удаляем диагональ (корреляция категории с самой собой)
      filter(Var1 != Var2) %>%
      # Удаляем дубликаты (A-B и B-A оставляем только одну)
      mutate(
        Var1 = as.character(Var1),
        Var2 = as.character(Var2),
        pair = ifelse(Var1 < Var2, paste(Var1, Var2), paste(Var2, Var1))
      ) %>%
      distinct(pair, .keep_all = TRUE) %>%
      select(-pair) %>%
      mutate(
        Var1 = factor(Var1, levels = unique(c(Var1, Var2))),
        Var2 = factor(Var2, levels = unique(c(Var1, Var2)))
      )
    
    return(cor_df)
  }
  
  output$correlation_heatmap <- renderPlot({
    req(data_clean())
    
    cor_data <- prepare_correlation_data(data_clean())
    req(nrow(cor_data) > 0)
    
    # Определяем порядок отображения (ДСР в начале)
    level_order <- unique(c("ДСР", sort(levels(cor_data$Var1))))
    
    ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Correlation, 2)), color = "black", size = 5) +
      scale_fill_gradient2(
        low = "#E15759", 
        mid = "white", 
        high = "#4E79A7",
        midpoint = 0,
        limit = c(-1, 1),
        space = "Lab",
        name = "Корреляция"
      ) +
      labs(
        x = NULL,
        y = NULL,
        title = "Корреляции между группами заданий",
        subtitle = "Исключены повторяющиеся и самокорреляции"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      coord_fixed()
  })
  
  student_data <- reactive({
    req(input$student_email, input$student_email != "", data_clean())
    data_clean() %>% 
      filter(`Адрес электронной почты` == input$student_email)
  })
  
  student_processed_data <- reactive({
    req(input$student_email, input$student_email != "", processed_data())
    processed_data() %>% 
      filter(`Адрес электронной почты` == input$student_email) %>%
      mutate(Задание = str_replace_all(Задание, "\\s*\\([^)]*\\)", ""))
  })
  
  prepare_category_data <- function(data) {
    data %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col())) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      filter(!is.na(Оценка)) %>%
      mutate(
        Категория = case_when(
          grepl("аттестация|экзамен|зачет", Тест, ignore.case = TRUE) ~ "Аттестация",
          grepl("контрольная", Тест, ignore.case = TRUE) ~ "Контрольные работы",
          grepl("лекционный", Тест, ignore.case = TRUE) ~ "Лекционные опросы",
          grepl("ДСР", Тест) ~ "Домашние самостоятельные работы",
          grepl("ДЗ", Тест) ~ "Домашние задания",
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
    if (nrow(student) == 0) return("Студент не найден.")
    
    final_grade <- student[[final_grade_col()]][1]
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade))
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
      labs(x = NULL, y = "Сумма оценок") +
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
    
    student_categories <- prepare_category_data(student) %>%
      mutate(Тип = "Студент")
    
    group_categories <- data_clean() %>%
      prepare_category_data() %>%
      group_by(Категория) %>%
      summarise(
        Средняя_оценка = mean(Средняя_оценка, na.rm = TRUE),
        Количество_заданий = mean(Количество_заданий, na.rm = TRUE),
        Суммарная_оценка = mean(Суммарная_оценка, na.rm = TRUE)
      ) %>%
      mutate(Тип = "Группа")
    
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
      labs(x = NULL, y = "Средняя оценка", fill = NULL) +
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
      labs(x = NULL, y = "Оценка") +
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