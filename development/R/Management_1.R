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
library(forcats)
library(purrr)

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

# Функция для категоризации итоговой оценки
categorize_final_grade <- function(grade) {
  case_when(
    is.na(grade) ~ "Нет данных",
    grade < 50 ~ "Не сдано",
    grade >= 50 & grade < 70 ~ "Удовлетворительно",
    grade >= 70 & grade < 86 ~ "Хорошо",
    grade >= 86 ~ "Отлично"
  )
}

# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(title = "Сравнение успеваемости групп"),
  dashboardSidebar(
    selectInput("github_files", "Выберите файлы групп", 
                choices = NULL, multiple = TRUE),
    actionButton("compare_btn", "Сравнить", icon = icon("chart-bar")),
    actionButton("clear_btn", "Очистить выбор", icon = icon("broom"))
  ),
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        status = "info",
        h2("Сравнение успеваемости групп"),
        p("Выберите файлы с оценками для сравнения и нажмите кнопку 'Сравнить'.")
      )
    ),
    fluidRow(
      box(
        title = "Распределение итоговых оценок",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput("group_comparison_distribution")
      )
    ),
    fluidRow(
      box(
        title = "Средние оценки по категориям заданий",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput("category_comparison_plot", height = "500px")
      )
    ),
    fluidRow(
      box(
        title = "Статистика по группам",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        DTOutput("group_stats_table")
      )
    )
  )
)

# Server часть приложения
server <- function(input, output, session) {
  all_data <- reactiveVal(list())
  
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
      updateSelectInput(session, "github_files", 
                        choices = files$name,
                        selected = NULL)
    } else {
      showNotification("Не удалось загрузить список файлов или папка пуста", type = "error")
    }
  })
  
  # Обработка нажатия кнопки очистки
  observeEvent(input$clear_btn, {
    updateSelectInput(session, "github_files", selected = character(0))
    all_data(list())
  })
  
  # Обработка нажатия кнопки сравнения
  observeEvent(input$compare_btn, {
    req(input$github_files)
    
    files <- github_files()
    selected_files <- files[files$name %in% input$github_files, ]
    req(nrow(selected_files) > 0)
    
    tryCatch({
      showModal(modalDialog("Загрузка и обработка файлов...", footer = NULL))
      
      data_list <- list()
      
      for (i in seq_len(nrow(selected_files))) {
        file_info <- selected_files[i, ]
        temp_file <- tempfile(fileext = ".xlsx")
        
        download_result <- GET(
          URLencode(file_info$url),
          write_disk(temp_file, overwrite = TRUE),
          add_headers("User-Agent" = "Mozilla/5.0"),
          progress()
        )
        
        data <- readxl::read_excel(temp_file, sheet = "Оценки")
        
        # Извлекаем название группы из имени файла
        group_name <- tools::file_path_sans_ext(file_info$name)
        group_name <- gsub("^Группа_", "", group_name)
        
        # Очистка данных
        clean_data <- data %>%
          select(where(~!all(is.na(.)))) %>%
          select(-matches("Участник групп")) %>%
          filter(!is.na(`Адрес электронной почты`)) %>%
          select(-last_col()) %>%
          mutate(Группа = group_name)
        
        grade_col <- "Итоговая оценка за курс (Значение)"
        grade_cols <- setdiff(names(clean_data), c("Адрес электронной почты", grade_col, "Группа"))
        
        clean_data <- clean_data %>%
          mutate(across(all_of(grade_cols), ~ ifelse(. == "-", NA, as.numeric(.))))
        
        data_list[[group_name]] <- clean_data
      }
      
      all_data(data_list)
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Ошибка:", e$message), type = "error", duration = 10)
    })
  })
  
  # Подготовка данных для сравнения распределения оценок (обновленная версия)
  prepare_distribution_data <- function(data_list) {
    # Все возможные категории оценок
    all_categories <- c("Нет данных", "Не сдано", "Удовлетворительно", "Хорошо", "Отлично")
    
    map_dfr(data_list, function(df) {
      # Создаем полный набор категорий для текущей группы
      group_name <- unique(df$Группа)
      full_categories <- expand.grid(
        Группа = group_name,
        GradeCategory = factor(all_categories, levels = all_categories, ordered = TRUE),
        stringsAsFactors = FALSE
      )
      
      # Считаем реальное распределение
      real_counts <- df %>%
        mutate(
          FinalGrade = as.numeric(`Итоговая оценка за курс (Значение)`),
          GradeCategory = factor(
            categorize_final_grade(FinalGrade),
            levels = all_categories,
            ordered = TRUE
          )
        ) %>%
        count(Группа, GradeCategory, .drop = FALSE)
      
      # Объединяем с полным набором категорий
      full_counts <- full_categories %>%
        left_join(real_counts, by = c("Группа", "GradeCategory")) %>%
        mutate(
          n = ifelse(is.na(n), 0, n),
          Percentage = n / sum(n) * 100
        )
      
      return(full_counts)
    })
  }
  
  # График сравнения распределения оценок
  output$group_comparison_distribution <- renderPlot({
    req(length(all_data()) > 0)
    
    dist_data <- prepare_distribution_data(all_data())
    
    ggplot(dist_data, aes(x = GradeCategory, y = Percentage, fill = Группа)) +
      geom_col(position = position_dodge(preserve = "single"), alpha = 0.8) +
      geom_text(
        aes(label = paste0(round(Percentage, 1), "%")), 
        position = position_dodge(width = 0.9),
        vjust = -0.5, size = 4
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Категория оценки",
        y = "Процент студентов",
        fill = "Группа"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0))
  })
  
  # Подготовка данных для сравнения по категориям заданий
  prepare_category_comparison_data <- function(data_list) {
    map_dfr(data_list, function(df) {
      grade_col <- "Итоговая оценка за курс (Значение)"
      grade_cols <- setdiff(names(df), c("Адрес электронной почты", grade_col, "Группа"))
      
      df %>%
        pivot_longer(
          cols = all_of(grade_cols),
          names_to = "Задание",
          values_to = "Оценка"
        ) %>%
        mutate(
          Категория = sapply(Задание, assign_group),
          Оценка = as.numeric(Оценка)
        ) %>%
        filter(!is.na(Категория), !is.na(Оценка)) %>%
        group_by(Группа, Категория) %>%
        summarise(
          Средняя_оценка = mean(Оценка, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }
  
  # График сравнения по категориям заданий
  output$category_comparison_plot <- renderPlot({
    req(length(all_data()) > 0)
    
    cat_data <- prepare_category_comparison_data(all_data())
    
    ggplot(cat_data, aes(x = Категория, y = Средняя_оценка, fill = Группа)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = round(Средняя_оценка, 1)), 
        position = position_dodge(width = 0.8),
        vjust = -0.5, size = 4
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Категория заданий",
        y = "Средняя оценка",
        fill = "Группа"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  # Подготовка статистики по группам
  prepare_group_stats <- function(data_list) {
    map_dfr(data_list, function(df) {
      df %>%
        summarise(
          `Количество студентов` = n(),
          `Средний балл` = round(mean(as.numeric(`Итоговая оценка за курс (Значение)`), na.rm = TRUE), 1),
          `Медианный балл` = round(median(as.numeric(`Итоговая оценка за курс (Значение)`), na.rm = TRUE), 1),
          `Минимальный балл` = min(as.numeric(`Итоговая оценка за курс (Значение)`), na.rm = TRUE),
          `Максимальный балл` = max(as.numeric(`Итоговая оценка за курс (Значение)`), na.rm = TRUE),
          `Процент сдавших` = round(mean(as.numeric(`Итоговая оценка за курс (Значение)`) >= 50, na.rm = TRUE) * 100, 1),
          .groups = "drop"
        ) %>%
        mutate(Группа = unique(df$Группа)) %>%
        select(Группа, everything())
    })
  }
  
  # Таблица статистики по группам
  output$group_stats_table <- renderDT({
    req(length(all_data()) > 0)
    
    stats_data <- prepare_group_stats(all_data())
    
    datatable(
      stats_data,
      rownames = FALSE,
      options = list(
        dom = 't',
        pageLength = nrow(stats_data),
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe'
    ) %>%
      formatStyle(names(stats_data), backgroundColor = 'white')
  })
}

# Запуск приложения
shinyApp(ui, server)