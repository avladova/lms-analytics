library(shinydashboard)
library(shiny)
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
library(plotly)
library(shinycssloaders)
library(shinyWidgets)

# Функция для определения группы задания
assign_group <- function(col_name) {
  col_name <- as.character(col_name)
  if (str_detect(col_name, "Тренинг|самостоятельной")) {
    return("Тренинг")
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
  dashboardHeader(
    title = span(tagList(icon("graduation-cap"), "Аналитика успеваемости")),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Сравнение групп", tabName = "comparison", icon = icon("chart-bar")),
      menuItem("О программе", tabName = "about", icon = icon("info-circle"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'comparison'",
      pickerInput(
        "github_files", 
        "Выберите группы для сравнения", 
        choices = NULL, 
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 3",
          `count-selected-text` = "{0} групп выбрано",
          `live-search` = TRUE
        )
      ),
      div(style = "display: flex; gap: 10px; margin-top: 15px;",
          actionButton("compare_btn", "Сравнить", 
                       icon = icon("chart-simple"), 
                       style = "color: #fff; background-color: #4CAF50; border: none; border-radius: 4px; padding: 8px 16px; font-weight: 500;"),
          
          actionButton("clear_btn", "Очистить", 
                       icon = icon("trash-can"), 
                       style = "color: #fff; background-color: #f44336; border: none; border-radius: 4px; padding: 8px 16px; font-weight: 500;")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        .main-header .logo { font-weight: bold; }
        .box-title { font-weight: bold; }
        .info-box { cursor: pointer; }
        .info-box:hover { opacity: 0.9; }
        .small-box { border-radius: 5px; }
        .highcharts-container { border-radius: 5px; }
        .content-wrapper { background-color: #f4f6f9; }
        
        /* Стили для минималистичных кнопок */
        #compare_btn, #clear_btn {
          transition: all 0.2s ease;
          cursor: pointer;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
        }
        
        #compare_btn:hover {
          background-color: #45a049 !important;
          transform: translateY(-1px);
          box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        
        #clear_btn:hover {
          background-color: #e53935 !important;
          transform: translateY(-1px);
          box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        
        #compare_btn:active, #clear_btn:active {
          transform: translateY(0);
          box-shadow: none;
        }
        
        /* Улучшенный внешний вид выпадающего списка */
        .bs-placeholder {
          color: #6c757d !important;
        }
        
        /* Убираем стандартные отступы у иконок в кнопках */
        .btn > i {
          margin-right: 5px;
        }
        
        /* Увеличиваем высоту контейнеров для графиков */
        .plotly.html-widget {
          height: 500px !important;
        }
        
        /* Увеличиваем высоту боксов с графиками */
        .box-body {
          min-height: 550px;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "comparison",
        fluidRow(
          valueBoxOutput("total_students_box", width = 3),
          valueBoxOutput("avg_grade_box", width = 3),
          valueBoxOutput("pass_rate_box", width = 3),
          valueBoxOutput("excellent_rate_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Распределение итоговых оценок", 
            status = "primary", solidHeader = TRUE,
            width = 12, collapsible = TRUE,
            withSpinner(plotlyOutput("group_comparison_distribution", height = "500px"), type = 6)
          )
        ),
        fluidRow(
          box(
            title = "Средние оценки по категориям заданий", 
            status = "primary", solidHeader = TRUE,
            width = 12, collapsible = TRUE,
            withSpinner(plotlyOutput("category_comparison_plot", height = "500px"), type = 6)
          )
        ),
        fluidRow(
          box(
            title = "Статистика по группам", 
            status = "primary", solidHeader = TRUE,
            width = 12, collapsible = TRUE,
            withSpinner(DTOutput("group_stats_table"), type = 6)
          )
        )
      ),
      tabItem(
        tabName = "about",
        box(
          title = "О программе", status = "primary", solidHeader = TRUE, width = 12,
          h3("Аналитика успеваемости студентов"),
          p("Данное приложение предназначено для анализа и сравнения успеваемости студенческих групп."),
          p("Функциональные возможности:"),
          tags$ul(
            tags$li("Сравнение нескольких групп по различным показателям"),
            tags$li("Анализ распределения итоговых оценок"),
            tags$li("Сравнение средних оценок по категориям заданий"),
            tags$li("Визуализация статистики успеваемости")
          ),
          hr(),
          p("Разработано для деканата с использованием R Shiny", style = "text-align: right; font-style: italic;")
        )
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
      updatePickerInput(session, "github_files", 
                        choices = files$name,
                        selected = NULL)
    } else {
      showNotification("Не удалось загрузить список файлов или папка пуста", type = "error")
    }
  })
  
  # Обработка нажатия кнопки очистки
  observeEvent(input$clear_btn, {
    updatePickerInput(session, "github_files", selected = character(0))
    all_data(list())
    showNotification("Выбор очищен", type = "message", duration = 2)
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
          add_headers("User-Agent" = "Mozilla/5.0")
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
      showNotification(paste("Загружено", length(data_list), "групп"), type = "message", duration = 3)
    }, error = function(e) {
      removeModal()
      showNotification(paste("Ошибка:", e$message), type = "error", duration = 10)
    })
  })
  
  # Информационные боксы для сравнения групп
  output$total_students_box <- renderValueBox({
    req(length(all_data()) > 0)
    
    total <- sum(sapply(all_data(), nrow))
    valueBox(
      total, "Всего студентов", 
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$avg_grade_box <- renderValueBox({
    req(length(all_data()) > 0)
    
    avg_grade <- map_dbl(all_data(), ~ mean(as.numeric(.x$`Итоговая оценка за курс (Значение)`), na.rm = TRUE)) %>%
      mean() %>% round(1)
    
    valueBox(
      avg_grade, "Средний балл", 
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$pass_rate_box <- renderValueBox({
    req(length(all_data()) > 0)
    
    pass_rate <- map_dbl(all_data(), ~ mean(as.numeric(.x$`Итоговая оценка за курс (Значение)`) >= 50, na.rm = TRUE)) %>%
      mean() %>% `*`(100) %>% round(1)
    
    valueBox(
      paste0(pass_rate, "%"), "Процент сдачи", 
      icon = icon("check-circle"),
      color = "olive"
    )
  })
  
  output$excellent_rate_box <- renderValueBox({
    req(length(all_data()) > 0)
    
    excellent_rate <- map_dbl(all_data(), ~ mean(as.numeric(.x$`Итоговая оценка за курс (Значение)`) >= 86, na.rm = TRUE)) %>%
      mean() %>% `*`(100) %>% round(1)
    
    valueBox(
      paste0(excellent_rate, "%"), "На отлично", 
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  # Подготовка данных для сравнения распределения оценок
  prepare_distribution_data <- function(data_list) {
    all_categories <- c("Нет данных", "Не сдано", "Удовлетворительно", "Хорошо", "Отлично")
    
    map_dfr(data_list, function(df) {
      group_name <- unique(df$Группа)
      full_categories <- expand.grid(
        Группа = group_name,
        GradeCategory = factor(all_categories, levels = all_categories, ordered = TRUE),
        stringsAsFactors = FALSE
      )
      
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
      
      full_counts <- full_categories %>%
        left_join(real_counts, by = c("Группа", "GradeCategory")) %>%
        mutate(
          n = ifelse(is.na(n), 0, n),
          Percentage = n / sum(n) * 100
        )
      
      return(full_counts)
    })
  }
  
  # График сравнения распределения оценок (интерактивный)
  output$group_comparison_distribution <- renderPlotly({
    req(length(all_data()) > 0)
    
    dist_data <- prepare_distribution_data(all_data())
    
    p <- ggplot(dist_data, aes(x = GradeCategory, y = Percentage, fill = Группа, text = paste0("Группа: ", Группа, "<br>Категория: ", GradeCategory, "<br>Процент: ", round(Percentage, 1), "%"))) +
      geom_col(position = position_dodge(preserve = "single"), alpha = 0.8) +
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
    
    ggplotly(p, tooltip = "text", height = 500) %>%
      layout(legend = list(orientation = "h", y = 1.1))
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
        # Исключаем данные о тренингах
        filter(Категория != "Тренинг") %>%
        group_by(Группа, Категория) %>%
        summarise(
          Средняя_оценка = mean(Оценка, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }
  
  # График сравнения по категориям заданий с двумя осями
  output$category_comparison_plot <- renderPlotly({
    req(length(all_data()) > 0)
    
    cat_data <- prepare_category_comparison_data(all_data())
    
    # Определяем порядок категорий для правильного отображения (без тренингов)
    category_order <- c("ДСР", "Контрольные", "РАР", "АСР", "Аттестация")
    cat_data <- cat_data %>%
      mutate(Категория = factor(Категория, levels = category_order))
    
    # Разделяем данные на обычные задания и аттестацию
    regular_data <- cat_data %>% filter(Категория != "Аттестация")
    attestation_data <- cat_data %>% filter(Категория == "Аттестация")
    
    # Создаем график
    p <- plot_ly() %>%
      # Добавляем данные для всех категорий кроме аттестации (левая ось Y)
      add_trace(
        data = regular_data,
        x = ~Категория, 
        y = ~Средняя_оценка, 
        color = ~Группа,
        type = "bar",
        name = ~Группа,
        hovertemplate = paste(
          "Группа: %{data.name}<br>",
          "Категория: %{x}<br>",
          "Средняя оценка: %{y:.1f}<br>",
          "<extra></extra>"
        ),
        yaxis = "y"
      ) %>%
      # Добавляем данные для аттестации (правая ось Y)
      add_trace(
        data = attestation_data,
        x = ~Категория, 
        y = ~Средняя_оценка, 
        color = ~Группа,
        type = "bar",
        name = ~Группа,
        hovertemplate = paste(
          "Группа: %{data.name}<br>",
          "Категория: %{x}<br>",
          "Оценка за аттестацию: %{y:.1f}/60<br>",
          "<extra></extra>"
        ),
        yaxis = "y2",
        showlegend = FALSE  # Важно: скрываем легенду для этого следа
      ) %>%
      layout(
        xaxis = list(
          title = "Категория задания",
          type = "category",
          tickangle = -45
        ),
        yaxis = list(
          title = "Средняя оценка (0-10)",
          side = "left",
          range = c(0, 10),
          showgrid = TRUE,
          zeroline = TRUE
        ),
        yaxis2 = list(
          title = "Оценка за аттестацию (0-60)",
          side = "right",
          overlaying = "y",
          range = c(0, 60),
          showgrid = FALSE,
          zeroline = FALSE
        ),
        legend = list(
          orientation = "h",
          y = 1.1,
          x = 0.5,
          xanchor = "center"
        ),
        barmode = "group",
        margin = list(r = 80), # Добавляем отступ для правой оси
        height = 500  # Увеличиваем высоту графика
      )
    
    return(p)
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
          `Процент отличников` = round(mean(as.numeric(`Итоговая оценка за курс (Значение)`) >= 86, na.rm = TRUE) * 100, 1),
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
      extensions = c('Buttons', 'Responsive'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        language = {
          list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json')
        }
      ),
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle(names(stats_data), backgroundColor = 'white') %>%
      formatStyle('Средний балл', 
                  backgroundColor = styleInterval(c(50, 70, 86), c('#ff6666', '#ffcc66', '#99cc99', '#66b3ff'))) %>%
      formatStyle('Процент сдавших', 
                  backgroundColor = styleInterval(c(50, 75, 90), c('#ff6666', '#ffcc66', '#99cc99', '#66b3ff')))
  })
}

# Запуск приложения
shinyApp(ui, server)