library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(shinyFiles)


# UI часть приложения
ui <- dashboardPage(
  dashboardHeader(title = "Анализ успеваемости студентов"),
  dashboardSidebar(
    # Добавляем кнопку выбора файла
    fileInput("file", "Выберите файл с оценками", 
              accept = c(".xlsx", ".xls"),
              buttonLabel = "Обзор...",
              placeholder = "Файл не выбран"),
    
    # Поля для ввода данных студента появляются только после загрузки файла
    conditionalPanel(
      condition = "output.file_loaded",
      textInput("email", "Введите ID студента:", placeholder = "например, 237829"),
      actionButton("submit", "Показать результаты"),
      tags$hr(),
      helpText("Введите ID студента и нажмите кнопку, чтобы увидеть его результаты.")
    )
  ),
  dashboardBody(
    # Информация о загрузке файла
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
    
    # Основной интерфейс появляется после загрузки файла
    conditionalPanel(
      condition = "output.file_loaded && input.submit == 0",
      box(
        width = 12,
        status = "info",
        h3("Файл успешно загружен"),
        p("Теперь вы можете ввести ID студента в поле слева и нажать кнопку 'Показать результаты'.")
      )
    ),
    
    # Результаты студента
    conditionalPanel(
      condition = "output.file_loaded && input.submit > 0",
      fluidRow(
        box(
          title = "Общая информация о студенте",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h3(textOutput("student_final_grade")),
          br(),
          uiOutput("student_plot_ui")
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

# Server часть приложения
server <- function(input, output, session) {
  # Реактивное значение для хранения данных
  data_clean <- reactiveVal(NULL)
  final_grade_col <- reactiveVal(NULL)
  
  # Обработка загрузки файла
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      # Показываем индикатор загрузки
      showModal(modalDialog("Загрузка файла...", footer = NULL))
      
      # Читаем файл
      data <- readxl::read_excel(input$file$datapath, sheet = "Оценки")
      
      # Очистка данных
      clean_data <- data %>%
        filter(!is.na(`Адрес электронной почты`)) %>%
        select(-last_col()) # Удаляем последний столбец
      
      # Сохраняем название столбца с итоговой оценкой
      grade_col <- "Итоговая оценка за курс (Значение)"
      
      # Преобразуем только столбцы с оценками в числовой формат
      grade_cols <- setdiff(names(clean_data), c("Адрес электронной почты", grade_col))
      clean_data <- clean_data %>%
        mutate(across(all_of(grade_cols), as.numeric))
      
      # Сохраняем данные
      data_clean(clean_data)
      final_grade_col(grade_col)
      
      # Закрываем индикатор загрузки
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Ошибка при загрузке файла:", e$message), type = "error")
    })
  })
  
  # Флаг, указывающий что файл загружен
  output$file_loaded <- reactive({
    !is.null(data_clean())
  })
  outputOptions(output, "file_loaded", suspendWhenHidden = FALSE)
  
  # Реактивное выражение для фильтрации данных по email
  student_data <- eventReactive(input$submit, {
    req(input$email, data_clean())
    data_clean() %>% 
      filter(`Адрес электронной почты` == input$email)
  })
  
  # Вывод итоговой оценки крупным шрифтом
  output$student_final_grade <- renderText({
    student <- student_data()
    if (nrow(student) == 0) return("Студент с таким ID не найден.")
    
    final_grade <- student[[final_grade_col()]][1]
    paste0("Итоговая оценка: ", ifelse(is.na(final_grade), "нет данных", final_grade))
  })
  
  # График успеваемости студента
  output$student_plot <- renderPlot({
    student <- student_data()
    req(nrow(student) > 0)
    
    grades_long <- student %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col())) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      filter(!is.na(Оценка))
    
    req(nrow(grades_long) > 0)
    
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
  
  # Обертка для графика с обработкой отсутствия данных
  output$student_plot_ui <- renderUI({
    student <- student_data()
    if (nrow(student) == 0) {
      return(tags$div(
        style = "text-align: center; padding: 20px;",
        h4("Данные студента не найдены"),
        p("Пожалуйста, проверьте правильность введенного ID и попробуйте снова.")
      ))
    }
    
    grades_long <- student %>%
      select(-`Адрес электронной почты`, -all_of(final_grade_col())) %>%
      pivot_longer(everything(), names_to = "Тест", values_to = "Оценка") %>%
      filter(!is.na(Оценка))
    
    if (nrow(grades_long) == 0) {
      return(tags$div(
        style = "text-align: center; padding: 20px;",
        h4("Нет данных об оценках"),
        p("У этого студента нет записанных оценок.")
      ))
    }
    
    plotOutput("student_plot", height = "600px")
  })
  
  # Таблица с детализацией оценок
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