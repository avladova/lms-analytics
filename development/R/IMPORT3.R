# Загружаем библиотеки:
library("xlsx") # Для считывания данных из # excel-файлов типа xlsx
library("dplyr") # Для glimpse и select

# Импорт файлов в формате xlsx
# проверка пути
getwd()
# при необходимости настройка с помощью setwd  # Пример("C:/Users/Vanshi/Desktop/gfg")

# Помещение файла в директорию PATH файла под названием data98 c раб стола
PATH <- "C:\\Users\\Админ\\Desktop\\data98.xlsx"
Data <- read.xlsx(file = PATH,sheetName =  '1', 
                  encoding = "UTF-8", header = TRUE) # Считывается 1-ый лист из указанной книги # Excel
glimpse(Data) # Смотрим структуру Data
Data # Смотрим саму таблицу данных Data


# Импорт файлов в формате csv
# Шаблон 1

Data <- read.csv("Путь и имя файла.csv", header =TRUE)
# Чтение данных из excel-файла формата csv
write.csv(Data, "Путь и имя файла.csv ", col.names =
            TRUE, row.names = FALSE) # Запись таблицы Data
# из R в Excel-файл формата c

# Шаблон 2 для импорта csv


library (tidyverse)
# импорт к примеру файла  example_1kb.csv" с рабочего стола
PATH2 <- "C:\\Users\\Админ\\Desktop\\example_1kb.csv"
D2 <- read.csv2(PATH2)
str(D2)
