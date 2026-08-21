# План настройки GitHub Project: LMS dashboard

## Основа доски

На существующей доске уже реализован поток **Backlog → Ready → In progress → In review** и представления `Current iteration`, `Next iteration`, `Prioritized backlog`, `Roadmap`, `In review`, `My items`. Эта структура соответствует исследовательскому циклу и должна быть сохранена. Для полного жизненного цикла следует добавить статус **Done** и скрывать его в активном представлении текущей итерации.

| Поле | Тип / значения | Зачем нужно |
| --- | --- | --- |
| `Status` | Backlog, Ready, In progress, In review, Done | Единый поток работы |
| `Priority` | P0, P1, P2 | Очередность задач |
| `Iteration` | Current, Next, Future | Управление короткими циклами |
| `Estimate` | 1, 2, 3, 5 | Относительная трудоёмкость |
| `Area` | Data, Methods, Modelling, Intervention, Dashboard, Repository | Тематическая группировка |
| `Deliverable` | Dictionary, Dataset, Notebook, Report, Dashboard, Pull request | Проверяемый результат |
| `Data period` | 2022/23, 2023/24, 2024/25, Cross-year | Явная граница данных |
| `Evidence / link` | URL | Ссылка на PR, отчёт, документ или выпуск данных |

> **WIP-правило:** одновременно в `In progress` допускается не более трёх задач, а в `In review` — не более пяти. Любая карточка с моделью должна содержать ссылку на версию данных, схему временной валидации и описание допустимого педагогического действия.

## Стартовые карточки

| № | Название | Area | Priority | Estimate | Iteration | Начальный статус | Критерий готовности |
| ---: | --- | --- | --- | ---: | --- | --- | --- |
| 1 | **Review LMS-repository-restructuring pull request** | Repository | P0 | 1 | Current | In review | Ветка `chore/lms-repository-restructure-20260821` проверена; документация, скрипты и пути согласованы. |
| 2 | **Create versioned activity and topic dictionaries** | Data | P0 | 3 | Current | Ready | Для каждого варианта заголовка есть нормализованный код, тип, тема, версия и журнал исключений. |
| 3 | **Define data contract and source manifest for corrected LMS exports** | Data | P0 | 2 | Current | Ready | Зафиксированы единица наблюдения, правила попыток/пропусков, период, SHA и источник. |
| 4 | **Build normalised long-format LMS dataset** | Data | P1 | 5 | Next | Backlog | Преобразование повторяемо; происхождение записи и версия словаря сохраняются. |
| 5 | **Profile data quality by course, cohort and academic year** | Methods | P1 | 3 | Next | Backlog | Есть агрегированный отчёт о полноте, шкалах, повторах и структурных расхождениях. |
| 6 | **Establish temporal baselines for early-risk prediction** | Modelling | P1 | 5 | Future | Backlog | Baseline и нелинейная модель проверены на последующем периоде; зафиксированы калибровка и ошибки. |
| 7 | **Validate student-segmentation profiles and stability** | Modelling | P2 | 3 | Future | Backlog | Профили кластеров устойчивы к правилам пропусков и прошли предметную интерпретацию. |
| 8 | **Prototype teacher-facing dashboard with traceable metrics** | Dashboard | P1 | 5 | Future | Backlog | Макет показывает происхождение метрик, период и уровень агрегации; не раскрывает индивидуальные данные без полномочий. |
| 9 | **Design and evaluate an academic-support pilot** | Intervention | P1 | 5 | Future | Backlog | Описаны действие, ответственный, этический протокол и измеримая метрика эффекта. |

## Настройка представлений

| Представление | Фильтр / сортировка | Назначение |
| --- | --- | --- |
| `Current iteration` | `iteration:@current`; группировка по `Status`; скрыть `Done` | Управление текущим циклом и WIP |
| `Next iteration` | `iteration:@next`; сортировка `Priority`, затем `Estimate` | Подготовка работ без преждевременного запуска |
| `Prioritized backlog` | `Status:Backlog`; сортировка `Priority`, `Area`, `Estimate` | Прозрачная очередь НИР |
| `Roadmap` | Группировка по `Iteration`; показывать `Area`, `Deliverable`, `Data period` | Связь задач и научных результатов |
| `In review` | `Status:"In review"`; показывать `Evidence / link`, `Deliverable` | Контроль качества PR, отчётов и дашбордов |
| `My items` | `assignee:@me`; группировка по `Status` | Индивидуальная очередь |

## Definition of Done

Карточка переводится в `Done`, только если результат воспроизводим, ссылка на артефакт заполнена, период/источник/версия словаря зафиксированы, ограничения описаны, а для риск-сигнала есть документированное педагогическое действие. Высокая метрика модели без временной проверки и интерпретации ошибок не является основанием для завершения задачи.
