# Load required libraries
library(readxl)
library(stringr)
library(dplyr)

process_student_grades <- function(file_path) {
  # Helpers
  clean_to_numeric <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- trimws(x)
      x[x %in% c("", "-")] <- NA
      x <- gsub(",", ".", x, fixed = TRUE)
    }
    suppressWarnings(as.numeric(x))
  }
  last_non_na_by_row <- function(mat_num) {
    if (is.null(mat_num) || length(mat_num) == 0) return(rep(NA_real_, 0))
    if (!is.matrix(mat_num)) mat_num <- as.matrix(mat_num)
    if (nrow(mat_num) == 0) return(numeric(0))
    res <- rep(NA_real_, nrow(mat_num))
    for (j in seq_len(ncol(mat_num))) {
      idx <- !is.na(mat_num[, j])
      if (any(idx)) res[idx] <- mat_num[idx, j]
    }
    res
  }
  extract_group_features_vector <- function(group_text) {
    if (is.null(group_text) || is.na(group_text)) return(character(0))
    txt <- as.character(group_text)
    # Collect statuses first (keep original matched text casing)
    status_matches <- str_extract_all(txt, "(?i)Академический отпуск|Академический|неизвестно|Отчисленные")[[1]]
    if (length(status_matches) == 0) status_matches <- character(0)
    # Extract sequences like <PREFIX><YY>-<N><suffix?>; prefix can be capitals or ff/ot/om/ol; keep order; capture year and optional suffix
    m <- str_match_all(txt, "((?:[A-ZА-ЯЁ]+|(?i:фф|от|ом|ол)))(2[0-5])-(?:1[0-9]|20|[1-9])(ву|в|с|к|у)?")[[1]]
    features <- character(0)
    if (nrow(m) > 0) {
      for (i in seq_len(nrow(m))) {
        year_token <- m[i, 3]
        if (!is.na(year_token) && nzchar(year_token)) features <- c(features, year_token)
        suffix_token <- m[i, 4]
        if (!is.na(suffix_token) && nzchar(suffix_token)) features <- c(features, suffix_token)
      }
    }
    # Also handle pattern like YY(N) where YY=21-24 and N=1..20 (ignore prefix, just take year)
    m2 <- str_match_all(txt, "(2[1-4])\\s*\\((?:1[0-9]|20|[1-9])\\)")[[1]]
    if (nrow(m2) > 0) {
      for (i in seq_len(nrow(m2))) {
        year_token <- m2[i, 2]
        if (!is.na(year_token) && nzchar(year_token)) features <- c(features, year_token)
      }
    }
    # Append statuses at the end (order of appearance overall is preserved enough for downstream usage)
    c(features, status_matches)
  }
  
  # Read the Excel file
  df <- read_excel(file_path)
  
  # Drop the last column globally before any calculations
  if (ncol(df) > 0) {
    df <- df[, -ncol(df), drop = FALSE]
  }
  
  # Remove download-related columns
  download_keywords <- c("Download files", "Загрузка файлов", "uploading", "Last downloaded", "Downloading")
  pattern <- paste0("(?i)", paste(download_keywords, collapse = "|"))
  drop_cols <- str_detect(names(df), pattern)
  drop_cols[is.na(drop_cols)] <- FALSE
  df <- df[, !drop_cols, drop = FALSE]
  
  # Initialize metadata columns
  n <- nrow(df)
  id <- if (ncol(df) >= 1) as.character(df[[1]]) else rep(NA_character_, n)
  groups <- if (ncol(df) >= 2) as.character(df[[2]]) else rep(NA_character_, n)
  total_grade <- if (ncol(df) >= 3) clean_to_numeric(df[[3]]) else rep(NA, n)
  
  # Extract task columns (columns 4 onwards)
  task_cols_all <- if (ncol(df) > 3) df[, 4:ncol(df), drop = FALSE] else data.frame()
  task_names <- names(task_cols_all)
  
  # Identify exam and training columns
  exam_keywords <- c("Зачет", "зачет", "экзамен", "Экзамен", "ЗАЧЕТ", "ЭКЗАМЕН", "Exam", "semester test", "АКР")
  exam_pattern <- paste0("(?i)", paste(exam_keywords, collapse = "|"))
  exam_flags <- str_detect(task_names, exam_pattern)
  exam_flags[is.na(exam_flags)] <- FALSE
  training_keywords <- c("тренинг", "training", "make-up")
  training_pattern <- paste0("(?i)", paste(training_keywords, collapse = "|"))
  training_flags <- str_detect(task_names, training_pattern)
  training_flags[is.na(training_flags)] <- FALSE
  
  # Student exam presence (for MaxPossible computation)
  student_exam_present <- rep(FALSE, n)
  if (any(exam_flags)) {
    exam_df <- task_cols_all[, exam_flags, drop = FALSE]
    for (j in seq_along(exam_df)) {
      col_char <- trimws(as.character(exam_df[[j]]))
      col_char[col_char %in% c("", "-")] <- NA
      student_exam_present <- student_exam_present | !is.na(col_char)
    }
  }
  
  # Build non-exam task set for maximum points calculation
  task_cols <- task_cols_all[, !exam_flags, drop = FALSE]
  
  # Remove empty columns (all NA or "-") among non-exam tasks only
  removed_count <- 0
  if (ncol(task_cols) > 0) {
    valid_cols <- rep(TRUE, ncol(task_cols))
    for (j in seq_along(task_cols)) {
      col_char <- trimws(as.character(task_cols[[j]]))
      is_empty <- all(is.na(col_char) | col_char == "-")
      if (is_empty) {
        valid_cols[j] <- FALSE
        removed_count <- removed_count + 1
      }
    }
    task_cols <- task_cols[, valid_cols, drop = FALSE]
  }
  
  # Calculate maximum points for non-exam tasks
  max_points <- 0
  if (ncol(task_cols) > 0) {
    for (j in seq_along(task_cols)) {
      col_num <- clean_to_numeric(task_cols[[j]])
      if (all(is.na(col_num))) next
      col_max <- max(col_num, na.rm = TRUE)
      if (is.finite(col_max)) {
        if (col_max %% 1 != 0) col_max <- ceiling(col_max)
        max_points <- max_points + col_max
      }
    }
  }
  
  # Calculate student-specific maximum possible score (adds 60 if exam present)
  student_max <- max_points + ifelse(student_exam_present, 60, 0)
  
  # Calculate normalized grade (handle division by zero)
  normalized_grade <- ifelse(student_max > 0, total_grade / student_max, NA_real_)
  
  # Create dataset for actual result calculations (excluding training columns)
  actual_task_cols <- task_cols_all
  actual_task_names <- task_names
  training_words <- c("make-up", "training", "тренинг")
  training_pattern <- paste0("(?i)", paste(training_words, collapse = "|"))
  training_col_flags <- str_detect(actual_task_names, training_pattern)
  training_col_flags[is.na(training_col_flags)] <- FALSE
  actual_task_cols <- actual_task_cols[, !training_col_flags, drop = FALSE]
  actual_task_names <- names(actual_task_cols)
  
  # Compute ACTUAL results
  # 1) Sum of non-exam tasks actually earned (training columns already removed)
  non_exam_sum <- rep(0, n)
  if (ncol(actual_task_cols) > 0) {
    actual_exam_flags <- str_detect(actual_task_names, exam_pattern)
    actual_exam_flags[is.na(actual_exam_flags)] <- FALSE
    non_exam_df <- actual_task_cols[, !actual_exam_flags, drop = FALSE]
    if (ncol(non_exam_df) > 0) {
      non_exam_num <- lapply(non_exam_df, clean_to_numeric)
      non_exam_mat <- as.matrix(as.data.frame(non_exam_num))
      non_exam_sum <- rowSums(non_exam_mat, na.rm = TRUE)
    }
  }
  # 2) Training breakdown (subset of non-exam normally)
  training_sum <- rep(0, n)
  if (any(training_flags)) {
    training_df <- task_cols_all[, training_flags, drop = FALSE]
    if (ncol(training_df) > 0) {
      training_num <- lapply(training_df, clean_to_numeric)
      training_mat <- as.matrix(as.data.frame(training_num))
      training_sum <- rowSums(training_mat, na.rm = TRUE)
    }
  }
  # 3) Exam actual: for each exam base (case-insensitive canonical), take last chronological value; prefer reserve per student, else fallback
  exam_actual_sum <- rep(0, n)
  if (ncol(actual_task_cols) > 0) {
    actual_exam_flags <- str_detect(actual_task_names, exam_pattern)
    actual_exam_flags[is.na(actual_exam_flags)] <- FALSE
    if (any(actual_exam_flags)) {
      exam_indices <- which(actual_exam_flags)
      exam_names <- actual_task_names[exam_indices]
      exam_names[is.na(exam_names)] <- ""
      is_reserve <- str_detect(exam_names, "(?i)(резерв|reserve)")
      base_names <- ifelse(str_detect(exam_names, "(?i)АКР|зачет"), "зачет_акр",
                           ifelse(str_detect(exam_names, "(?i)экзамен|exam|semester test"), "экзамен", "прочее"))
      unique_bases <- unique(base_names)
      for (base in unique_bases) {
        cols_for_base <- exam_indices[base_names == base]
        if (length(cols_for_base) == 0) next
        reserve_mask <- is_reserve[base_names == base]
        cols_reserve <- cols_for_base[reserve_mask]
        cols_nonreserve <- cols_for_base[!reserve_mask]
        # Keep original order (assumed chronological left->right)
        cols_reserve <- sort(cols_reserve)
        cols_nonreserve <- sort(cols_nonreserve)
        # Compute last-present among reserve and non-reserve separately
        last_reserve <- rep(NA_real_, n)
        if (length(cols_reserve) > 0) {
          df_res <- actual_task_cols[, cols_reserve, drop = FALSE]
          num_res <- lapply(df_res, clean_to_numeric)
          mat_res <- as.matrix(as.data.frame(num_res))
          last_reserve <- last_non_na_by_row(mat_res)
        }
        last_nonres <- rep(NA_real_, n)
        if (length(cols_nonreserve) > 0) {
          df_non <- actual_task_cols[, cols_nonreserve, drop = FALSE]
          num_non <- lapply(df_non, clean_to_numeric)
          mat_non <- as.matrix(as.data.frame(num_non))
          last_nonres <- last_non_na_by_row(mat_non)
        }
        chosen <- ifelse(!is.na(last_reserve), last_reserve, last_nonres)
        chosen[is.na(chosen)] <- 0
        exam_actual_sum <- exam_actual_sum + chosen
      }
    }
  }
  
  actual_total <- non_exam_sum + exam_actual_sum
  actual_div_100 <- actual_total / 100
  
  # Participation coefficient
  part_task_cols <- task_cols_all
  if (ncol(part_task_cols) > 0) {
    valid_cols_part <- rep(TRUE, ncol(part_task_cols))
    for (j in seq_along(part_task_cols)) {
      col_char <- trimws(as.character(part_task_cols[[j]]))
      if (all(is.na(col_char) | col_char == "-")) {
        valid_cols_part[j] <- FALSE
      }
    }
    part_task_cols <- part_task_cols[, valid_cols_part, drop = FALSE]
  }
  part_task_names <- names(part_task_cols)
  exam_flags_part <- if (length(part_task_names) > 0) str_detect(part_task_names, exam_pattern) else logical(0)
  if (length(exam_flags_part) > 0) exam_flags_part[is.na(exam_flags_part)] <- FALSE
  
  non_exam_count <- if (ncol(part_task_cols) > 0) sum(!exam_flags_part) else 0
  exam_count <- 0
  if (any(exam_flags_part)) {
    exam_indices_part <- which(exam_flags_part)
    exam_names_part <- part_task_names[exam_indices_part]
    exam_names_part[is.na(exam_names_part)] <- ""
    base_names_part <- ifelse(str_detect(exam_names_part, "(?i)АКР|зачет"), "зачет_акр",
                              ifelse(str_detect(exam_names_part, "(?i)экзамен|exam|semester test"), "экзамен", "прочее"))
    exam_count <- length(unique(base_names_part))
  }
  denom_cols <- non_exam_count + exam_count
  
  participation_numerator <- rep(0L, n)
  if (ncol(part_task_cols) > 0) {
    # Non-exam columns: count positive points per student
    if (any(!exam_flags_part)) {
      ne_df <- part_task_cols[, !exam_flags_part, drop = FALSE]
      ne_num <- lapply(ne_df, clean_to_numeric)
      ne_mat <- as.matrix(as.data.frame(ne_num))
      ne_count <- rowSums(ifelse(!is.na(ne_mat) & ne_mat > 0, 1L, 0L))
      participation_numerator <- participation_numerator + as.integer(ne_count)
    }
    # Exam bases: add 1 if any column in the base has positive points
    if (any(exam_flags_part)) {
      exam_indices_part <- which(exam_flags_part)
      exam_names_part <- part_task_names[exam_indices_part]
      exam_names_part[is.na(exam_names_part)] <- ""
      base_names_part <- ifelse(str_detect(exam_names_part, "(?i)АКР|зачет"), "зачет_акр",
                                ifelse(str_detect(exam_names_part, "(?i)экзамен|exam|semester test"), "экзамен", "прочее"))
      unique_bases_part <- unique(base_names_part)
      for (base in unique_bases_part) {
        cols_for_base <- exam_indices_part[base_names_part == base]
        base_df <- part_task_cols[, cols_for_base, drop = FALSE]
        base_num <- lapply(base_df, clean_to_numeric)
        base_mat <- as.matrix(as.data.frame(base_num))
        has_points <- apply(base_mat, 1, function(x) any(!is.na(x) & x > 0))
        participation_numerator <- participation_numerator + as.integer(has_points)
      }
    }
  }
  participation_coeff <- if (denom_cols > 0) participation_numerator / denom_cols else rep(NA_real_, n)
  
  # Build group features columns from 'groups'
  features_list <- lapply(groups, extract_group_features_vector)
  max_feats <- max(c(0, vapply(features_list, length, integer(1))), na.rm = TRUE)
  feature_cols <- list()
  if (max_feats > 0) {
    for (k in seq_len(max_feats)) {
      feature_cols[[paste0("GroupFeat", k)]] <- vapply(features_list, function(v) if (length(v) >= k) v[k] else NA_character_, character(1))
    }
  }
  
  # Create result dataframe
  result_df <- data.frame(
    ID = id,
    Groups = groups,
    FileResult = total_grade,
    MaxPossible = student_max,
    NormalizedGrade = normalized_grade,
    NonExamSum = non_exam_sum,
    ExamActualSum = exam_actual_sum,
    TrainingSum = training_sum,
    ActualTotal = actual_total,
    ActualResult = actual_div_100,
    ParticipationCoeff = participation_coeff,
    stringsAsFactors = FALSE
  )
  if (length(feature_cols) > 0) {
    result_df <- cbind(result_df, as.data.frame(feature_cols, stringsAsFactors = FALSE))
  }
  
  # Return results and removed column count
  return(list(
    data = result_df,
    removed_columns_count = removed_count
  ))
}

# Batch processing over multiple files
file_paths <- c(
  "C:/НИР/БаллыСтуд/MEN24-5.xlsx",
  "C:/НИР/БаллыСтуд/MEN24-6.xlsx",
  "C:/НИР/БаллыСтуд/MEN24-9.xlsx",
  "C:/НИР/БаллыСтуд/MEN24-11 (1).xlsx",
  "C:/НИР/БаллыСтуд/MEN24-12.xlsx",
  "C:/НИР/БаллыСтуд/MEN24-13.xlsx",
  "C:/НИР/БаллыСтуд/КП_22-23_1_сем_всё.xlsx",
  "C:/НИР/БаллыСтуд/AD_1(22-23).xlsx",
  "C:/НИР/БаллыСтуд/AD_1(23-24).xlsx",
  "C:/НИР/БаллыСтуд/AD_1(24-25).xlsx",
  "C:/НИР/БаллыСтуд/AD_2(23-24).xlsx",
  "C:/НИР/БаллыСтуд/AD_2(24-25).xlsx",
  "C:/НИР/БаллыСтуд/AD_2_(22-23).xlsx",
  "C:/НИР/БаллыСтуд/CP_1_22-23.xlsx",
  "C:/НИР/БаллыСтуд/CP_1_23-24.xlsx",
  "C:/НИР/БаллыСтуд/CP_1_24-25.xlsx",
  "C:/НИР/БаллыСтуд/CP_2_22-23.xlsx",
  "C:/НИР/БаллыСтуд/CP_2_23-24.xlsx",
  "C:/НИР/БаллыСтуд/CP_2_24-25.xlsx",
  "C:/НИР/БаллыСтуд/DM_1_22-23_RE.xlsx",
  "C:/НИР/БаллыСтуд/DM_1_23-24_RE.xlsx",
  "C:/НИР/БаллыСтуд/DM_2_22-23_RE.xlsx",
  "C:/НИР/БаллыСтуд/DM_2_23-24_RE.xlsx",
  "C:/НИР/БаллыСтуд/DM_2_24-25_RE.xlsx",
  "C:/НИР/БаллыСтуд/DM_RE_1_(24-25).xlsx",
  "C:/НИР/БаллыСтуд/ENGLISH DA_1(22-23).xlsx",
  "C:/НИР/БаллыСтуд/ENGLISH DA_1(23-24).xlsx",
  "C:/НИР/БаллыСтуд/ENGLISH DA_1(24-25).xlsx",
  "C:/НИР/БаллыСтуд/ENGLISH DA_2_(22-23).xlsx",
  "C:/НИР/БаллыСтуд/ENGLISH_DA_2(23-24).xlsx",
  "C:/НИР/БаллыСтуд/ENGLISH_DA_2(24-25).xlsx",
  "C:/НИР/БаллыСтуд/MAT_1(23-24)_все.xlsx",
  "C:/НИР/БаллыСтуд/MAT_1(24-25)_все.xlsx",
  "C:/НИР/БаллыСтуд/MAT_1_22-23_все.xlsx"
)

results_list <- list()
for (fp in file_paths) {
  if (!file.exists(fp)) {
    message("File not found: ", fp)
    next
  }
  cat("\n===== ", fp, " =====\n", sep = "")
  res <- tryCatch(process_student_grades(fp), error = function(e) {
    message("Error processing ", fp, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(res)) next
  df_out <- res$data
  df_out$SourceFile <- basename(fp)
  print(df_out)
  results_list[[fp]] <- df_out
}

# Combined results (optional for further analysis)
all_results <- if (length(results_list) > 0) bind_rows(results_list) else data.frame()

# Write results to a file usable in Excel
output_dir <- "C:/НИР/БаллыСтуд/outputs"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (length(results_list) > 0) {
  can_write_xlsx <- requireNamespace("writexl", quietly = TRUE)
  if (can_write_xlsx) {
    make_sheet_name <- function(path, existing) {
      nm <- tools::file_path_sans_ext(basename(path))
      nm <- gsub("[\\[\\]\\*:/\\\\?]", "_", nm)
      if (nchar(nm) == 0) nm <- "Sheet"
      nm <- substr(nm, 1, 31)
      base <- nm
      k <- 1
      while (nm %in% existing) {
        suffix <- paste0("_", k)
        nm <- substr(paste0(base, suffix), 1, 31)
        k <- k + 1
      }
      nm
    }
    sheets <- list()
    used <- character(0)
    sheets[["COMBINED"]] <- all_results
    used <- c(used, "COMBINED")
    for (fp in names(results_list)) {
      nm <- make_sheet_name(fp, used)
      sheets[[nm]] <- results_list[[fp]]
      used <- c(used, nm)
    }
    out_xlsx <- file.path(output_dir, paste0("AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
    ok <- tryCatch({ writexl::write_xlsx(sheets, path = out_xlsx); TRUE }, error = function(e) FALSE)
    if (ok) {
      message("Wrote Excel workbook: ", out_xlsx)
    } else {
      alt_path <- file.path(tempdir(), paste0("AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
      writexl::write_xlsx(sheets, path = alt_path)
      message("Excel write failed to ", out_xlsx, "; wrote to temp instead: ", alt_path)
    }
  } else {
    out_csv <- file.path(output_dir, paste0("AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    ok <- tryCatch({ write.csv(all_results, out_csv, row.names = FALSE, fileEncoding = "UTF-8"); TRUE }, error = function(e) FALSE)
    if (ok) {
      message("Package 'writexl' not installed; wrote CSV instead: ", out_csv)
    } else {
      alt_path <- file.path(tempdir(), paste0("AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
      write.csv(all_results, alt_path, row.names = FALSE, fileEncoding = "UTF-8")
      message("CSV write failed to ", out_csv, "; wrote to temp instead: ", alt_path)
    }
  }
} else {
  message("No results to write.")
}