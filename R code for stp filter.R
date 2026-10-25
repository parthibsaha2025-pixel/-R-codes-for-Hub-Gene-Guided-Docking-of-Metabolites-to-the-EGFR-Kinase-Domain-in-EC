# Load required packages
library(readxl)
library(writexl)
library(dplyr)

# 1. INPUT folder (where your 194 original files are)
input_folder <- "Path to File"

# 2. OUTPUT folder (new subfolder inside "Mini (Parthib)")
output_folder <- "Path to File"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("Created output folder:", output_folder, "\n")
} else {
  cat("Output folder already exists:", output_folder, "\n")
}

# 3. List all Excel files in the input folder
excel_files <- list.files(
  path = input_folder,
  pattern = "\\.xlsx$|\\.xls$",
  full.names = TRUE
)

cat("Found", length(excel_files), "Excel files in input folder.\n\n")

filtered_list <- list()
saved_count <- 0
skipped_empty <- character(0)
failed_files <- character(0)

# 4. Loop through each file
for (file in excel_files) {
  
  dataset_name <- tools::file_path_sans_ext(basename(file))
  
  # --- Find the real header row (scan first 10 rows for "prob") ---
  raw_df <- tryCatch(
    read_excel(file, sheet = 1, col_names = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(raw_df)) {
    cat("FAILED to read:", dataset_name, "\n")
    failed_files <- c(failed_files, dataset_name)
    next
  }
  
  header_row <- NA
  max_scan_rows <- min(10, nrow(raw_df))
  
  for (i in 1:max_scan_rows) {
    row_values <- as.character(raw_df[i, ])
    if (any(grepl("prob", row_values, ignore.case = TRUE))) {
      header_row <- i
      break
    }
  }
  
  if (is.na(header_row)) {
    cat("No header row with 'Probability' found in:", dataset_name, "\n")
    failed_files <- c(failed_files, dataset_name)
    next
  }
  
  # --- Re-read properly, with correct headers ---
  df <- read_excel(file, sheet = 1, skip = header_row - 1)
  names(df) <- trimws(gsub("\\*", "", names(df)))
  
  # --- Identify ONLY the probability column — every other column is left untouched ---
  prob_col <- names(df)[grepl("prob", names(df), ignore.case = TRUE)]
  
  if (length(prob_col) == 0) {
    cat("No probability-like column found in:", dataset_name, "\n")
    failed_files <- c(failed_files, dataset_name)
    next
  }
  
  if (length(prob_col) > 1) {
    cat("Multiple probability-like columns in", dataset_name, "- using:", prob_col[1], "\n")
  }
  prob_col <- prob_col[1]   # This is the ONLY column the cutoff logic will ever look at
  
  # Make sure that ONE column is numeric (does not touch any other column)
  df[[prob_col]] <- as.numeric(gsub("[^0-9.]", "", as.character(df[[prob_col]])))
  
  # --- THE CUTOFF: keep rows where [prob_col] >= 0.5. No other column is checked. ---
  filtered_df <- df %>% filter(.data[[prob_col]] >= 0.5)
  
  filtered_list[[dataset_name]] <- filtered_df
  
  if (nrow(filtered_df) == 0) {
    skipped_empty <- c(skipped_empty, dataset_name)
    cat("0 rows after cutoff (still saving empty file):", dataset_name, "\n")
  }
  
  # --- Save this filtered dataframe as its own Excel file ---
  safe_filename <- gsub('[\\\\/:*?"<>|]', "_", dataset_name)
  out_path <- file.path(output_folder, paste0(safe_filename, "_filtered.xlsx"))
  
  result <- tryCatch({
    write_xlsx(filtered_df, path = out_path)
    TRUE
  }, error = function(e) {
    cat("FAILED to save:", dataset_name, "-", conditionMessage(e), "\n")
    FALSE
  })
  
  if (result) saved_count <- saved_count + 1
}

# 5. Make each dataframe available individually in R too
list2env(filtered_list, envir = .GlobalEnv)

# 6. Summary
cat("\n========== SUMMARY ==========\n")
cat("Total files found       :", length(excel_files), "\n")
cat("Successfully processed  :", length(filtered_list), "\n")
cat("Successfully saved      :", saved_count, "\n")
cat("Files with 0 rows >=0.5 :", length(skipped_empty), "\n")
cat("Files that failed       :", length(failed_files), "\n")
cat("Output folder           :", output_folder, "\n")

saved_files <- list.files(output_folder, pattern = "\\.xlsx$", full.names = FALSE)
cat("\nFiles actually present in output folder:", length(saved_files), "\n")

