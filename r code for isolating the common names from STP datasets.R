# Load required packages
library(readxl)
library(writexl)
library(dplyr)

# ------------------------------------------------------------------
# 1. INPUT and OUTPUT folder paths
# ------------------------------------------------------------------
input_folder  <- "Path to File"
output_folder <- "Path to File"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("Created output folder:", output_folder, "\n")
}

# ------------------------------------------------------------------
# 2. List all Excel files in the input folder
# ------------------------------------------------------------------
excel_files <- list.files(input_folder, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
cat("Found", length(excel_files), "Excel files.\n\n")

common_name_list <- list()
failed_files <- character(0)
saved_count <- 0

# ------------------------------------------------------------------
# 3. Loop through each file
# ------------------------------------------------------------------
for (file in excel_files) {
  
  dataset_name <- tools::file_path_sans_ext(basename(file))
  
  raw_df <- tryCatch(read_excel(file, sheet = 1, col_names = FALSE), error = function(e) NULL)
  
  if (is.null(raw_df)) {
    cat("FAILED to read:", dataset_name, "\n")
    failed_files <- c(failed_files, dataset_name)
    next
  }
  
  header_row <- NA
  for (i in 1:min(10, nrow(raw_df))) {
    row_values <- as.character(raw_df[i, ])
    if (any(grepl("common name", row_values, ignore.case = TRUE))) {
      header_row <- i
      break
    }
  }
  
  if (is.na(header_row)) {
    cat("No 'Common name' column found in:", dataset_name, "\n")
    failed_files <- c(failed_files, dataset_name)
    next
  }
  
  df <- read_excel(file, sheet = 1, skip = header_row - 1)
  names(df) <- trimws(gsub("\\*", "", names(df)))
  
  name_col <- names(df)[grepl("common\\s*name", names(df), ignore.case = TRUE)]
  
  if (length(name_col) == 0) {
    cat("No 'Common name' column found in:", dataset_name, "\n")
    failed_files <- c(failed_files, dataset_name)
    next
  }
  name_col <- name_col[1]
  
  # --- FIX: force the column to character BEFORE filtering, so an all-NA
  # (logical) column never clashes with character columns from other files ---
  common_name_df <- df %>%
    select(all_of(name_col)) %>%
    rename(Common_Name = all_of(name_col)) %>%
    mutate(Common_Name = as.character(Common_Name)) %>%   # <-- key fix
    filter(!is.na(Common_Name) & Common_Name != "") %>%
    mutate(Source_File = dataset_name, .before = 1)
  
  common_name_list[[dataset_name]] <- common_name_df
  
  if (nrow(common_name_df) == 0) {
    cat("Note: 0 non-empty common names in:", dataset_name, "\n")
  }
  
  safe_filename <- gsub('[\\\\/:*?"<>|]', "_", dataset_name)
  out_path <- file.path(output_folder, paste0(safe_filename, "_CommonName.xlsx"))
  
  result <- tryCatch({
    write_xlsx(common_name_df, path = out_path)
    TRUE
  }, error = function(e) {
    cat("FAILED to save:", dataset_name, "-", conditionMessage(e), "\n")
    FALSE
  })
  
  if (result) saved_count <- saved_count + 1
}

# ------------------------------------------------------------------
# 4. Make each dataframe available individually in R too
# ------------------------------------------------------------------
list2env(common_name_list, envir = .GlobalEnv)

# ------------------------------------------------------------------
# 5. Combine all 194 into one file (now safe, since all are character type)
# ------------------------------------------------------------------
combined_common_names <- bind_rows(common_name_list)
write_xlsx(combined_common_names, path = file.path(output_folder, "All_Common_Names_Combined.xlsx"))

# ------------------------------------------------------------------
# 6. SUMMARY
# ------------------------------------------------------------------
cat("\n========== SUMMARY ==========\n")
cat("Total files found        :", length(excel_files), "\n")
cat("Successfully processed   :", length(common_name_list), "\n")
cat("Successfully saved       :", saved_count, "\n")
cat("Failed / column missing  :", length(failed_files), "\n")
cat("Output folder            :", output_folder, "\n")
cat("Combined file            :", file.path(output_folder, "All_Common_Names_Combined.xlsx"), "\n")

if (length(failed_files) > 0) {
  cat("\nFiles with issues:\n")
  print(failed_files)
}

