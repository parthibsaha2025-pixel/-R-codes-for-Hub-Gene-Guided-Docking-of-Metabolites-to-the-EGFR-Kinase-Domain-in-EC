library(openxlsx)
library(dplyr)

input_folder <- "Path to file"

all_files <- list.files(input_folder, pattern = "\\.xlsx$",
                        recursive = TRUE, full.names = TRUE)
cat("Files found:", length(all_files), "\n")

if (length(all_files) == 0) {
  stop("No .xlsx files found — check input_folder path.")
}

# Helper: scan the first N rows (no header assumption) to find which row
# actually contains "common name" as a cell value, and return that row number
find_header_row <- function(f, max_scan_rows = 10) {
  preview <- tryCatch(
    read.xlsx(f, colNames = FALSE, rows = 1:max_scan_rows),
    error = function(e) NULL
  )
  if (is.null(preview)) return(NA_integer_)
  
  for (r in seq_len(nrow(preview))) {
    row_vals <- tolower(trimws(as.character(preview[r, ])))
    row_vals_clean <- gsub("[^a-z]", "", row_vals)
    if (any(row_vals_clean == "commonname", na.rm = TRUE)) {
      return(r)
    }
  }
  return(NA_integer_)
}

find_common_name_col <- function(col_names) {
  cleaned <- gsub("[^a-z]", "", tolower(col_names))
  match_idx <- which(cleaned == "commonname")
  if (length(match_idx) == 0) return(NA_character_)
  col_names[match_idx[1]]
}

raw_data <- bind_rows(lapply(all_files, function(f) {
  
  hdr_row <- find_header_row(f)
  
  if (is.na(hdr_row)) {
    cat("Could not locate a 'common name' header in first 10 rows of:", basename(f), "\n")
    # Show what row 1 actually looks like, to help diagnose
    row1_preview <- tryCatch(read.xlsx(f, colNames = FALSE, rows = 1),
                             error = function(e) NULL)
    if (!is.null(row1_preview)) {
      cat("   Row 1 contents:", paste(row1_preview[1, ], collapse = " | "), "\n")
    }
    return(NULL)
  }
  
  df <- tryCatch(
    read.xlsx(f, startRow = hdr_row),
    error = function(e) {
      cat("Could not read:", basename(f), "-", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(df) || nrow(df) == 0) {
    cat("Empty or unreadable file (after header fix):", basename(f), "\n")
    return(NULL)
  }
  
  cn_col <- find_common_name_col(names(df))
  if (is.na(cn_col)) {
    cat("Header row found but column still not matched in:", basename(f),
        "| columns were:", paste(names(df), collapse = ", "), "\n")
    return(NULL)
  }
  
  metabolite_val <- sub("\\.xlsx$", "", basename(f))
  
  data.frame(
    metabolite  = metabolite_val,
    common_name = df[[cn_col]],
    stringsAsFactors = FALSE
  )
}))

cat("Class of raw_data:", class(raw_data), "| Rows:", nrow(raw_data), "\n")

if (is.null(raw_data) || nrow(raw_data) == 0) {
  stop("raw_data is empty — check diagnostic messages above for header issues.")
}

# ---- Clean NA/blank common names ----
g <- trimws(raw_data$common_name)
na_like <- is.na(g) | g == "" | grepl("^n[\\s\\.\\/]*a$", g, ignore.case = TRUE, perl = TRUE)
data_clean <- raw_data[!na_like, , drop = FALSE]
data_clean$common_name <- trimws(data_clean$common_name)
cat("Rows after cleaning:", nrow(data_clean), "\n")

if (nrow(data_clean) == 0) stop("data_clean is empty after cleaning.")

# ---- Count how many distinct metabolites each common name is related to ----
metabolite_counts <- data_clean %>%
  distinct(metabolite, common_name) %>%
  count(common_name, name = "n_metabolites") %>%
  arrange(desc(n_metabolites))

cat("Unique common names found:", nrow(metabolite_counts), "\n")
print(head(metabolite_counts, 20))

# ---- Save results ----
out_dir <- "Path to file"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(metabolite_counts,
          file.path(out_dir, "metabolite_counts_per_common_name.csv"),
          row.names = FALSE)

cat("Saved to:", file.path(out_dir, "metabolite_counts_per_common_name.csv"), "\n")

