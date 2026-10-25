# Load required packages
library(readxl)
library(writexl)
library(dplyr)

# ------------------------------------------------------------------
# 1. INPUT file and OUTPUT folder
# ------------------------------------------------------------------
input_file <- "Path to file"
output_folder <- "Path to file"
# ------------------------------------------------------------------
# 2. Read the combined file
# ------------------------------------------------------------------
combined_df <- read_excel(input_file, sheet = 1)
names(combined_df) <- trimws(names(combined_df))

cat("Total rows before removing duplicates:", nrow(combined_df), "\n")

# ------------------------------------------------------------------
# 3. Clean the Common_Name column (ensure character type, trim whitespace)
# ------------------------------------------------------------------
combined_df$Common_Name <- trimws(as.character(combined_df$Common_Name))

# ------------------------------------------------------------------
# 4. Remove duplicates based on Common_Name (case-insensitive)
#    Keeps the FIRST occurrence of each unique name (along with its Source_File)
# ------------------------------------------------------------------
no_duplicates_df <- combined_df %>%
  filter(!is.na(Common_Name) & Common_Name != "") %>%
  mutate(Common_Name_Upper = toupper(Common_Name)) %>%
  distinct(Common_Name_Upper, .keep_all = TRUE) %>%
  select(-Common_Name_Upper) %>%
  arrange(Common_Name)

cat("Total rows after removing duplicates:", nrow(no_duplicates_df), "\n")
cat("Duplicates removed:", nrow(combined_df) - nrow(no_duplicates_df), "\n")

# ------------------------------------------------------------------
# 5. Save the de-duplicated dataframe
# ------------------------------------------------------------------
out_path <- file.path(output_folder, "All_Common_Names_No_Duplicates.xlsx")
write_xlsx(no_duplicates_df, path = out_path)

cat("\nDe-duplicated file saved to:", out_path, "\n")

# 'no_duplicates_df' is now available in your R environment as the
# separate dataframe holding only unique Common Names.