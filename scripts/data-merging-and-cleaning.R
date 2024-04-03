# Load required libraries
library(tidyverse) # Load the tidyverse package for data manipulation and visualization
library(here) # Load the here package for managing project directories
library(readxl) # Load the readxl package for reading Excel files


# Create a vector of file names containing all files with the specified pattern in the costs_hospital_level folder
file_names <- list.files(here::here("data", "costs_hospital_level"), pattern = "*.xls", full.names = TRUE)

# Read and process each Excel file in the list of file names
data_list <- map(file_names, ~ {
  read_excel(.x, skip = 1) %>% # Read the Excel file, skipping the first row
    mutate(
      date = substr(basename(.x), 1, 7), # Extract the date from the file name
      `Брой в опаковка` = as.numeric(`Брой в опаковка`)
    ) # Convert a column to numeric
})

# Combine all data frames into a single tibble
d <- bind_rows(data_list)

# Process column names and select relevant columns
d <- d |>
  mutate(
    inn = coalesce( # Fill missing values for INN column
      `Международно непатентно наименование (INN)`,
      `Междунар.непатентно наименов.`
    ),
    inn = tolower(inn) # Convert INN column values to lowercase
  ) |>
  rename( # Rename columns
    region = "РЗОК",
    hospital = "Наименование на леч.заведение",
    atc = "ATC код",
    аmount = "Колич. на лекарственото в-во",
    n_in_package = "Брой в опаковка",
    market_name = "Търговско наименование",
    icd = "МКБ код",
    n_patients = "Брой на ЗОЛ-броени за периода",
    packages = "Опаковки",
    costs = "Реимбурсна сума"
  ) |>
  select( # Select specific columns
    "region",
    "hospital",
    "atc",
    "аmount",
    "n_in_package",
    "icd",
    "n_patients",
    "market_name",
    "packages",
    "costs",
    "date",
    "inn"
  ) |>
  mutate( # Modify region column values based on certain conditions
    region = case_when(
      region == "01" ~ "Благоевград",
      region == "02" ~ "Бургас",
      region == "03" ~ "Варна",
      region == "04" ~ "Велико Търново",
      region == "06" ~ "Враца",
      region == "07" ~ "Габрово",
      region == "08" ~ "Добрич",
      region == "13" ~ "Пазарджик",
      region == "15" ~ "Плевен",
      region == "16" ~ "Пловдив",
      region == "18" ~ "Русе",
      region == "22" | region == "София - град" ~ "София-град",
      region == "23" | region == "София - област" ~ "София-област",
      region == "24" ~ "Стара Загора",
      region == "26" ~ "Хасково",
      region == "27" ~ "Шумен",
      TRUE ~ region
    ) # Keep original value if no condition is met
  ) |>
  mutate(date = ym(date)) |> # Convert date column to year-month format
  mutate( # Add additional columns for year, month, and quarter
    year = year(date),
    month = month(date),
    quarter = quarter(date)
  )

# Write the processed data to a CSV file
d |> write_csv("d.csv")
