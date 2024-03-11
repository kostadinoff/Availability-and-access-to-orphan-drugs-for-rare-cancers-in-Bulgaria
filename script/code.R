# Loading necessary libraries
library(tidyverse) # For data manipulation and visualization
library(ggthemes) # For additional ggplot themes
library(readxl) # For reading Excel files
library(dplyr) # For data manipulation
library(readr) # For reading and writing data
library(janitor) # For cleaning data
library(rsample) # For splitting data into training and testing sets
library(stringr) # For string manipulation
library(scales) # For graphical scales
library(ggpattern) # For adding pattern fills to ggplot objects
library(infer) # For statistical inference
library(gt) # For creating tables with the gt package
library(patchwork) # For combining multiple ggplot objects
library(easystats) # For summarizing and visualizing statistical analyses
library(gtsummary) # For creating summary tables
library(rstatix) # For pipe-friendly statistical functions
library(ggridges) # For plotting ridgeline plots
library(ggrepel) # For avoiding overlapping labels in ggplot
library(boot) # For bootstrapping
library(ggtext) # For formatting text in ggplot
library(sf) # For working with spatial data

# Load the 'here' package to specify the location of the data folder
library(here)

# Setting up locale
Sys.setlocale("LC_TIME", "English")

# Define a custom theme for ggplot
theme_nice <- ggthemes::theme_tufte() +
  theme(
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.length = unit(4, "mm"),
    plot.title = element_text(
      family = "Sofia Sans Condensed",
      size = 20,
      hjust = 0,
      vjust = 2
    ),
    plot.subtitle = element_text(family = "Sofia Sans Condensed",
                                 size = 18),
    plot.caption = element_text(
      family = "Sofia Sans Condensed",
      size = 16,
      hjust = 1
    ),
    axis.title = element_text(family = "Sofia Sans Condensed",
                              size = 18),
    axis.text = element_text(family = "Sofia Sans Condensed",
                             size = 16),
    axis.text.x = element_text(margin = margin(5, b = 10)),
    strip.text = element_text(family = "Sofia Sans Condensed",
                              size = 18)
  )

# Set the defined theme as the default theme
theme_set(theme_nice)

# Read CSV file from the data folder
d <- read_csv(here::here("data", "d.csv")) |>
  mutate(
    market_name = tolower(market_name),
    hospital = tolower(hospital),
    market_name = case_when(
      market_name == "inlita" ~ "inlyta",
      market_name == "imatinib actavis group" ~ "imatinib actavis",
      market_name == 'epirubicin "ebewe"' ~ "epirubicin ebewe",
      market_name == "\"epirubicin \"\"ebewe\"\"\"" ~ "epirubicin ebewe",
      market_name == 'paclitaxel "ebewe"' ~ "paclitaxel ebewe",
      market_name == "5-fluorouracil ebe we" ~ "5-fluorouracil ebewe",
      market_name == "\"paclitaxel \"\"ebewe\"\"\"" ~ "paclitaxel ebewe",
      market_name == 'carboplatin "ebe we"' ~ "carboplatin ebewe",
      market_name == "carboplatin ebe we" ~ "carboplatin ebewe",
      market_name == 'epirubicin "ebewe"' ~ "epirubicin ebewe",
      market_name == 'epirubicin "ebe we"' ~ "epirubicin ebewe",
      market_name == 'carboplatin "ebewe"' ~ "carboplatin ebewe",
      market_name == 'mitoxantron "ebewe"' ~ "mitoxantron ebewe",
      market_name == "epirubicin ebe we" ~ "epirubicin ebewe",
      market_name == "methotrexate ebe we" ~ "methotrexate ebewe",
      market_name == 'mitoxantron "ebe we"' ~ "mitoxantron ebewe",
      market_name == "mitoxantron ebe we" ~ "mitoxantron ebewe",
      market_name == "oxaliplatin ebe we" ~ "oxaliplatin ebewe",
      market_name == 'paclitaxel "ebe we"' ~ "paclitaxel ebewe",
      market_name == "paclitaxel ebe we" ~ "paclitaxel ebewe",
      market_name == "roferon - a" ~ "roferon-a",
      market_name == "vinorelbin ebe we " ~ "vinorelbin ebewe",
      market_name == "gefiticon" ~ "gefitinib mylan",
      market_name == "imatinib teva pharma" ~ "imatinib teva",
      market_name == "myocet liposomal" ~ "myocet",
      TRUE ~ market_name
    )
  )

# Read the PDL in Bulgaria from the specified URL
pdl_online <- read_csv(
  "https://data.egov.bg/resource/download/ba1ab4be-8695-4377-9edf-464233e2fc39/csv"
) |>
  select(
    atc = 1,
    inn = 2,
    market_name = 3,
    company = 8,
    price = 11,
    packege_price = 13,
    reimbursement = 14,
    extra = 17,
  ) |>
  mutate(date = as.Date(
    str_extract(extra,
                "\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b"),
    format = "%d.%m.%Y"
  )) |>
  select(-extra) |>
  mutate(
    market_name = tolower(market_name),
    company = tolower(company),
    cancer_drug = str_split_i(atc, "", 1)
  ) |>
  filter(cancer_drug == "L") |>
  select(-cancer_drug, -company, -atc, -inn, -price, -packege_price) |>
  group_by(market_name) |>
  filter(date == min(date))

# Read Excel file
ema <- janitor::clean_names(read_csv(here::here("data", "ema.csv")))
ema <-
  ema |>
  mutate(market_name = tolower(market_name))
# Joining EMA and NHIF data

d <- d |>
  left_join(ema,
            by = c("market_name"),
            relationship =
              "many-to-many")



# How many drugs are not into EMA list

d |>
  mutate(cancer = str_split_i(icd, "", 1),
         cancer_drug = str_split_i(atc.x, "", 1)) |>
  filter(cancer == "C",
         cancer_drug == "L") |>
  filter(cancer == "C" &
           cancer_drug == "L") |>
  filter(is.na(url)) |>
  count(market_name)



# Fine tuning the data and converting costs in BGN to EUR

d <- d |>
  mutate(cancer = str_split_i(icd, "", 1),
         cancer_drug = str_split_i(atc.x, "", 1)) |>
  filter(cancer == "C",
         cancer_drug == "L") |>
  select(
    -c(
      atc.y,
      inn.y,
      vet_pharmacotherapeutic_group,
      at_cvet_code,
      category,
      species,
      cancer_drug,
      biosimilar,
      at_cvet_code,
      biosimilar,
      species
    )
  ) |>
  mutate(
    costs = costs / 1.95583,
    orphan_medicine = case_when(orphan_medicine == "yes" ~ "yes",
                                TRUE ~ "no")
  ) |>
  mutate(
    region_en = case_when(
      region == "Благоевград" ~ "Blagoevgrad",
      region == "Бургас" ~ "Burgas",
      region == "Варна" ~ "Varna",
      region == "Велико Търново" ~ "Veliko Tarnovo",
      region == "Враца" ~ "Vratsa",
      region == "Габрово" ~ "Gabrovo",
      region == "Добрич" ~ "Dobrich",
      region == "Пазарджик" ~ "Pazardzhik",
      region == "Плевен" ~ "Pleven",
      region == "Пловдив" ~ "Plovdiv",
      region == "Русе" ~ "Ruse",
      region == "София-град" ~ "Sofia-city",
      region == "София-област" ~ "Sofia",
      region == "Стара Загора" ~ "Stara Zagora",
      region == "Хасково" ~ "Haskovo",
      region == "Шумен" ~ "Shumen",
      TRUE ~ region
    )
  )


# Orphan drugs in NHIF list and PDL

pdl <-
  tibble::tribble(
    ~ market_name,
    ~ pdl_date,
    "adcetris",
    "16.12.2014",
    "besponsa",
    "24.03.2018",
    "blincyto",
    "21.12.2017",
    "darzalex",
    "20.12.2018",
    "gazyvaro",
    "08.05.2015",
    "imnovid",
    "23.12.2019",
    "kyprolis",
    "24.04.2019",
    "mylotarg",
    "20.09.2019",
    "nexavar",
    "22.01.2009",
    "ninlaro",
    "21.12.2018",
    "onivyde pegylated liposomal",
    "02.08.2019",
    "polivy",
    "30.12.2020",
    "rydapt",
    "11.05.2018",
    "vyxeos liposomal",
    "14.12.2022",
    "xospata",
    "11.12.2020"
  ) |>
  mutate(pdl_date = lubridate::dmy(pdl_date))

# How many orphan drugs are in the EMA list and whats is there time distribution


ema_orha <- ema |>
  filter(status == "Authorised") |>
  mutate(cancer_drug = str_split_i(atc, "", 1)) |>
  filter(cancer_drug == "L") |>
  filter(orphan_medicine == "yes")


ema |>
  filter(status == "Authorised") |>
  mutate(cancer_drug = str_split_i(atc, "", 1)) |>
  filter(cancer_drug == "L") |>
  filter(orphan_medicine == "yes") |>
  select(market_name, marketing_authorisation_date) |>
  mutate(
    year = year(marketing_authorisation_date),
    month = month(marketing_authorisation_date),
    year_month = paste(year, month, sep = "-")
  ) |>
  count(year) |>
  mutate(cum_n = cumsum(n),
         pct_change = (cum_n - lag(cum_n)) / lag(cum_n) * 100)



# How many orphan drugs are funded by NHIF

ema |>
  mutate(cancer_drug = str_split_i(atc, "", 1)) |>
  filter(status == "Authorised") |>
  filter(cancer_drug == "L") |>
  filter(orphan_medicine == "yes") |>
  filter(market_name %in% d$market_name) |>
  select(market_name, condition_indication)



# How many orphan drugs are in the NHIF list and whats is there time distribution

d |>
  filter(orphan_medicine == "yes") |>
  select(market_name, date) |>
  mutate(
    year = year(date),
    month = month(date),
    year_month = paste(year, month, sep = "-")
  ) |>
  group_by(year) |>
  count(market_name) |>
  select(-n) |>
  ungroup() |>
  count(year) |>
  mutate(pct_change = (n - lag(n)) / lag(n) * 100)

# Numbers of orhans EMA/ Bulgaria comparison

time_0 <-
  ema |>
  mutate(cancer_drug = str_split_i(atc, "", 1)) |>
  filter(status == "Authorised") |>
  filter(cancer_drug == "L") |>
  filter(orphan_medicine == "yes") |>
  mutate(year = year(marketing_authorisation_date)) |>
  count(year) |>
  ungroup() |>
  mutate(sumEMA = cumsum(n)) |>
  select(-n) |>
  left_join(
    pdl |>
      mutate(year = year(pdl_date)) |>
      group_by(year) |>
      count(year) |>
      ungroup() |>
      mutate(sumBG = cumsum(n)) |>
      select(-n)
  ) |>
  tidyr::fill(sumBG, .direction = "down") |>
  mutate(sumBG = replace_na(sumBG, 0)) |>
  mutate(pct = (sumBG / sumEMA) * -100) |>
  ggplot(aes(x = year, y = sumEMA)) +
  geom_line(size = 1.5,
            color = "#1D785A") +
  geom_point(size = 3,
             color = "white") +
  geom_point(size = 1,
             color = "#1D785A") +
  geom_line(aes(y = sumBG),
            color = "#750E21",
            size = 1.5) +
  geom_point(aes(y = sumBG),
             size = 3,
             color = "white") +
  geom_point(aes(y = sumBG),
             size = 1,
             color = "#750E21") +
  geom_rangeframe(data = tibble(sumEMA = c(0, 50),
                                year = c(2006, 2023))) +
  scale_x_continuous(breaks = seq(2006, 2023, 1)) +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  geom_text(
    aes(label = sumEMA),
    color = "#1D785A",
    hjust = 0.75,
    vjust = -1,
    family = "Sofia Sans Condensed",
    fontface = "bold",
    size = 5
  ) +
  geom_text(
    aes(y = sumBG,
        label = sumBG),
    color = "#750E21",
    hjust = 0.5,
    vjust = +1.5,
    family = "Sofia Sans Condensed",
    fontface = "bold",
    size = 5
  ) +
  expand_limits(y = c(-5, 55)) +
  annotate(
    geom = "label",
    x = 2018,
    y = -3,
    label = "Bulgarian PLD",
    color = "#750E21",
    family = "Sofia Sans Condensed",
    fontface = "bold",
    size = 5
  ) +
  annotate(
    geom = "label",
    x = 2018,
    y = 40,
    label = "EMA",
    color = "#1D785A",
    family = "Sofia Sans Condensed",
    fontface = "bold",
    size = 5
  ) +
  geom_ribbon_pattern(
    aes(ymin = (sumBG + 0.5), ymax = (sumEMA - 0.5)),
    pattern = "stripe",
    pattern_angle = 90,
    pattern_density = 0.02,
    pattern_spacing = 0.02,
    fill = "white"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "",
       subtitle = "A. # of OCD authorised by EMA vs. # OCD included
       in the Bulgarian PDL list")


# What is the lag from the market authorisation to the first NHIF funding

time_1 =
  d |>
  filter(orphan_medicine == "yes") |>
  group_by(market_name) |>
  summarise(
    first_marketing_authorisation_date = first(marketing_authorisation_date),
    first_date = first(date),
    days_difference = as.numeric(difftime(
      first(date), first(marketing_authorisation_date), units = "days"
    ))
  ) |>
  mutate(market_name = as_factor(market_name),
         market_name = str_to_title(market_name)) |>
  filter(year(first_date) > 2020) |>
  ggplot(aes(x = days_difference, y = fct_reorder(market_name, days_difference))) +
  geom_col(aes(fill = ifelse(
    days_difference == max(days_difference), "no", "color"
  ))) +
  geom_rangeframe(data =
                    tibble(
                      days_difference = c(0, 1647),
                      market_name = c("Vyxeos Liposomal", "Polivy")
                    )) +
  scale_fill_manual(values = c("grey50", "#1D785A"), guide = "none") +
  scale_x_continuous(
    breaks = seq(0, 1647, 183),
    labels = scales::number_format(accuracy = 1, suffix = "")
  ) +
  geom_vline(
    xintercept = seq(183, 1647, 183),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.4,
    color = "white"
  ) +
  expand_limits(x = 2000) +
  geom_text(
    aes(label = ifelse(
      days_difference == max(days_difference),
      round(days_difference),
      ""
    )),
    hjust = -0.5,
    size = 5,
    vjust = +0.5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "#1D785A"
  ) +
  geom_text(
    aes(label = ifelse(
      days_difference == max(days_difference),
      "",
      round(days_difference)
    )),
    hjust = -0.5,
    size = 5,
    vjust = +0.5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "grey50"
  ) +
  labs(
    subtitle = "C. Days from the EMA marketing authorisation to the \nfirst NHIF funding",
    y = "",
    x = "",
    caption = ""
  )

# what is the lag from the market authorisation to the PDL inclusion

times =
  d |>
  filter(orphan_medicine == "yes") |>
  group_by(market_name) |>
  left_join(pdl, by = "market_name") |>
  summarise(
    first_marketing_authorisation_date = first(marketing_authorisation_date),
    first_pld_date = first(pdl_date),
    first_date = first(date),
    days_difference_pdl = as.numeric(difftime(
      first(pdl_date), first(marketing_authorisation_date), units = "days"
    )),
    days_difference_market = as.numeric(difftime(
      first(date), first(marketing_authorisation_date), units = "days"
    )),
    interlag = days_difference_market - days_difference_pdl
  ) |>
  mutate(market_name = as_factor(market_name),
         market_name = str_to_title(market_name))

# What is the lag from the market authorisation to the PDL inclusion

time_2 <-
  times |>
  ggplot(aes(x = days_difference_pdl, y = fct_reorder(market_name, days_difference_pdl))) +
  geom_col(aes(fill = ifelse(
    days_difference_pdl == max(days_difference_pdl), "no", "color"
  ))) +
  geom_rangeframe(data =
                    tibble(
                      days_difference_pdl = c(0, 2555),
                      market_name = c("Imnovid", "Rydapt")
                    )) +
  scale_fill_manual(values = c("grey50", "#1D785A"), guide = "none") +
  scale_x_continuous(
    breaks = seq(0, 2555, 365),
    labels = scales::number_format(accuracy = 1, suffix = "")
  ) +
  geom_vline(
    xintercept = seq(182.5, 2555, 182.5),
    linetype = "dashed",
    linewidth = 0.5,
    alpha = 0.8,
    color = "white"
  ) +
  expand_limits(x = 2600) +
  geom_text(
    aes(label = ifelse(
      days_difference_pdl == max(days_difference_pdl),
      round(days_difference_pdl),
      ""
    )),
    hjust = -0.25,
    size = 5,
    vjust = +0.5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "#1D785A"
  ) +
  geom_text(
    aes(label = ifelse(
      days_difference_pdl == max(days_difference_pdl),
      "",
      round(days_difference_pdl)
    )),
    hjust = -0.25,
    size = 5,
    vjust = +0.5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "grey50"
  ) +
  labs(
    y = "",
    x = "",
    caption = "All OCD listed in the Bulgarian PDL",
    subtitle = "B. External delay between the marketing authorisation \nand the PDL inclusion"
  )


# Internal lag - from PDL inclusion to the NHIF funding

time_3 <-
  times |>
  filter(year(first_date) > 2020) |>
  ggplot(aes(x = interlag, y = fct_reorder(market_name, interlag))) +
  geom_col(aes(fill = ifelse(interlag == max(interlag), "no", "color"))) +
  geom_rangeframe(data =
                    tibble(
                      interlag = c(0, 540),
                      market_name = c("Onivyde Pegylated Liposomal", "Polivy")
                    )) +
  scale_fill_manual(values = c("grey50", "#1D785A"), guide = "none") +
  scale_x_continuous(
    breaks = seq(0, 540, 60),
    labels = scales::number_format(accuracy = 1, suffix = "")
  ) +
  geom_vline(
    xintercept = seq(60, 540, 60),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.4,
    color = "white"
  ) +
  expand_limits(x = 600) +
  geom_text(
    aes(label = ifelse(
      interlag == max(interlag),
      round(interlag),
      ""
    )),
    hjust = -0.5,
    size = 5,
    vjust = +0.5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "#1D785A"
  ) +
  geom_text(
    aes(label = ifelse(
      interlag == max(interlag), "",
      round(interlag)
    )),
    hjust = -0.5,
    size = 5,
    vjust = +0.5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "grey50"
  ) +
  labs(
    subtitle = "D. Internal delay from PDL inclusion \nto NHIF funding",
    y = "",
    x = "",
    caption = "New ODCT funded in 2021-2023 \nEach block signifies a two-month period"
  )

# Combining the plots

(time_0 + time_2) / (time_1 + time_3)
ggsave(
  "time.png",
  path = here::here(),
  bg = "white",
  width = 40,
  height = 30,
  units = "cm",
  dpi = 900
)


# dataset of all ODCT

odct = times |>
  mutate(market_name = tolower(market_name)) |>
  select(market_name)

non_odct_time <-
  pdl_online |>
  filter(market_name %in% ema$market_name) |>
  distinct(market_name, .keep_all = TRUE) |>
  select(market_name, date) |>
  filter(!market_name %in% odct$market_name) |>
  right_join(ema, by = "market_name") |>
  filter(!is.na(date)) |>
  filter(!is.na(marketing_authorisation_date)) |>
  select(market_name, date, marketing_authorisation_date) |>
  mutate(marketing_authorisation_date =
           as.POSIXct(marketing_authorisation_date,
                      format = "%Y-%m-%d")) |>
  mutate(days_difference_pdl = as.numeric(date - ymd(marketing_authorisation_date)),
         type = "non-ODCT") |>
  select(market_name, days_difference_pdl, type)


odct_times <-
  times |>
  select(market_name, days_difference_pdl) |>
  mutate(type = "ODCT")

days_comparison <- rbind(non_odct_time, odct_times) |>
  ungroup()

days_comparison |>
  filter(type == "ODCT") |>
  arrange(desc(days_difference_pdl)) |>
  
  
  boot_medians =
  days_comparison  |>
  ungroup() |>
  bootstraps(times = 4000) |>
  mutate(median = map(splits, ~ analysis(.x) %>%
                        summarise(
                          median =
                            median(days_difference_pdl,
                                   na.rm = TRUE),
                          .by = type
                        ))) |>
  unnest(median)


boot_medians |>
  select(id, type, median) |>
  pivot_wider(names_from = type, values_from = median) |>
  mutate(diff = `non-ODCT` - ODCT) |>
  count(diff > 0)

# Set the number of bootstrap samples
num_bootstraps <- 4000

# Load necessary libraries
library(boot)

# Assuming your data is named 'days_comparison'
# Replace 'your_variable' with the actual variable/column name you are interested in
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  non_ODCT_median <-
    median(d$days_difference_pdl[d$type == "non-ODCT"])
  ODCT_median <- median(d$days_difference_pdl[d$type == "ODCT"])
  median_difference <- non_ODCT_median - ODCT_median
  
  return(median_difference)
}

# Perform bootstrap
boot_results <- boot(data = days_comparison,
                     statistic = boot_fun,
                     R = num_bootstraps)
boot.ci(boot_results, type = "perc")




cost0 =
  d |>
  filter(market_name %in% odct$market_name) |>
  group_by(date) |>
  summarise(sum = sum(costs)) |>
  ggplot(aes(x = date, y = sum)) +
  geom_rangeframe(data =
                    tibble(date = c(
                      as.Date("2020-07-01"),
                      as.Date("2023-09-01")
                    ),
                    sum = c(1e6, 4e6))) +
  # geom_vline(
  #   xintercept = seq(as.Date("2020-07-01"),
  #                    by = "2 month",
  #                    length.out = 20),
  #   linetype = "dashed",
  #   linewidth = 0.5,
  #   color = "#B6BBC4"
  # ) +
  geom_hline(
    yintercept = seq(1e6, 4e6, 0.5e6),
    linetype = "dashed",
    linewidth = 0.5,
    color = "#B6BBC4"
  ) +
  geom_line(linewidth = 1.1,
            alpha = 1.1,
            color = "#1D785A") +
  # geom_point(size = 2.5,
  #            color = "white") +
  # geom_point(size = 1.5,
  #            color = "black") +
  geom_smooth(
    # method = "lm",
    se = FALSE,
    color = "#1D785A",
    linetype = "dashed",
    size = 0.5
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y",
    limits = c(as.Date("2020-07-01"),
               as.Date("2023-09-01"))
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(
      suffix = "M €",
      prefix = "",
      scale = 1e-6,
      accuracy = 0.1
    ),
    breaks = seq(1e6, 4e6, 0.5e6)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-07-01"), as.Date("2023-09-01"))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "",
       subtitle =  "A. OCD expenditures over time")

# Plotting the total # of patients per month

cost1 =
  d |>
  group_by(date) |>
  filter(market_name %in% odct$market_name) |>
  summarise(sum_patients = sum(n_patients)) |>
  ggplot(aes(x = date, y = sum_patients)) +
  geom_line() +
  geom_rangeframe(data =
                    tibble(
                      date = c(as.Date("2020-07-01"),
                               as.Date("2023-09-01")),
                      sum_patients = c(200, 600)
                    )) +
  # geom_vline(
  #   xintercept = seq(as.Date("2020-07-01"),
  #                    by = "2 month",
  #                    length.out = 20),
  #   linetype = "dashed",
  #   linewidth = 0.5,
  #   color = "#B6BBC4"
  # ) +
  geom_smooth(
    # method = "lm",
    se = FALSE,
    color = "#1D785A",
    linetype = "dashed",
    size = 0.5
  ) +
  geom_hline(
    yintercept = seq(200, 600, 100),
    linetype = "dashed",
    linewidth = 0.5,
    color = "#B6BBC4"
  ) +
  geom_line(linewidth = 1.1,
            alpha = 1.1,
            color = "#1D785A") +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y",
    limits = c(as.Date("2020-07-01"), as.Date("2023-09-01"))
  ) +
  scale_y_continuous(
    labels = scales::comma_format(suffix = "", prefix = ""),
    breaks = seq(200, 600, 100)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-07-01"), as.Date("2023-09-01"))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "",
       y = "",
       subtitle = "A. Number of patients treated with OCD per month")

# Analysis of the extree costs


d |>
  group_by(date) |>
  filter(date == "2023-01-01" | date == "2022-12-01") |>
  filter(market_name %in% odct$market_name) |>
  group_by(date, market_name) |>
  summarise(sum = sum(costs),
            sum_patients = sum(n_patients)) |>
  pivot_wider(names_from = date,
              values_from = c(sum, sum_patients)) |>
  mutate(
    `sum_2023-01-01` = replace_na(`sum_2023-01-01`, 0),
    `sum_2022-12-01` = replace_na(`sum_2022-12-01`, 0),
    `sum_patients_2023-01-01` = replace_na(`sum_patients_2023-01-01`, 0),
    `sum_patients_2022-12-01` = replace_na(`sum_patients_2022-12-01`, 0)
  ) |>
  mutate(
    dif_sum = `sum_2023-01-01` - `sum_2022-12-01`,
    dif_patients = `sum_patients_2023-01-01` - `sum_patients_2022-12-01`
  )


# Plotting the total costs per month (orha medicines) per patient

cost2  =
  d  |>
  group_by(date, orphan_medicine)  |>
  summarise(sum = sum(costs),
            n_patients = sum(n_patients))  |>
  pivot_wider(names_from = orphan_medicine,
              values_from = c(sum, n_patients))  |>
  ungroup()  |>
  mutate(
    cpt_orha = sum_yes / n_patients_yes,
    cpt_nonorhn = sum_no / n_patients_no,
    ratio = (sum_yes / n_patients_yes) / (sum_no / n_patients_no)
  )  |>
  ggplot(aes(x = date, y = ratio)) +
  geom_line(linewidth = 0.9,
            alpha = 1.1,
            color = "#1D785A") +
  geom_smooth(
    se = FALSE,
    color = "#1D785A",
    lty = 2,
    alpha = 0.7,
    linewidth = 0.7
  ) +
  geom_rangeframe(data =
                    tibble(date = c(
                      as.Date("2020-07-01"),
                      as.Date("2023-09-01")
                    ),
                    ratio = c(3.5, 5))) +
  # geom_vline(
  #   xintercept = seq(as.Date("2020-07-01"),
  #                    by = "2 month",
  #                    length.out = 20),
  #   linetype = "dashed",
  #   linewidth = 0.4,
  #   alpha = 0.5,
  #   color = "#B6BBC4"
  # ) +
  # geom_hline(
  #   yintercept = c(3.72, 4.86),
#   linetype = 1,
#   linewidth = 0.6,
#   alpha = 0.8,
#   color = "#7E2553") +
geom_hline(
  yintercept = seq(3.5, 5, 0.5),
  linetype = "dashed",
  linewidth = 0.4,
  alpha = 0.7,
  color = "gray50"
)  +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y",
    limits = c(as.Date("2020-07-01"),
               as.Date("2023-09-01"))
  ) +
  scale_y_continuous(
    labels = scales::comma_format(suffix = "", prefix = ""),
    breaks = seq(3.5, 5, 0.5)
  ) +
  coord_cartesian(xlim = c(as.Date("2020-07-01"), as.Date("2023-09-01"))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = "",
    subtitle = "D. Expenditure per patient ratio OCD vs. non-OCD",
    y = "",
    caption = "Month expenditures for OCD and non-OCD are divided by
       the corresponding number of patients treated per month."
  )

# Ploting the total costs per orphan medicines

cost3 =
  d %>%
  filter(orphan_medicine == "yes") %>%
  group_by(market_name) %>%
  summarise(sum = sum(costs)) %>%
  mutate(market_name = str_to_title(market_name),
         prop = sum / sum(sum)) %>%
  ggplot(aes(y = fct_reorder(market_name, sum), x = sum)) +
  geom_col(fill = "#1D785A") +
  geom_rangeframe(data = tibble(
    sum = c(0, 24e6),
    market_name = c("Vyxeos Liposomal", "Darzalex")
  )) +
  scale_x_continuous(
    labels = scales::comma_format(
      suffix = "M €",
      prefix = "",
      scale = 1e-6
    ),
    breaks = c(seq(0, 24e6, 2e6), 24e6)
  ) +
  geom_text(
    aes(label = paste0(
      scales::dollar(
        sum,
        suffix = "M €",
        prefix = "",
        accuracy = 0.1,
        scale = 1e-6
      ),
      " (",
      label = scales::percent(prop, accuracy = 0.1),
      ")"
    )),
    hjust = -0.05,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    size = 4,
    fontface = "bold",
    color = "#1D785A"
  ) +
  geom_vline(
    xintercept = seq(2e6, 22e6, 2e6),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.7,
    color = "white"
  ) +
  expand_limits(x = 30e6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = "",
    y = "",
    subtitle = "B. Total expenditures per OCD",
    capion = "One block represents 2M €"
  )


# Comparison of the total costs per ODCT and non-ODCT

com_d =
  d |>
  group_by(market_name) |>
  summarise(sum = sum(costs)) |>
  mutate(orhan_medicine =
           ifelse(market_name %in% d$market_name[d$orphan_medicine == "yes"],
                  "OCD", "non-OCD"))

com_d |>
  mutate(prop = round(sum / sum(sum), 3)) |>
  arrange(desc(prop)) |>
  mutate(cum_p = cumsum(prop),
         cumsum = cumsum(sum))


cost4 =
  com_d |>
  ggplot(aes(
    x = sum,
    y = fct_rev(orhan_medicine),
    fill = orhan_medicine
  )) +
  stat_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2,
    scale = 3,
    color = "white"
  ) +
  scale_fill_manual(values = c("#750E21", "#1D785A"), guide = "none") +
  geom_rangeframe(data = tibble(
    sum = c(2.6, 26e7),
    orhan_medicine = c("OCD", "non-OCD")
  )) +
  scale_x_log10(
    labels = scales::dollar_format(
      suffix = "€",
      prefix = "",
      accuracy = 0.1,
    ),
    breaks = c(2.6, 26, 260, 26e2, 26e3, 26e4, 26e5, 26e6, 26e7)
  ) +
  annotate(
    geom = "label",
    x = 2156224,
    y = 0.7,
    label = "2 156 224€",
    family = "Sofia Sans Condensed",
    size = 4,
    fontface = "bold",
    color = "#1D785A"
  ) +
  annotate(
    geom = "label",
    x = 150000,
    y = 1.8,
    label = "235 535€",
    family = "Sofia Sans Condensed",
    size = 4,
    fontface = "bold",
    color = "#750E21"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    x = "",
    y = "",
    subtitle = "C. Distribution of total expenditure per type of product \nOCD vs. non-OCD",
    caption = "X asis is represented on a log scale.
       White lines represent the median"
  )

# Combining the plots

(cost0 + cost3) / (cost2 + cost4)
ggsave(
  "costs.png",
  path = here::here(),
  bg = "white",
  width = 36,
  height = 29,
  units = "cm",
  dpi = 900
)

# Expenditure per over time
d |> group_by(date, orphan_medicine) |>
  summarise(sum = sum(costs),
            sum_patients = sum(n_patients)) |>
  ungroup() |>
  group_by(orphan_medicine, month(date)) |>
  get_summary_stats() |>
  filter(orphan_medicine == "yes" & variable == "sum")

d |> group_by(date, orphan_medicine) |>
  summarise(sum = sum(costs),
            sum_patients = sum(n_patients)) |>
  filter(orphan_medicine == "no") |>
  ungroup() |>
  summarise(median(sum))

# Difference in medians

kt <-  kruskal_test(sum ~ orhan_medicine, data = com_d)
kes <-  kruskal_effsize(sum ~ orhan_medicine, data = com_d)

## Mann-Whitney's U Tes

wc = wilcox_test(
  sum ~ orhan_medicine,
  data = com_d,
  paired = FALSE,
  alternative = "two.sided"
)

rank_biserial(sum ~ orhan_medicine, data = com_d)

cliffs_delta(sum ~ orhan_medicine, data = com_d)

original_data <- com_d$sum
group_indicator <- com_d$orhan_medicine

## Bootstrap

### Set the number of bootstrap samples
num_bootstraps <- 4000

### Define the bootstrapping function for median ratio
boot_fun <- function(data, indices) {
  sample_data <- data[indices]
  median_ratio <-
    median(sample_data[group_indicator[indices] == "ODCT"]) /
    median(sample_data[group_indicator[indices] == "non-ODCT"])
  return(c(median_ratio))
}


### Perform bootstrap
boot_results <- boot(original_data, boot_fun, R = num_bootstraps)
boot.ci(boot_results)

### Tidy up the results
tidy_boot_results <- tidy(boot_results)
tidy_boot_results

### Visualize the results

# boot_medians <-
#   com_d |>
#   specify(sum  ~ orhan_medicine) %>%
#   generate(reps = 4000, type = "bootstrap") %>%
#   calculate("diff in medians", order = c("ODCT", "non-ODCT"))
#
# boostrapped_confint <-
#   boot_medians |>
#   get_confidence_interval()

# Permutation test

## Step 2: Invent a world where δ is null
# diff_medians_null <- com_d %>%
#   specify(sum  ~ orhan_medicine) %>%
#   hypothesize(null = "independence") %>%
#   generate(reps = 5000, type = "permute") %>%
#   calculate("diff in medians",
#             order = c("ODCT", "non-ODCT"))

## Step 3: Put actual observed δ in the null world and see if it fits
# diff_medians_null %>%
#   visualize() +
#   geom_vline(xintercept = diff_medians$stat, size = 1, color = "#77002c")
#
#
# diff_medians_null  |>
#   get_p_value(obs_stat = diff_medians,
#               direction = "both") |>
#   mutate(p_value_clean = scales::pvalue(p_value))


# CHAINS <- 4
# ITER <- 2000
# WARMUP <- 1000
# BAYES_SEED <- 1234
# options(mc.cores = parallel::detectCores())  # Use all cores
#
# brms_uneq <- brm(
#   bf(log(sum)  ~ orhan_medicine, sigma ~ orhan_medicine),
#   data = mutate(com_d, orhan_medicine = fct_rev(orhan_medicine)),
#   chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED
# )
#
# ## draw from the posterior
#
# brms_uneq %>%
#   spread_draws(b_Intercept, b_orhan_medicineno) |>
#   mutate(diff = b_orhan_medicineno + b_Intercept) |>
#   mutate(diff = exp(diff),
#          intercept = exp(b_Intercept)) |>
#

# Plotting the average patient treated with an orphan medicine

d |>
  group_by(date, orphan_medicine, region_en)  |>
  summarise(n_patients = sum(n_patients))  |>
  pivot_wider(names_from = orphan_medicine,
              values_from = n_patients)  |>
  mutate(ratio = (yes / (no + yes) * 100),
         ratio = coalesce(ratio, 0)) |>
  ungroup() |>
  group_by(region_en) |>
  summarise(ratio = mean(ratio, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(region_en, ratio), y = ratio)) +
  geom_col(aes(fill = factor(
    ifelse(region_en == "Sofia-city", "Highlighted", "Normal")
  ))) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format(suffix = "%", prefix = ""),
                     breaks = seq(0, 3, 1)) +
  expand_limits(y = 3) +
  geom_rangeframe(data = tibble(
    region_en = c("Sofia-region", "Varna"),
    ratio = c(0, 3)
  )) +
  geom_hline(
    yintercept = seq(0, 3, 0.5),
    linetype = "dashed",
    linewidth = 0.5,
    alpha = 0.6,
    color = "#B6BBC4"
  ) +
  geom_hline(
    yintercept = 1.372348,
    linetype = 2,
    linewidth = 0.8,
    alpha = 0.5,
    color = "#00c9b2"
  ) +
  geom_text(
    aes(label = paste0(round(ratio, 2), "%")),
    hjust = -0.2,
    fontface = "bold",
    size = 5,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "#002f3c"
  ) +
  annotate(
    geom = "label",
    x = 3.5,
    y = 1.4,
    size = 6,
    label = "Overall average - 1.37%",
    family = "Sofia Sans Condensed",
    color = "#002f3c",
    fontface = "bold"
  ) +
  scale_fill_manual(name = "area", values = c("#005b4d", "#002f3c")) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "Average monthly % of Cancer patients \ntreated with orpha designated medicines",
       caption = "Source: NHIF, 2020-2023")

d |> group_by(date, region_en) |>
  summarise(n_patients = sum(n_patients),
            sum = sum(costs)) |>
  mutate(per_pat = sum / n_patients) |>
  ggplot(aes(x = date, y = per_pat)) +
  geom_hline(
    yintercept = seq(0, 2000, 500),
    linetype = "dashed",
    linewidth = 0.5,
    color = "#B6BBC4"
  ) +
  geom_line() +
  facet_wrap(~ region_en, ncol = 4) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y",
    limits = c(as.Date("2020-07-01"), as.Date("2023-09-01"))
  ) +
  scale_y_continuous(
    labels = scales::comma_format(suffix = "", prefix = ""),
    breaks = seq(0, 2000, 500)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none",
    axis.line = element_line(linewidth = 0.4, color = "black")
  ) +
  labs(x = "",
       y = "Total cancer medicine expendency per patient",
       caption = "Source: NHIF, 2020-2023")

# Regional differences

## ODCT per region

hospitals =
  d |>
  filter(market_name %in% odct$market_name) |>
  count(region_en, hospital) |>
  select(-n) |>
  count(region_en) |>
  rename(n_hospitals = n)


regions0 =
  d |>
  filter(market_name %in% odct$market_name) |>
  count(region_en, market_name) |>
  select(-n) |>
  count(region_en) |>
  mutate(prop = n / 15) |>
  left_join(hospitals, by = "region_en") |>
  ggplot(aes(y = reorder(region_en, prop), x = prop)) +
  geom_col(aes(fill = ifelse(region_en == "Sofia-city", "no", "color"))) +
  geom_rangeframe(data = tibble(
    region_en = c("Varna", "Gabrovo"),
    prop = c(0, 1)
  )) +
  scale_fill_manual(values = c("grey50", "#1D785A"), guide = "none") +
  scale_x_continuous(
    labels = scales::percent_format(
      suffix = "%",
      prefix = "",
      accuracy = 1
    ),
    breaks = seq(0, 1, 0.2)
  ) +
  geom_text(
    aes(label =
          ifelse(
            region_en == "Sofia-city",
            paste0(n),
            paste0("")
          )),
    hjust = -0.2,
    fontface = "bold",
    size = 6,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "#1D785A"
  ) +
  geom_text(
    aes(label =
          ifelse(
            region_en != "Sofia-city",
            paste0(n),
            paste0("")
          )),
    hjust = -0.2,
    fontface = "bold",
    size = 6,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "grey50"
  ) +
  geom_text(
    aes(label = paste0(n_hospitals, "(H)")),
    hjust = +1,
    fontface = "bold",
    size = 6,
    family = "Sofia Sans Condensed",
    color = "white"
  ) +
  expand_limits(x = 1.05) +
  geom_vline(
    xintercept = seq(0.2, 1, 0.2),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.4,
    color = "white"
  ) +
  labs(
    subtitle = "A. Number of OCD funded per region and the number of
    (H)ospitals in each region providing OCD",
    y = "",
    x = "",
    caption = "Sofia-city is highlighted as the capital of Bulgaria"
  )

## Patient treated with ODCT per region over time (table)

d |>
  filter(market_name %in% odct$market_name) |>
  group_by(date, region_en) |>
  summarise(n_patients = sum(n_patients)) |>
  group_by(year(date), region_en) |>
  summarise(n_patients = round(median(n_patients, na.rm = TRUE), 0)) |>
  pivot_wider(names_from = `year(date)`,
              values_from = n_patients) |>
  gt()

## Hospitals providing ODCT per region

d |>
  filter(market_name %in% odct$market_name) |>
  count(region_en, hospital) |>
  select(-n) |>
  count(region_en) |>
  ggplot(aes(y = reorder(region_en, n), x = n)) +
  geom_col(aes(fill = ifelse(region_en == "Sofia-city", "no", "color"))) +
  geom_rangeframe(data = tibble(
    region_en = c("Sofia-city", "Blagoevgrad"),
    n = c(0, 16)
  )) +
  scale_fill_manual(values = c("grey50", "#1D785A"), guide = "none") +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  geom_text(
    aes(label =
          ifelse(
            region_en == "Sofia-city",
            paste0(n),
            paste0("")
          )),
    hjust = -0.2,
    fontface = "bold",
    size = 6,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "#1D785A"
  ) +
  geom_text(
    aes(label =
          ifelse(
            region_en != "Sofia-city",
            paste0(n),
            paste0("")
          )),
    hjust = -0.2,
    fontface = "bold",
    size = 6,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "grey50"
  ) +
  expand_limits(x = 17) +
  geom_vline(
    xintercept = seq(2, 16, 2),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.4,
    color = "white"
  ) +
  labs(
    subtitle = "B. Number of hospitals providing OCD per region",
    y = "",
    x = "",
    caption = ""
  )

## For each ODCT, the number of hospitals where its provided


odct_region =
  d |>
  filter(market_name %in% odct$market_name) |>
  count(market_name, region) |>
  select(-n) |>
  count(market_name) |>
  mutate(regions = n) |>
  select(-n)

regions1 <-
  d |>
  filter(market_name %in% odct$market_name) |>
  count(market_name, hospital) |>
  select(-n) |>
  count(market_name) |>
  left_join(odct_region) |>
  mutate(market_name = str_to_title(market_name),
         prop = n / 46) |>
  ggplot(aes(y = reorder(market_name, n), x = n)) +
  geom_col(fill = "#1D785A") +
  geom_rangeframe(data = tibble(
    market_name = c("Nexavar", "Vyxeos Liposomal"),
    n = c(0, 36)
  )) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  geom_text(
    aes(label = paste0(n, " (", round(prop * 100, 1), "%", ")")),
    hjust = -0.1,
    fontface = "bold",
    size = 6,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "#1D785A"
  ) +
  geom_text(
    aes(label = paste0(regions, "(R)")),
    hjust = "right",
    fontface = "bold",
    size = 6,
    vjust = 0.5,
    family = "Sofia Sans Condensed",
    color = "white"
  ) +
  expand_limits(x = 42) +
  geom_vline(
    xintercept = seq(3, 36, 3),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.4,
    color = "white"
  ) +
  labs(
    subtitle = "B. Number of hospitals and (R)egions providing OCD per product",
    y = "",
    x = "",
    caption = ""
  )

# Number of hospitals providing ODCT over time


## The median sum per year per region

d |>
  filter(market_name %in% odct$market_name) |>
  group_by(date, region_en) |>
  summarise(n_patients = sum(n_patients),
            sum = sum(costs)) |>
  group_by(year(date), region_en) |>
  summarise(midpat = round(median(n_patients, na.rm = TRUE), 0),
            midsum = round(median(sum, na.rm = TRUE), 0)) |>
  pivot_wider(names_from = `year(date)`,
              values_from = c(midpat, midsum)) |>
  mutate(
    "2020" = paste0(midpat_2020, " (", midsum_2020, "€", ")"),
    "2021" = paste0(midpat_2021, " (", midsum_2021, "€", ")"),
    "2022" = paste0(midpat_2022, " (", midsum_2022, "€", ")"),
    "2023" = paste0(midpat_2023, " (", midsum_2023, "€", ")")
  ) |>
  select(
    -c(
      midpat_2020,
      midpat_2021,
      midpat_2022,
      midpat_2023,
      midsum_2020,
      midsum_2021,
      midsum_2022,
      midsum_2023
    )
  ) |>
  gt()

## Maping the total sum per region

# Read the shapefile for Bulgaria from the data folder
bulgaria <-
  st_read(here::here("data", "shapefiles", "bgr_admbnda_adm1_unicef_20221012.shp"))

# Aggregate the total sum per region
map_data_odct <- d %>%
  filter(market_name %in% odct$market_name) %>%
  group_by(region_en) %>%
  summarise(sum = sum(costs)) %>%
  mutate(sum = round(sum, 0)) %>%
  mutate(region_en = str_to_title(region_en)) |>
  mutate(prop = sum / sum(sum, na.rm = TRUE))

# Join the aggregated data with the shapefile data
bulgaria_odct <- bulgaria |>
  left_join(map_data_odct, by = c("ADM1_EN" = "region_en"))

# Create the ggplot
regions2 <- ggplot() +
  geom_sf(data = bulgaria_odct,
          aes(fill = prop)) +
  scale_fill_gradient2(
    low = "white",
    high = "#0B3024",
    mid = "#1D785A",
    midpoint = 0.20,
    na.value = "white",
    guide = "none"
  ) +
  geom_sf_label(
    data = bulgaria_odct,
    aes(label = scales::percent(prop, accuracy = 0.01)),
    size = 5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "white",
    fill = "black",
    alpha = 0.5
  ) +
  labs(
    subtitle = "D. % of all OCD costs per region",
    y = "",
    x = "",
    caption = ""
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank()
  )
## Maping the total sum per region
map_data_nonodct <-
  d %>%
  filter(!market_name %in% odct$market_name) %>%
  group_by(region_en) %>%
  summarise(sum = sum(costs)) %>%
  mutate(sum = round(sum, 0)) %>%
  mutate(region_en = str_to_title(region_en)) |>
  mutate(prop = sum / sum(sum, na.rm = TRUE))



bulgaria_nonodct <-
  bulgaria |>
  left_join(map_data_nonodct, by = c("ADM1_EN" = "region_en"))

# Create the ggplot
regions3 <-
  ggplot() +
  geom_sf(data = bulgaria_nonodct,
          aes(fill = prop)) +
  scale_fill_gradient2(
    low = "white",
    high = "#2E050D",
    mid = "#750E21",
    midpoint = 0.20,
    na.value = "white",
    guide = "none"
  ) +
  geom_sf_label(
    data = bulgaria_nonodct,
    aes(label = scales::percent(prop, accuracy = 0.01)),
    size = 5,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "white",
    fill = "black",
    alpha = 0.5
  ) +
  labs(
    subtitle = "C. % of all non-OCD costs per region",
    y = "",
    x = "",
    caption = ""
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank()
  )

(regions0 + regions1) / (regions3 + regions2)
ggsave(
  "regions.png",
  path = here::here(),
  bg = "white",
  width = 42,
  height = 30,
  units = "cm",
  dpi = 900
)

# Analysis of tge markert authorisation companies

company <-
  d |>
  mutate(marketing_authorisation_holder_company_name =
           tolower(trimws(
             marketing_authorisation_holder_company_name
           ))) |>
  mutate(marketing_authorisation_holder_company_name =
           sapply(strsplit(marketing_authorisation_holder_company_name, "\\s+"),
                  function(x) {
                    x[1]
                  })) |>
  group_by(orphan_medicine,
           marketing_authorisation_holder_company_name) |>
  summarise(sum = sum(costs)) |>
  pivot_wider(names_from = orphan_medicine,
              values_from = sum) |>
  filter(!is.na(yes)) |>
  mutate(
    marketing_authorisation_holder_company_name =
      str_to_title(marketing_authorisation_holder_company_name)
  ) |>
  mutate(
    marketing_authorisation_holder_company_name =
      case_when(
        marketing_authorisation_holder_company_name == "Wyeth" ~ "Wyeth Europa",
        marketing_authorisation_holder_company_name == "Jazz" ~ "Jazz Pharmaceuticals",
        marketing_authorisation_holder_company_name == "Les" ~ "Servier",
        marketing_authorisation_holder_company_name == "Bristol-Myers" ~ "Bristol-Myers Squibb",
        marketing_authorisation_holder_company_name == "Janssen-Cilag" ~ "Janssen Pharmaceutica",
        TRUE ~ marketing_authorisation_holder_company_name
      )
  ) |>
  rename(company = marketing_authorisation_holder_company_name) |>
  mutate(no = replace_na(no, 0))



predominant <-
  company |>
  mutate(prop_odct = round((yes / 74353493) * 100, 2),
         prop_non_odct = round((no / 1239322655) * 100),
         2) |>
  pivot_longer(
    cols = c(prop_odct, prop_non_odct),
    names_to = "type",
    values_to = "prop"
  ) |>
  select(company, type, prop) |>
  pivot_wider(names_from = type,
              values_from = prop) |>
  mutate(diff = prop_odct - prop_non_odct,
         stand = round(standardize(diff), 2)) |>
  select(company, diff, stand) |>
  gt()


company |>
  mutate(prop_odct = yes / 74353493,
         prop_non_odct = no / 1239322655) |>
  pivot_longer(
    cols = c(prop_odct, prop_non_odct),
    names_to = "type",
    values_to = "prop"
  ) |>
  ggplot(aes(
    y = reorder(company, prop),
    x = prop,
    fill = type
  )) +
  geom_col(
    data =
      company |>
      mutate(
        prop_odct = yes / 74353493,
        prop_non_odct = no / 1239322655
      ) |>
      pivot_longer(
        cols = c(prop_odct, prop_non_odct),
        names_to = "type",
        values_to = "prop"
      ) |>
      group_by(company) |>
      filter(prop == max(prop)) |>
      ungroup(),
    aes(x = prop + 0.05, y = company),
    fill = "white",
    color = "gray50",
    lty = 2
  ) +
  geom_col(position = "dodge2",
           color = "white",
           width = 0.85) +
  geom_rangeframe(data = tibble(
    company = c("Janssen Pharmaceutica",
                "Jazz Pharmaceuticals"),
    prop = c(0, 0.3),
    type = c("prop_odct", "prop_odct")
  )) +
  scale_fill_manual(values = c("#750E21", "#1D785A")) +
  geom_text(
    aes(
      label = scales::percent(prop, accuracy = 0.01),
      group = type
    ),
    position = position_dodge(width = 0.9),
    size = 5,
    hjust = -0.1,
    fontface = "bold",
    family = "Sofia Sans Condensed",
    color = "black",
    # fill = "white",
    alpha = 0.5
  ) +
  expand_limits(x = 0.35) +
  geom_vline(
    xintercept = seq(0, 0.3, 0.05),
    color = "white",
    linetype = "dashed",
    alpha = 0.6
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.3, 0.05)) +
  theme(legend.position = "none",
        plot.subtitle = element_markdown()) +
  labs(
    y = "",
    x = "",
    subtitle = paste0(
      "% Of all costs for <span style = 'color:#1D785A'>**OCDs**</span> and <span style = 'color:#750E21'>**non-OCDs**</span> per company"
    ),
    caption = ""
  )

ggsave(
  "company.png",
  path = here::here(),
  bg = "white",
  width = 20,
  height = 30,
  units = "cm",
  dpi = 900
)

d |>
  filter(orphan_medicine == "yes") |>
  group_by(region_en) |>
  summarise(sum = sum(costs)) |>
  arrange(desc(sum)) |>
  mutate(prop = sum / sum(sum))

d |>
  filter(orphan_medicine == "yes") |>
  group_by(region_en, date) |>
  summarise(sum = sum(costs)) |>
  arrange(desc(sum)) |>
  ungroup() |>
  group_by(region_en) |>
  summarise(median = median(sum),
            min = min(sum),
            max = max(sum)) |>
  arrange(desc(median))


d |>
  filter(orphan_medicine == "yes") |>
  group_by(region_en, date) |>
  summarise(sum = sum(n_patients)) |>
  arrange(desc(sum)) |>
  ungroup() |>
  group_by(region_en) |>
  summarise(median = median(sum),
            min = min(sum),
            max = max(sum)) |>
  arrange(desc(median))

d |>
  count(hospital) |>
  select(-n)

d |>
  filter(orphan_medicine == "yes") |>
  group_by(hospital, date) |>
  summarise(n = n()) |>
  mutate(median(n),
         min(n),
         max(n)) |>
  ungroup() |>
  summarise(median(`median(n)`),
            min(`min(n)`),
            max(`max(n)`))



observed_statistic <-
  d |>
  count(market_name, hospital, orphan_medicine) |>
  select(-n) |>
  count(market_name, orphan_medicine) |>
  specify(n ~ orphan_medicine) |>
  calculate(stat = "diff in medians", order = c("no", "yes"))

observed_statistic

boostrapped_confint <- observed_statistic |>
  get_confidence_interval()

# Step 2: Invent a world where δ is null
null <-
  d |>
  count(market_name, hospital, orphan_medicine) |>
  select(-n) |>
  count(market_name, orphan_medicine) |>
  specify(n ~ orphan_medicine) |>
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in medians", order = c("no", "yes"))

# Step 3
null |>
  get_p_value(obs_stat = observed_statistic,
              direction = "both") %>%
  mutate(p_value_clean = pvalue(p_value))



company_2 <-
  d |>
  mutate(marketing_authorisation_holder_company_name =
           tolower(trimws(
             marketing_authorisation_holder_company_name
           ))) |>
  mutate(marketing_authorisation_holder_company_name =
           sapply(strsplit(marketing_authorisation_holder_company_name, "\\s+"),
                  function(x) {
                    x[1]
                  })) |>
  group_by(date,
           marketing_authorisation_holder_company_name,
           orphan_medicine) |>
  summarise(sum = sum(costs))



company_2 |>
  pivot_wider(names_from = orphan_medicine,
              values_from = sum) |>
  filter(!is.na(yes)) |>
  mutate(
    marketing_authorisation_holder_company_name =
      str_to_title(marketing_authorisation_holder_company_name)
  ) |>
  mutate(
    marketing_authorisation_holder_company_name =
      case_when(
        marketing_authorisation_holder_company_name == "Wyeth" ~ "Wyeth Europa",
        marketing_authorisation_holder_company_name == "Jazz" ~ "Jazz Pharmaceuticals",
        marketing_authorisation_holder_company_name == "Les" ~ "Servier",
        marketing_authorisation_holder_company_name == "Bristol-Myers" ~ "Bristol-Myers Squibb",
        marketing_authorisation_holder_company_name == "Janssen-Cilag" ~ "Janssen Pharmaceutica",
        TRUE ~ marketing_authorisation_holder_company_name
      )
  ) |>
  rename(company = marketing_authorisation_holder_company_name) |>
  mutate(no = replace_na(no, 0)) |>
  ggplot(aes(
    x = date,
    y = yes,
    fill = company,
    label = company,
    color = company
  )) +
  geom_line() +
  facet_wrap( ~ company, ncol = 3, scales = "free_y")