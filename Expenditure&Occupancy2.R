# -------------------------------------------------------------------------
# Analyzing Housing Voucher Costs vs. Utilization (2014-2024)
# Author: Pratish Patel
# -------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(scales)
library(ggtext)
library(here)

# 1. DATA LOADING ---------------------------------------------------------

years <- 2014:2024
raw_list <- list()

for (yr in years) {
  message(paste("Fetching data for year:", yr))
  
  # HUD changed their naming convention for the 2020 census data
  url <- if (yr >= 2022) {
    paste0("https://www.huduser.gov/portal/datasets/pictures/files/US_", yr, "_2020census.xlsx")
  } else {
    paste0("https://www.huduser.gov/portal/datasets/pictures/files/US_", yr, ".xlsx")
  }
  
  # Read and clean names immediately
  try({
    df <- read.xlsx(url)
    raw_list[[as.character(yr)]] <- df %>% 
      mutate(year = yr) %>%
      # Ensure key columns are numeric (Excel often imports them as chars)
      mutate(across(c(total_units, pct_occupied, number_reported, spending_per_month,
                      pct_wage_major, pct_welfare_major, pct_2adults, 
                      pct_female_head, pct_female_head_child, pct_disabled_all,
                      pct_minority, rent_per_month, hh_income, spending_per_month,
                      months_from_movein), as.numeric)) 
  }, silent = TRUE)
}

# Combine and aggregate
vouch_data_2014_2024 <- bind_rows(raw_list) %>%
                filter(program == 3,
                       total_units > 0) %>%
                select(total_units, pct_occupied, number_reported, spending_per_month,
                 pct_wage_major, pct_welfare_major, pct_2adults, 
                 pct_female_head, pct_female_head_child, pct_disabled_all,
                 pct_minority, rent_per_month, year, name, hh_income, spending_per_month,
                 months_from_movein) %>%
                mutate(total_vouchers = number_reported,
                      total_spend    = sum(spending_per_month * 12 * number_reported, na.rm = TRUE)
                ) 



yr_2025 <- 2025
url_2025 <- paste0("https://www.huduser.gov/portal/datasets/pictures/files/US_",yr_2025, "_2020census.xlsx")
# Read and clean names immediately

  df_2025 <- read.xlsx(url_2025)%>% 
    mutate(year = yr_2025) %>%
    filter(program == 3,
           sub_program == "N/A")%>%
    mutate(across(c(total_units, pct_occupied, number_reported, spending_per_month,
                    pct_wage_major, pct_welfare_major, pct_2adults, 
                    pct_female_head, pct_female_head_child, pct_disabled_all,
                    pct_minority, rent_per_month, hh_income, spending_per_month,
                    months_from_movein), as.numeric)) %>%
    select(total_units, pct_occupied, number_reported, spending_per_month,
         pct_wage_major, pct_welfare_major, pct_2adults, 
         pct_female_head, pct_female_head_child, pct_disabled_all,
         pct_minority, rent_per_month, year, name, hh_income, spending_per_month,
         months_from_movein) %>%
    mutate(total_vouchers = number_reported,
           total_spend    = sum(spending_per_month * 12 * number_reported, na.rm = TRUE)
    ) 

  yr_2005 <- 2005
  url_2005 <- "https://www.huduser.gov/picture2005/2005_us.xlsx"
    # Read and clean names immediately
  
  df_2005 <- read.xlsx(url_2005)  %>% 
              mutate(year = yr_2005) %>%
              filter(program == 3) %>%
              mutate(
                pct_wage_major = pct_wage_maj,
                pct_welfare_major = pct_welf_maj
              ) %>%
    mutate(across(c(total_units, pct_occupied, number_reported, spending_per_month,
                    pct_wage_major, pct_welfare_major, pct_2adults, 
                    pct_female_head, pct_female_head_child, pct_disabled_all,
                    pct_minority, rent_per_month, hh_income, spending_per_month,
                    months_from_movein), as.numeric)) %>%
    select(total_units, pct_occupied, number_reported, spending_per_month,
           pct_wage_major, pct_welfare_major, pct_2adults, 
           pct_female_head, pct_female_head_child, pct_disabled_all,
           pct_minority, rent_per_month, year, name, hh_income, spending_per_month,
           months_from_movein) %>%
    mutate(total_vouchers = number_reported,
           total_spend    = sum(spending_per_month * 12 * number_reported, na.rm = TRUE)
    ) 
  
  AllData <- bind_rows(df_2005, vouch_data_2014_2024, df_2025) %>%
            mutate(spending_per_year = spending_per_month * 12)

  # Helper Function. 
  
plot_vs_2005 <- function(data,
                           var,
                           y_label,                  # REQUIRED
                           year_var = year,
                           baseline_year = 2005,
                           bar_year_min = 2014,
                           bar_year_max = 2025,
                           caption = NULL,
                           bar_color = "#E07B39",
                           line_color = "#2E6E8E",
                           y_formatter = scales::label_number()) {
    
    if (missing(y_label)) {
      stop("You must supply y_label.")
    }
    
    var      <- rlang::enquo(var)
    year_var <- rlang::enquo(year_var)
    
    # Baseline value (2005)
    baseline_value <- data |>
      dplyr::filter(!!year_var == baseline_year) |>
      dplyr::pull(!!var)
    
    if (length(baseline_value) == 0 || is.na(baseline_value[1])) {
      stop("Baseline value missing. Check baseline_year and variable.")
    }
    
    baseline_value <- baseline_value[1]
    
    # Data for bars
    plot_df <- data |>
      dplyr::filter(dplyr::between(!!year_var, bar_year_min, bar_year_max)) |>
      dplyr::arrange(!!year_var)
    
    # Title with colored span formatting
    full_title <- paste0(
      y_label, ": ",
      "<span style='color:", bar_color, "'>",
      bar_year_min, "–", bar_year_max,
      "</span> vs ",
      "<span style='color:", line_color, "'>",
      baseline_year, " baseline</span>"
    )
    
    ggplot2::ggplot(plot_df, ggplot2::aes(x = factor(!!year_var))) +
      ggplot2::geom_col(ggplot2::aes(y = !!var),
                        fill = bar_color,
                        width = 0.75,
                        alpha = 0.85) +
      ggplot2::geom_hline(yintercept = baseline_value,
                          color = line_color,
                          linewidth = 1.2) +
      ggplot2::scale_y_continuous(labels = y_formatter,
                                  expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::labs(
        title = full_title,
        x = "Year",
        caption = caption
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(face = "bold", size = 16),
        axis.title.y = ggplot2::element_blank(),   # remove y-axis label
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()
      )
  }

# Plot of Pct Occupied. 
  p_occ <- plot_vs_2005(
    AllData,
    var = pct_occupied,
    y_label = "Percent occupied",
    y_formatter = scales::label_percent(scale = 1)
  )
p_occ  
ggsave("plots/Chapter1_Book/Occupancy.png", 
       p_occ , 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Plot of Pct Wage Major. 
p_wage <- plot_vs_2005(
  AllData,
  var = pct_wage_major,
  y_label = "Wages as major source of income",
  y_formatter = scales::label_percent(scale = 1)
)
p_wage
ggsave("plots/Chapter1_Book/WageMajor.png", 
       p_wage , 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Plot of Percent 2 adults. 
p_2adults <- plot_vs_2005(
  AllData,
  var = pct_2adults,
  y_label = "Percent Two Adult Families",
  y_formatter = scales::label_percent(scale = 1)
)
p_2adults
ggsave("plots/Chapter1_Book/Percent2Adults.png", 
       p_2adults , 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")


# Plot of Percent female head. 
p_femalehead <- plot_vs_2005(
  AllData,
  var = pct_female_head,
  y_label = "Percent Female Head",
  y_formatter = scales::label_percent(scale = 1)
)
p_femalehead
ggsave("plots/Chapter1_Book/PercentFemaleHeads.png", 
       p_femalehead , 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")


# Plot of Percent disabled. 
p_disabled_all <- plot_vs_2005(
  AllData,
  var = pct_disabled_all,
  y_label = "Percent Disabled",
  y_formatter = scales::label_percent(scale = 1)
)
p_disabled_all
ggsave("plots/Chapter1_Book/PercentDisabled.png", 
       p_disabled_all , 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")


# Plot of Minority 
p_minority <- plot_vs_2005(
  AllData,
  var = pct_minority,
  y_label = "Percent Minority",
  y_formatter = scales::label_percent(scale = 1)
)
p_minority
ggsave("plots/Chapter1_Book/PercentMinority.png", 
       p_minority , 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Rent Per Month 
p_rent <- plot_vs_2005(
  AllData,
  var = rent_per_month,
  y_label = "Rent Per Month",
  y_formatter = scales::label_dollar(scale = 1)
)
p_rent
ggsave("plots/Chapter1_Book/RentPerMonth.png", 
       p_rent, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Annualized Spending Per Year 
p_spend <- plot_vs_2005(
  AllData,
  var = spending_per_year,
  y_label = "Annualized Spending per Voucher Holder",
  y_formatter = scales::label_dollar(scale = 1)
)
p_spend

ggsave("plots/Chapter1_Book/SpendingPerYear.png", 
       p_spend, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Number Reported 
p_total <- plot_vs_2005(
  AllData,
  var = total_vouchers,
  y_label = "Number of Voucher Holders",
  y_formatter = scales::label_comma(scale = 1)
)
p_total

ggsave("plots/Chapter1_Book/total.png", 
       p_total, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# Months from move in 
p_movein <- plot_vs_2005(
  AllData,
  var = months_from_movein,
  y_label = "Months from Move in",
  y_formatter = scales::label_comma(scale = 1)
)
p_movein

ggsave("plots/Chapter1_Book/tenure.png", 
       p_movein, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

# HHincome 
p_hhincome <- plot_vs_2005(
  AllData,
  var = hh_income,
  y_label = "Average household income",
  y_formatter = scales::label_dollar(scale = 1)
)
p_hhincome

ggsave("plots/Chapter1_Book/hhincome.png", 
       p_hhincome, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")
