# ===============================================
# PHV_hexagon_map.R
# Public Housing Voucher Occupancy by State — Hexagon Map
# (Modified to analyze the difference in pct_occupancy from 2014)
# ===============================================

# Clear workspace
rm(list = ls())

# --- Install & load required packages ---
required_packages <- c("tidyverse", "showtext", "geojsonio", 
                       "patchwork", "openxlsx", "readr", "here", 
                       "sf", "cowplot", "stringr")

for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --- Set up fonts ---

# --- Define color palette ---
color_scheme <- c("#E07B39", "#2E6E8E")

# --- Load and clean Public Housing Voucher data for 2024 ---
PHV_data_2025 <- openxlsx::read.xlsx("https://www.huduser.gov/portal/datasets/pictures/files/PLACE_2025_2020census.xlsx")

PHV_data_filter_2025 <- PHV_data_2025 %>%
  filter(program == 3,
         sub_program == "TBV, All",
         total_units > 2000,
         nchar(name) > 2
  )%>%
  mutate(years_from_movein = months_from_movein / 12) %>%
  mutate(city_clean = str_remove(entities, "^[0-9]+ ") %>%
           str_remove(", [A-Za-z ]+$")) %>%
  mutate(city_clean = str_trim(str_extract(city_clean, ".*(?=\\s+\\S+$)"))) 

top10 <- PHV_data_filter_2025 %>%
  arrange(desc(total_units)) %>%
  slice_head(n = 20) %>%
  mutate(
    years_from_movein = months_from_movein / 12,
    bar_color = ifelse(years_from_movein >= 10, 
                       "#E07B39", 
                       "#2E6E8E")
  )

bar_plot <- ggplot(top10, 
                   aes(x = reorder(city_clean, years_from_movein), 
                       y = years_from_movein,
                       fill = bar_color)) +
  geom_col(width = 0.7) +
  scale_fill_identity() +
  geom_hline(yintercept = 5, 
             linetype = "dashed", 
             color = "gray40", 
             linewidth = 1.0) +
  coord_flip() +
  labs(
    title = "Average Voucher Tenure in the Twenty Largest Voucher Programs",
    subtitle = "Orange bars indicate cities where average tenure exceeds ten years.\nThe dashed vertical line shows the 5-year original intent.",
    x = NULL,
    y = "Average Years Since Move-In",
    caption = "Source: HUD Picture of Subsidized Households, 2025.\nCities ranked by total voucher units."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    plot.caption = element_text(color = "gray50", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray80", 
                                      linetype = "dashed", 
                                      linewidth = 0.4),
    legend.position = "none"
  )
bar_plot

ggsave("plots/Prologue/Tenure_Bar.png", 
       bar_plot, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")

PHV_data_filter_2025 <- PHV_data_filter_2025 %>%
  mutate(years_from_movein = months_from_movein / 12)

histogram_plot <- ggplot(PHV_data_filter_2025, 
                         aes(x = years_from_movein)) +
  geom_histogram(binwidth = 2, 
                 fill = "#E07B39", 
                 color = "white",
                 linewidth = 0.3) +
  geom_vline(xintercept = 5, 
             linetype = "dashed", 
             color = "#2E6E8E", 
             linewidth = 0.8) +
  labs(
    title = "Distribution of Average Voucher Tenure Across Cities",
    subtitle = "The dashed vertical line shows the 5-year original intent.",
    x = "Average Years Since Move-In",
    y = "Number of Cities",
    caption = "Source: HUD Picture of Subsidized Households, 2025.\nEach bar represents a two-year interval."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.caption = element_text(color = "gray50", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", 
                                      linetype = "dashed", 
                                      linewidth = 0.4)
  )
histogram_plot
ggsave("plots/Prologue/Tenure_Histogram.png", 
       histogram_plot, 
       width = 9, 
       height = 6, 
       dpi = 300,
       bg = "white")
