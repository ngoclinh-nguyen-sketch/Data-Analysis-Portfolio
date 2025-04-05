# Load libraries (consolidated)
library(tidyverse)
library(haven)
library(writexl)
library(readxl)
library(janitor)
library(gtsummary)

# Helper functions
extract_labels <- function(df) {
  labels <- sapply(df, function(x) attr(x, "label")) %>% 
    unlist() %>% 
    replace(is.na(.), "")
}

restore_labels <- function(df, label_list) {
  for (var in intersect(names(df), names(label_list))) {
    attr(df[[var]], "label") <- label_list[var]
  }
  df
}

# Load and preprocess datasets
load_and_prep <- function(path, year) {
  read_dta(path) %>% 
    select(all_of(common_vars)) %>% 
    mutate(year = year) %>% 
    restore_labels(labels_pci18) %>% 
    mutate(across(everything(), as.character))
}

# Main processing
pci18_original <- read_dta("C:/Users/This MC/OneDrive - Fulbright University Vietnam/Capstone/PCI2018_DDI_cleanK_Final.dta")
labels_pci18 <- extract_labels(pci18_original)

common_vars <- reduce(list(names(pci18_original), 
                           names(read_dta("C:/Users/This MC/OneDrive - Fulbright University Vietnam/Capstone/PCI2020_DDI_cleanK_Final.dta")),
                           names(read_dta("C:/Users/This MC/OneDrive - Fulbright University Vietnam/Capstone/PCI2022_DDI_cleaned.dta"))), 
                      intersect)

# Load and merge datasets
merged_data <- bind_rows(
  load_and_prep("C:/Users/This MC/OneDrive - Fulbright University Vietnam/Capstone/PCI2018_DDI_cleanK_Final.dta", 2018),
  load_and_prep("C:/Users/This MC/OneDrive - Fulbright University Vietnam/Capstone/PCI2020_DDI_cleanK_Final.dta", 2020),
  load_and_prep("C:/Users/This MC/OneDrive - Fulbright University Vietnam/Capstone/PCI2022_DDI_cleaned.dta", 2022)
) %>% 
  restore_labels(labels_pci18)

# Export variable info
var_info <- data.frame(
  Variable = names(merged_data),
  Description = map_chr(names(merged_data), ~ attr(merged_data[[.x]], "label") %||% NA)
)
write_xlsx(var_info, "PCI_Variables.xlsx")

# Rename columns and clean data
cleaned_data <- merged_data %>% 
  rename(
    year_established = "a1", latest_performance = "a9", gov_holds_shares = "a13_3",
    former_family_business = "a13_4", owner_former_official = "a14_3",
    owner_former_SOE_manager = "a14_5", land_process_days = "b6_2_1",
    customs_inspections = "d2_4", main_sector = "a5", area = "b1",
    bribe_in_inspection = "d3_2016", lurc = "b4", land_bribe = "b6_2_5",
    fair_compensation_land_expropriated = "b4_4", days_clear_export = "k5",
    days_clear_imports = "k6", customs_bribe = "k7", tax_negotiation_necessary = "d14_3",
    bribe_known = "d11_2017", similar_firms_bribe = "d10", no_employees = "SO_LAODONG",
    year_formal = "a2", bribery_general = "d11", no_inspections = "d1"
  ) %>% 
  mutate(no_employees = as.numeric(no_employees)) %>% 
  filter(
    no_employees > 9 | is.na(no_employees),
    !is.na(mobility),
    !is.na(main_sector),
    main_sector != "(lựa chọn)"
  ) %>% 
  mutate(
    main_sector = case_when(
      str_detect(main_sector, "1") ~ "Industry/Manufacturing",
      str_detect(main_sector, "2") ~ "Construction",
      str_detect(main_sector, "3") ~ "Service/Commerce",
      str_detect(main_sector, "4") ~ "Agri/Forestry/Aquaculture",
      str_detect(main_sector, "5") ~ "Mining",
      TRUE ~ NA_character_
    ),
    year = as.numeric(year),
    year_established = as.numeric(year_established),
    years_since_establishment = year - year_established,
    former_soe = ifelse(a13_1 == 1 | a13_2 == 1, 1, 0)
  )

# Create summary tables
create_summary_table <- function(data, caption) {
  data %>% 
    select(
      bribery_general, bribe_known, years_since_establishment, no_employees,
      mean_employment, log_employment, b4, main_sector, former_soe,
      gov_holds_shares, owner_former_official, d1, mean_inspections
    ) %>% 
    tbl_summary(
      statistic = list(
        all_continuous() ~ "{mean} ({sd}), {median}, {min}, {max}",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(all_continuous() ~ c(2, 2), all_categorical() ~ c(0, 1)),
      type = list(where(is.numeric) ~ "continuous", main_sector ~ "categorical")
    ) %>% 
    modify_header(label = "**Variable**") %>% 
    modify_caption(caption) %>% 
    bold_labels()
}

# Generate and save tables
wb <- createWorkbook()
walk2(
  list(cleaned_data, filter(cleaned_data, mobility == 1)),
  c("All Firms", "Mobile Firms"),
  ~ addWorksheet(wb, .y) %>% 
    writeData(wb, .y, as_gt(create_summary_table(.x, paste("Descriptive statistics of", tolower(.y)))) %>% 
                as.data.frame())
)
saveWorkbook(wb, "summary_tables.xlsx", overwrite = TRUE)

# Save final dataset
write_xlsx(cleaned_data, "cleaned_pooled_pci.xlsx")




#####GENERATE FIGURES
# Summarize mobility/ and public_service_outcomes across provinces
plot_data <- pooled_pci %>% 
  dplyr::select(public_service_outcomes, mobility/, bribe_known_dummy, province) %>%
  filter(!is.na(bribe_known_dummy)) %>%
  mutate(
    public_service_outcomes = as.numeric(factor(public_service_outcomes, 
                                                levels = c("Always", "Usually", "Sometimes", "Seldom", "Never"), 
                                                labels = c(1, 2, 3, 4, 5)))
  ) %>%
  group_by(province, bribe_known_dummy) %>%  
  summarize(
    public_service_outcomes = mean(public_service_outcomes, na.rm = TRUE),
    mobility/ = mean(mobility/, na.rm = TRUE),  
    .groups = "drop"
  )

# Figure 1
p <- ggplot(data = plot_data, aes(y = public_service_outcomes, x = mobility/, 
                                  colour = factor(bribe_known_dummy), 
                                  linetype = factor(bribe_known_dummy), 
                                  fill = factor(bribe_known_dummy))) +
  geom_point(size = 4) + 
  geom_smooth(formula = y ~ x, method = "lm", lwd = 1.5, alpha = 0.5, se = FALSE) +  # Use lm now that x is continuous
  scale_color_manual(name="Bribe Known in Advance", labels=c("No", "Yes"), values=c("#C0C0C0","#696969")) +
  scale_linetype_manual(name="Bribe Known in Advance", labels=c("No", "Yes"), values=c("solid","dashed")) +
  scale_fill_manual(name="Bribe Known in Advance", labels=c("No", "Yes"), values=c("#C0C0C0","#696969")) +
  theme_minimal() +
  theme(
    legend.position = "top", 
    legend.key = element_rect(fill='white'), 
    legend.text = element_text(size=18), 
    legend.title = element_text(size=18),
    axis.text.x = element_text(size = 22), 
    axis.text.y = element_text(size = 20), 
    axis.title.x = element_text(size = 22), 
    axis.title.y = element_text(size = 22, angle = 90), 
    title = element_text(size = 15)
  ) +
  labs(x = "Average Capital Mobility",  y = "Public Service Outcomes")

print(p)


#Figure 2
library(ggplot2)
install.packages("Rmisc") 
library(Rmisc)

pooled_pci$days_clear_imports<-as.numeric(pooled_pci$days_clear_imports)
# Frequency histogram
p1 <- pooled_pci %>% 
  ggplot(aes(x = days_clear_imports)) +
  geom_histogram() +
  labs(y = "Frequency", x = "Days of customs clearance (import)")

# Probability histogram
p2 <- pooled_pci %>% 
  ggplot(aes(x = days_clear_export)) +
  geom_histogram() +
  labs(y = "Frequency", x = "Days of customs clearance (export)")

Rmisc::multiplot(p1, p2, cols = 2)
