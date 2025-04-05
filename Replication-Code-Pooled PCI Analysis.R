# Load required libraries
library(dplyr)
library(fixest)
library(haven)
library(readxl)
library(modelsummary)

# Load data
pooled_pci <- read.xlsx("cleaned_pooled_pci.xlsx")
pci18_original <- read_dta("PCI2018_DDI_cleanK_Final.dta")
pci20_original <- read_dta("PCI2020_DDI_cleanK_Final.dta") 
pci22_original <- read_dta("PCI2022_DDI_cleaned.dta")

# Replacing NA values with 0
pooled_pci <- pooled_pci %>%
  mutate(
    across(c(gov_holds_shares, owner_former_SOE_manager, owner_former_official,
             former_family_business, former_soe), ~replace_na(., 0)),
    mobility = as.numeric(mobility),
    customs_inspections = as.numeric(customs_inspections),
    bribe_business_registration = c1_3
  )

# Merge port data
pooled_pci <- pooled_pci %>%
  left_join(select(pci18_original, id, port_used = k4), by = "id") %>%
  left_join(select(pci20_original, id, port_used_pci20 = k4), by = "id") %>%
  mutate(port_used = coalesce(port_used, port_used_pci20)) %>%
  select(-port_used_pci20)

# Recode variables 
pooled_pci <- pooled_pci %>%
  mutate(
    similar_firms_bribe = case_when(
      tolower(similar_firms_bribe) == "strongly disagree" ~ 1,
      tolower(similar_firms_bribe) == "disagree" ~ 2,
      tolower(similar_firms_bribe) == "agree" ~ 3,
      tolower(similar_firms_bribe) == "strongly agree" ~ 4,
      TRUE ~ NA_real_
    ),
    customs_bribe = ifelse(customs_bribe %in% 2:3, 0, customs_bribe)
  )



###############
###First-stage results 
# Create the leave-one-out (LOO) instrument for customs bribe
pooled_pci_customs_coded_filtered <- pooled_pci_customs_coded_filtered %>%
  group_by(main_sector, year) %>%
  mutate(LO_customs_bribe = sapply(1:n(), function(i) {
    # Exclude firms from the same province and the current firm itself
    other_firms <- pooled_pci_customs_coded_filtered %>%
      filter(main_sector == main_sector[i], year == year[i], province != province[i])
    
    # Calculate the average customs bribe from other firms
    if (nrow(other_firms) > 0) {
      mean(other_firms$customs_bribe, na.rm = TRUE)
    } else {
      NA # If no other firms exist, return NA
    }
  }))

table(pooled_pci_customs_coded_filtered$LO_customs_bribe)

# Fit the model with separate dummies for each airport
import_model_ivlo <- feols(
  log(days_clear_imports) ~ 
    customs_inspections + mobility + no_employees + 
    gov_holds_shares + 
    owner_former_official + owner_former_SOE_manager + 
    former_family_business + former_soe + 
    years_since_establishment + latest_performance+
    bribe_known|  # Exogenous regressors
    province +main_sector+ port1+year|  # Fixed Effects
    customs_bribe + customs_bribe:mobility ~  
    LO_customs_bribe + LO_customs_bribe:mobility,  # Instrument for customs_bribe
  data = pooled_pci_customs_coded_filtered
)

# Check the summary of the model
summary(import_model_ivlo, #stage=1)

#####IV1        
# IV model 1: Equation 1 for import
import_model_iv_equation1 <- feols(
  log(days_clear_imports) ~ customs_inspections + no_employees + gov_holds_shares + 
    owner_former_official + owner_former_SOE_manager + former_family_business + 
    former_soe + years_since_establishment + latest_performance + bribe_known |
    province + main_sector + port1 + year |#FEs
    customs_bribe ~ bribe_business_registration,~ #IVs
  data = pooled_pci_customs_coded
)

# IV model 2: Equation 2 for import
import_model_iv <- feols(
  log(days_clear_imports) ~ customs_inspections + mobility + no_employees + 
    gov_holds_shares + owner_former_official + owner_former_SOE_manager + 
    former_family_business + former_soe + years_since_establishment + 
    latest_performance + bribe_known |
    province + main_sector + port1 + year | #FEs 
    customs_bribe + customs_bribe:mobility ~ #IVs
    bribe_business_registration + bribe_business_registration:mobility,
  data = pooled_pci_customs_coded
)

# IV model 3: Equation 1 for export
export_model_iv_equation1 <- feols(
  log(days_clear_export) ~ customs_inspections + no_employees + gov_holds_shares + 
    owner_former_official + owner_former_SOE_manager + former_family_business + 
    former_soe + years_since_establishment + latest_performance + bribe_known |
    province + main_sector + port1 + year |#FEs
    customs_bribe ~ bribe_business_registration,~ #IVs
    data = pooled_pci_customs_coded
)

# IV model 4: Equation 2 for export
export_model_iv <- feols(
  log(days_clear_exports) ~ customs_inspections + mobility + no_employees + 
    gov_holds_shares + owner_former_official + owner_former_SOE_manager + 
    former_family_business + former_soe + years_since_establishment + 
    latest_performance + bribe_known |
    province + main_sector + port1 + year | #FEs 
    customs_bribe + customs_bribe:mobility ~ #IVs
    bribe_business_registration + bribe_business_registration:mobility,
  data = pooled_pci_customs_coded
)

# Create and save results table to Latex
models <- list(
  "(1)" = import_model_iv_equation1,
  "(2)" = import_model_iv,
  "(3)" = export_model_iv_equation1,
  "(4)" = export_model_iv
)


cm = c('fit_customs_bribe'    = 'Fitted customs bribe',
       'fit_customs_bribe:mobility'    = 'Fitted customs bribe x Mobility',
       'customs_inspections'    = 'Firm inspected by customs officials in the last year (Dummy)',
       'mobility'    = 'Mobility (Dummy)',
       'no_employees' = 'Number of employees',
       'gov_holds_shares' = 'Government holds shares (Dummy)',
       'owner_former_official' = 'Firm owner is former government official (Dummy)',
       'owner_former_SOE_manager1' = 'Firm owner is former SOE manager (Dummy)',
       'former_family_business1' = 'Firm is former family-owned business (Dummy)',
       'former_soe' = 'Firm is former state-owned business (Dummy)',
       'years_since_establishment' = 'Years since establishment',
       'latest_performancelarge losses' = 'Last year performance: large losses',
       'latest_performancesmall losses' = 'Last year performance: small losses',
       'latest_performanceprofits as planned' = 'Last year performance: profits as planned',
       'latest_performancesmall profits' = 'Last year performance: small profits',
       'bribe_known' = 'Firm knows bribe size in advance')

msummary(
  models,
  vcov = "HC1",
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_map = cm,
  output = "latex"
)