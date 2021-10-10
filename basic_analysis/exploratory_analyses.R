library(dplyr)
library(ggplot2)
library(readr)
library(scales)

## need to modify to reduce the number of dependencies
responses_2021_22_cleaned <- read_delim("survey_results/GRC_Survey_Cleaned_2021-22.tsv", 
                                        delim = "\t")
## filter out RSI
responses_2021_22_cleaned <- responses_2021_22_cleaned %>% 
  filter(dept.short != "RSI")
dir.create("figures")


### monetary support needed delineated by degree
responses_2021_22_cleaned %>%
  group_by(`What degree program are you in?`,
           `How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?`) %>%
  tally() %>% group_by(`What degree program are you in?`) %>% 
  transmute(proportion = n/sum(n), add_mon_support = 
              factor(`How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?`,
                     levels = c("0.0", "$1 - $1,000", "$1,000 - $5,000",
                                "$5,000 - $10,000", "$10,000 - $15,000",
                                "$15,000 - $20,000", "$20,000+")), n,
            deg_prog = `What degree program are you in?`) %>%
  ggplot() + 
  geom_col(aes(y = proportion, fill = add_mon_support, x = add_mon_support), position = "dodge") +
  geom_text(aes(y = proportion + 0.01, label = n, x = add_mon_support)) +
  facet_grid(cols = vars(deg_prog)) +
  labs(title = "How much additional monetary support would you need to meet your day-to-day\nexpenses each year (above your graduate living allowance)?") +
  theme(axis.text.x = element_text(angle = 270, hjust =0, vjust =0.5))
ggsave("figures/addtnl_mon_support_by_deg.png")

### interactions between food/housing/medical insecurity
responses_2021_22_cleaned %>% 
  filter(!is.na(`Do you experience any level of housing insecurity?`), 
         !is.na(`Do you experience any level of food insecurity?`), 
         !is.na(`Do you experience any level of health care insecurity?`)) %>%
  transmute(housing = case_when(
    `Do you experience any level of housing insecurity?` == "No" ~ 0,
    grepl("Marginal", `Do you experience any level of housing insecurity?`) ~ 1, 
    grepl("Moderate", `Do you experience any level of housing insecurity?`) ~ 2, 
    grepl("Severe", `Do you experience any level of housing insecurity?`) ~ 3 
  ), food = case_when(
    `Do you experience any level of food insecurity?` == "No" ~ 0,
    grepl("Marginal", `Do you experience any level of food insecurity?`) ~ 1, 
    grepl("Moderate", `Do you experience any level of food insecurity?`) ~ 2, 
    grepl("Severe", `Do you experience any level of food insecurity?`) ~ 3 
  ), medical = case_when(
    grepl("No", `Do you experience any level of health care insecurity?`) ~ 0,
    grepl("Marginal", `Do you experience any level of health care insecurity?`) ~ 1, 
    grepl("Moderate", `Do you experience any level of health care insecurity?`) ~ 2, 
    grepl("Severe", `Do you experience any level of health care insecurity?`) ~ 3 
  )) %>% filter(medical > 0 | housing > 0 | food > 0) %>%
  ggplot(aes(x = food, y = housing)) +  
  geom_density_2d_filled(alpha = 0.6, contour_var = "count")

