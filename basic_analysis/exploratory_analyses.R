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

## parental education levels (asked for by GLSE)
par_ed_df <- responses_2021_22_cleaned %>% 
  transmute(par_ed = `Parents'/Guardians'/Caregivers' highest level of education?`,
         par_ed_simp = case_when(
           grepl("college|post-grad", par_ed) & grepl("in Canada", par_ed) & grepl("outside Canada", par_ed) ~ "Minimum college education both in and outside Canada",
           grepl("college|post-grad", par_ed) & grepl("in Canada", par_ed) ~ "At least one minimum college education in Canada",
           grepl("college|post-grad", par_ed) & !grepl("in Canada", par_ed) ~ "At least one minimum college education outside Canada",
           TRUE ~ par_ed
         ),
         mon_supp = factor(`How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?`,
                           levels = rev(c("0.0", "$1 - $1,000","$1,000 - $5,000",
                                      "$5,000 - $10,000", "$10,000 - $15,000",
                                      "$15,000 - $20,000", "$20,000+")))
        ) 
par_ed_df %>% count(par_ed_simp) %>%
  ggplot(aes(x = par_ed_simp, y = n, fill = par_ed_simp)) + geom_col(show.legend = F) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(x = "Parental education")
ggsave("figures/par_ed_attainment.png")

par_ed_df %>%
  count(par_ed_simp, mon_supp) %>%
  group_by(par_ed_simp) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(y = prop, x = par_ed_simp, fill = mon_supp)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  labs(x = "Parental education", y = "Proportion") +
  guides(fill = guide_legend("Additional monetary support needed")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  theme_bw()
ggsave("figures/par_ed_attainment_mon_supp_needed.png")


## looking at application for awards
responses_2021_22_cleaned %>% 
       select(contains("motivation to apply for sch")) %>%
  tidyr::pivot_longer(everything(), 
                      names_to = "motivation_type", values_to = "rank", 
                      values_transform = list(rank = as.character)) %>%
  mutate(motivation_type = gsub(".*\\[(.*)\\].*","\\1", motivation_type),
         rank = ifelse(rank == "1", "1 - most important", 
                       ifelse(rank == "3", "3 - least important", rank))) %>%
  group_by(rank, motivation_type) %>% count() %>% 
  group_by(rank) %>% mutate(prop = n/sum(n)) %>%
  ggplot() + geom_col(aes(x = rank, y = prop, fill = motivation_type)) +
  theme_bw()
ggsave("figures/award_app_motivs.png")
