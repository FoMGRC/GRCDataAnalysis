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

## all degree program types
responses_2021_22_cleaned %>% 
  ggplot() + 
  geom_bar(aes(x = `What department are you in?`, fill = `What degree program are you in?`),
           position = "fill") +
  coord_flip() +
  labs(title = "Responses from each department")
ggsave("figures/what_dept.png")

## % additional monetary support needed
responses_2021_22_cleaned %>%
  group_by(`How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?`) %>%
  tally() %>% ungroup() %>% 
  transmute(proportion = n/sum(n), add_mon_support = 
              factor(`How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?`,
                     levels = c("0.0", "$1 - $1,000", "$1,000 - $5,000",
                                "$5,000 - $10,000", "$10,000 - $15,000",
                                "$15,000 - $20,000", "$20,000+")), n) %>%
  ggplot() + 
  geom_col(aes(y = n, fill = add_mon_support, x = add_mon_support), position = "dodge") +
  geom_text(aes(y = n + 5, label = scales::percent(proportion, 0.01), x = add_mon_support)) +
  labs(title = "How much additional monetary support would you need to meet your day-to-day\nexpenses each year (above your graduate living allowance)?") +
  theme(axis.text.x = element_text(angle = 270, hjust =0, vjust =0.5))
ggsave("figures/addtnl_mon_support.png")

## hours/week working outside lab (TAing + side)
hrs_wrked <- responses_2021_22_cleaned %>% 
  transmute(hrs_wrk_outsd_lab = 
              as.numeric(`How many hours per week do you work as a teaching assistant?`) + 
              as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`)) 
hrs_wrked_mean <- mean(hrs_wrked$hrs_wrk_outsd_lab) 
hrs_wrked %>% 
  ggplot() + 
  geom_histogram(aes(x = hrs_wrk_outsd_lab), binwidth = 5, bins = 10, fill = "grey60", colour = "black") +
  geom_vline(xintercept = hrs_wrked_mean, linetype = 2) +
  geom_text(aes(x = hrs_wrked_mean+2, y = 280, label = "mean")) +
  labs(x ="Hours/week working outside of lab (TAing + Side jobs)")
ggsave("figures/wrk_outside_lab.png")

## hours/week working outside lab (just side)
hrs_wrked <- responses_2021_22_cleaned %>% 
  transmute(hrs_wrk_outsd_lab = 
              as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`)) 
hrs_wrked_mean <- mean(hrs_wrked$hrs_wrk_outsd_lab) 
hrs_wrked %>% 
  ggplot() + 
  geom_histogram(aes(x = hrs_wrk_outsd_lab), binwidth = 5, bins = 10, fill = "grey60", colour = "black") +
  geom_vline(xintercept = hrs_wrked_mean, linetype = 2) +
  geom_text(aes(x = hrs_wrked_mean+2, y = 350, label = "mean")) +
  labs(x ="Hours/week working outside of lab (Just Side jobs)")
ggsave("figures/wrk_outside_lab_just_side.png")

## anx + depression
responses_2021_22_cleaned %>% 
  filter(!is.na(`Do you struggle with anxiety or depression?`)) %>%
  count(`Do you struggle with anxiety or depression?`) %>% mutate(perc = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x = `Do you struggle with anxiety or depression?`, y = n), 
           position = "dodge") +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.01), 
                x = `Do you struggle with anxiety or depression?`,
                y = n+ 5))## ~70% of students (more than 3 in 5 from last year)
ggsave("figures/anx_depr.png")

responses_2021_22_cleaned %>% 
  filter(`Do you struggle with anxiety or depression?` == "Yes", 
         !is.na(`My financial situation has impacted my mental health.`)) %>%
  mutate(affected = ifelse(`My financial situation has impacted my mental health.` > 1, 
                           "Affected", "Not Affected")) %>%
  count(affected) %>% mutate(perc = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x = affected, y = n), 
           position = "dodge") +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.01), x = affected,
                y = n + 5)) +
  labs(x = "Students w/ anxiety or depression who cite financial situation as impacting their mental health")
ggsave("figures/anx_depr_finances.png")

## Women, Gender Minorities, Racialized Students
### able to support selves on stipend alone
responses_2021_22_cleaned %>% filter(!is.na(`Gender Identity`)) %>%
  mutate(gend_id = 
           ifelse(`Gender Identity` == "man" | `Gender Identity` == "woman", 
                  `Gender Identity`, "gender minority")) %>% group_by(gend_id) %>%
  count(`Can you support all of your day-to-day living expenses exclusively from your graduate funding (i.e stipend and awards/top-ups)?`) %>% 
  group_by(gend_id) %>% mutate(perc = n/sum(n)) %>% 
  filter(`Can you support all of your day-to-day living expenses exclusively from your graduate funding (i.e stipend and awards/top-ups)?` == "Yes") %>% 
  ggplot() + geom_col(aes(x = gend_id, y = perc)) +
  geom_text(aes(label = paste0(n, "/", n/perc, " respondents"), 
                x = gend_id, y = (perc + 0.01))) +
  labs(title = "Students able to Support themselves on Stipend Alone", x = "Gender Identity", y = "Percentage")
ggsave("figures/gender_stipend_support.png")

### experiencing food/housing/medical insecurity, and racialized
responses_2021_22_cleaned %>% 
  filter(!is.na(`Do you experience any level of housing insecurity?`), 
         !is.na(`Do you experience any level of food insecurity?`), 
         !is.na(`Do you experience any level of health care insecurity?`),
         !is.na(`Do you identify as a racialized person?`),
         `Do you identify as a racialized person?` != "Prefer not to say") %>%
  select(`Do you experience any level of housing insecurity?`, 
         `Do you experience any level of food insecurity?`, 
         `Do you experience any level of health care insecurity?`,
         `Do you identify as a racialized person?`) %>%
  transmute(insecurity_type = case_when(
    `Do you experience any level of housing insecurity?` != "No" & 
      `Do you experience any level of food insecurity?` != "No" ~ "Food + Housing",
    `Do you experience any level of housing insecurity?` != "No" ~ "Housing",
    `Do you experience any level of food insecurity?` != "No" ~ "Food",
    TRUE ~ "No Insecurity"
  ), racialized = `Do you identify as a racialized person?`) %>% 
  group_by(racialized, insecurity_type) %>% count() %>% 
  group_by(racialized) %>% mutate(perc_of_racialized_group = n/sum(n)) %>%
  ggplot() + geom_col(aes(x = racialized, y = perc_of_racialized_group)) +
  geom_text(aes(label = paste0(n, "/", n/perc_of_racialized_group), 
                x = racialized, y = perc_of_racialized_group + 0.02)) +
  facet_grid(cols = vars(insecurity_type)) +
  labs(title = "Students Experiencing Food or Housing Insecurity",
       x = "Identified as Racialized", y = "Percentage")
ggsave("figures/racialization_insecurity.png")

### experiencing food/housing/medical insecurity, and international
responses_2021_22_cleaned %>% 
  filter(!is.na(`Do you experience any level of housing insecurity?`), 
         !is.na(`Do you experience any level of food insecurity?`), 
         !is.na(`Do you experience any level of health care insecurity?`),
         !is.na(`Are you an international or domestic student?`),
         `Do you identify as a racialized person?` != "Prefer not to say") %>%
  select(`Do you experience any level of housing insecurity?`, 
         `Do you experience any level of food insecurity?`, 
         `Do you experience any level of health care insecurity?`,
         `Are you an international or domestic student?`) %>%
  transmute(insecurity_type = case_when(
    `Do you experience any level of housing insecurity?` != "No" & 
      `Do you experience any level of food insecurity?` != "No" ~ "Food + Housing",
    `Do you experience any level of housing insecurity?` != "No" ~ "Housing",
    `Do you experience any level of food insecurity?` != "No" ~ "Food",
    TRUE ~ "No Insecurity"
  ), international = `Are you an international or domestic student?`) %>% 
  group_by(international, insecurity_type) %>% count() %>% 
  group_by(international) %>% mutate(perc_of_ntnl = n/sum(n)) %>%
  ggplot() + geom_col(aes(x = international, y = perc_of_ntnl)) +
  geom_text(aes(label = paste0(n, "/", n/perc_of_ntnl), 
                x = international, y = perc_of_ntnl + 0.02)) +
  facet_grid(cols = vars(insecurity_type)) +
  labs(title = "Students Experiencing Food or Housing Insecurity",
       x = "International or Domestic", y = "Percentage")
ggsave("figures/racialization_insecurity.png")
