library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

theme_set(theme(axis.text = element_text(size = 13, face = "bold"),
                plot.title = element_text(size = 15, hjust = 0),
                panel.background = element_rect(fill = NA, colour = "grey90")))
## need to modify to reduce the number of dependencies
responses_2021_22_cleaned <- 
  read_delim("GRCDataAnalysis/survey_results/cleaned/GRC_Survey_Cleaned_2021-22.tsv", 
             delim = "\t")
## filter out RSI
responses_2021_22_cleaned <- responses_2021_22_cleaned %>% 
  filter(dept.short != "RSI")

dir.create("GRCDataAnalysis/figures/distributions/", showWarnings = F)

qstns <- c(year = "What is your year of study (as of September 2021)?",
           deg_prog = "What degree program are you in?",
           age = "Age",
           department = "What department are you in?",
           research_area = "What is your area of research?",
           gender = "Gender Identity",
           race = "What racial and ethnic origins do you identify with?",
           international = "Are you an international or domestic student?"
           )

for (i in 1:length(qstns)) {
  qstn <- qstns[i]
  p <- responses_2021_22_cleaned %>% ggplot() +
    geom_histogram(aes_(x = as.name(qstn)), stat = "count", fill = "#0A4B97") +
    theme_bw() + #theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) +
    labs(title = qstn) 
  
  if (is.numeric(responses_2021_22_cleaned[[qstn]])) {
    m <- mean(responses_2021_22_cleaned[[qstn]])
    p <- p + geom_vline(xintercept = m) +
      scale_x_binned(labels = ~stringr::str_wrap(., width = 10)) +
      theme(axis.text.x = element_blank())
  } else{
    p <- p +
      scale_x_discrete(labels = ~stringr::str_wrap(., width = 10))
  }
  
  ggsave(filename = file.path("GRCDataAnalysis/figures/distributions", 
                              paste0(names(qstns[i]), ".png")),
         plot = p, width = 8, height = 3.5, units = "in")
}

## race and racialized compare
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(y = `What racial and ethnic origins do you identify with?`,
                     fill = `Do you identify as a racialized person?`), stat = "count") +
  theme(axis.text.y = element_text(size = 5)) 
ggsave("figures/distributions/race_racialized.png", width = 20)

## scholarship types/amounts
responses_2021_22_cleaned %>% 
  select(contains("What type of scholarship(s) did you receive during the 2020/2021 academic year?")) %>%
  tidyr::pivot_longer(everything(), names_to = "Scholarship", values_to = "Value") %>%
  filter(Value != "0.0") %>%
  mutate(Scholarship = gsub(".*\\[(.*)\\].*","\\1", Scholarship),
         Value = 
           factor(Value, 
                  levels = c("$1 - $5,000", "$5,000 - $9,999",
                             "$10,000 - $14,999","$15,000+"))) %>%
  ggplot(aes(x = Scholarship, fill = Value)) +
  geom_bar(stat = "count", position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_x_discrete(labels = ~stringr::str_wrap(., width = 10), drop=FALSE) + 
  theme_bw() + labs(title = "What type of scholarship(s) did you receive during the 2020/2021 academic year?") +
  theme(axis.text.x = element_text(size = 8), legend.position = "top")
ggsave(filename = "GRCDataAnalysis/figures/distributions/scholarships.png",
       width = 8, height = 3, units = "in")

## break up ethnic origins
responses_2021_22_cleaned %>% select(`What racial and ethnic origins do you identify with?`) %>%
  transmute(ethnicities = `What racial and ethnic origins do you identify with?`,
         eth_mod = gsub("), ", ")%", ethnicities),
         eth_split = strsplit(eth_mod, "%"),
         num_resp = as.character(sapply(eth_split, length))) %>%
  tidyr::unnest(eth_split) %>%
  filter(!is.na(eth_split)) %>%
  ggplot(aes(y = eth_split, fill = num_resp)) + geom_bar(stat = "count") +
  scale_y_discrete(labels = ~stringr::str_wrap(., width = 50)) + 
  scale_x_continuous(expand = expansion(add = c(1, 5))) +
  theme_bw() + theme(axis.text.y = element_text(size = 7), legend.position = "bottom", 
                     legend.box.margin = margin(0,0,0,0), 
                     legend.key = element_rect(size = 2)) +
  guides(fill = guide_legend(title = "Number of ethnic origins selected")) +
  labs(title = "What racial and ethnic origins do you identify with?",
       x = "Ethnic Origin")
ggsave(filename = "GRCDataAnalysis/figures/distributions/ethnicities.png",
       width = 8, height = 6, units = "in") 


## top-up
responses_2021_22_cleaned %>% 
  ggplot(
    aes(x = `What was the monetary value, beyond your base stipend, that you received as a result of winning any awards (i.e. \\top-ups\\) for the 2020/2021 academic year?`)) + 
  geom_histogram(breaks = seq(0, 5000, 1000), fill = "#0A4B97") + 
  theme_bw() + theme(plot.title = element_text(size = 12)) +
  labs(x = "Top-up amount", title = "What was the monetary value, beyond your base stipend,\nthat you received as a result of winning any awards (i.e. top-ups) for the 2020/2021 academic year?",
                                            )
ggsave(filename = "GRCDataAnalysis/figures/distributions/top-up.png",
       width = 8, height = 3, units = "in") 

## monetary support sought + sources
responses_2021_22_cleaned %>% select(contains("How much monetary support")) %>%
  pivot_longer(everything(), names_to = "Source", values_to = "Amount") %>%
  filter(Amount != "0.0") %>%
  mutate(
    Source = gsub("How much monetary support do you seek for your day-to-day expenses \\(annually\\)\\?  What are the sources\\? |\\[|\\]","",Source),
    Amount = factor(Amount, 
                    levels = c("$1 - $1,000", "$1,000 - $5,000", "$5,000 - $10,000",
                               "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+"))) %>%
  ggplot(aes(x = Source, fill = Amount)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = ~str_wrap(., width = 20)) +
  labs(x = "Source of additional income", y = "# of respondents",
       title = str_wrap("How much monetary support do you seek for your day-to-day expenses (annually)?  What are the sources?", 90)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, size = 15, vjust = 1))
ggsave("GRCDataAnalysis/figures/distributions/monetary_supp_sources_amount.png", 
       width = 10, height = 4)

## non-monetary support sought + sources
responses_2021_22_cleaned %>% select(contains("How much non-monetary support")) %>%
  pivot_longer(everything(), names_to = "Source", values_to = "Amount") %>%
  filter(Amount != "0.0") %>%
  mutate(
    Source = gsub("How much non-monetary support do you receive in the following forms and how much is their monetary value \\(annually\\)\\? |\\[|\\]","",Source),
    Amount = factor(Amount, 
                    levels = c("$1 - $1,000", "$1,000 - $5,000", "$5,000 - $10,000",
                               "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+"))) %>%
  ggplot(aes(x = Source, fill = Amount)) +
  geom_bar(position = position_dodge2(width = 1, preserve = "single")) +
  scale_x_discrete(labels = ~str_wrap(., width = 20)) +
  labs(x = "Source of additional non-monetary support", y = "# of respondents",
       title = str_wrap("How much non-monetary support do you receive in the following forms and how much is their monetary value (annually)?", 90)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, size = 15, vjust = 1))
ggsave("GRCDataAnalysis/figures/distributions/non_monetary_supp_sources_amount.png", 
       width = 10, height = 4)


## monthly income change due to COVID
responses_2021_22_cleaned %>% select(contains("How much has your monthly income")) %>%
  mutate(month_inc = `How much has your monthly income ($) changed due to the COVID-19 pandemic in 2020-2021?`,
         month_inc_binned = case_when(
           month_inc == 0 ~ "$0",
           month_inc < 0 & month_inc >= -1000 ~ "$0 to -$1,000",
           month_inc < -1000 & month_inc >= -5000 ~ "-$1,000 to -$5,000",
           month_inc < -5000 ~ "> -$5,000",
           month_inc > 0 & month_inc <= 1000 ~ "$0 to $1,000",
           month_inc > 1000 & month_inc <= 5000 ~ "$1,000 to $5,000",
           month_inc > 5000 ~ "> $5,000"
         )) %>%
  ggplot(aes(x = factor(month_inc_binned, 
                        levels = c("> -$5,000","-$1,000 to -$5,000",
                                   "$0 to -$1,000", "$0", "$0 to $1,000",
                                   "$1,000 to $5,000", "> $5,000")))) +
  geom_bar(fill = "#0A4B97") +
  labs(x = "Monthly income change", y = "Count", 
       title = "How much has your monthly income ($) changed due to the COVID-19 pandemic in 2020-2021?")
ggsave("GRCDataAnalysis/figures/distributions/monthly_income_change_covid.png", 
       width = 10, height = 4)

## monthly income change due to COVID
responses_2021_22_cleaned %>% select(contains("How much have your monthly expense")) %>%
  mutate(month_inc = `How much have your monthly expenses ($) changed due to the COVID-19 pandemic in 2020-2021?`,
         month_inc_binned = case_when(
           month_inc == 0 ~ "$0",
           month_inc < 0 & month_inc >= -1000 ~ "$0 to -$1,000",
           month_inc < -1000 & month_inc >= -5000 ~ "-$1,000 to -$5,000",
           month_inc < -5000 ~ "> -$5,000",
           month_inc > 0 & month_inc <= 1000 ~ "$0 to $1,000",
           month_inc > 1000 & month_inc <= 5000 ~ "$1,000 to $5,000",
           month_inc > 5000 ~ "> $5,000"
         )) %>%
  ggplot(aes(x = factor(month_inc_binned, 
                        levels = c("> -$5,000","-$1,000 to -$5,000",
                                   "$0 to -$1,000", "$0", "$0 to $1,000",
                                   "$1,000 to $5,000", "> $5,000")))) +
  geom_bar(fill = "#0A4B97") +
  labs(x = "Monthly income change", y = "Count", 
       title = "How much have your monthly expenses ($) changed due to the COVID-19 pandemic in 2020-2021?")
ggsave("GRCDataAnalysis/figures/distributions/monthly_exp_change_covid.png", 
       width = 10, height = 4)

## Not applying to OSAP reasons
responses_2021_22_cleaned %>% 
  select(contains("If you DID NOT")) %>%
  filter(!grepl("N/A",`If you DID NOT apply for OSAP, what was the reason?`)) %>%
  count(`If you DID NOT apply for OSAP, what was the reason?`) %>%
  arrange(desc(n)) %>%
  mutate(reason = factor(`If you DID NOT apply for OSAP, what was the reason?`, 
                         levels = `If you DID NOT apply for OSAP, what was the reason?`)) %>%
  ggplot(aes(x = reason, fill = reason, y = n)) + 
  geom_col(show.legend = F, position = "dodge") +
  scale_x_discrete(labels = ~str_wrap(., 20)) +
  labs(title = "If you DID NOT apply for OSAP, what was the reason?", 
       x = "Reason", y = "Number of respondents")
ggsave("GRCDataAnalysis/figures/distributions/non_osap_app.png", 
       width = 10, height = 4) 

## Applied to OSAP did not receive reasons
responses_2021_22_cleaned %>% 
  select(contains("If you APPLIED")) %>%
  transmute(reason = 
              str_split(
                gsub("\"","",
                     gsub(", such that", " such that", 
                          gsub(", but this", " but this", 
                               `If you APPLIED but you did not receive funds from OSAP in 2020-2021, what was the reason?`))), 
                ", ")) %>%
  filter(!grepl("N/A I did not apply|I applied and received funds", reason)) %>%
  unnest(reason) %>% 
  mutate(reason2 = case_when(
    grepl("[Ss]tipend", reason) ~ "Stipend",
    grepl("[Pp]arent|[Ff]amily", reason) ~ "Family",
    grepl("[Ss]cholarship", reason) ~ "Scholarship",
    grepl("only offered a loan", reason) ~ "Only offered loan, did not want more student debt",
    grepl("[Ss]avings", reason) ~ "Savings",
    TRUE ~ reason
  )) %>%
  filter(!is.na(reason2)) %>%
  count(reason2) %>% arrange(n) %>%
  mutate(reason2 = factor(reason2, levels = reason2)) %>%
  ggplot(aes(y = reason2, fill = reason2, x = n)) + 
  geom_col(show.legend = F, position = "dodge") +
  scale_y_discrete(labels = ~str_wrap(., 30)) +
  labs(title = "If you APPLIED but you did not receive funds from OSAP in 2020-2021, what was the reason?", 
       x = "# of times cited", y = "Reason cited")
   
ggsave("GRCDataAnalysis/figures/distributions/osap_app_not_receive.png", 
       width = 11, height = 4) 
  
## OSAP received - loans
responses_2021_22_cleaned %>% 
  select(contains("receive in OSAP loans")) %>%
  filter(`How much money ($) did you receive in OSAP loans?` > 0) %>%
  ggplot(aes(x = `How much money ($) did you receive in OSAP loans?`)) +
  geom_bar(fill = "#0A4B97") +
  scale_x_binned(show.limits = F, breaks = seq(0, 20000, 2000)) +
  labs(x = "Amount", title = "How much money ($) did you receive in OSAP loans?")
ggsave("GRCDataAnalysis/figures/distributions/osap_loans_received.png", 
       width = 11, height = 4) 
## OSAP received - grants
responses_2021_22_cleaned %>% 
  select(contains("receive in OSAP grants")) %>%
  filter(`How much money ($) did you receive in OSAP grants?` > 0) %>%
  ggplot(aes(x = `How much money ($) did you receive in OSAP grants?`)) +
  geom_bar(fill = "#0A4B97") +
  scale_x_binned(breaks = seq(0, 20000, 2000)) +
  labs(x = "Amount", title = "How much money ($) did you receive in OSAP grants?")
ggsave("GRCDataAnalysis/figures/distributions/osap_grants_received.png", 
       width = 11, height = 4) 


## OSAP impact situation
responses_2021_22_cleaned %>% 
  select(contains("If you received OSAP, how did it")) %>%
  filter(`If you received OSAP, how did it impact your financial situation?` != "N/A I did not apply") %>%
  transmute(impact = str_split(`If you received OSAP, how did it impact your financial situation?`, ", ")) %>%
  unnest(impact) %>% count(impact) %>% arrange(n) %>% mutate(impact = factor(impact, levels = impact)) %>%
  ggplot(aes(y = impact, x = n)) + geom_col(fill = "#0A4B97") +
  scale_y_discrete(labels = ~str_wrap(., 35)) +
  labs(y = "Impact (119 respondents)", x = "Number of times selected", 
       title = "If you received OSAP, how did it impact your financial situation?")
ggsave("GRCDataAnalysis/figures/distributions/osap_impact.png", 
       width = 11, height = 4)  

## CERB - amount
responses_2021_22_cleaned %>% 
  select(contains("How much money ($) did you receive from CERB")) %>%
  filter(`How much money ($) did you receive from CERB?` > 0) %>%
  ggplot(aes(x = `How much money ($) did you receive from CERB?`)) + 
  geom_bar(fill = "#0A4B97") +
  scale_x_binned(show.limits = F, breaks = seq(0, 14000, 2000)) +
  labs(y = "# of respondents", x = "Amount",
       title = "How much money ($) did you receive from CERB?")
ggsave("GRCDataAnalysis/figures/distributions/cerb_amount.png", 
       width = 11, height = 4)

## CERB - impact
responses_2021_22_cleaned %>% 
  select(contains("If you received CERB")) %>%
  filter(across(everything(), ~.!= "N/A I did not apply")) %>%
  mutate(impact = gsub(", I received a.*|, CESB, not CERB","", 
                       `If you received CERB, how did it impact your financial situation?`)) %>%
  transmute(impact = str_split(impact, ", ")) %>%
  unnest(impact) %>% filter(across(everything(), ~.!= "N/A I did not apply")) %>%
  count(impact) %>% arrange(n) %>% mutate(impact = factor(impact, impact)) %>%
  ggplot(aes(x = n, y = impact)) + geom_col(fill = "#0A4B97") +
  labs(y = "Impact (120 respondents)", x = "Number of times selected",
       title = "If you received CERB, how did it impact your financial situation?")
ggsave("GRCDataAnalysis/figures/distributions/cerb_impact.png", 
       width = 11, height = 4)

## monthly housing exp
med_housing_exp = median(responses_2021_22_cleaned$`Approximately what are your individual monthly housing expenses (i.e. rent, mortgage, maintenance fees, utilities, etc.)?`)
responses_2021_22_cleaned %>%
  ggplot() +
  geom_histogram(aes(x = `Approximately what are your individual monthly housing expenses (i.e. rent, mortgage, maintenance fees, utilities, etc.)?`),
                 fill = "#0A4B97", binwidth = 500) +
  geom_vline(xintercept = med_housing_exp, linetype = 2) +
  geom_text(label = paste0("median = $", med_housing_exp), 
            aes(x = 2200, y = 165)) +
  labs(x = "Amount ($)", y = "# of respondents",
       title = "Approximately what are your individual monthly housing expenses (i.e. rent, mortgage, maintenance fees, utilities, etc.)?")
ggsave("GRCDataAnalysis/figures/distributions/month_housing_exp.png", 
       width = 11, height = 4)

## main modes of transportation
responses_2021_22_cleaned %>%
  transmute(transp = str_split(gsub("TTC, GO", "TTC or GO", `What are your main modes of transportation?`), ", ")) %>% 
  unnest(transp) %>% filter(!is.na(transp)) %>% count(transp) %>%
  arrange(desc(n)) %>% mutate(transp = factor(transp, levels = transp)) %>%
  ggplot() + geom_col(aes(y = n, x = transp), fill = "#0A4B97") +
  scale_x_discrete(labels = ~str_wrap(., 15)) +
  labs(x = "Mode of transportation", y = "# of responses", title = "What are your main modes of transportation?")
ggsave("GRCDataAnalysis/figures/distributions/transp_modes.png", 
       width = 11, height = 4)

## transportation spending
med_transp <- median(responses_2021_22_cleaned$`How much do you spend on transit per month?`)
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `How much do you spend on transit per month?`),
                 fill = "#0A4B97", binwidth = 50, colour = "black") +
  geom_vline(xintercept = med_transp, linetype = 2) +
  geom_text(label = paste0("median = $", med_transp), 
            aes(x = 150, y = 115)) +
  labs(x = "Amount ($)", y = "# of respondents",
       title = "How much do you spend on transit per month?")
ggsave("GRCDataAnalysis/figures/distributions/transp_exp.png", 
       width = 11, height = 4)

## commute length
med_commute <- median(responses_2021_22_cleaned$`How long on average is your commute (please report for one-way in minutes)?`)
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `How long on average is your commute (please report for one-way in minutes)?`),
                 fill = "#0A4B97", binwidth = 10, colour = "black") +
  geom_vline(xintercept = med_commute, linetype = 2) +
  geom_text(label = paste0("median = ", med_commute, " mins"), 
            aes(x = 40, y = 110)) +
  labs(x = "Minutes (one-way)", y = "# of respondents",
       title = "How long on average is your commute (please report for one-way in minutes)?")
ggsave("GRCDataAnalysis/figures/distributions/commute_length.png", 
       width = 11, height = 4)

## transportation subsidy proposals
responses_2021_22_cleaned %>% 
  transmute(subs_prop = `Which of the following transportation subsidy proposals would interest you?`) %>%
  filter(!grepl("^Don't|^I submit my transit|^I would vote|^Significantly|^tiered subsidy based|^UPASS|^Free TTC Pass", subs_prop)) %>%
  transmute(subs_prop = str_split(gsub(", I could.*|, No opinion|, TTC pass with.*|, BikeShare has.*|, Something in partnership.*|, Graduate students.*| \\(once the COVID-19 pandemic has passed\\)", 
                             "", subs_prop),", ")) %>% 
  unnest(subs_prop) %>% count(subs_prop) %>% arrange(desc(n)) %>% 
  mutate(subs_prop = factor(subs_prop, subs_prop)) %>%
  ggplot() + geom_col(aes(x = subs_prop, y = n),  fill = "#0A4B97") +
  scale_x_discrete(labels = ~str_wrap(., 15)) +
  labs(x = "Subsidy Proposal", y = "Proposal selection count", 
       title = "Which of the following transportation subsidy proposals would interest you?")
ggsave("GRCDataAnalysis/figures/distributions/subsidy_props.png", 
       width = 11, height = 4)

## monthly health care expenses not covered by GSU insurance
med_exp <- median(responses_2021_22_cleaned$`Approximately what are your individual monthly health care expenses not covered by the U of T insurance plan (i.e. left over dental costs, emergency dental, physiotherapy, additional eye care, counselling, etc.)?`, na.rm = T)
mean_exp <- mean(responses_2021_22_cleaned$`Approximately what are your individual monthly health care expenses not covered by the U of T insurance plan (i.e. left over dental costs, emergency dental, physiotherapy, additional eye care, counselling, etc.)?`, na.rm = T)

responses_2021_22_cleaned %>% transmute(exps = `Approximately what are your individual monthly health care expenses not covered by the U of T insurance plan (i.e. left over dental costs, emergency dental, physiotherapy, additional eye care, counselling, etc.)?`) %>%
  ggplot() + geom_histogram(aes(x = exps), fill = "#0A4B97", binwidth = 500) + 
  geom_vline(xintercept = med_exp, linetype = 2) +
  geom_text(label = paste0("mean = $", round(mean_exp)), aes(x = 650, y = 280)) +
  labs(x = "Remaining health care expenses ($)", y = "# of Respondents", 
       title = str_wrap("Approximately what are your individual monthly health care expenses not covered by the U of T insurance plan (i.e. left over dental costs, emergency dental, physiotherapy, additional eye care, counselling, etc.)?", 
                        110))
ggsave("GRCDataAnalysis/figures/distributions/health_care_exp_after_insur.png", 
       width = 11, height = 4)

## Intl why Canada
responses_2021_22_cleaned %>% 
  transmute(why_can = str_split(`What made you decide to come to Canada for graduate school?`, ", ")) %>% 
  unnest(why_can) %>% filter(!is.na(why_can)) %>%
  mutate(why_can_summ = ifelse(
    !grepl("^Accessibility|^Canadian|Family|Financial|Top-notch|Want to settle|No particular", why_can), "Other", why_can)) %>% 
  count(why_can_summ) %>% arrange(desc(n)) %>% mutate(why_can_summ = factor(why_can_summ, why_can_summ)) %>%
  ggplot() + geom_col(aes(x = why_can_summ, y = n), fill = "#0A4B97") +
  scale_x_discrete(labels = ~str_wrap(., 15)) +
  labs(x = "Reason", y = "Selection count", title = "What made you decide to come to Canada for graduate school?")
ggsave("GRCDataAnalysis/figures/distributions/intl_canada_grad.png", 
       width = 11, height = 4)
  
  
  