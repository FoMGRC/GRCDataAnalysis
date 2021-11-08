library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

theme_set(theme(legend.position = c(0.8,0.8), 
                legend.background = element_rect(colour = "transparent"),
                legend.text = element_text(size = 16),
                legend.key.height = unit(0.5, "cm"),
                legend.title = element_text(face = "bold", size = 18, hjust = 0),
                plot.title = element_text(hjust = 0.5, size = 20, vjust = -6),
                plot.margin = unit(c(1,1,1,1), "cm"),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.line = element_blank(),
                axis.title = element_text(size = 15,face = "bold"),
                axis.text = element_text(size = 12, face = "bold")
))

pal <- c("#FAE5A1","#99C19A","#725F7A","#B46980","#FDFCB7",
         "#87B3C4","#85A173", "#C09968", "#CB74F5", "8CC9AE", "#BE7BBB", "#ACE8E9")

## make presentation fig dir
dir.create("figures/pres_iter")

## import responses
responses_2021_22_cleaned <- 
  read_delim("GRCDataAnalysis/survey_results/cleaned/GRC_Survey_Cleaned_2021-22.tsv", 
             delim = "\t")
responses_2021_22_cleaned <- responses_2021_22_cleaned %>% 
  filter(`What department are you in?` != "Department of Rehabilitation Sciences Institute")

## sources of additional support
responses_2021_22_cleaned %>%
  transmute(add_supp_short = `If you require additional monetary support for your day-to-day expenses, what are the sources?`,
    add_supp_sources = case_when(
      grepl("OSAP, line", add_supp_short) | 
        grepl("CERB,", add_supp_short) ~ 
        gsub("CERB,", "CERB;", gsub("OSAP, line", "OSAP; line", add_supp_short)),
      grepl(", I do not receive", add_supp_short) ~ 
        gsub(", I do not receive additional support|, Note I usually take OSAP but did not get it this year", 
             "", add_supp_short),
      TRUE ~ add_supp_short
    )) %>%
  filter(!grepl("As an international student from China|Note I usually|OSAP does not support", add_supp_short)) %>%
  transmute(add_supp_sources = str_split(add_supp_sources,", ")) %>%
  unnest(add_supp_sources) %>% count(add_supp_sources) %>% 
  filter(n > 1) %>%
  arrange(n) %>%
  mutate(perc = n/444, add_supp_sources = 
           factor(add_supp_sources, levels = add_supp_sources)) %>% 
  ggplot(aes(x = add_supp_sources, y = perc)) + 
  geom_col(aes(fill = add_supp_sources), show.legend = F) + 
  scale_fill_manual(
    values = c(rep(pal[6], 2), rep(pal[11], 2), rep(pal[6], 2))) +
  scale_y_continuous(labels = percent) + 
  scale_x_discrete(labels = ~ str_wrap(., 15)) +
  labs(y = "% of respondents", x = "Source of additional support")
ggsave("figures/pres_iter/sources_addtnl_supp.pdf", width = 8, height = 5)
ggsave("figures/pres_iter/sources_addtnl_supp.png", width = 8, height = 5)

## finances discouraging transfer to PhD, by gender/race
disc_df <- responses_2021_22_cleaned %>% 
  transmute(transfer_intend = `If MSc, do you intend to transfer to the PhD program?`, 
            transfer_disc = `If you were discouraged to transfer to a PhD, what specifically impacted your decision?`, 
            gend_ident = case_when(
              `Gender Identity` != "man" & `Gender Identity` != "woman" ~ "Gender minorities",
              `Gender Identity` == "man" ~ "Men",
              `Gender Identity` == "woman" ~ "Women",
              TRUE ~ `Gender Identity`),
            racialized = case_when(
              `Do you identify as a racialized person?` == "Yes" ~ "Identify as racialized",
              `Do you identify as a racialized person?` == "No" ~ "Do not identify as racialized",
              TRUE ~ `Do you identify as a racialized person?`), 
            `Are you an international or domestic student?`) %>%
  filter(!is.na(transfer_intend)) %>%
  mutate(transfer_disc_summ = case_when(
    grepl("afford|[Ff]inancial|financially|cost|money|debt|expenses|Underpaid|unsustainable|dependents|pay", 
          transfer_disc) ~ "Financial",
    is.na(transfer_disc) ~ "Not discouraged",
    TRUE ~ "Other"
  )) 

## Yes or No (domestic)
responses_2021_22_cleaned %>% 
  filter(!is.na(`Does/did the lack of financial security during graduate school discourage your decision to transfer to a PhD?`),
         grepl("Domestic", `Are you an international or domestic student?`)) %>%
  transmute(transfer_disc_summ = factor(`Does/did the lack of financial security during graduate school discourage your decision to transfer to a PhD?`,
              levels = c("No", "Yes"))) %>%
  count(transfer_disc_summ) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(fill = transfer_disc_summ, x = "2020-21", y = prop)) + 
  geom_col(colour = "black") +
  scale_y_continuous(breaks = seq(0.0,1.0,0.2)) +
  scale_fill_manual(values = c(Yes = pal[11], No = "#E2E3E7")) +
  guides(fill = guide_legend(title = "Response")) +
  labs(x = "", y = "Proportion of Domestic Student Responses") +
  theme(legend.position = "right")
ggsave("figures/pres_iter/transfer_discouraged_finances_domestic.pdf", 
       width = 4, height = 5)
  
### by gender
disc_df %>% group_by(transfer_disc_summ, transfer_intend, gend_ident) %>% 
  count() %>%
  ggplot(aes(x = transfer_intend, y = n, 
             fill = factor(transfer_disc_summ, 
                           levels = c("Financial", "Other", "Not discouraged")))
         ) +
  facet_wrap(vars(gend_ident), ncol = 1, strip.position = "top")  +
  geom_col(position = "identity") +
  #geom_text(aes(label = n, group = transfer_intend))# +
  labs(x = "Intend to transfer to PhD") +
  scale_fill_manual(values = pal[c(11,6,12)]) + 
  guides(fill = guide_legend(title = str_wrap("Transfer discouragement reason", 25))) +
  theme(legend.position = "right",
        strip.background = element_rect(fill = NA, size = NA),
        strip.text = element_text(size = 13,face = "bold", hjust = 0, vjust = 1))
ggsave("figures/pres_iter/transfer_discouraged_gender.png", width = 8, height = 5)
ggsave("figures/pres_iter/transfer_discouraged_gender.pdf", width = 8, height = 5)


### by racialized/not
disc_df %>% group_by(transfer_disc_summ, transfer_intend, racialized) %>% 
  count() %>%
  filter(racialized != "Prefer not to say") %>%
  ggplot(aes(x = transfer_intend, y = n, 
             fill = factor(transfer_disc_summ, 
                           levels = c("Financial", "Other", "Not discouraged")))
  ) +
  facet_wrap(vars(racialized), ncol = 1, strip.position = "top")  +
  geom_col(position = "identity") +
  scale_fill_manual(values = pal[c(11,6,12)]) + 
  #geom_text(aes(label = n, group = transfer_intend))# +
  labs(x = "Intend to transfer to PhD") +
  guides(fill = guide_legend(title = str_wrap("Transfer discouragement reason", 25))) +
  theme(legend.position = "right",
        strip.background = element_rect(fill = NA, size = NA),
        strip.text = element_text(size = 13,face = "bold", hjust = 0, vjust = 1))
ggsave("figures/pres_iter/transfer_discouraged_racialized.png", width = 8, height = 5)
ggsave("figures/pres_iter/transfer_discouraged_racialized.pdf", width = 8, height = 5)

## motivations for jobs
responses_2021_22_cleaned %>% select(contains("If you have additional employment, what is your motivation")) %>%
  pivot_longer(everything(), names_to = "motivation", values_to = "rank") %>% filter(!is.na(rank)) %>%
  group_by(rank, motivation) %>% count() %>% group_by(rank) %>% 
  mutate(prop = n/sum(n), 
         motivation = gsub("If you have additional employment, what is your motivation \\(including TA-ships\\)\\? |\\[|\\]",
                           "", motivation),
         rank = factor(as.character(rank), levels = rev(c("1","2","3")))) %>%
  ggplot(aes(y = prop, fill = rank, x = "2021-22")) + geom_col(colour = "black") +
  facet_wrap(vars(motivation), nrow = 1) +
  scale_fill_manual(values = c("1"=pal[11], "2" = pal[6], "3" = "#E2E3E7")) +
  scale_y_continuous(breaks = seq(0.0,1.0,0.2)) +
  guides(fill = guide_legend(title = "Rank", reverse = F)) +
  labs(x = "2021-2022", y = "Proportion of responses") +
  theme(legend.position = "right", 
        strip.background = element_rect(fill = NA, colour = "black"),
        strip.text = element_text(size = 13),
        axis.text.x = element_blank(), axis.ticks = element_blank()
        )
ggsave("figures/pres_iter/addtnl_emp_motivation_ranking_21-22.pdf", width = 7, height = 5)


## satisfaction w/ current housing
responses_2021_22_cleaned %>% select(`Are you satisfied with your housing options?`) %>%
  count(`Are you satisfied with your housing options?`) %>% mutate(prop = n/sum(n))

## % students financial situation mental health
responses_2021_22_cleaned %>% 
  filter(`Do you struggle with anxiety or depression?` == "Yes", 
         !is.na(`My financial situation has impacted my mental health.`)) %>%
  mutate(affected = factor(ifelse(`My financial situation has impacted my mental health.` > 1, 
                           "Affected", "Not Affected"),
                           levels = rev(c("Affected", "Not Affected")))) %>%
  count(affected) %>% mutate(perc = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x = "2021-2022", y = perc, fill = affected)) +
  # geom_text(aes(label = scales::percent(perc, accuracy = 0.01), x = affected,
  #               y = n + 5)) +
  scale_y_continuous(breaks = seq(0.0,1.0,0.2)) +
  scale_fill_manual(values =  c(Affected = pal[11], `Not Affected` = "#E2E3E7")) +
  labs(y = "Proportion", x = "") +
  guides(fill = guide_legend(title = "Response")) +
  theme(legend.position = "right")
ggsave("figures/pres_iter/financ_impacted_mental_health_in_anxdep_yes.pdf", width = 4, height = 6)



