library(ggplot2)
library(dplyr)
library(readr)

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

