library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)
library(stringr)
library(patchwork)

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
## make report fig dir
dir.create("figures/report_iter")

## import responses
responses_2021_22_cleaned <- 
  read_delim("GRCDataAnalysis/survey_results/cleaned/GRC_Survey_Cleaned_2021-22.tsv", 
             delim = "\t")
responses_2021_22_cleaned <- responses_2021_22_cleaned %>%
  filter(`What department are you in?` != "Department of Rehabilitation Sciences Institute")

## Import response #s
GRC_StudList_15Oct21 <- 
  read_excel("GRCDataAnalysis/external_data/GRC_StudList_15Oct21.xlsx")
studlist_sum <- GRC_StudList_15Oct21 %>% 
  filter(grepl("DOC|Masters",`Prog Type`),
         `Department Code` != "Rehabilitation Sciences"
  ) %>%
  mutate(prog_type_simp = 
           ifelse(`Prog Type` == "DOC", "PhD", "MSc")) %>%
  group_by(`Department Code`, prog_type_simp) %>% count(name = "enrolled_count")

## Page 1

### Department response bar graph
responses_2021_22_cleaned %>% 
  mutate(`Department Code` = gsub("Department of ", "", `What department are you in?`),
         prog_type_simp = ifelse(grepl("PhD", `What degree program are you in?`), 
                                        "PhD", "MSc")) %>%
  group_by(prog_type_simp,`Department Code`) %>%
  count(name = "respondent_count") %>% ungroup() %>%
  left_join(studlist_sum, by = c("Department Code", "prog_type_simp")) %>%
  mutate(prop_responding = respondent_count/enrolled_count) %>%
  ggplot(aes(x = `Department Code`, y = prop_responding*100, 
             fill = factor(prog_type_simp, levels = rev(c("MSc", "PhD"))))) +
  geom_col(position = position_dodge()) +
  ylab("Percent of MSc and PhD responses from each department") +
  scale_fill_manual(values = pal[c(6,11)]) +
  scale_y_continuous(expand = expansion(add = c(1, 20))) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE, title = "Degree Program"))
ggsave("figures/report_iter/response_percentages.png")
ggsave("figures/report_iter/response_percentages.pdf")

### Numbers for % that can support themselves from stipend alone (2021-22)
need_extra_by_can_live <- read_tsv("GRCDataAnalysis/Two-Page Report Graphics Iteration/need_extra_by_can_live.tsv")
need_extra_by_can_live_long <- need_extra_by_can_live %>% 
  pivot_longer(c(No, Yes), names_to = "Can_support_expenses_on_stipend", 
               values_to = "count") %>% 
  group_by(Can_support_expenses_on_stipend) %>% 
  mutate(total = paste0("n = ", sum(count)), prop = count/sum(count))

need_extra_by_can_live_long %>% 
  ggplot(aes(x = Can_support_expenses_on_stipend, y = prop,
             fill = 
               factor(Amount, levels = rev(c("0.0", "$1 - $1,000", "$1,000 - $5,000",
                                         "$5,000 - $10,000", "$10,000 - $15,000",
                                         "$15,000 - $20,000", "$20,000+"))))) +
  geom_col() + geom_text(aes(x = Can_support_expenses_on_stipend, label = total), 
                         y = 1.02, size = 6) +
  scale_fill_manual(values = c("#F5E099","#90B48B","#5C4C65","#98585E","#FDFDB2",
                          "#7FA3B4","#799065")) +
  theme(legend.position = "right") +
  scale_y_continuous(labels = function(x) percent(x)) +
  labs(x = str_wrap("Can support day-to-day living expenses exclusively from stipend and awards/top-ups", 
                    90),
       y = "Percentage") +
  guides(fill = 
           guide_legend(reverse = T, 
                             title = str_wrap("Additional Monetary Support Needed", 20)))
ggsave("figures/report_iter/need_extra_by_can_live.png")
ggsave("figures/report_iter/need_extra_by_can_live.pdf")

### External employment
#### percentage employed
ext_employment <- responses_2021_22_cleaned %>% 
  transmute(work_TA = 
              ifelse(as.numeric(`How many hours per week do you work as a teaching assistant?`) > 0, "Yes", "No"),
            work_side = ifelse(as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`) > 0, "Yes", "No")) %>%
  group_by(work_TA, work_side) %>%
  count() %>% ungroup() %>% mutate(perc_all = n/sum(n))
ext_employment %>% ggplot(aes(x = work_TA, y= work_side, fill =perc_all)) +
  geom_tile() + geom_text(aes(label = n), size = 10) +
  theme(legend.position = "right") +
  scale_fill_gradient(low = "#7FA3B4",high = "#FDFDB2") +
  guides(fill = guide_colorbar(title = str_wrap("% of respondents", 20))) +
  labs(x = "Work as TAs", y = "Work non-TA side-jobs")
ggsave("figures/report_iter/perc_ext_employed.png")
ggsave("figures/report_iter/perc_ext_employed.pdf")

#### histogram of # of hours
hrs_wrkd <- responses_2021_22_cleaned %>% 
  transmute(`Hours worked as TA` = as.numeric(`How many hours per week do you work as a teaching assistant?`),
            `Hours worked on side-job` = as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`)) %>%
  pivot_longer(everything(), names_to = "Job Type", values_to = "Hours worked per week") %>%
  filter(`Hours worked per week` > 0)
hrs_wrkd_means <- hrs_wrkd %>% group_by(`Job Type`) %>% 
  summarize(mean_wrked = meaqn(`Hours worked per week`), 
            median_wrked = median(`Hours worked per week`))
for (i in c(6,11)) {
  p <- hrs_wrkd %>%
    ggplot(aes(x = `Hours worked per week`)) +
    geom_bar(fill = pal[i], colour = "black", size = 0.7, width = 1, ) +
    scale_x_binned(show.limits = F, breaks = c(1, seq(5,40,5))) +
    #geom_histogram(fill = pal[i], colour = "black", size = 0.7, binwidth = 5) + 
    facet_wrap(vars(`Job Type`), nrow = 2, strip.position = "top") +
    ylab("# of Students") +
    geom_vline(data = hrs_wrkd_means, aes(xintercept = median_wrked), linetype = 2) +
    theme(strip.background = element_rect(fill = NA, size = NA),
          strip.text = element_text(size = 13,face = "bold", hjust = 0, vjust = 1))
  ggsave(file.path("figures/report_iter", paste0("hrs_wrkd_hist_", i,".png")), 
         plot = p, width = 9, height =4)
  ggsave(file.path("figures/report_iter", paste0("hrs_wrkd_hist_", i,".pdf")), 
         plot = p, width = 9, height =4)
  
  # hrs_wrkd %>%
  #   ggplot(aes(x = `Hours worked per week`)) +
  #   geom_histogram(fill = pal[i], colour = "black", size = 0.7, binwidth = 5, center = 0) +
  #   facet_wrap(vars(`Job Type`), nrow = 2, strip.position = "top")
}


#### % of people listing additional income as 1st reason for ext employment: 121 / 219 = 0.5525114155
#### 219 is the number of people that work >0 hours as TA or external

#### boxplot of hours worked in lab to external work hours?
responses_2021_22_cleaned %>% 
  #filter(grepl("PhD", `What degree program are you in?`)) %>%
  transmute(`Hours worked on side-job` = as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`), 
            `Hours worked as TA` = as.numeric(`How many hours per week do you work as a teaching assistant?`),
            `Hours worked on research` = 
              gsub("More than", ">", gsub("Less than", "<", gsub(" hours", "", `On average, how many hours a week do you typically work on research-related activities?`))),
            `Hours worked on side-job and TAing` = `Hours worked on side-job` + `Hours worked as TA`) %>%
  filter(`Hours worked on side-job` > 0) %>%
  group_by(`Hours worked on research`) %>%
  mutate(Count = n()) %>%
  ggplot(aes(
    x = factor(`Hours worked on research`, 
               levels = gsub(" hours", "", c("N/A - It's my first year", "< 11 hours",
                          "11-20 hours", "21-30 hours", "31-40 hours",
                          "41-50 hours", "51-60 hours", "61-70 hours",
                          "71-80 hours", "> 80 hours"
                          ))), 
    y = `Hours worked on side-job`, fill = Count)) +
  geom_boxplot() +  
  labs(x = "Hours worked on research per week") +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_fill_gradient(#low = "#7FA3B4", high = "#FDFDB2"
    low = pal[6], high = pal[11]) +
  theme(legend.position = c(0.08, 0.8))
ggsave("figures/report_iter/hrs_wrkd_outside_research.png", width = 9)
ggsave("figures/report_iter/hrs_wrkd_outside_research.pdf", width = 9)

### Mental Health Numbers (longitudinal)
anxiety_depression_yes <- read_tsv("GRCDataAnalysis/Two-Page Report Graphics Iteration/anxiety_depression_yes_2019-21.tsv")
anxiety_depression_yes %>% group_by(year) %>% 
  transmute(year, Yes = value, No = 1 - Yes) %>%
  pivot_longer(c(Yes, No), values_to = "Percentage", 
               names_to = "Struggle with anxiety or depression") %>%
  ggplot(aes(x = year, y = Percentage, 
             fill = factor(`Struggle with anxiety or depression`,
                           levels = c("No", "Yes")))) + 
  geom_col(show.legend = T, colour = "black") +
  labs(y = "Percentage", title = "Struggle with anxiety or depression") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = pal[c(6,11)]
                    #c("#C7C9CA",pal[11])
                    ) +
  guides(fill = guide_legend(reverse = F, title = NULL)) +
  theme(axis.title.x = element_blank(), legend.position = "right",
        plot.title = element_text(size = 20, vjust =1.5))
ggsave("figures/report_iter/anxiety_depression_perc_p+b.png", width = 7)
ggsave("figures/report_iter/anxiety_depression_perc_p+b.pdf", width = 7)

## Page 2
### % of people suffering from X number of insecurities
insecurity <- responses_2021_22_cleaned %>% select(contains("insecurity")) %>%
  filter(!if_all(everything(), ~is.na(.))) %>%
  mutate(across(everything(), ~case_when(
    grepl("Severe", .x) ~ "Severe",
    grepl("Moderate", .x) ~ "Moderate",
    grepl("Marginal", .x) ~ "Marginal",
    is.na(.x) ~ "NA",
    TRUE ~ "None"
  )))
insecurity_long <- insecurity %>% pivot_longer(everything(), names_to = "Insecurity type", 
                       values_to = "Insecurity severity") %>%
  mutate(`Insecurity type` = gsub("Do you experience any level of |\\?| insecurity", "", `Insecurity type`),
         `Insecurity severity` = factor(`Insecurity severity`, 
                                         levels = c("None", "Moderate", "Marginal", "Severe"))) %>%
  filter(!is.na(`Insecurity severity`))
#pairs(insecurity %>% table())
insecurity_long %>% group_by(`Insecurity type`, `Insecurity severity`) %>% count() %>%
  group_by(`Insecurity type`) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = `Insecurity type`, y = prop, fill = `Insecurity severity`)) +
  geom_col() + guides(guide_legend(reverse = F)) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = pal[c(2,5,4,3)]) +
  labs(y = "Percentage of Students") +
  theme(legend.position = "right")
ggsave("figures/report_iter/insecurity_perc.png")
ggsave("figures/report_iter/insecurity_perc.pdf")

colnames(insecurity) <- gsub("Do you experience any level of |\\?| insecurity", "", colnames(insecurity))
insecurity %>% rowwise() %>% 
  mutate(num_insec = sum(c_across(everything()) != "None" & 
                           c_across(everything()) != "NA")) %>% 
  count(num_insec) %>%
  ggplot(aes(x = num_insec, y = n)) + 
  geom_col(fill = pal[3]) +
  labs(x = "# of Insecurities (Food, Housing, and/or Health)",
       y = "Respondent Count")
ggsave("figures/report_iter/insecurity_count_basic.png")
ggsave("figures/report_iter/insecurity_count_basic.pdf")

### interactions between food/housing/medical insecurity
insec_tbl <- insec %>% table() %>% as.data.frame() %>% 
  mutate(across(1:3, ~factor(., levels = c("None","Marginal","Moderate","Severe")))) %>%
  group_by(Health.care, Food) %>%
  mutate(cat_prop = ifelse(Freq == 0, 0, Freq/sum(Freq)))
fd_sums <- insec_tbl %>% group_by(Food, Health.care) %>% 
  mutate(column_sum = sum(Freq))
insec_tbl %>%
  ggplot(aes(x = Food, y = cat_prop, fill = Housing)) +
  geom_col() +
  geom_text(data = fd_sums, aes(label = column_sum, x = Food, y = 1.02)) +
  facet_wrap(vars(Health.care), nrow = 1) + 
  guides(fill = guide_legend(title = "Housing Insecurity", reverse = F)) +
  xlab("Food Insecurity") + ylab("Percentage") + 
  labs(title = "Healthcare Insecurity") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = pal[c(2,5,4,3)]) +
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 12, face = "bold", vjust = 1),
        legend.position = "right", 
        axis.text.x = element_text(angle = 300, vjust = 0.5, hjust = 0),
        plot.title = element_text(hjust = 0.5, size = 14, vjust = 2, face = "bold"),
        legend.title = element_text(size = 14))
ggsave("figures/report_iter/insec_intersections_bar_plot_all.pdf", width = 8, height = 4)
ggsave("figures/report_iter/insec_intersections_bar_plot_all.png", height = 4)



### Instead of cost of living bar graphs, add the "positive note" figures;
#### Gap between stipend and poverty line vs. % of people that can support themselves
stip_pov_gap_v_perc_support <- read_xlsx("GRCDataAnalysis/external_data/2021 External Stats.xlsx",
                                         sheet = "TSV for R") %>%
  mutate(`Gap in 2019 Constant $` = `MBM 2019` - `PhD 2019`) %>% 
  filter(Year >= 2015, Year <= 2021)
fill_col <- "black" #pal[4]
prop_supp <- stip_pov_gap_v_perc_support %>% 
  ggplot(aes(x = Year, y = `Proportion able to Support Themselves`)) +
  geom_line(colour = fill_col) + geom_point(colour = fill_col) + 
  scale_y_continuous(labels = percent) +
  labs(title = str_wrap("% of Students able to support all expenses on stipend", 45)) 
gap <- stip_pov_gap_v_perc_support  %>%
  ggplot(aes(x = Year, y = `Gap in 2019 Constant $`), fill = fill_col) +
  labs(title = str_wrap("Gap between PhD living allowance and MBM (2019 constant $s)", 45)) + 
  geom_rect(xmin = 2018, xmax = 2021, ymax = 4100, ymin = 0, fill = "#DFECE0") +
  #geom_rect(xmin = 2021, xmax = 2022, ymax = 3900, ymin = 0, fill = "#FBF0C5") +
  geom_line(colour = fill_col) + geom_point(colour = fill_col) #+
  #lims(y = c(1400, 3600))
stip <- stip_pov_gap_v_perc_support %>% 
  ggplot(aes(x = Year, y = `PhD 2019`), fill = fill_col) +
  labs(title = "PhD living allowance (2019 constant $s)") +
  geom_rect(xmin = 2018, xmax = 2021, ymax = 30000, ymin = 0, fill = "#DFECE0") +
  #geom_rect(xmin = 2021, xmax = 2022, ymax = 30000, ymin = 0, fill = "#FBF0C5") +
  geom_line(colour = fill_col) + geom_point(colour = fill_col) 

patch_plot <- ((gap) *
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_text(),
        panel.spacing = unit(seq(0, 4),"cm")))/stip &
  theme(plot.title = element_text(hjust = 0, size = 12, vjust = 1),
        axis.title.y = element_blank(), 
        plot.margin = unit(c(0.2,0.5,0,0.5),"cm")) &
  scale_x_continuous(breaks = seq(2015,2021,1))
  xlab("Year preceding survey") 
ggsave("figures/report_iter/stipend_gap_MBM.png", patch_plot,
       width = 5, height = 5.4)
ggsave("figures/report_iter/stipend_gap_MBM.pdf", patch_plot, 
       width = 5, height = 5.4)


### Number for % of students that have PhD decision discouraged by $$
master_transfer_by_financial_discourage <- read_tsv("GRCDataAnalysis/Two-Page Report Graphics Iteration/master_transfer_by_financial_discourage.tsv") %>% 
  `colnames<-`(c("intend_transfer_phd", "Not Discouraged", "Discouraged"))
master_transfer_by_financial_discourage %>% 
  pivot_longer(c("Not Discouraged", "Discouraged"), 
               names_to = "Discouraged to transfer to PhD due to financial insecurity",
               values_to = "Respondent Count") %>%
  mutate(
    intend_transfer_phd = factor(case_when(
      intend_transfer_phd == "No" ~ "Do not intend to transfer",
      intend_transfer_phd == "Yes" ~ "Intend to transfer",
      TRUE ~ "Undecided"), 
      levels = c("Do not intend to transfer", "Undecided", "Intend to transfer"))) %>%
  group_by(`Discouraged to transfer to PhD due to financial insecurity`) %>%
  mutate(total_disc_categ = sum(`Respondent Count`), 
         `Percentage` = `Respondent Count`/total_disc_categ) %>%
  ggplot(aes(x = `Discouraged to transfer to PhD due to financial insecurity`, 
             y = `Percentage`, fill = intend_transfer_phd)) +
  labs(x = str_wrap("Discouraged to transfer to PhD due to financial insecurity", 45)) +
  geom_col() +
  geom_text(aes(label = paste0("n = ", total_disc_categ), y = 1.03), size = 5.5) +
  scale_fill_manual(values = pal[4:6]) +
  guides(fill = guide_legend(title = "Intent to transfer to PhD")) +
  theme(legend.position = "right")
ggsave("figures/report_iter/masters_transfer_by_financial_discourage.png",
       width = 8, height = 6)
ggsave("figures/report_iter/masters_transfer_by_financial_discourage.pdf",
       width = 8, height = 6)

responses_2021_22_cleaned %>% 
  filter(!grepl("Course",`What degree program are you in?`),
         `On average, how many hours a week do you typically work on research-related activities?` != 
           "N/A - It's my first year") %>%
  mutate(res_hrs = `On average, how many hours a week do you typically work on research-related activities?`,
         res_hrs_numeric = case_when(
           res_hrs == "Less than 11 hours" ~ (10-1)/2+1,
           res_hrs == "11-20 hours" ~ (20-11)/2+11,
           res_hrs == "21-30 hours" ~ (30-21)/2+21,
           res_hrs == "31-40 hours" ~ (40-31)/2+31,
           res_hrs == "41-50 hours" ~ (50-41)/2+41,
           res_hrs == "51-60 hours" ~ (60-51)/2+51,
           res_hrs == "61-70 hours" ~ (70-61)/2+61,
           res_hrs == "71-80 hours" ~ (80-71)/2+71,
           res_hrs == "More than 80 hours" ~ 80
  ),
  work_TA = 
    ifelse(as.numeric(`How many hours per week do you work as a teaching assistant?`) > 0, "Yes", "No"),
  work_side = ifelse(as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`) > 0, "Yes", "No"),
  either = ifelse(work_TA == "Yes" | work_side == "Yes", "Yes", "No")) %>% 
  group_by(work_side) %>% summarise(rs_hrs_m = mean(res_hrs_numeric),
                                    rs_hrs_m = median(res_hrs_numeric))
  

responses_2021_22_cleaned %>% 
  filter(!grepl("Course",`What degree program are you in?`)) %>%
  transmute(work_TA = 
              ifelse(as.numeric(`How many hours per week do you work as a teaching assistant?`) > 0, "Yes", "No"),
            work_side = ifelse(as.numeric(`How many hours per week do you work at your side job (excluding teaching assistantships)?`) > 0, "Yes", "No"),
            either = ifelse(work_TA == "Yes" | work_side == "Yes", "Yes", "No")) %>%
  count(either)
responses_2021_22_cleaned %>% 
  filter(!grepl("Course",`What degree program are you in?`)) %>%
  transmute(reason) %>%
  count(either)
    
127/(310+127) ##0.2906178
199/(238+199) ##0.455

  

