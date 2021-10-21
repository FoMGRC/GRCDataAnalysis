## need to modify to reduce the number of dependencies
responses_2021_22_cleaned <- read_delim("survey_results/cleaned/GRC_Survey_Cleaned_2021-22.tsv", 
                                        delim = "\t")
## filter out RSI
responses_2021_22_cleaned <- responses_2021_22_cleaned %>% 
  filter(dept.short != "RSI")

dir.create("figures/distributions")

## year
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `What is your year of study (as of September 2021)?`),
                 stat = "count")
ggsave("figures/distributions/year.png")

## degree program
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `What degree program are you in?`), stat = "count")
ggsave("figures/distributions/deg_prog.png")


## ages
mean_age <- mean(responses_2021_22_cleaned$Age)
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = Age), stat = "count") + geom_vline(xintercept = mean_age)
ggsave("figures/distributions/age.png")

## department
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `What department are you in?`), stat = "count") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0, size = 8))
ggsave("figures/distributions/department.png")

## area of research
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(y = `What is your area of research?`), stat = "count") +
  theme(axis.text.y = element_text(size = 8))
ggsave("figures/distributions/research_area.png")

## gender
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `Gender Identity`), stat = "count") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0, size = 8))
ggsave("figures/distributions/gender.png")

## race/ethnic origins
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(y = `What racial and ethnic origins do you identify with?`), stat = "count") +
  theme(axis.text.y = element_text(size = 5)) 
ggsave("figures/distributions/race.png", width = 10)

responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(y = `What racial and ethnic origins do you identify with?`,
                     fill = `Do you identify as a racialized person?`), stat = "count") +
  theme(axis.text.y = element_text(size = 5)) 
ggsave("figures/distributions/race_racialized.png", width = 20)

## international/domestic
responses_2021_22_cleaned %>% ggplot() +
  geom_histogram(aes(x = `Are you an international or domestic student?`), stat = "count") 
ggsave("figures/distributions/international.png")
