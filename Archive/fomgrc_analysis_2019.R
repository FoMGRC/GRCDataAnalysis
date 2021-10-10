# 2019-20 student finance survey analysis #

# Load libraries
library(ggplot2)
theme_set(theme_bw(base_size = 16))
library(dplyr)
library(ggthemes)
# Load survey data #
dat <- read_xlsx(path = "GRCDataAnalysis/survey_results/Faculty of Medicine Graduate Student Survey_2019_20 (Responses).xlsx", na = c("", "NA", "N/A"))

#add dept column
dat$dept <- dat$`What department are you in?`

# Remove duplicate answers
dat <- dat[-which(duplicated(x = dat[-1])),]

dat <- dat[-which(is.na(dat$dept)),]
# Add new dept name
dat$dept.short <- factor(dat$dept, labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "IMS"))


### Data cleaning ###
# Research location

levels(dat$res.loc)
table(dat$res.loc)
dat$res.loc <- as.character(dat$res.loc)
dat[grep("Baycrest|Rotman", x = dat$res.loc), 'res.loc'] <- "Rotman Research Institute (Baycrest)"
dat[grep("Sick|PGCRL", x = dat$res.loc), 'res.loc'] <- "Hospital for Sick Children/PGCRL"
dat[grep("Princess|mars|oicr", x = dat$res.loc, ignore.case = T), 'res.loc'] <- "MaRS (PMCRT/OICR)"
dat[grep("camh|addiction", x = dat$res.loc, ignore.case = T), 'res.loc'] <- "Centre for Addiction and Mental Health"
dat[grep("krembil", x = dat$res.loc, ignore.case = T), 'res.loc'] <- "Toronto Western Hospital"
dat[grep("blood|CBS", x = dat$res.loc, ignore.case = T), 'res.loc'] <- "Canadian Blood Services"
dat[grep("lunenfeld", x = dat$res.loc, ignore.case = T), 'res.loc'] <- "Mount Sinai Hospital"

dat$res.loc <- factor(dat$res.loc)
table(dat$res.loc)

# Awards applied for
# Cleaning fields with extra entries
dat$sch.apply <- gsub(pattern = ", ", x = dat$sch.apply, replacement = ";")
# dat$sch.apply <- gsub(pattern = "Vanier/CIHR", x = dat$sch.apply, replacement = "CIHR/NSERC Doctoral;Vanier")

dat$sch.apply <- strsplit(x = as.character(dat$sch.apply), split = ";") # Split and convert field into list

# Replace in list
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = ".*restra.*|.*retra.*", x = x, replacement = "SickKids Restracomp", ignore.case = T), how = "replace") 

# Awards won
dat$sch.won.type <- strsplit(x = as.character(dat$sch.won.type), split = ";") # Split and convert field into list
# Replace in list
dat$sch.won.type <- rapply(dat$sch.won.type, function(x) gsub(pattern = ".*restra.*|.*retra.*", x = x, replacement = "SickKids Restracomp", ignore.case = T), how = "replace")

#write cleaned data to file
write_delim(dat, "GRCDataAnalysis/survey_results/cleaned/GRC_Survey_Cleaned_2019-20.csv")

# Scholarship value
summary(dat$sch.tot.value)
# Top up value
summary(dat$sch.topup)

# Out of all the award money won, how much actually went into student's pockets?

# This may be skwewed by a few misreported top ups
sum(dat$sch.topup, na.rm = T) / sum(dat$sch.tot.value, na.rm = T)

# If we remove the person with 65k topup...?
with(dat[-200,], sum(sch.topup, na.rm = T) / sum(sch.tot.value, na.rm = T))
summary(dat[-200, 'sch.topup'])

# Now if we adapt our new top up rules
dat.new <- dat[-200,]
select(dat.new, sch.topup, sch.tot.value)

dat.new$sch.topup.proposed <- with(dat.new, ifelse(sch.tot.value <= 4000, sch.tot.value, ifelse(sch.tot.value <= 16000, 4000, pmin(7500, 0.25 * sch.tot.value))))

dat.new$sch.topup.2018.2019.rules <- with(dat.new, ifelse(sch.tot.value <= 2000, sch.tot.value, ifelse(sch.tot.value <= 15000, 2000, 4000)))


select(dat.new, sch.tot.value, sch.topup, sch.topup.2018.2019.rules, sch.topup.proposed)

# Summary statistics
with(dat.new, sum(sch.topup, na.rm = T) / sum(sch.tot.value, na.rm = T))
with(dat.new, sum(sch.topup.2018.2019.rules, na.rm = T) / sum(sch.tot.value, na.rm = T))
with(dat.new, sum(sch.topup.proposed, na.rm = T) / sum(sch.tot.value, na.rm = T))



summary(dat$stip.sup)

# Simplify degree column
dat$degree.simple <- factor(ifelse(grepl(x = dat$degree, pattern = "phd", ignore.case = T, perl = T), "PhD", "MSc"))

# Adjust support
# dat[dat$ext.inc.source != "I do not receive additional support", 'stip.sup'] <- "No"

# Sam's plot
a <- ggplot(dat, aes(as.factor(1))) + geom_bar(aes(fill = stip.sup), position = "fill") + theme_classic() + theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "right", aspect.ratio = 1) + scale_fill_economist() + labs(title = "Students living off only stipend", x = NULL, y = "Proportion", fill = NULL) + scale_y_continuous(expand = c(0,0))

dat$ext.support.value <- factor(dat$ext.support.value, levels = rev(c("Up to $1,000", "$1,000 - $5,000", "$5,000 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+")))

b<- ggplot(filter(dat, !is.na(ext.support.value)), aes(as.factor(1))) + geom_bar(aes(fill = ext.support.value), position = "fill") + theme_classic() + theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "right", aspect.ratio = 1) + scale_fill_wsj() + labs(title = "Additional income support", x = NULL, y = "Proportion", fill = NULL) + scale_y_continuous(expand = c(0,0), position = "right")

library(ggpubr)
ab <- ggarrange(a, b, align = "hv")
ab
ggsave(ab, filename = "stipend.support.plot.pdf", height = 8, width = 10)

# Travel time histogram
transpo.time.hist <- gghistogram(data = dat, x = "transport.time", fill = "lightgrey", add = "mean") + labs(x = "Average commute (mins)", y = "Count") + theme(aspect.ratio = 1)
ggsave(transpo.time.hist, filename = "transpo.time.hist.pdf", height = 4, width = 4)

# Hours worked histogram
hrs.work.hist <- gghistogram(data = filter(dat, ext.emp.hours != 0), x = "ext.emp.hours", fill = "lightgrey", add = "mean") + labs(x = "Average hours per week", y = "Count") + theme(aspect.ratio = 1)
hrs.work.hist
ggsave(hrs.work.hist, filename = "hrs.work.hist.pdf", height = 4, width = 4)

# Other expenses
# dat[dat$expenses == "I can`t afford to spend money on this", 'expenses'] <- "NA"
# dat$expenses <- as.numeric(as.character(dat$expenses))

### Basic survey demographics ###
# Total responses
n_responses <- nrow(dat)
# Responses per stream
ggplot(dat, aes(x = degree.simple, y = ..count..)) + geom_bar() + labs(x = "Stream", y = "Count")
summary(dat$degree.simple)
# Per department
ggplot(dat, aes(x = dept, y = ..count..)) + geom_bar(aes(fill = dept)) + labs(x = NULL, y = "No. of responses") + coord_flip() + theme(legend.position = "none")
# Per department per stream
no.per.dept <- ggplot(dat, aes(x = dept.short, y = ..count..)) + geom_bar(aes(fill = degree.simple), position = "dodge") + labs(x = NULL, y = "No. of responses", fill = NULL) + coord_flip() + scale_fill_gdocs() + scale_y_continuous(expand = c(0,0)) + theme_classic(base_size = 18) + theme(legend.position = c(0.9, 0.9)) + theme(aspect.ratio = 1)
no.per.dept
ggsave(plot = no.per.dept, filename = "no.per.dept.pdf", height = 4, width = 4)
# Per year of research
ggplot(dat, aes(x = year, y = ..count..)) + geom_bar() + labs(x = "Year of study", y = "Count")
# Per year of research per stream
no.per.year <- ggplot(dat, aes(x = year, y = ..count..)) + geom_bar(aes(fill = degree.simple), width = 0.5) + labs(x = "Year of study", y = "No. of responses", fill = NULL) + scale_fill_gdocs() + theme_classic(base_size = 18) + scale_y_continuous(expand = c(0,0)) + theme(legend.position = c(0.8, 0.8)) + coord_flip() + theme(apsect.ratio = 1)
no.per.year
ggsave(plot = no.per.year, filename = "no.per.year.pdf", height = 4, width = 4)

# Per gender
ggplot(dat, aes(x = gender, y = ..count..)) + geom_bar() + labs(x = "Gender", y = "Count")

# Age distribution
ggplot(dat, aes(x = age)) + geom_histogram(bins = 20) + labs(x = "Age", y = "Count") + theme_classic()
ggplot(dat, aes(x = age)) + geom_histogram(bins = 20) + labs(x = "Age", y = "Count") + facet_grid(.~stream)

summary(dat$age)

# Scholarship value vs topup received
summary(dat$sch.tot.value)
sum(dat$sch.tot.value, na.rm = TRUE)
sum(dat$sch.topup, na.rm = TRUE)

prop.table(table(dat$stip.sup))
prop.table(table(dat$ext.support.value))

# External work
dat[dat$ext.emp.hours == 0 | is.na(dat$ext.emp.hours), 'ext.emp.hours'] <- NA
summary(dat$ext.emp.hours)



# Rent
summary(dat$housing.cost)
# Filter out those who do not pay
summary(filter(dat, housing.cost != 0)$housing.cost)

prop.table(table(dat$aware.hssa))

with(dat, table(living.situation, stip.sup))

with(dat, table(stip.sup))


1 - (8 / 47)

16 / 47

54 / 213

with(dat, round(prop.table(table(living.situation, stip.sup))), 2)


# Extra expenses
