## 2020-21 student finance survey analysis ###########################################################

library(readxl)
library(data.table)

## HELPER #########################################################################################

replace_entry <- function(dat, column, search, replace, ignore_case = FALSE){

    dat[grep(search, x = dat[[column]], ignore.case = ignore_case), column] <- replace

    return(dat)
}

replace_in_list <- function(ll, search, replace, ignore_case = FALSE){

    ll <- sapply(ll, function(x) ifelse(grepl(search, x, ignore.case = ignore_case), replace, x))

    # ll <- rapply(ll, function(x) gsub(pattern = search, x = x, replacement = replace, ignore.case = T), how = "replace")

    return(ll)
}


## DATA ###########################################################################################
# Load survey data #
file <- "~/Documents/grc_data/Raw/Faculty of Medicine Graduate Student Survey_2021_22  (Responses).xlsx"
# only read first sheet (doesn't really work)
dat <- read_excel(file, na = c("", "NA", "N/A"), sheet = 1)
print(dim(dat))

# Stop if duplicated answers exist
stopifnot(!any(duplicated(dat)))
# dat <- dat[-which(duplicated(x = dat[-1])),]
# dat <- dat[-which(is.na(dat$dept)),]

# remove those with no timestamp
dat <- dat[!is.na(dat$Timestamp), ]



# Stop if duplicated answers exist
stopifnot(!any(duplicated(dat))) 
# dat <- dat[-which(duplicated(x = dat[-1])),]

# dat <- dat[-which(is.na(dat$dept)),]
# Add new dept name



dat$dept.short <- factor(dat[["What department are you in?"]], labels = c("BCHM", "Imm.", "LMP", "MBP", "MoGen", "NutriSci", "PharmTox", "Phys", "RSI", "IMS"))




### Data cleaning ###
# Research location

levels(dat[["What is your primary research location?"]])
table(dat[["What is your primary research location?"]])



dat[grep("Baycrest|Rotman|RRI", x = dat[["What is your primary research location?"]]), "What is your primary research location?"] <- "Rotman Research Institute (Baycrest)"
dat[grep("Sick|PGCRL", x = dat[["What is your primary research location?"]]), "What is your primary research location?"] <- "Hospital for Sick Children/PGCRL"
dat[grep("Princess|mars|oicr|Ontario", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "MaRS (PMCRT/OICR)"
dat[grep("camh|addiction", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Centre for Addiction and Mental Health (CAMH)"
dat[grep("krembil", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto Western Hospital/Krembil"
dat[grep("blood|CBS", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Canadian Blood Services"
dat[grep("lunenfeld", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Mount Sinai Hospital/LTRI"
dat[grep("500|Rehabilitation Sciences|RSI", x = dat[["What is your primary research location?"]], ignore.case = F), "What is your primary research location?"] <- "Rehabilitation Sciences Institute"
dat[grep("General|TGH", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto General Hospital"
dat[grep("Toronto Rehab|TRI|Rumsey", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Toronto Rehab (Downtown)"
dat[grep("Leslie Dan", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Leslie Dan Faculty of Pharmacy"
dat[grep("Holland", x = dat[["What is your primary research location?"]], ignore.case = T), "What is your primary research location?"] <- "Holland Bloorview"


dat[["What is your primary research location?"]] <- factor(dat[["What is your primary research location?"]])
table(dat[["What is your primary research location?"]])




dat$sch.apply <- dat[["What graduate award(s) did you apply for to be held in the 2019-2020 academic year?"]]

# Awards applied for
# Cleaning fields with extra entries
dat$sch.apply <- gsub(pattern = ", | & R", 
                      x = dat$sch.apply,
                      replacement = ";")
# dat$sch.apply <- gsub(pattern = "Vanier/CIHR", x = dat$sch.apply, replacement = "CIHR/NSERC Doctoral;Vanier")

dat$sch.apply <- strsplit(x = as.character(dat$sch.apply), split = ";") # Split and convert field into list

# Replace in list
dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Alzheimer Society.*", x = x, replacement = "Alzheimer Society of Canada", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Banting & Best Diabetes.*|BBDC.*", x = x, replacement = "Banting & Best Diabetes Centre", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "CBS.*", x = x, replacement = "CBS", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "FRQS.*", x = x, replacement = "FRQS Doctoral fellowship", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "OHTN Student.*", x = x, replacement = "OHTN Student Leader Award", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Ontario Respiratory.*", x = x, replacement = "Ontario Respiratory Society Fellowship", ignore.case = T), how = "replace") 

dat$sch.apply <- rapply(dat$sch.apply, function(x) gsub(pattern = "Ted Rogers.*", x = x, replacement = "Ted Rogers Centre for Heart Research Education Fund", ignore.case = T), how = "replace") 

table(unlist(dat$sch.apply))

dat[["What graduate award(s) did you apply for to be held in the 2019-2020 academic year?"]] <- sapply(dat$sch.apply, paste, collapse=", ")
write.csv(dat[,1:98], file="GRC_Survey_Cleaned.csv")

scholarships.won.columns <- grep(x=colnames(dat), pattern="What type of scholarship(s) did you receive during the 2019/2020 academic year?", fixed=TRUE)


smallDat <- dat[,scholarships.won.columns]
colnames(smallDat) <- gsub(colnames(smallDat), pat="What type of scholarship\\(s\\) did you receive during the 2019/2020 academic year\\? \\[|\\]", rep="")

toPlot <- reshape2::toPlot(as.data.frame(smallDat))

ggplot2(smallDat)




dat$sch.tot.value <- dat[["What was the total value ($) of all scholarships you received during the 2019/2020 academic year?"]]
dat$sch.topup <- dat[["What was the monetary value beyond your base stipend that you received as a result of winning any awards (i.e. \"top-ups\") for the 2019/2020 academic year?"]]

### Making some appendix plots

dat$inc.change <- as.numeric(dat[["How much has your monthly income ($) changed due to the COVID-19 pandemic?"]]) 
png("covid_income_change.png", height=4, width=6, res=300, units="in")
ggplot(dat, aes(inc.change)) + geom_histogram() + xlim(-2000,2000) + xlab("Monthly income change in Dollars") + ggtitle("How has your income been affected\nby the COVID-19 pandemic?")
dev.off()


dat$exp.change <- as.numeric(dat[["How much have your monthly expenses ($) changed due to the COVID-19 pandemic?"]]) 
png("covid_expense_change.png", height=4, width=6, res=300, units="in")
ggplot(dat, aes(exp.change)) + geom_histogram() + xlim(-2000,2000) + xlab("Monthly expense change in Dollars") + ggtitle("How have your expenses been affected\nby the COVID-19 pandemic?")
dev.off()


png("topup_hist.png", height=4, width=6, res=300, units="in")
ggplot(dat, aes(sch.topup)) + geom_histogram(binwidth=1000) + xlab("Dollars") + ggtitle("How much did you\nreceive above your base stipend last year?")
dev.off()



### Exclusing RSI

dat <- dat[dat$dept.short != "RSI",]


### Continuing



# Scholarship value
summary(dat$sch.tot.value)
# Top up value
summary(dat$sch.topup)

# Out of all the award money won, how much actually went into student's pockets?

# This may be skwewed by a few misreported top ups
sum(dat$sch.topup, na.rm = T) / sum(dat$sch.tot.value, na.rm = T)

# If we remove the person with 65k topup...?
# with(dat[-200,], sum(sch.topup, na.rm = T) / sum(sch.tot.value, na.rm = T))
# summary(dat[-200, 'sch.topup'])

# # Now if we adapt our new top up rules
# dat.new <- dat[-200,]
# select(dat.new, sch.topup, sch.tot.value)

# dat.new$sch.topup.proposed <- with(dat.new, ifelse(sch.tot.value <= 4000, sch.tot.value, ifelse(sch.tot.value <= 16000, 4000, pmin(7500, 0.25 * sch.tot.value))))

# dat.new$sch.topup.2018.2019.rules <- with(dat.new, ifelse(sch.tot.value <= 2000, sch.tot.value, ifelse(sch.tot.value <= 15000, 2000, 4000)))


# select(dat.new, sch.tot.value, sch.topup, sch.topup.2018.2019.rules, sch.topup.proposed)

# # Summary statistics
# with(dat.new, sum(sch.topup, na.rm = T) / sum(sch.tot.value, na.rm = T))
# with(dat.new, sum(sch.topup.2018.2019.rules, na.rm = T) / sum(sch.tot.value, na.rm = T))
# with(dat.new, sum(sch.topup.proposed, na.rm = T) / sum(sch.tot.value, na.rm = T))


### Proper award retention rate calculation:


dat$stip.sup <- dat[["Can you support all of your day-to-day living expenses exclusively from your graduate funding (i.e stipend and awards/top-ups)?"]]

dat$stip.sup <- factor(dat$stip.sup, levels=c("No", "Yes"))

summary(dat$stip.sup)

dat$degree <- dat[["What degree program are you in?"]]

# Simplify degree column
dat$degree.simple <- factor(ifelse(grepl(x = dat$degree, pattern = "phd", ignore.case = T, perl = T), "PhD", "MSc"))

# Adjust support
# dat[dat$ext.inc.source != "I do not receive additional support", 'stip.sup'] <- "No"

# Sam's plot
a <- ggplot(dat, aes(as.factor(1))) + geom_bar(aes(fill = stip.sup), position = "fill") + theme_classic() + theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "right", aspect.ratio = 1) + scale_fill_economist() + labs(title = "Students living off only stipend", x = NULL, y = "Proportion", fill = NULL) + scale_y_continuous(expand = c(0,0))

dat$ext.support.value <- dat[["How much additional monetary support would you need to meet your day-to-day expenses each year (above your graduate living allowance)?"]]

dat$ext.support.value <- factor(dat$ext.support.value, levels = rev(c("0.0", "$1 - $1,000", "$1,000 - $5,000", "$5,000 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+")))

b <- ggplot(dat[dat$ext.support.value != "0.0",], aes(as.factor(1))) + geom_bar(aes(fill = ext.support.value), position = "fill") + theme_classic() + theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "right", aspect.ratio = 1) + scale_fill_wsj() + labs(title = "Additional income support", x = NULL, y = "Proportion", fill = NULL) + scale_y_continuous(expand = c(0,0), position = "right")

library(ggpubr)
ab <- ggarrange(a, b, align = "hv")
ab
ggsave(ab, filename = "stipend.support.plot.pdf", height = 8, width = 10)

# dat$transport.time <- 

# # Travel time histogram
# transpo.time.hist <- gghistogram(data = dat, x = "transport.time", fill = "lightgrey", add = "mean") + labs(x = "Average commute (mins)", y = "Count") + theme(aspect.ratio = 1)
# ggsave(transpo.time.hist, filename = "transpo.time.hist.pdf", height = 4, width = 4)


dat$ext.emp.hours <- rowSums(dat[,grep(x=colnames(dat), pattern =  "How many hours per week")])

# Hours worked histogram
hrs.work.hist <- ggplot(data = dat[dat$ext.emp.hours != 0 & dat$ext.emp.hours <= 50,], aes(x = ext.emp.hours)) + 
    geom_histogram(fill = "lightgrey", binwidth = 2.5, boundary=0) + theme_classic() + 
    labs(x = "Hours/Week working outside lab", y = "# of students") + 
    theme(aspect.ratio = 1) + scale_x_continuous(breaks=seq(0,100,10)) + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA)) + 
    geom_vline(linetype="dashed", xintercept=mean(dat[dat$ext.emp.hours != 0 & dat$ext.emp.hours <= 50,][["ext.emp.hours"]], na.rm=T))
hrs.work.hist
ggsave(hrs.work.hist, filename = "hrs.work.hist.pdf", height = 4, width = 4)
ggsave(hrs.work.hist, filename = "hrs.work.hist.png", height = 4, width = 4, dpi=600)


# Hours worked histogram
hrs.work.dens <- ggplot(data = dat[dat$ext.emp.hours != 0 & dat$ext.emp.hours <= 50,], aes(x = ext.emp.hours)) + 
    geom_density(fill = "lightgrey", binwidth = 2.5, boundary=0) + theme_classic() + 
    labs(x = "Hours/Week working outside lab", y = "# of students") + 
    theme(aspect.ratio = 1) + scale_x_continuous(breaks=seq(0,100,10)) + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA)) + 
    geom_vline(linetype="dashed", xintercept=mean(dat[dat$ext.emp.hours != 0 & dat$ext.emp.hours <= 50,][["ext.emp.hours"]], na.rm=T))
hrs.work.dens
ggsave(hrs.work.dens, filename = "hrs.work.dens.pdf", height = 4, width = 4)
ggsave(hrs.work.dens, filename = "hrs.work.dens.png", height = 4, width = 4, dpi=600)


png("hrs.work.hist.png", height = 4, width = 4, units="in", res = 600,bg = "transparent")
hrs.work.hist
dev.off()
# Other expenses
# dat[dat$expenses == "I can`t afford to spend money on this", 'expenses'] <- "NA"
# dat$expenses <- as.numeric(as.character(dat$expenses))

dat$year <- factor(dat[["What is your year of study (as of September 2020)?"]])

### Basic survey demographics ###
# Total responses
n_responses <- nrow(dat)
# Responses per stream
ggplot(dat, aes(x = degree.simple, y = ..count..)) + geom_bar() + labs(x = "Stream", y = "Count")
summary(dat$degree.simple)
# Per department
ggplot(dat, aes(x = dept.short, y = ..count..)) + geom_bar(aes(fill = dept.short)) + labs(x = NULL, y = "No. of responses") + coord_flip() + theme(legend.position = "none")
# Per department per stream
no.per.dept <- ggplot(dat, aes(x = dept.short, y = ..count..)) + geom_bar(aes(fill = degree.simple), position = "dodge") + labs(x = NULL, y = "No. of responses", fill = NULL) + coord_flip() + scale_fill_gdocs() + scale_y_continuous(expand = c(0,0)) + theme_classic(base_size = 18) + theme(legend.position = c(0.9, 0.9)) + theme(aspect.ratio = 1)
no.per.dept
ggsave(plot = no.per.dept, filename = "no.per.dept.pdf", height = 4, width = 4)
# Per year of research
ggplot(dat, aes(x = year, y = ..count..)) + geom_bar() + labs(x = "Year of study", y = "Count")
# Per year of research per stream
no.per.year <- ggplot(dat, aes(x = year, y = ..count..)) + geom_bar(aes(fill = degree.simple), width = 0.5) + labs(x = "Year of study", y = "No. of responses", fill = NULL) + scale_fill_gdocs() + theme_classic(base_size = 18) + scale_y_continuous(expand = c(0,0)) + theme(legend.position = c(0.8, 0.8)) + coord_flip() #+ theme(apsect.ratio = 1)
no.per.year
ggsave(plot = no.per.year, filename = "no.per.year.pdf", height = 4, width = 4)


## Basic survey demographics as percent of departments

dept.enrol <- read.csv("dept_enrolment.csv")

dept.enrol <- dept.enrol[!dept.enrol[,1] %in% c("RSI", "Grand Total"),]
colnames(dept.enrol)[3] <- "MSc"

## By department first
dat.dt <- data.table(dat)
by.dept.data <- dat.dt[,.N, .(dept.short, degree.simple)]



by.dept.enrol <- melt(dept.enrol[,1:3])

colnames(by.dept.enrol) <- c("dept.short", "degree.simple", "Total Students")

toPlot <- merge(by.dept.data, by.dept.enrol, by=c("dept.short", "degree.simple"))

toPlot[,`Percent of Department` := N/`Total Students`*100]

no.per.dept <- ggplot(toPlot, aes(x = dept.short, y = `Percent of Department`)) + geom_bar(stat = "identity", aes(fill = degree.simple), position = "dodge") + labs(x = NULL, y = "Percent of Students", fill = NULL) + coord_flip() + scale_fill_gdocs() + scale_y_continuous(limits=c(0,75)) + theme_classic(base_size = 18) + theme(legend.position = c(0.9, 0.9)) + theme(aspect.ratio = 1)
no.per.dept
ggsave(plot = no.per.dept, filename = "no.per.dept.pdf", height = 4, width = 4)
ggsave(plot = no.per.dept, filename = "no.per.dept.png", height = 4, width = 4, dpi=600)


by.year.data <- dat.dt[,.N, .(degree.simple, year)]

toPlot <- merge(by.dept.data, by.dept.enrol, by=c("degree.simple"))



# dat$gender <- 

# Per gender
ggplot(dat, aes(x = `Identity:`, y = ..count..)) + geom_bar() + labs(x = "Identity", y = "Count") + 
        theme_classic() + theme(axis.text.x = element_text(hjust=1, angle=45))

# Age distribution
ggplot(dat, aes(x = `Age:`)) + geom_histogram(bins = 20) + labs(x = "Age", y = "Count") + theme_classic()
ggplot(dat, aes(x = `Age:`)) + geom_histogram(bins = 20) + labs(x = "Age", y = "Count") + facet_grid(.~degree.simple)

summary(dat$`Age:`)

# Scholarship value vs topup received
summary(dat$sch.tot.value)
sum(dat$sch.tot.value, na.rm = TRUE)
sum(dat$sch.topup, na.rm = TRUE)

prop.table(table(dat$stip.sup))
prop.table(table(dat$ext.support.value))

# External work
dat[dat$ext.emp.hours == 0 | is.na(dat$ext.emp.hours), 'ext.emp.hours'] <- NA
summary(dat$ext.emp.hours)


dat$housing.cost <- dat[["Approximately what are your individual monthly housing expenses (i.e. rent, mortgage, maintenance fees, utilities, etc.)?"]]

# Rent
summary(dat$housing.cost)
# Filter out those who do not pay
summary(dat$housing.cost[dat$housing.cost != 0])

dat$aware.hssa <- dat[["Are you aware of the Harmonized Base Funding Agreement (HBFA) and how it impacts your stipend funding? *RSI not included in the HBFA"]]

prop.table(table(dat$aware.hssa))

dat$living.situation.split <- strsplit(x = as.character(dat[["Select the option(s) that best describe your current living situation?"]]), split = ", ")

dat$struggling.perm.housing <- sapply(dat$living.situation.split, function(x) return("Struggling to find permanent housing" %in% x))
prop.table(table(dat$struggling.perm.housing))

dat$food.insc <- dat[["Do you experience any level of food insecurity?"]]
dat$housing.insc <- dat[["Do you experience any level of housing insecurity?"]]

prop.table(table(dat$food.insc))

prop.table(table(dat$housing.insc))


## Think about what to colour these by?
ggplot(dat, aes(x = food.insc, y = ..count..)) + geom_bar() + labs(x = NULL, y = "No. of responses") + theme(legend.position = "none")



with(dat, table(living.situation, stip.sup))

with(dat, table(stip.sup))


with(dat, round(prop.table(table(living.situation, stip.sup))), 2)


### Looking at specific plots:


dat$identify.simple <- dat[["Identity:"]]

dat$identify.simple[!dat$identify.simple %in% c("Man", "Woman")] <- "Other"


### Students support on stipend by gender

toPlot <- as.data.frame(dat[,c("identify.simple", "stip.sup")])

toPlot <- data.frame(prop.table(table(toPlot), 1))
toPlot[,1] <- factor(toPlot[,1], c("Man", "Woman", "Other"))

toPlot$total <- paste("n =", as.numeric(table(dat[["identify.simple"]])[as.character(toPlot$identify.simple)]))

pdf("StudentStipendSupportByGender.pdf", height = 3.5, width = 3.5)
ggplot(toPlot, aes(x=identify.simple, fill=stip.sup, y=Freq, label=total)) + geom_bar(stat="identity") + geom_text(y=.97) + scale_fill_manual(values=c("grey", "black")) + theme_classic() + xlab("Gender Identity") + ylab("Percentage") +  labs(fill = "Can support self\non stipend alone") + theme(legend.position = "top")
dev.off()


png("StudentStipendSupportByGender.png", height = 3, width = 3, units="in", res=600, bg="transparent")
ggplot(toPlot[toPlot$stip.sup=="Yes",], aes(x=identify.simple, y=Freq*100, label=round(Freq*100, 1))) + geom_bar(stat="identity", width=0.8, fill="black") + 
theme_classic() + ylim(0,31) + geom_text(nudge_y = 1.5, color="black", fontface="bold") + 
xlab("Gender Identity") + ylab("Percentage") +  ggtitle("Students able to support\n themselves on stipend alone") + 
theme(legend.position = "top",plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color="gray"), legend.background = element_rect(fill = "transparent", colour = NA), panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()


### Students support on stipend by gender using self sufficiency

dat$supportSelfByMoney <- ifelse(!rowSums(dat[,grep(x=colnames(dat), pat="seek", v=T)] != "0.0")>0, "Yes", "No")

toPlot <- as.data.frame(dat[,c("identify.simple", "supportSelfByMoney")])

toPlot <- data.frame(prop.table(table(toPlot), 1))
toPlot[,1] <- factor(toPlot[,1], c("Man", "Woman", "Other"))

toPlot$total <- paste("n =", as.numeric(table(dat[["identify.simple"]])[as.character(toPlot$identify.simple)]))

pdf("StudentSeekMoneyByGender.pdf", height = 3.5, width = 3.5)
ggplot(toPlot[toPlot$supportSelfByMoney=="Yes",], aes(x=identify.simple, fill=supportSelfByMoney, y=Freq, label=total)) + geom_bar(stat="identity") + geom_text(y=.97) + scale_fill_manual(values=c("grey", "black")) + theme_classic() + xlab("Gender Identity") + ylab("Percentage") +  labs(fill = "Can support self\non stipend alone") + theme(legend.position = "top")
dev.off()


png("StudentSeekMoneyByGender.png", height = 3, width = 3, units="in", res=600, bg="transparent")
ggplot(toPlot[toPlot$supportSelfByMoney=="Yes",], aes(x=identify.simple, y=Freq*100, label=round(Freq*100, 1))) + geom_bar(stat="identity", width=0.8, fill="black") + 
theme_classic() + ylim(0,15) + geom_text(nudge_y = 1, color="black", fontface="bold") + 
xlab("Gender Identity") + ylab("Percentage") +  ggtitle("Students able to support\n themselves on stipend alone") + 
theme(legend.position = "top",plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_line(color="gray"), legend.background = element_rect(fill = "transparent", colour = NA), panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()



#### Support Self by Racialized

dat$racialized <- dat[["Do you identify as a racialized person?"]]

dat$racialized <- factor(dat$racialized, levels=c("Yes", "No"))



toPlot <- as.data.frame(dat[,c("racialized", "stip.sup")])

toPlot <- data.frame(prop.table(table(toPlot), 1))
toPlot[,1] <- factor(toPlot[,1], c("Yes", "No"))

toPlot$total <- paste("n =", as.numeric(table(dat[["racialized"]])[as.character(toPlot$racialized)]))

pdf("StudentStipendSupportByRace.pdf", height = 3.5, width = 3.5)
ggplot(toPlot, aes(x=racialized, fill=stip.sup, y=Freq, label=total)) + 
geom_bar(stat="identity") + geom_text(y=.97) + 
scale_fill_manual(values=c("grey", "black")) + theme_classic() + 
xlab("Identified as Racialized") + ylab("Percentage") +  labs(fill = "Can support self\non stipend alone") + theme(legend.position = "top")
dev.off()




### Student support on stipend by living with family

dat$Liv.with.family <- sapply(dat$living.situation.split, function(x) "With family/relatives" %in% x)

toPlot <- as.data.frame(dat[,c("Liv.with.family", "stip.sup")])
toPlot <- data.frame(prop.table(table(toPlot), 1))

## TODO: finish once clarified


### Housing insecurity by racialized 


dat$housing.insc <- gsub("^Marginal.*", "Marginal", x=dat$housing.insc)
dat$housing.insc <- gsub("^Moderate.*", "Moderate", x=dat$housing.insc)
dat$housing.insc <- gsub("^Severe.*", "Severe", x=dat$housing.insc)



toPlot <- dat[,c("racialized", "housing.insc")]


toPlot <- data.frame(prop.table(table(toPlot), 1))

toPlot$housing.insc <- factor(toPlot$housing.insc, levels=c("No", "Marginal", "Moderate", "Severe"))

toPlot$total <- as.data.frame(table(dat[,c("racialized", "housing.insc")]))$Freq

pdf("HousingInsqByRace.pdf", height = 5, width = 5)
ggplot(toPlot, aes(x=racialized, y=Freq, fill=housing.insc, label=total)) + 
        geom_bar(stat="identity", position = "dodge") + 
        scale_fill_manual(values=c('#fee5d9','#fcae91','#fb6a4a','#cb181d'))  + geom_text(position=position_dodge(.9), mapping=aes(y=Freq+0.015)) +
        theme_classic() + xlab("Identified as Racialized") + ylab("Percentage") +  labs(fill = "Level of Housing Insecurity")
dev.off()


## Binarize 

dat$hous.inc.binary <- ifelse(dat$housing.insc != "No", "Yes", "No")

toPlot <- dat[,c("racialized", "hous.inc.binary")]


toPlot <- data.frame(prop.table(table(toPlot), 1))

toPlot$total <- as.data.frame(table(dat[,c("racialized")])[as.character(toPlot$racialized)])$Freq


pdf("HousingInsqBinaryByRace.pdf", height = 5, width = 5)
ggplot(toPlot, aes(x=racialized, y=Freq, fill=hous.inc.binary, label=total)) + 
        geom_bar(stat="identity")  + 
        scale_fill_manual(values=c('gray', 'black'))  +
        theme_classic() + theme(legend.position="top") + xlab("Identified as Racialized") + ylab("Percentage") +  labs(fill = "Experience Housing Insecurity")
dev.off()


toPlotBinHousing <- toPlot

### Food insecurity by racialized


dat$food.insc <- gsub("^Marginal.*", "Marginal", x=dat$food.insc)
dat$food.insc <- gsub("^Moderate.*", "Moderate", x=dat$food.insc)
dat$food.insc <- gsub("^Severe.*", "Severe", x=dat$food.insc)


toPlot <- dat[,c("racialized", "food.insc")]

toPlot <- data.frame(prop.table(table(toPlot), 1))

toPlot$food.insc <- factor(toPlot$food.insc, levels=c("No", "Marginal", "Moderate", "Severe"))

toPlot$total <- as.data.frame(table(dat[,c("racialized", "food.insc")]))$Freq

pdf("FoodInsqByRace.pdf", height = 5, width = 5)
ggplot(toPlot, aes(x=racialized, y=Freq, fill=food.insc, label=total)) + 
        geom_bar(stat="identity", position = "dodge") + 
        scale_fill_manual(values=c('#fee5d9','#fcae91','#fb6a4a','#cb181d'))  + geom_text(position=position_dodge(.9), mapping=aes(y=Freq+0.015)) +
        theme_classic() + xlab("Identified as Racialized") + ylab("Percentage") +  labs(fill = "Level of Food Insecurity")
dev.off()


dat$Food.inc.binary <- ifelse(dat$food.insc != "No", "Yes", "No")

toPlot <- dat[,c("racialized", "Food.inc.binary")]


toPlot <- data.frame(prop.table(table(toPlot), 1))

toPlot$total <- as.data.frame(table(dat[,c("racialized")])[as.character(toPlot$racialized)])$Freq


pdf("FoodInsqBinaryByRace.pdf", height = 5, width = 5)
ggplot(toPlot, aes(x=racialized, y=Freq, fill=Food.inc.binary, label=total)) + 
        geom_bar(stat="identity")  + 
        scale_fill_manual(values=c('gray', 'black'))  +
        theme_classic() + theme(legend.position="top") + xlab("Identified as Racialized") + ylab("Percentage") +  labs(fill = "Experience Food Insecurity")
dev.off()

toPlot$Var <- "Food"
toPlotBinHousing$Var <- "Housing"

colnames(toPlot)[[2]] <- "Insecurity"

colnames(toPlotBinHousing)[[2]] <- "Insecurity"

toPlotBoth <- rbind(toPlot, toPlotBinHousing)

png("BothInsByRace.png", height = 3, width = 3, units="in", res=600, bg="transparent")
ggplot(toPlotBoth[toPlotBoth$Insecurity=="Yes",], aes(x=racialized, y=Freq*100, label=round(Freq*100,1))) + 
        geom_bar(stat="identity", fill = "black") + geom_text(nudge_y = 2, color="black", fontface="bold") + facet_grid(~Var) + 
        scale_fill_manual(values=c('gray', 'black'))   + 
        xlab("Identified as Racialized") + ylab("Percentage") +  ggtitle("Students Experiencing\n Food or Housing Insecurity") +
        theme_classic() + theme(legend.position="top", plot.title = element_text(hjust = 0.5), 
                                panel.grid.major.y = element_line(color="gray"),
                                legend.background = element_rect(fill = "transparent", colour = NA), 
                                panel.background = element_rect(fill = "transparent", colour = NA), 
                                plot.background = element_rect(fill = "transparent", colour = NA), 
                                strip.background = element_rect(fill="transparent", colour=NA),
                                strip.text.x = element_text(size=14))
dev.off()


#### Awards held split by gender and racialization
library(ggbeeswarm)

toPlot <- dat[,c("sch.tot.value", "racialized", "identify.simple")]

toPlot$identify.simple <- factor(toPlot$identify.simple, levels=c("Man", "Woman", "Other"))


# toPlot[is.na(toPlot$sch.tot.value),"sch.tot.value"] <- 0

pdf("AwardTotalByRaceandGender.pdf", height = 5, width = 5)
ggplot(toPlot[complete.cases(toPlot),], aes(x = identify.simple, y=sch.tot.value, fill=racialized)) + geom_boxplot() + xlab("Gender Identity") + ylab("Total Value of Scholarships") + labs(fill="Identity as\n Racialized") 
dev.off()

## For Report



png("AwardTotalByRaceandGender.png", height = 3, width = 3, units="in", res=600, bg="transparent")
ggplot(toPlot[complete.cases(toPlot),], aes(x = identify.simple, y=sch.tot.value, fill=racialized)) + 
geom_boxplot(color="grey") + xlab("Gender Identity") + ylab("Total Value of Scholarships") + labs(fill="Identity as\n Racialized") + 
scale_fill_manual(values=c("white", "black")) + theme_classic() + 
theme(legend.position = "top", legend.background = element_rect(fill = "transparent", colour = NA), panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()






pdf("AwardTotalByGender.pdf", height = 5, width = 5)
ggplot(toPlot[complete.cases(toPlot),], aes(x = identify.simple, y=sch.tot.value)) + geom_boxplot() + xlab("Gender Identity") + ylab("Total Value of Scholarships") 
dev.off()


pdf("AwardTotalByRacialized.pdf", height = 5, width = 5)
ggplot(toPlot[complete.cases(toPlot),], aes(x = racialized, y=sch.tot.value)) + geom_boxplot() + xlab("Identify as Racialized") + ylab("Total Value of Scholarships") 
dev.off()


### WHat about getting an award?


toPlot <- dat[,c("sch.tot.value", "racialized", "identify.simple")]

toPlot$identify.simple <- factor(toPlot$identify.simple, levels=c("Man", "Woman", "Other"))


toPlot$AnyAward <- is.na(toPlot$sch.tot.value)
toPlot$sch.tot.value <- NULL

table(toPlot)

ggplot(toPlot, aes(x=identify.simple, fill=racialized, y=..Freq..)) + geom_bar()


#### Looking at decision to transfer


dat$intend.trans <- dat[["If MSc, do you intend to transfer to the PhD program?"]]
dat$finance.impact.trans <- dat[["Does/did the lack of financial security during graduate school discourage your decision to transfer to a PhD?"]]


toPlot <- dat[,c("racialized", "intend.trans")]

toPlot <- data.frame(prop.table(table(toPlot), 1))

toPlot$total <- as.data.frame(table(dat[,c("racialized", "intend.trans")]))$Freq

pdf("TransferByRacialized.pdf")
ggplot(toPlot, aes(x=racialized, fill=intend.trans, y=Freq)) + geom_bar(stat="identity", position="dodge") 
dev.off()





toPlot <- dat[,c("food.insc", "intend.trans")]

toPlot <- data.frame(prop.table(table(toPlot), 1))
toPlot$food.insc <- factor(toPlot$food.insc, levels=c("No", "Marginal", "Moderate", "Severe"))

toPlot$total <- as.data.frame(table(dat[,c("food.insc", "intend.trans")]))$Freq

pdf("TransferByFoodInsc.pdf", height = 5, width = 5)
ggplot(toPlot, aes(x=food.insc, fill=intend.trans, y=Freq)) + theme(axis.text.x = element_text(angle=45, hjust = 1)) + geom_bar(stat="identity", position="dodge") + xlab("Level of Food Insecurity") + ylab("Percentage (within category)") + labs(fill="Intention to\n Transfer to PhD")
dev.off()




toPlot <- dat[,c("housing.insc", "intend.trans")]

toPlot <- data.frame(prop.table(table(toPlot), 1))
toPlot$housing.insc <- factor(toPlot$housing.insc, levels=c("No", "Marginal", "Moderate", "Severe"))

toPlot$total <- as.data.frame(table(dat[,c("housing.insc", "intend.trans")]))$Freq

pdf("TransferByHousingInsc.pdf", height = 5, width = 5)
ggplot(toPlot, aes(x=housing.insc, fill=intend.trans, y=Freq)) + theme(axis.text.x = element_text(angle=45, hjust = 1)) + geom_bar(stat="identity", position="dodge") + xlab("Level of Housing Insecurity") + ylab("Percentage (within category)") + labs(fill="Intention to\n Transfer to PhD")
dev.off()

### International Student Questions




# [91] "What made you decide to come to Canada for graduate school?"
#  [92] "My department provided enough help with my transition to Canada."
#  [93] "How was your experience in securing a guarantor?"
#  [94] "There are adequate award opportunities for international students."
#  [95] "How do you support yourself financially?"



dat$why.canada.split <- strsplit(dat[["What made you decide to come to Canada for graduate school?"]], split=", ")

main.cats <- c("Top-notch research", "Want to settle in Canada for the long term", "Accessibility of Canadian schools to international students", "No particular reason", "Financial outlook", "Family")

toPlot <- sapply(main.cats, function(x) return(sum(sapply(dat$why.canada.split, function(y) x %in% y))))

toPlot <- data.frame(x=names(toPlot), y=toPlot)
toPlot$x <- factor(toPlot$x, main.cats)
pdf("decisionComeCanada.pdf", height = 10, width = 12)
ggplot(toPlot, aes(x,y)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45, hjust=1)) + ylab("Count") + xlab("Decision to Come to Canada")
dev.off()


pdf("DeptSupportComeCanada.pdf", height=10, width=10)
ggplot(dat, aes(`My department provided enough help with my transition to Canada.`)) + geom_bar()
dev.off()

dat$guarantor <- factor(dat[["How was your experience in securing a guarantor?"]], c("Extremely easy", "Easy", "Moderate", "Difficult", "Extremely difficult"))

pdf("Guarantor.pdf", height=10, width=10)
ggplot(dat[!is.na(dat$guarantor),], aes(guarantor)) + geom_bar() + xlab("How was your experience in securing a guarantor?")
dev.off()

dat$int.awards <- factor(dat[["There are adequate award opportunities for international students."]], levels=c(1,2,3,4,5))


pdf("IntAwards.pdf", height=10, width=10)
ggplot(dat[!is.na(dat$int.awards),], aes(int.awards)) + geom_bar() + scale_x_discrete(drop=FALSE) + xlab("There are adequate award opportunities for international students.")
dev.off()


### Lets look at correlation between hours in lab and hours in work

dat$lab.hours <- dat[["On average, how many hours a week do you typically work on research-related activities?"]]
dat$lab.hours <- factor(dat$lab.hours)
dat$lab.hours <- relevel(dat$lab.hours, "Less than 11 hours")

pdf("hoursLabvsJob.pdf", width=12, height=10)
ggplot(dat, aes(lab.hours, ext.emp.hours)) + geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Hours spent on research") + ylab("Hours externally employed")
dev.off()

cor(as.numeric(dat[["lab.hours"]]), as.numeric(dat[["ext.emp.hours"]]), method="s", use="pair")


### Are you more likely to transfer with external support?

dat$any.support.fam <- dat[["How much monetary support do you seek for your day-to-day expenses (annually)?  What are the sources? [Parents/Spouse/Relative]"]]


#### Discrimination vs PhD transfer

dat$disc.any <- !is.na(dat[["Have you experienced unequal treatment or discrimination on the basis of...?"]])

toPlot <- dat[dat$degree.simple =="MSc",c("If MSc, do you intend to transfer to the PhD program?","disc.any")]

prop.table(t(table(toPlot)), 1)

ggplot(toPlot, aes(fill=disc.any, 
                   x=`If MSc, do you intend to transfer to the PhD program?`)) + geom_bar(position="dodge")
