## longitudinal questions ###########################################################

library(readxl)
library(data.table)
library(plyr)
library(BoutrosLab.plotting.general)

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

setwd('~/Documents/grc_data/Longitudinal/')

date <- Sys.Date()

latest_rds <- list.files('./', '*matched_lists_2017-2021.RDS')
latest_rds <- latest_rds[length(latest_rds)]

matched_lists <- readRDS(latest_rds)

## HELP ###########################################################################################

yes_no_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(2, 'pastel'),
            labels = c('Yes', 'No'),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

three_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(3, 'pastel'),
            labels = c('Yes', 'Somewhat', 'No'),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

support_values <- c("0.0", "Up to $1,000",
    "$1,000 - $5,000", "$5,000 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+")
names(support_values) <- c("$0", "$1 - $1,000",
    "$1,000 - $5,000", "$5,000 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+")

support_values_2 <- c("0.0", "$1 - $1,000",
    "$1,000 - $5,000", "$5,000 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+")
names(support_values_2) <- c("$0", "$1 - $1,000",
    "$1,000 - $5,000", "$5,000 - $10,000", "$10,000 - $15,000", "$15,000 - $20,000", "$20,000+")

value_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(7, 'pastel'),
            labels = names(support_values),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

agreement_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = rev(default.colours(5, 'pastel')),
            labels = rev(c('Strongly Disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly Agree')),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

## TRANSFER #######################################################################################

# Intent to Transfer
raw <- matched_lists[['Intent to Transfer']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'Undecided', 'No'))

# legend
transfer_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(3, 'pastel'),
            labels = c('Yes', 'Undecided', 'No'),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

create.barplot(
    main = 'Intent to Transfer to PhD in MSc Respondents',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(3, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = transfer_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_msc_transfer', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Impact Transfer to PhD
raw <- matched_lists[['Impact Transfer to PhD']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'No'))

create.barplot(
    main = 'Finances Discourage Transfer to PhD in MSc Respondents',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(2, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = yes_no_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_msc_transfer_finance', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

## AWARENESS ######################################################################################

# HBFA Awareness
raw <- matched_lists[['HBFA Awareness']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'Somewhat', 'No'))

create.barplot(
    main = 'Awareness of HBFA and its Stipend Impacts',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(3, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = three_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_hbfa_awareness', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Stipend Increase Awareness
raw <- matched_lists[['Stipend Increase Awareness']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'No'))

create.barplot(
    main = 'Awareness of Stipend Increase',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(2, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = yes_no_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_stipend_increase_awareness', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

## SUPPORT ########################################################################################

# Supprt Living Expenses
raw <- matched_lists[['Supprt Living Expenses']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'No'))

create.barplot(
    main = 'Able to Support Day-to-Day Living with Graduate Funding',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(2, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = yes_no_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_can_live', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Stipend Conpensates My Work
raw <- matched_lists[['Stipend Conpensates My Work']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable)

create.barplot(
    main = 'My Stipend Adequately Compensates for My Work',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(5, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = agreement_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_stipend_compensates', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Source of Support
support_cols <- c('support.family', 'support.ta', 'support.employment',
    'support.loans', 'support.savings', 'support.none')
raw <- matched_lists[support_cols]
raw <- lapply(raw, function(x) data.frame(do.call(rbind, lapply(x, table))))
raw <- lapply(raw, function(x) { x / rowSums(x) })
raw_df <- data.frame(do.call(cbind, raw))
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$source <- gsub('.*\\.(.*)\\.(.*)\\.', '\\1', raw_df_stack$variable)
raw_df_stack$source <- factor(raw_df_stack$source, c('family', 'ta', 'employment', 'loans', 'savings', 'none'))
raw_df_stack <- raw_df_stack[!grepl('FALSE', raw_df_stack$variable), ]

# legend
support_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(6, 'pastel'),
            labels = c('Parents/Spouse/Relative', 'Teaching Assistantship',
                'External Emplyment', 'Loans', 'Personal Savings', 'No Additional Support'),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

create.barplot(
    main = 'Sources of Additional Support',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = source,
    stack = FALSE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(6, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = support_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_source', '.pdf'),
    width = 10,
    height = 6,
    resolution = 300
    )

create.scatterplot(
    main = 'Sources of Additional Support',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = source,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xlimits = c(0.5, nrow(raw_df) + 0.5),
    xat = raw_df$order,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    type = c('p', 'l'),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(6, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = support_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_source_l', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Source of Support in those with Need
support_cols <- c('support.family.no', 'support.ta.no', 'support.employment.no',
    'support.loans.no', 'support.savings.no', 'support.none.no')
raw <- matched_lists[support_cols]
raw <- lapply(raw, function(x) data.frame(do.call(rbind, lapply(x, table))))
raw <- lapply(raw, function(x) { x / rowSums(x, na.rm=T) })
raw_df <- data.frame(do.call(cbind, raw))
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$source <- gsub('.*\\.(.*)\\.(.*)\\..*\\.', '\\1', raw_df_stack$variable)
raw_df_stack$source <- factor(raw_df_stack$source, c('family', 'ta', 'employment',
    'loans', 'savings', 'none'))
raw_df_stack <- raw_df_stack[!grepl('FALSE', raw_df_stack$variable), ]

# legend
support_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(6, 'pastel'),
            labels = c('Parents/Spouse/Relative', 'Teaching Assistantship',
                'External Emplyment', 'Loans', 'Personal Savings', 'No Additional Support'),
            title = expression(bold(underline('Response'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

create.barplot(
    main = 'Sources of Additional Support in those with Need',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = source,
    stack = FALSE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(6, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = support_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_source_need', '.pdf'),
    width = 10,
    height = 6,
    resolution = 300
    )

create.scatterplot(
    main = 'Sources of Additional Support in those with Need',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = source,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xlimits = c(0.5, nrow(raw_df) + 0.5),
    xat = raw_df$order,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    type = c('p', 'l'),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(6, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = support_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_source_need_l', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Value of Additional Support
raw <- matched_lists[['Total Value of Additional Support']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, support_values)

create.barplot(
    main = 'Total Value of Additional Support Received',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(7, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = value_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_total_support', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Value of Additional Support by Category
value_support_cols <- c('Value of Additional Support Family', 'Value of Additional Support TA',
    'Value of Additional Support Employment', 'Value of Additional Support Loans',
    'Value of Additional Support Savings', 'Value of Additional Support Other')
raw <- matched_lists[value_support_cols]
raw <- lapply(raw, function(x) lapply(x, function(y) as.data.frame(rbind(table(y)))))
raw <- lapply(raw, function(x) data.frame(do.call(rbind.fill, x), check.names = F))
raw <- lapply(raw, function(x) { x / rowSums(x, na.rm=T) })
raw_df <- data.frame(do.call(cbind, raw), check.names=F)
raw_df$year <- gsub('X', '', names(matched_lists[value_support_cols][[1]]))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$value[is.na(raw_df_stack$value)] <- 0
raw_df_stack$source <- gsub('(.*?)\\.(.*)', '\\1', raw_df_stack$variable)
raw_df_stack$source <- gsub('Value of Additional Support ', '', raw_df_stack$source)
raw_df_stack$source <- factor(raw_df_stack$source, c('Family', 'TA', 'Employment', 'Loans', 'Savings', 'Other'))
raw_df_stack$support <- gsub('(.*?)\\.(.*)', '\\2', raw_df_stack$variable)
raw_df_stack$support <- factor(raw_df_stack$support, support_values_2)

create.barplot(
    main = 'Value of Additional Support Received by Category',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order | source,
    groups = support,
    stack = TRUE,
    layout = c(length(value_support_cols), 1),
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(7, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = value_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_category', '.pdf'),
    width = 12,
    height = 6,
    resolution = 300
    )

# Total Value of Needed Support
raw <- matched_lists[['Total Value of Needed Support']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, support_values_2)

create.barplot(
    main = 'Total Value of Additional Support Needed',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(7, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = value_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_need', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

create.scatterplot(
    main = 'Total Value of Additional Support Needed',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xlimits = c(0.5, nrow(raw_df) + 0.5),
    xat = raw_df$order,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    type = c('p', 'l'),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(7, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = value_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_support_need_l', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

## EMPLOYMENT #####################################################################################

# Hours TA
raw <- matched_lists[['Hours TA']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df <- raw_df[raw_df$value != 0, ]

create.histogram(
    main = 'Number of TA Hours in those who TA',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Hours',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(0, 31),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, max(raw_df$value), 1),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_ta_hours', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Hours External Employment
raw <- matched_lists[['Hours External Employment']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df <- raw_df[raw_df$value != 0 & !is.na(raw_df$value), ]

create.histogram(
    main = 'Number of External Employment Hours in those Employed',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Hours',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(0, 31),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, max(raw_df$value), 1),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_employment_hours', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# External Employment Motivation
work_motivation_cols <- c('External Employment Motivation Additional Income',
    'External Employment Motivation Work Experience',
    'External Employment Motivation Personal Interest')
raw <- matched_lists[work_motivation_cols]
raw <- lapply(raw, function(x) lapply(x, function(y) as.data.frame(rbind(table(y)))))
raw <- lapply(raw, function(x) data.frame(do.call(rbind, x), check.names = F))
raw <- lapply(raw, function(x) { x / rowSums(x, na.rm=T) })
raw_df <- data.frame(do.call(cbind, raw), check.names=F)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))

raw_df_stack$source <- gsub('(.*?)\\.(.*)', '\\1', raw_df_stack$variable)
raw_df_stack$source <- gsub('External Employment Motivation ', '', raw_df_stack$source)
raw_df_stack$rank <- gsub('(.*?)\\.(.*)', '\\2', raw_df_stack$variable)
raw_df_stack$rank <- factor(raw_df_stack$rank)

# legend
rank_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(3, 'pastel'),
            labels = c('1', '2', '3'),
            title = expression(bold(underline('Rank'))),
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 1,
    title.just = 'left'
    )

create.barplot(
    main = 'Rank of Motivation for Employment',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order | source,
    groups = rank,
    stack = TRUE,
    layout = c(3, 1),
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(3, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = rank_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_work_motivation_rank', '.pdf'),
    width = 8,
    height = 6,
    resolution = 300
    )

## SCHOLARSHIP ####################################################################################

# Applied Hold Scholarships Binary
scholarship_cols <- c('scholarship.applied', 'scholarship.held')
raw <- matched_lists[scholarship_cols]
raw <- lapply(raw, function(x) data.frame(do.call(rbind, lapply(x, table))))
raw <- lapply(raw, function(x) { x / rowSums(x) })
raw_df <- data.frame(do.call(cbind, raw))
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$source <- gsub('.*\\.(.*)\\.(.*)\\.', '\\1', raw_df_stack$variable)
raw_df_stack$source <- factor(raw_df_stack$source, c('applied', 'held'))
raw_df_stack <- raw_df_stack[!grepl('FALSE', raw_df_stack$variable), ]

# legend
scholarship_legend <- legend.grob(
    legends = list(
        legend = list(
            colours = default.colours(2, 'pastel'),
            labels = c('Applied for Scholarships for the Coming Year', 'Held Scholarships in the Previous Year'),
            title = '',
            lwd = 0.3
            )
        ),
    label.cex = 0.8,
    title.cex = 0,
    title.just = 'left'
    )

create.scatterplot(
    main = 'Scholarships Status',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = source,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xlimits = c(0.5, nrow(raw_df) + 0.5),
    xat = raw_df$order,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    type = c('p', 'l'),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(2, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        inside = list(fun = scholarship_legend,
            x = 0.05,
            y = 0.95)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_scholarship_status', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Total Value Top Up
raw <- matched_lists[['sch.topup.held']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df <- raw_df[raw_df$value != 0 & !is.na(raw_df$value), ]
raw_df$value <- raw_df$value / 1000

create.histogram(
    main = 'Total Value of Top-Ups Received in those with Awards',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Top-Ups ($k)',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    xlimits = c(-0.5, 11),
    yaxis.cex = 0.8,
    ylimits = c(0, 56),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, max(raw_df$value), 1),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_topup_held', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Total Value Scholarships
raw <- matched_lists[['sch.tot.value.held']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df <- raw_df[raw_df$value != 0 & !is.na(raw_df$value), ]
raw_df$value <- raw_df$value / 1000
# remove crazy values
raw_df <- raw_df[raw_df$value != 350.004, ]

create.histogram(
    main = 'Total Value of Scholarships in those with Awards',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Scholarship Face Value ($k)',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    xlimits = c(-2, 52),
    yaxis.cex = 0.8,
    ylimits = c(0, 31),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, max(raw_df$value), 1),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_scholarships_held', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

## LIVING #########################################################################################

# Total Cost of Housing
raw <- matched_lists[['Total Cost of Housing']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$value[is.na(raw_df$value)] <- 0
raw_df$value <- raw_df$value / 1000

create.histogram(
    main = 'Monthly Housing Expensis',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Cost ($k)',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    xlimits = c(-0.5, 3.5),
    yaxis.cex = 0.8,
    ylimits = c(0, 60),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, ceiling(max(raw_df$value)), 0.25),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_housing_monthly', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Satisfaction with Housing
raw <- matched_lists[['Satisfaction with Housing']]
raw <- lapply(raw, function(y) as.data.frame(rbind(table(y))))
raw <- data.frame(do.call(rbind.fill, raw), check.names = F)
raw <- raw / rowSums(raw, na.rm=T)
raw_df <- data.frame(do.call(cbind, raw), check.names=F)
raw_df$year <- gsub('X', '', names(matched_lists[['Satisfaction with Housing']]))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$value[is.na(raw_df_stack$value)] <- 0
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'Could be better', 'No'))

create.barplot(
    main = 'Satisfaction with Current Housing',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(3, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = three_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_housing', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

## TRANSPORT ######################################################################################

# Minutes of Commute
raw <- matched_lists[['Minutes of Commute']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$value[is.na(raw_df$value)] <- 0

create.histogram(
    main = 'Average One-Way Commute Length',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Time (min)',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    xlimits = c(-5, 215),
    xat = seq(0, 210, 30),
    yaxis.cex = 0.8,
    ylimits = c(0, 17),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, ceiling(max(raw_df$value)), 5),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_commute', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Total Cost of Transport
raw <- matched_lists[['Total Cost of Transport']]
raw_df <- lapply(seq_along(raw), function(i) data.frame(value = raw[[i]], name = names(raw)[i]))
raw_df <- data.frame(do.call(rbind, raw_df), check.names = F)
raw_df$year <- gsub('X', '', raw_df$name)
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$value[is.na(raw_df$value)] <- 0

create.histogram(
    main = 'Monthly Transportation Cost',
    main.cex = 1,
    x = ~ value | year,
    data = raw_df,
    layout = c(1, length(unique(raw_df$year))),
    type = 'percent',
    xlab.label = 'Cost ($)',
    xlab.cex = 1,
    ylab.label = 'Percentage of Responses',
    ylab.cex = 1,
    # xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    xlimits = c(-20, 1000),
    # xat = seq(0, 210, 30),
    yaxis.cex = 0.8,
    ylimits = c(0, 21),
    yaxis.tck = c(0.5, 0),
    breaks = seq(0, ceiling(max(raw_df$value)), 10),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(1, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    # legend = list(
    #     right = list(fun = value_legend)
    #     ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_transportation_cost', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

## MENTAL HEALTH ##################################################################################

# Anxiety or Depression
raw <- matched_lists[['Anxiety or Depression']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable, c('Yes', 'No'))

create.barplot(
    main = 'Do You Suffer from Depression or Anxiety',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(2, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = yes_no_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_depression', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Financial Impact Mental Health
raw <- matched_lists[['Financial Negative Mental Health']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack_negative <- reshape2::melt(raw_df, id.vars = c('year', 'order'))

raw <- matched_lists[['Financial Impact Mental Health']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
colnames(raw_df) <- colnames(raw_df)[ncol(raw_df):1]
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df) + 2
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))

raw_df_stack <- rbind(raw_df_stack_negative, raw_df_stack)
raw_df_stack$variable <- factor(raw_df_stack$variable)

create.barplot(
    main = 'My Financial Situation Negatively Impacts My Mental Health',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = unique(raw_df_stack$year),
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(5, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = agreement_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_finance_impact_mental', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )

# Adequate Mental Health Resources
raw <- matched_lists[['Adequate Mental Health Resources']]
raw_df <- lapply(raw, table)
raw_df <- data.frame(do.call(rbind, raw_df))
raw_df <- raw_df / rowSums(raw_df)
raw_df$year <- gsub('X', '', rownames(raw_df))
raw_df$year <- gsub('_', '-', raw_df$year)
raw_df$order <- 1:nrow(raw_df)
raw_df_stack <- reshape2::melt(raw_df, id.vars = c('year', 'order'))
raw_df_stack$variable <- factor(raw_df_stack$variable)

create.barplot(
    main = 'Available Resources Adequately Support My Mental Health Needs',
    main.cex = 1,
    data = raw_df_stack,
    formula = value ~ order,
    groups = variable,
    stack = TRUE,
    xlab.label = '',
    xlab.cex = 0,
    ylab.label = 'Proportion of Responses',
    ylab.cex = 1,
    xaxis.lab = raw_df$year,
    xaxis.cex = 0.8,
    xaxis.tck = c(0.5, 0),
    yaxis.cex = 0.8,
    ylimits = c(-0.05, 1.05),
    yaxis.tck = c(0.5, 0),
    # abline.v = breakpoints,
    # abline.col = 'grey70',
    # abline.lwd = 1,
    # abline.lty = 2,
    col = default.colours(5, 'pastel'),
    # add.text = FALSE,
    # text.labels = unique(patients_noblood),
    # text.x = get_midpoints(patients_noblood),
    # text.y = 1.02,
    # text.col = 'black',
    # text.cex = 0.8,
    # text.fontface = 'bold',
    # LEGEND
    legend = list(
        right = list(fun = agreement_legend)
        ),
    use.legacy.settings = TRUE,
    filename = paste0(date, '_mental_health_resources_adequate', '.pdf'),
    width = 6,
    height = 6,
    resolution = 300
    )
