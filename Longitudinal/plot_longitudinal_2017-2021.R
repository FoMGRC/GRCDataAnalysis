## longitudinal questions ###########################################################

library(readxl)
library(data.table)
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

## HELP ###########################################################################################

## TRANSFER #######################################################################################

# Intent to Transfer

# Impact Transfer to PhD


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

# legend
hbfa_legend <- legend.grob(
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
        right = list(fun = hbfa_legend)
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

# legend
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

# Value of Additional Support


# Total Value of Needed Support

## EMPLOYMENT #####################################################################################

# Hours TA

# Hours External Employment

# External Employment Motivation

## SCHOLARSHIP ####################################################################################

# Applied Scholarships Hold

# Hold Scholarships Binary

# Total Value Top Up

# Total Value Scholarships

## LIVING #########################################################################################

# Current Living Situation

# Total Cost of Housing

# Minutes of Commute

# Satisfaction with Housing

## LIVING #########################################################################################

# Main Modes of Transportation

# Total Cost of Transport

## MENTAL HEALTH ##################################################################################

# Anxiety or Depression

# Financial Impact Mental Health

# Adequate Mental Health Resources



