library(readxl)

## survey responses must be in directory called survey_results
survey_response_files <-
  list.files("survey_results", full.names = T)

## import surveys
for (fil in survey_response_files) {
  year <- gsub("-","_", gsub(".*(20[12][0-9][-_][12][0-9]).*", "\\1", fil))
  if (grepl(".csv", fil)) {
    assign(paste0("responses_", year), 
           read_csv(fil) %>% mutate(year = year)) %>% 
      filter(!is.na(Timestamp))
  }
  if (grepl(".xlsx", fil)) {
    assign(paste0("responses_", year), 
           read_xlsx(fil) %>% mutate(year = year)) %>% 
      filter(!is.na(Timestamp))
  }
  if (grepl(".tsv", fil)) {
    assign(paste0("responses_", year), 
           read_delim(fil, delim = "\t") %>% mutate(year = year)) %>% 
      filter(!is.na(Timestamp))
  }
}

## find out all possible pairs of surveys
pairs <- list(c("responses_2017_18", "responses_2018_19"),
              c("responses_2018_19", "responses_2019_20"),
              c("responses_2019_20", "responses_2020_21"),
              c("responses_2020_21", "responses_2021_22"))
dir.create("question_correspondences")
j <- 1
correspondence_list <- list()

## loop through all possible pairs of surveys, and get pairwise string distances
for (i in 1:length(pairs)) {
  df1 <- pairs[[i]][1]
  df2 <- pairs[[i]][2]
  ## assign the shorter q set to x, and the longer q set y
  if(length(colnames(get(df1))) < length(colnames(get(df2)))){
    x <- colnames(get(df1))
    x_nm <- df1
    y <- colnames(get(df2))
    y_nm <- df2
  } else {
    x <- colnames(get(df2))
    x_nm <- df2
    y <- colnames(get(df1))
    y_nm <- df1
  }
  x <- c(x, rep("XXXX", length(y) - length(x)))
  
  ## get approximate string distances (Levenshtein) between all questions
  dist_mat <- adist(x, y) ## could be improved with better distance metric? 
  
  ## get vector of indices for x (shorter question set) with the smallest distance
  min_2 <- apply(dist_mat, 2, which.min)
  
  correspondences <- data.frame(x[min_2], y)
  colnames(correspondences) <- c(x_nm, y_nm)
  unmatched_x <- setdiff(x, x[min_2])
  unmatched_x <- unmatched_x[unmatched_x != "XXXX"]
  correspondences <- rbind(correspondences, 
                           setNames(data.frame(unmatched_x, rep(NA,length(unmatched_x))), c(x_nm,y_nm)))
  correspondence_list[[j]] <- correspondences
  j <- j + 1
}

## do a series of joins to combine everything
correspondence_df <- correspondence_list[[1]]
for (i in 2:length(correspondence_list)) {
  correspondence_df <- full_join(correspondence_df, correspondence_list[[i]])
}
write.table(correspondence_df, 
            file.path("question_correspondences.tsv"), 
            quote = F, sep = "\t", row.names = F)
