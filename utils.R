library(dplyr)
library(stringr)

dir = 'specdata'
min <- 1
max <- 332
default <- 20


read_file <- function(ID) {
    ID <- str_pad(ID, width = 3, pad = '0')
    return (read.csv(paste0(dir, '/', ID, '.csv')))
}

get_data <- function(id = 1:max) {
    df <- bind_rows(lapply(id, read_file))
    df <- subset(df, complete.cases(df))
    return (df)
}

pollutantmean <- function(pollutant, id = 1:max) {
    df <- get_data(id)
    vec <- df[!is.na(df[pollutant]), pollutant]
    return (mean(vec))
}

complete <- function(id = 1:max) {
    df_output <- data.frame()
    
    for (ID in id) {
        df <- read_file(ID)
        df_output <- rbind(df_output, list(ID, sum(complete.cases(df))))
    }
    
    colnames(df_output) <- c('id', 'nobs')
    return (df_output)
}

corr <- function(threshold = 0) {
    vec <- numeric()
    df_complete <- complete()
    id <- df_complete[df_complete$nobs > threshold, 'id']
    
    for (ID in id) {
        df <- read_file(ID)
        df <- df[complete.cases(df), ]
        c <- cor(df$sulfate, df$nitrate)
        vec <- append(vec, c)
    }
    
    return (vec)
}
