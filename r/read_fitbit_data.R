library(data.table)
library(lubridate)
library(readxl)
library(stringr)

get_fitbit_data <- function(dir_path = "static/data/fitbit/"){
    sheets <- c("Body", "Activities", "Sleep")
    data <- list()
    for(file in list.files(dir_path)){
        for(sheet in sheets){
            temp_data <- read_xls(paste0(dir_path, file), sheet=sheet) %>%
                as.data.table()
            data[[sheet]] <- rbind(data[[sheet]], temp_data)
        }
    }
    
    data[["Body"]]$Date <- as_datetime(
        data[["Body"]]$Date, format="%d-%m-%y")
    data[["Activities"]]$Date <- as_datetime(
        data[["Activities"]]$Date, format="%d-%m-%y")
    data[["Sleep"]]$`Start Time` <- as_datetime(
        data$Sleep$`Start Time`, format="%d-%m-%Y %I:%M %p")
    data[["Sleep"]]$`End Time` <- as_datetime(
        data$Sleep$`End Time`, format="%d-%m-%Y %I:%M %p")
    rm_comma_numeric <- function(x){
        return(as.numeric(gsub(",", "", x)))
    }
    numeric_cols <- c("Weight", "BMI", "Fat")
    data$Body[, (numeric_cols) := lapply(.SD, rm_comma_numeric), .SDcols=numeric_cols]
    numeric_cols <- c("Calories Burned", "Steps", "Distance", "Floors",
                      "Minutes Sedentary", "Minutes Lightly Active",
                      "Minutes Fairly Active", "Minutes Very Active",
                      "Activity Calories")
    data$Activities[, (numeric_cols) := lapply(.SD, rm_comma_numeric), .SDcols=numeric_cols]
    numeric_cols <- c("Minutes Asleep", "Minutes Awake", "Number of Awakenings",
                      "Time in Bed", "Minutes REM Sleep", "Minutes Light Sleep",
                      "Minutes Deep Sleep")
    data$Sleep[, (numeric_cols) := lapply(.SD, rm_comma_numeric), .SDcols=numeric_cols]
    return(data)
}
