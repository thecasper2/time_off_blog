library(data.table)
library(lubridate)
library(stringr)

get_weight_data <- function(path){
    scales_data <- fread(path)
    colnames(scales_data) <- c("time", "weight", "bmi", "body_fat",
                               "fat_free_body_weight", "subcutaneous_fat",
                               "visceral_fat", "body_water", "skeletal_muscle",
                               "muscle_mass", "bone_mass", "protein", "bmr",
                               "metabolic_age", "remarks")
    
    remove_percent <- function(string){
        return(as.numeric(str_remove(string, "%"))/100)
    }
    remove_kg <- function(string){
        return(as.numeric(str_remove(string, "kg")))
    }
    remove_kcal <- function(string){
        return(as.numeric(str_remove(string, "kcal")))
    }
    scales_data[, `:=` (
        time = as_datetime(
            str_remove_all(time, "\\."),
            format="%b %d, %Y %I:%M:%S %p"
        ),
        weight = remove_kg(weight),
        bmi = as.numeric(bmi),
        body_fat = remove_percent(body_fat),
        body_fat_percent = remove_percent(body_fat),
        fat_free_body_weight = remove_kg(fat_free_body_weight),
        subcutaneous_fat = remove_percent(subcutaneous_fat),
        visceral_fat = as.numeric(visceral_fat),
        body_water = remove_percent(body_water),
        skeletal_muscle = remove_percent(skeletal_muscle),
        muscle_mass = remove_kg(muscle_mass),
        bone_mass = remove_kg(bone_mass),
        protein = remove_percent(protein),
        bmr = remove_kcal(bmr),
        metabolic_age = as.numeric(metabolic_age)
    )]
    scales_data[, `:=` (
        body_fat = weight*body_fat_percent,
        muscle_mass_percent = muscle_mass / weight,
        bone_mass_percent = bone_mass / weight,
        time_of_day = ifelse(
            hour(time) >= 5 & hour(time) < 12, "morning", ifelse(
                hour(time) >= 12 & hour(time) < 18, "afternoon", "evening"
            )
        )
    )]
    scales_data$time_of_day <- factor(
        scales_data$time_of_day, levels=c("morning", "afternoon", "evening")
    )
    return(scales_data)
}