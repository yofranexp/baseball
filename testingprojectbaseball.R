#testing project baseball
library(dplyr)
library(tidyr)
library(purrr)
atbats <- read.csv(file =  "C:/Users/info/OneDrive/Desktop/work/Laboratorio/atbats.csv"
)
df_p <- read.csv(file =  "C:/Users/info/OneDrive/Desktop/work/Laboratorio/pitches.csv"
)


gameview <- function(game_id){
  options(scipen = 999)


  ## complete data =====

  complete_frame <- df_p %>%
    full_join(atbats, by ='ab_id')

  # apply(is.na(complete_frame), 2, sum)


  complete_frame <- complete_frame %>%
    filter(!is.na(px), !is.na(pz), !is.na(start_speed), !is.na(end_speed),
           !is.na(spin_rate), !is.na(spin_dir), !is.na(break_length), !is.na(break_y),
           !is.na(pitch_type), !is.na(outs), !is.na(vx0),
           !is.na(code))


  complete_frame <- complete_frame %>%
    mutate(Code = case_when(code == "*B" ~ "Ball in dirt",
                            code == "B" ~ "Ball",
                            code == "S" ~ "Swinging Strike",
                            code == "C" ~ "Called Strike",
                            code == "F" ~ "Foul",
                            code == "T" ~ "Foul Tip",
                            code == "L" ~ "Foul Bunt",
                            code == "I" ~ "Intentional Ball",
                            code == "W" ~ "Swinging Strike (Blocked)",
                            code == "P" ~ "Pitchout",
                            code == "Q" ~ "Swinging pitchout",
                            code == "R" ~ "Foul pitchout",
                            code == "X" ~ "In play, out(s)",
                            code == "D" ~ "In play, no out",
                            code == "E" ~ "In play, runs",
                            code == "H" ~ "Hit by pitch",
                            TRUE ~ "Others"
    ),
    PitchType = case_when(pitch_type == "CH" ~ "Changeup",
                          pitch_type == "CU" ~ "Curveball",
                          pitch_type == "EP" ~ "Eephus",
                          pitch_type == "FC" ~ "Cutter",
                          pitch_type == "FF" ~ "Four-seam Fastball",
                          pitch_type == "FS" ~ "Splitter",
                          pitch_type == "FT" ~ "Two-seam Fastball",
                          pitch_type == "IN" ~ "Intentional ball",
                          pitch_type == "KC" ~ "Knuckle curve",
                          pitch_type == "KN" ~ "Knuckeball",
                          pitch_type == "SL" ~ "Slider",
                          pitch_type == "PO" ~ "Pitchout",
                          pitch_type == "SC" ~ "Screwball",
                          pitch_type == "SI" ~ "Sinker",
                          pitch_type == "FO" ~ "Pitchout",
                          TRUE ~ "Unknown"))

  dt <- complete_frame %>%
    nest(-g_id)


  dt %>%
    filter(g_id == game_id) %>%
    pmap(function(g_id, data)
      rmarkdown::render("C:/Users/info/OneDrive/Desktop/machine learning/projectbaseball/projectbaseball.Rmd",
                        output_file = paste0("C:/Users/info/OneDrive/Desktop/game",g_id,
                                             ".html"),
                        params = list(CustomTitle = paste0("MLB GAME"), data = data)))
}
gameview(game_id = 201500008)
