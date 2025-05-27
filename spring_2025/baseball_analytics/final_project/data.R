library(tidyverse)
bwar_bat = read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt", na = "NULL")
bwar_pit = read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt", na = "NULL") 


write_csv(bwar_bat, file = "C:/Users/danie/Desktop/stat430/sp25_stat430_caccamo3/final/bwar_bat.csv")
write_csv(bwar_pit, file = "C:/Users/danie/Desktop/stat430/sp25_stat430_caccamo3/final/bwar_pit.csv")



