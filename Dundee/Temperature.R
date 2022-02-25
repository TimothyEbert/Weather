if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)
library(readxl)
library(tidyverse)
###############################################################
#####   Read Data Files
###############################################################
DunD_path<-"C:/Users/tebert/OneDrive - University of Florida/Work/Arnold Schumann/2020 Report/Temperature20/Dundee/DunDee_Phase_1_Indoor_Outdoor.xlsx"
DunD_inside<-read_excel(DunD_path,sheet=2)
DunD_outside<-read_excel(DunD_path,sheet=1)
### Date_Time  Temp_F_Mean  Temp_F_max  Temp_F_min  Dew_pt_mean  Dew_pt_min  Solar_rad_w_m2  VPD_kPa_mean  
###    VPD_kPa_min  RH_pct_mean  RH_pct_max  RH_pct_min  Rain_hourly_total  WindSpeed_mean  WindSpeed_max1  
###    WindSpeed_max2  Solar_panel_last  Battery_last  DeltaT_F_mean  DeltaT_F_max  DeltaT_F_min  Daily_ET0_in

DunD <-  DunD_outside %>% inner_join(DunD_inside, by="Date_Time")
DunD <- DunD %>% na.omit()
range(DunD$Out_Temp_F_Mean)
range(DunD$Temp_F_Mean)

DunD_out <- DunD %>% select(Date_Time, Out_Temp_F_Mean,  Out_Temp_F_max,  Out_Temp_F_min,  Out_Dew_pt_mean,  Out_Dew_pt_min,
                        Out_Solar_rad_w_m2,  Out_VPD_kPa_mean, Out_VPD_kPa_min, Out_RH_pct_mean,  Out_RH_pct_max,
                        Out_RH_pct_min,  Out_Rain_hourly_total,  Out_WindSpeed_mean,  Out_WindSpeed_max1,  Out_WindSpeed_max2)
DunD_in <- DunD %>% select(Date_Time, Temp_F_Mean,  Temp_F_max,  Temp_F_min,  Dew_pt_mean,  Dew_pt_min,
                        Solar_rad_w_m2,  VPD_kPa_mean, VPD_kPa_min, RH_pct_mean,  RH_pct_max,
                        RH_pct_min,  Rain_hourly_total,  WindSpeed_mean,  WindSpeed_max1,  WindSpeed_max2)
colnames(DunD_out) <- c("Date_Time", "Temp_F_Mean",  "Temp_F_max",  "Temp_F_min",  "Dew_pt_mean",  "Dew_pt_min",
                        "Solar_rad_w_m2",  "VPD_kPa_mean", "VPD_kPa_min", "RH_pct_mean",  "RH_pct_max",
                        "RH_pct_min",  "Rain_hourly_total",  "WindSpeed_mean",  "WindSpeed_max1",  "WindSpeed_max2")
DunD_out$Loc <- 0
DunD_in$Loc <- 1
DunD2 <- rbind(DunD_out, DunD_in)

ggplot(DunD2, aes(x=Loc, y=Temp_F_Mean, group=Loc)) + geom_boxplot()

DunD$Diff <- DunD$Out_Temp_F_Mean - DunD$Temp_F_Mean
ggplot(DunD, aes(x=Diff)) + geom_histogram(bins=50)




