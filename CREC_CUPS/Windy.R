if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)
library(readxl)
library(tidyverse)
###############################################################
#####   Read Data Files
###############################################################
CUPSPath<-"C:/Users/tebert/OneDrive - University of Florida/Work/Arnold Schumann/2020 Report/Temperature20/ISSData.xlsx"
CUPSData<-read_excel(CUPSPath,sheet=1)
#There are four channels, but only two are useful: channel 1 and 2
CUPSData <- CUPSData %>% filter(ChannelIndex==1 | ChannelIndex==2)
CUPSData <- CUPSData %>% filter(year(RecDateTime)>2013)
#Convert temperature (divide by ten to get degrees F)
CUPSData$TempOut <-CUPSData$TempOut/10
#Filter for equipment error. Temperature out of range
CUPSData <- CUPSData %>% filter(TempOut>10 & TempOut<150)
CUPSData$TempOutC <-(CUPSData$TempOut-32)*5/9
CUPSData <- CUPSData %>% select(ChannelIndex, RecDateTime, WindSpeed, TempOutC, TempOut, UV, HiUV, SolarRad)
#I want all of the FAWN data. Then calculate temperature difference and correlate with outside values.
#I need to find the solar radiation level where inside temp = outside temperature.
#Use that to help see where CUPS is warmer (day) or colder (night)
#Import FAWN data.

#ReceiverRecID	ChannelIndex	RecDateTime	
#TempOut	HiTempOut	LowTempOut	HumOut	
#WindSpeed	ScalerAvgWindDir	HiWindSpeed	HiWindDir	DominantDir	
#DewPoint	LowWindChill	HeatIndex	THSWIndex	
#RainCollectorType	RainCollectorInc	TotalRainClicks	HiRainRate	
#ET	UV	HiUV	SolarRad	HiSolarRad	IntervalIndex

#FAWN variables
#StationID	local_eastern_time	
#temp_soil_10cm_C	temp_air_60cm_C	temp_air_2m_C	temp_air_10m_C
#rh_2m_pct	temp_dp_2m_C	rain_2m_inches	wind_speed_10m_mph	wind_direction_10m_deg	
#rfd_2m_wm2

FAWNPath<-"C:/Users/tebert/OneDrive - University of Florida/Work/Arnold Schumann/2020 Report/Temperature20/FAWNLkAlfred2014_2020.xlsx"
FAWNData<-read_excel(FAWNPath,sheet=1)
FAWNData<- na.omit(FAWNData) #This discards data from all sensors if even one sensor goes bad.
FAWNData<- FAWNData %>% filter(temp_air_2m_C>-10 & temp_air_2m_C<50)

#Join FAWN and CUPS
#FAWN is every 15 minutes. CUPS is also every 15 minutes.
WeatherMax <-  FAWNData %>% inner_join(CUPSData, by=c("local_eastern_time" = "RecDateTime"))
WeatherMax <- WeatherMax %>% na.omit() %>% mutate(DiffTemp=TempOutC - temp_air_2m_C, 
                                                  month=as.factor(month(local_eastern_time,label=TRUE)),
                                                  hour=as.factor(hour(local_eastern_time)),
                                                  year=as.factor(year(local_eastern_time)),
                                                  day = day(local_eastern_time),
                                                  time=time(local_eastern_time),
                                                  Time_target=60*hour(local_eastern_time)+minute(local_eastern_time))
#Diff0 <- WeatherMax %>% filter(DiffTemp==0) #Almost never equal not a good approach
WindMax <- WeatherMax %>% select(local_eastern_time, hour, day, month, year, wind_speed_10m_mph, wind_direction_10m_deg) %>% na.omit()
WindMax$spd.binned <- 
##################################################################################
WindMax2 %>% ggplot(aes(x = wind_direction_10m_deg, 
                       fill = as.factor(hist(WindMax2$wind_speed_10m_mph)$mids), 
                       y = wind_speed_10m_mph
       ))+
  geom_bar() + 
  scale_x_discrete(drop = FALSE,
                   labels = c("N","NNE","NE","ENE", "E", 
                              "ESE", "SE","SSE", 
                              "S","SSW", "SW","WSW", "W", 
                              "WNW","NW","NNW")) +
  coord_polar(start = -((22.5/2)/360) * 2*pi) +
  scale_fill_manual(name = "Wind Speed (m/s)", 
#                   values = colorRampPalette(brewer.pal(1, palette="YlGnBu"))(15),
                    drop = FALSE) +
  theme(axis.title.x = element_blank()) + 
  scale_y_continuous(labels = percent) +
  ylab("Frequencia")


ggplot(WindMax, aes(x=wind_speed_10m_mph)) + geom_histogram(bins=50)
WindMax2<-WindMax
WindMax2$wind_speed_10m_mph<-ifelse(WindMax$wind_speed_10m_mph>=15, 15, WindMax$wind_speed_10m_mph)
ggplot(WindMax2, aes(x=wind_speed_10m_mph)) + geom_histogram(bins=50)
hist(WindMax2$wind_speed_10m_mph) #15 bins

### The difference in wind speed as a function of windspeed.
Wind_comp <- WeatherMax %>% select(local_eastern_time, hour, day, month, year, wind_speed_10m_mph, WindSpeed, rain_2m_inches) %>% na.omit()
Wind_comp$Diff_wind <- Wind_comp$wind_speed_10m_mph - Wind_comp$WindSpeed
Wind_comp$Prop_wind <- (Wind_comp$wind_speed_10m_mph - Wind_comp$WindSpeed)/Wind_comp$wind_speed_10m_mph
Wind_comp_low <- Wind_comp %>% filter(wind_speed_10m_mph<WindSpeed)
Wind_comp_high <- Wind_comp %>% filter(wind_speed_10m_mph >= WindSpeed)


ggplot(Wind_comp,aes(x=wind_speed_10m_mph, y=(wind_speed_10m_mph-WindSpeed)/wind_speed_10m_mph))+geom_point()


Wind_comp_low <- Wind_comp %>% filter(wind_speed_10m_mph<WindSpeed)
# There are times when wind in CUPS is higher than outside.Gusty winds CUPS intertia
ggplot(Wind_comp_low, aes(x=wind_speed_10m_mph, y=(wind_speed_10m_mph-WindSpeed)/wind_speed_10m_mph))+geom_point()
ggplot(Wind_comp_low, aes(x=wind_speed_10m_mph, y=wind_speed_10m_mph-WindSpeed))+geom_point()
Wind_comp_high <- Wind_comp %>% filter(wind_speed_10m_mph >= WindSpeed)
ggplot(Wind_comp_high, aes(x=wind_speed_10m_mph, y=wind_speed_10m_mph-WindSpeed))+geom_point()

ggplot(Wind_comp, aes(x=wind_speed_10m_mph, y=Diff_wind))+geom_point()
ggplot(Wind_comp, aes(x=Diff_wind))+geom_histogram(bins=500)
ggplot(Wind_comp_low, aes(x=Prop_wind))+geom_histogram(bins=500)
ggplot(Wind_comp_high, aes(x=Prop_wind))+geom_histogram(bins=500)
### Wind speed can be zero. Filter out the zeros
Wind_comp_high_no_zero <- Wind_comp_high %>% filter(wind_speed_10m_mph>0 & WindSpeed>0)
Wind_comp_low_no_zero <- Wind_comp_low %>% filter(wind_speed_10m_mph>0 & WindSpeed>0)
ggplot(Wind_comp_high_no_zero, aes(x=Prop_wind))+geom_histogram(bins=500)
ggplot(Wind_comp_low_no_zero, aes(x=Prop_wind))+geom_histogram(bins=500)

ggplot(Wind_comp_high_no_zero, aes(x=Diff_wind))+geom_histogram(bins=500)
ggplot(Wind_comp_low_no_zero, aes(x=Diff_wind))+geom_histogram(bins=500)
median(Wind_comp_low_no_zero$Diff_wind)
median(Wind_comp_high_no_zero$Diff_wind)
mean(Wind_comp_low_no_zero$Diff_wind)
mean(Wind_comp_high_no_zero$Diff_wind)

### Windspeed and CUPS is tricky.Even trickier when the screen is wet.
Wind_comp_no_zero <- Wind_comp %>% filter(wind_speed_10m_mph>0 & WindSpeed>0)
ggplot(Wind_comp_no_zero, aes(x=Prop_wind))+geom_histogram(bins=500) + scale_x_continuous(limits=c(-2.5,1))+theme_classic()
ggplot(Wind_comp_no_zero, aes(x=Diff_wind))+geom_histogram(bins=500)

Wind_comp_rain <-Wind_comp %>% filter(rain_2m_inches>0 & wind_speed_10m_mph>0)
ggplot(Wind_comp_rain, aes(x=Prop_wind))+geom_histogram(bins=500) + scale_x_continuous(limits=c(-2.5,.99))+theme_classic()
ggplot(Wind_comp_rain, aes(x=Diff_wind))+geom_histogram(bins=500) + scale_x_continuous(limits=c(-2.5,10))+theme_classic()
ggplot(Wind_comp_rain, aes(x=wind_speed_10m_mph,y=Diff_wind)) + geom_point() #When there is rain, 


Wind_comp_rain <-Wind_comp %>% filter(rain_2m_inches>0.2 & wind_speed_10m_mph>0)
ggplot(Wind_comp_rain, aes(x=Diff_wind))+geom_histogram(bins=500) + scale_x_continuous(limits=c(-2.5,10))+theme_classic()


Wind_comp$dry <- ifelse(Wind_comp$rain_2m_inches>0,1,0)
mean(ifelse(Wind_comp$wind_speed_10m_mph == Wind_comp$WindSpeed,1,0))  #Almost never at 0.3% equal
ggplot(Wind_comp, aes(x=Diff_wind))+geom_histogram(bins=500) + scale_x_continuous(limits=c(-2.5,10))+theme_classic()+facet_wrap(~dry)






#CUPS inertia: Do we care? How long must a wind last to get CUPS up to speed?
#Wind is not constant, so 
#CUPS sustained.
    


# WindRose.R
#require(ggplot2)
#require(RColorBrewer)
#require(scales)
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
if(!require(scales)){install.packages("scales")}
library(RColorBrewer)
library(scales)

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 22.5,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3, n.colors.in.range), min(9, n.colors.in.range)),
                                            palette))(n.colors.in.range)
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq, max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq])),
                    paste(spdmax, "-", max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
    
  }  
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned
                           ,y = (..count..)/sum(..count..)
                       ))+
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","NE","ENE", "E", 
                                "ESE", "SE","SSE", 
                                "S","SSW", "SW","WSW", "W", 
                                "WNW","NW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) + 
    scale_y_continuous(labels = percent) +
    ylab("Frequencia")
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  # print the plot
  print(p.windrose)  
  # return the handle to the wind rose
  return(p.windrose)
}

plot.windrose(data=WindMax, spd=wind_speed_10m_mph, dir=wind_direction_10m_deg)













openair::windRose(mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA,
                  ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
                  = "default", grid.line = NULL, width = 1, seg = NULL, auto.text
                  = TRUE, breaks = 4, offset = 10, normalise = FALSE, max.freq =
                    NULL, paddle = TRUE, key.header = NULL, key.footer = "(m/s)",
                  key.position = "bottom", key = TRUE, dig.lab = 5, statistic =
                    "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
                    315, border = NA, ...)
sort(unique(WeatherMax$wind_direction_10m_deg))
if(!require(openair)){install.packages("openair")}
library(openair)
head(WeatherMax$wind_speed_10m_mph)
windRose(WeatherMax, ws=wind_speed_10m_mph, wd=wind_direction_10m_deg)
, ws2 = NA, wd2 = NA,
ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
= "default", grid.line = NULL, width = 1, seg = NULL, auto.text
= TRUE, breaks = 4, offset = 10, normalise = FALSE, max.freq =
  NULL, paddle = TRUE, key.header = NULL, key.footer = "(m/s)",
key.position = "bottom", key = TRUE, dig.lab = 5, statistic =
  "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
  315, border = NA, ...)

