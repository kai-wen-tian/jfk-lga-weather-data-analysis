# read the csv file
weatherdata = read.csv(file = "D:/4. Hourly weather data.csv", header=TRUE, sep=",")
head(weatherdata)
#Install & load ggplot2 package
install.packages("ggplot2")
install.packages("dplyr")
install.packages("magrittr")
installed.packages("dplyr")
library(ggplot2)
library(dplyr)
library(magrittr)
library(dplyr)

summary(weatherdata)

#weatherdata pre-processing
# TO fill na for wind_dir using median
wind_dirContainNA = filter(weatherdata , is.na(wind_dir)) %>%
  select(month) %>%
  unique()

weatherdata = mutate(weatherdata, wind_dir_n=wind_dir)

for (i in wind_dirContainNA$month){
  fdwd= filter(weatherdata, month==i, !is.na(wind_dir)) %>%
    select(wind_dir)
  
  mid= median(fdwd$wind_dir, na.rm=TRUE)
  
  weatherdata= mutate(weatherdata,
               wind_dir_n=
                 ifelse(is.na(wind_dir)& month ==i, mid , wind_dir_n))
  
}

#To fill na for wind_speed by mean
wind_speedContainNA = filter(weatherdata , is.na(wind_speed)) %>%
  select(month) %>%
  unique()

weatherdata = mutate(weatherdata, wind_speed_n=wind_speed)

for (i in wind_speedContainNA$month){
  fdws= filter(weatherdata, month==i, !is.na(wind_speed)) %>%
    select(wind_speed)
  
  avgws= mean(fdws$wind_speed, na.rm=TRUE)
  
  weatherdata= mutate(weatherdata,
               wind_speed_n=
                 ifelse(is.na(wind_speed)& month ==i, avgws , wind_speed_n))
}

#Fill na for wind_gust using mean
Wind_gustContainNA=filter(weatherdata, is.na(wind_gust)) %>% 
  select(month) %>% 
  unique()

weatherdata = mutate(weatherdata, wind_gust_n=wind_gust)

for (i in Wind_gustContainNA$month){
  fdwg= filter(weatherdata, month==i, !is.na(wind_gust)) %>% 
    select(wind_gust)
  avgwg = mean(fdwg$wind_gust, na.rm = TRUE)
  weatherdata = mutate(weatherdata, 
                wind_gust_n=
                  ifelse(is.na(wind_gust) & month==i, avgwg, wind_gust_n))
}

# To fill na for pressure
pressureContainNA=filter(weatherdata, is.na(pressure)) %>% 
  select(month) %>% 
  unique()

weatherdata = mutate(weatherdata, pressure_n=pressure)

for (i in pressureContainNA$month){
  fdp= filter(weatherdata, month==i, !is.na(pressure)) %>% 
    select(pressure)
  avgp = mean(fdp$pressure, na.rm = TRUE)
  weatherdata = mutate(weatherdata, 
                pressure_n=
                  ifelse(is.na(pressure) & month==i, avgp, pressure_n))
}


# Analysis example 1
# In this example, an analysis between month and the mean of the humidity for a month is given by showing their variation.
datamh1= weatherdata %>%
  group_by(month)%>%
  summarise(avghm= mean(humid))%>%
  ggplot(aes(x=as.factor(month),y=avghm,group=1))+ geom_line()+ 
  labs(title ="Line Plot between Month and Humidity", x="Month", y="Humidity") + theme_bw()

pdmh1=ggplot_build((datamh1))


# Analysis example 2
# In this example, an analysis of the dew point is given by showing its distribution
weatherdata = mutate(weatherdata, dewp_category=dewp)
weatherdata = mutate(weatherdata, dewp_category=ifelse(weatherdata$dewp <= 55.4,"Pleasant",
                                                  ifelse(weatherdata$dewp >=55.5 & weatherdata$dewp<=60.4,"Comfortable",
                                                    ifelse(weatherdata$dewp >=60.5 & weatherdata$dewp<=65.4,"Sticky",
                                                      ifelse(weatherdata$dewp >=65.5 & weatherdata$dewp<=70.4,"Uncomfortable",
                                                        ifelse(weatherdata$dewp >=70.5 & weatherdata$dewp<=75.4,"Oppresive",
                                                          ifelse(weatherdata$dewp >= 75.5,"Miserable",dewp_category)))))))

positions <- c("Pleasant","Comfortable","Sticky","Uncomfortable","Oppresive","Miserable")
datadp2=ggplot(weatherdata, aes(x=dewp_category, fill=dewp_category))+ scale_x_discrete(limits = positions) +geom_bar()+ 
  scale_y_continuous( breaks = seq(0, 12000, by = 1000))+
  labs(title ="Bar chart of dew point category ", x="Dew Point Category", y="Count")
pddp2=ggplot_build(datadp2)
bar_datapd2 = data.frame(x = pddp2$data[[1]]$x, y = pddp2$data[[1]]$y)

# Analysis example 3
# In this example, an analysis of of wind gust for each origin is given by showing its distribution
datawg3=weatherdata%>%
  ggplot(aes(x=wind_gust_n))+
  geom_histogram(binwidth= 4, col="black" , fill="blue") + 
  labs(title ="Histogram of Wind gust for each origin ", x="Wind Gust (mph)", y="Frequency")+
  facet_wrap(~origin)+ coord_cartesian(xlim = c(15, 70))

pdwg3=ggplot_build(datawg3)
hist_datawg3=data.frame(panel=pdwg3$data[[1]]$PANEL, xmin=pdwg3$data[[1]]$xmin,xmax=pdwg3$data[[1]]$xmax,y=pdwg3$data[[1]]$y)


#4 Analysis example 4
# In this example, an analysis of temperature in October is given by showing its distribution
datat4=weatherdata %>%
  filter(month==10)%>%
  ggplot(aes(x=temp))+geom_histogram(binwidth=3, col="black", fill="darkgreen")+ 
  labs(title ="Histogram of temperature in October ", x="Temperature (F)", y="Frequency")

pdt4=ggplot_build(datat4)
hist_datat4= data.frame(xmin=pdt4$data[[1]]$xmin,xmax=pdt4$data[[1]]$xmax,y=pdt4$data[[1]]$y)

#5 Analysis example 5
# In this example, an analysis of daily pressure is given by showing its variation
datap5 = weatherdata %>%
  group_by(month,day) %>%
  summarise(avgp5= mean(pressure_n))%>%
  ggplot(aes(x=avgp5)) + geom_histogram(binwidth=3,col="white", fill="blue") + 
  scale_x_continuous( breaks = seq(990, 1050, by = 10))+
  labs(title = "Histogram of daily average pressure", x = "Pressure (millibars)")
pdp5 = ggplot_build(datap5)
hist_datap5 = data.frame(xmin = pdp5$data[[1]]$xmin, xmax = pdp5$data[[1]]$xmax, y = pdp5$data[[1]]$y)

#6 Analysis example 6
# In this example, an analysis of daily average humidity is given by showing its distribution
datah6 = weatherdata %>%
  group_by(month,day)%>%
  summarise(avgdailyh=mean(humid))%>%
  ggplot(aes(x=avgdailyh))+
  geom_freqpoly(binwidth=5)+
  labs(title = "Frequency Polygon of average daily humidity", x="Average daily humidity")+
  theme_minimal()
pdh6=ggplot_build(datah6)
fre_datah6 = data.frame(xmin = pdh6$data[[1]]$xmin, xmax = pdh6$data[[1]]$xmax, y = pdh6$data[[1]]$y)

#7 Analysis example 7
# In this example, an analysis of wind speed in January, March and June for each origin is given by showing its distribution
dataws7= weatherdata %>%
  filter(month==1| month==3 |month==6 )%>%
  ggplot(aes(x=wind_speed_n,color=origin))+
  geom_freqpoly(binwidth=5)+ facet_wrap(~month)+
  labs(title = "Frequency Polygon of wind speed in January, March and June for both origin", x="wind speed (mph)")

pdws7=ggplot_build(dataws7)
fre_dataws7=data.frame(panel=pdws7$data[[1]]$PANEL,group=pdws7$data[[1]]$group, xmin=pdws7$data[[1]]$xmin,xmax=pdws7$data[[1]]$xmax,y=pdws7$data[[1]]$y)

#8 Analysis example 8
# In this example, an analysis of wind direction is given by showing its distribution
datawd8= ggplot(weatherdata, aes(x=wind_dir_n))+geom_freqpoly(binwidth=25)+
  labs(title= "Frequency Polygon of wind direction ", x="Wind direction (degrees)")

pdwd8=ggplot_build(datawd8)
fre_datah8=data.frame(panel=pdwd8$data[[1]]$PANEL, xmin=pdwd8$data[[1]]$xmin,xmax=pdwd8$data[[1]]$xmax,y=pdwd8$data[[1]]$y)


#9 Analysis example 9
# In this example, an analysis of dew point in September is given by showing its variation
datad9= weatherdata%>%
  filter(month==9)%>%
  ggplot(aes(y=dewp,x=1))+geom_boxplot()+labs(title="Boxplot of dew point in September",y="Dew Point (F)")
pdd9=ggplot_build(datad9)
box_datad9=data.frame(ymin=pdd9$data[[1]]$ymin, lower=pdd9$data[[1]]$lower,middle=pdd9$data[[1]]$middle,upper=pdd9$data[[1]]$upper,
                      ymax=pdd9$data[[1]]$ymax)
box_datad9_outlier= pdd9[["data"]][[1]][["outliers"]]

#10 Analysis example 10
# In this example, an analysis of wind speed for each origin is given by showing its distribution
dataws10=ggplot(weatherdata, aes(y = wind_speed_n, x= origin))+geom_boxplot() + 
  labs(title = "Boxplot of wind speed for each origin", x="Origin", y="Wind Speed (mph)")
pdws10=ggplot_build(dataws10)
box_dataws10=data.frame(ymin=pdws10$data[[1]]$ymin, lower=pdws10$data[[1]]$lower,middle=pdws10$data[[1]]$middle,upper=pdws10$data[[1]]$upper,
                      ymax=pdws10$data[[1]]$ymax)
box_dataws10_outlier = pdws10[["data"]][[1]][["outliers"]] 

#11 Analysis example 11
# In this example, an analysis of daily maximum pressure for months is given by showing its variation
datap11=weatherdata%>%
  group_by(origin,month,day)%>%
  summarise(maxtempday=max(temp),.groups='drop')%>%
  ggplot(aes(x=as.factor(month), y= maxtempday))+
  geom_boxplot(aes(group=cut_width(month,1)))+
  labs(title="Boxplot of daily maximum pressure for months", x="Month", y="Daily Maximum Tempature (F)")

pdp11=ggplot_build(datap11)
box_datap11=data.frame(ymin=pdp11$data[[1]]$ymin, lower=pdp11$data[[1]]$lower,middle=pdp11$data[[1]]$middle,upper=pdp11$data[[1]]$upper,
                        ymax=pdp11$data[[1]]$ymax)
box_datap11_outlier =pdp11[["data"]][[1]][["outliers"]]


#12 Analysis example 12
#In this example, an analysis between temperature and dewpoint is given by showing their co-variation
ggplot(weatherdata, aes(x=temp, y =dewp))+ geom_point()+geom_smooth(method="lm", formula='y ~ x')+
  labs(title = "Scatterplot between temperature and dewpoint in Farenheit (F)", x="Temperature (?F)", y="Dewpoint (?F)")

cor(x=weatherdata$temp,y=weatherdata$dewp, use= "complete.obs")



#13 Analysis example 13
#In this example, an analysis between wind gust and temperature for John F. Kennedy International Airport (JFK) is given by showing their co-variation
weatherdata%>%
  filter(origin=="JFK")%>%
  ggplot(aes(x=wind_gust_n, y=temp))+geom_point()+ 
  geom_smooth(method="lm", se=FALSE, formula = 'y~x')+
  labs(title = "Scatterplot between wind gust and temperature", x="Wind gust (mph)", y="Temperature (?F)")

datawgt13=weatherdata%>%
  filter(origin=="JFK")%>%
  select(wind_gust_n,temp)


cor(x=datawgt13$wind_gust_n,y=datawgt13$temp, use= "complete.obs")


#14 Analysis example 14
#In this example, an analysis between precipitation and visibility for LaGuardia Airport (LGA) is given by showing their co-variation
weatherdata %>%
  filter(origin=="LGA")%>%
  ggplot(aes(x=precip, y=visib))+geom_point()+
  geom_smooth(method="lm", se=FALSE, formula = 'y~x')+
  labs(title = "Scatterplot between precipitatoin and visibility for LaGuardia Airport (LGA)", x="Precipitation (inches)", y="Visibility (miles)")

datapv14= weatherdata%>%
  filter(origin=="LGA")%>%
  select(precip,visib)

cor(x=datapv141$precip,y=datapv141$visib, use= "complete.obs")



#15 Analysis example 15
#In this example, an analysis between temperature and wind speed is given by showing their co-variation
weatherdata%>%
  ggplot(aes(x=temp, y=wind_speed_n)) +geom_point()+
  geom_smooth(method="lm", se=FALSE, formula = 'y~x')+
  labs(title = "Scatterplot between temperature and wind speed", x="Temperature (?F)", y="Wind Speed (mph)")

cor(x=weatherdata$temp,y=weatherdata$wind_speed_n, use= "complete.obs")



#1 Additional features 1
#In this example, an analysis between pressure and humidity is given by showing their co-variation
#Scatterplot between pressure and humidity
weatherdata %>%
  ggplot(aes(x=pressure_n, y=humid))+geom_point()+
  geom_smooth(method="lm", se=FALSE, formula = 'y~x')+
  labs(title = "Scatterplot between pressure and humidity", x="Pressure (millibars)", y="Humidity")

#2D density plot between pressure and humidity
weatherdata%>%
  ggplot(aes(x=pressure_n, y=humid))+geom_density2d()+
  geom_smooth(method="lm", se=FALSE, formula = 'y~x')+
  labs(title = "2D density plot between pressure and humidity", x="Pressure (millibars)", y="Humidity")

#2D density plot with viridis color map between pressure and humidity 
install.packages("viridis")
library(viridis)
datapha1=weatherdata%>%
  ggplot(aes(x=pressure_n, y=humid)) + 
  stat_density_2d(geom="tile",
                  aes(fill=..density..),
                  contour=FALSE)+
  scale_fill_viridis()+
  labs(title = "2D density plot with viridis color map between pressure and humidity", x="Pressure (millibars)", y="Humidity")+
  geom_smooth(method="lm", se=FALSE, formula = 'y~x')

pdpha1=ggplot_build(datapha1)
dpv_datappha1=data.frame(density=pdpha1$data[[1]]$density, x=pdpha1$data[[1]]$x,y=pdpha1$data[[1]]$y)
cor(x=weatherdata$pressure_n,y=weatherdata$humid, use= "complete.obs")

#It is hard to see the scatterplot when the data set is large as they will over lap each other
#The highest density in the graph cannot be easily identified as they are mostly black dots.
#With 2D density plot, the highest density in the graph can be identified much easily. However, it is still not very obviously.
#With 2D density plot with viridis color map, the highest density can be identified immediately, as there are color and density scale to visualize the graph.



#2 Additional features 2
#In this example, analysis between dew point and temperature, dew point and humidity as well as temperature and humidity are given by showing their co-variation.
install.packages("scatterplot3d")
library(scatterplot3d)

#3d Scatterplot
data3da2 = weatherdata %>%
  group_by(month,day)%>%
  summarise(avgdp=mean(dewp),avgtemp=mean(temp),avgh=mean(humid))
#Without type="h"
with(data3da2,{
  scatterplot3d(x=avgdp, y=avgtemp,z=avgh,
                color = "steelblue",pch=16,
                main="3-D Scatterplot of dewpoint, temperature, visibility",
                xlab="Average daily Dewpoint",
                ylab="Average daily Temperature",
                zlab = "Average daily Visibility")
})
#With type="h"
with(data3da2,{
  scatterplot3d(x=avgdp,y=avgtemp, z=avgh,
                color = "steelblue",pch=16,type="h",
                main="3-D Scatterplot of dewpoint, temperature, visibility",
                xlab="Average daily Dewpoint",
                ylab="Average daily Temperature",
                zlab = "Average daily Visibility")
})

cor(data3da2)
summary (weatherdata)
weatherdata=mutate(weatatherdata, )

# Scatterplot only allows 2 variables to be plotted 
#3D scatterplot allows 3 variables to be plotted
# With and without the function type h can make a difference
# With the function type h, there will be line for each point in the graph which makes it easy to read the x,y and z axis and it helps the graph to be analyzed easily.