library(data.table)
trip_data = read.csv("2014_trip_data.csv",header=TRUE)
print(names(trip_data))

print(length(trip_data$Trip.ID))

print(summary(trip_data$Duratio/60))

print(mean(trip_data$Duration/60,trim =0.01))

start_stn=unique(trip_data$Start..Terminal)
print(sort(start_stn ))

print(length(start_stn))

end_stn = unique(trip_data$End.Terminal)
print(sort(end_stn))
print(length(end_stn))
  
require(plyr)
library(dplyr)

res=filter(trip_data,Start..Terminal==1010,End.Terminal==1015)

head(res)

trip_data_sorted=unique(arrange(trip_data,Start..Station,End.Station))

head(trip_data_sorted)

trip_data_sorted=arrange(trip_data,desc(Start..Station),End.Station)
head(res)


byStartStation=unique(group_by(trip_data,Start..Station))

res=summarise(byStartStation,count=n(),time=mean(Duration)/60)
print(res)

library(ggplot2)
ggplot(res, aes(x=count, y=time, label=Start..Station))+
  geom_point(color= "red", size=3)+
  geom_text(hjust=0, vjust=1, size=3)+
  labs(title = "Trip Count vs Average Duration",
       x = "Trip Count", y = "Avg Trip Duration (minutes)")

