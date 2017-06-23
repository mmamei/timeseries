library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

file1 = "../Maranello_20170601_20170622"
file2 = "../Maranello_20170608_20170613"

data = read.csv(file1,header=FALSE,stringsAsFactors=FALSE, na.strings = c("65535"))
names(data) = c("time","cell","value")
data$rtime = as.POSIXct(fast_strptime(data$time, "%Y%m%d_%H%M"))

select = c(
  "486-1004",
  "456-989",
  "458-983"
)
x = filter(data, cell %in% select)


# REMOVE OUTLIERS

#for (c in select){
#  outlier = boxplot.stats(x[x$cell==c,]$value)$out
#  z = x[x$cell==c,]$value
#  z = ifelse(z %in% outlier, NA, z)
#  x[x$cell==c,]$value = z
#}

outs = unnest(x %>% group_by(cell) %>% do(out=boxplot.stats(.$value)$out))
x = left_join(x,outs,by=c("cell"="cell","value"="out"))

#x = filter(x, rtime > "2017-06-9 00:00:00")
#x = filter(x, rtime < "2017-06-9 23:00:00")

plot(x$value)


ggplot(data=x,aes(x=rtime,y=value,colour=cell,group=cell))+geom_point()+geom_line()



####################################################
# PRINT AGGREGATE

x$weekday = weekdays(x$rtime)
x$hm = format(x$rtime,"%H%M")

da = x[,c('cell','value','hm')]
dailyAvg = aggregate( . ~ cell + hm , data = da, mean)

ggplot(data=dailyAvg,aes(x=hm,y=value,colour=cell,group=cell))+geom_point()+geom_line()


