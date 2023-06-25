##Lets Load the Data set
bikes = read.csv("bikeshare copy.csv")
head(bikes) ##We are trying to predict the total count of bikes rented during each hour covered by the set


##Lets visualize our data to find anything interesting 
ggplot(bikes, aes(x = temp, y = count)) + geom_point(aes(color = temp), alpha = 0.2)
view(bikes)
?as.data.frame.POSIXct()
##Converting the date time format to date 
bikes$datetime = date(bikes$datetime)
head(bikes$datetime)

strip.tz(bikes$datetime)
#Lets plot datetime versus count
ggplot(bikes, aes(x = datetime, y = count)) + geom_point(aes(color = temp), alpha = 0.4)+
  scale_color_gradient(low = "#009ed3",high = "red")

##Correlations between variables
new.df = dplyr::select(bikes,temp,count)
cor.data = cor(new.df)
cor.data

##There is a slight correlation between temp and count of bikes. Lets investigate further
names(bikes)
ggplot(bikes, aes(x = factor(season), y = count)) + geom_boxplot(aes(color = factor(season)))

##Since we want to predict the count at each hour, lets create an hour column
bikes = bikes %>% 
  mutate("hour" = format(as.POSIXct(datetime), format = "%H"))
head(bikes)

## Now plot count versus hours on working days and non working days

pl1 = bikes %>% 
  subset(workingday == 1) %>% 
  ggplot(aes(x = hour, y = count)) + geom_point(aes(color = temp),alpha=0.5,position = position_jitter(width = 1, height = 0))+
  scale_color_gradientn(colors =c("blue","#3240a8","#40e0d0","#32cd32","#feff00","#ffd700","#ff494d")); pl1

#Non working days
pl2 = bikes %>% 
  subset(workingday == 0) %>% 
  ggplot(aes(x = hour, y = count)) + geom_point(aes(color = temp),alpha=0.5,position = position_jitter(width = 1, height = 0))+
  scale_color_gradientn(colors =c("blue","#3240a8","#40e0d0","#32cd32","#feff00","#ffd700","#ff494d")); pl2

#Building Models
temp.model = lm(count~temp, data = bikes)
summary(temp.model)

##How many bike rentals would we predict if the temp was 25 degrees Celsius
predict(temp.model,data.frame(temp = 25), interval = "confidence")

##Lets change the hour column to a column of numeric values
head(bikes)
bikes$hour = sapply(bikes$hour, as.numeric)
head(bikes)

str(bikes$hour)
?factor()

#Building a new model
 new.model = lm(count~season+holiday+workingday+weather+temp+humidity+windspeed+hour,bikes)
summary(new.model)
par(mfrow = c(2,2))
plot(new.model)




