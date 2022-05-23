
### WORKING DIRECTORY AND PACKAGES
###########################################################
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(RColorBrewer) #display.brewer.all()
library(zoo)
library(corrplot) 
library(lme4) 
library(lubridate)
library(car)

# info from buoys loaded separately,then, all these files will be joined 
# with the prev dataset that will be uploaded last

###########################################################
### BELLINGHAM
###########################################################
data <- read.table("Bellingham buoy/CSVs/Bellingham_buoy.csv", header=T,sep=",")
colnames(data) <- c('Date','DOsat', 'SST','Salinity','DOconc', 'Turbidity','Chlorophyll')
data <- mutate(data, Buoy = "Bellingham") # adding col for buoy
data$Date <- as.Date(data$Date) 

# splitting into seasons and year
data <- as.data.frame(data)
yq <- as.yearqtr(as.yearmon(data$Date, "%m/%d/%Y") + 1/12)
data$Season <- factor(format(yq, "%q"), levels = 1:4, 
                      labels = c("Winter", "Spring", "Summer", "Fall"))
data <- data %>% separate(Date, sep="-", into = c("Year"))

todrop <- which(data$Salinity > 50) 
data <- data[-todrop,]
data$ID <- paste(data$Year, data$Season, data$Buoy)

# creating summary table 
# data.summ <- data %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# data.summ$ID <- paste(data.summ$Year, data.summ$Season, data.summ$Buoy)

# plotting env data just to check
# plot <-  ggplot(data, aes(x= Season, y = Salinity, color=Salinity, show.legend = FALSE, fill=Season), na.rm=T)  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm=T) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm=T) 
# plot + theme_classic(base_size = 18) 

###########################################################
### Carr Inlet
###########################################################
carr <- read.table("Carr Inlet/CSVs/Carr.csv", header=T,sep=",")
colnames(carr) <- c('Date','pH', 'Salinity', 'SST')
carr <- mutate(carr, Buoy = "Carr Inlet") # adding col for buoy
carr$Date <- mdy(carr$Date) 

# splitting into seasons and year
carr <- as.data.frame(carr)
yq <- as.yearqtr(as.yearmon(carr$Date, "%m/%d/%Y") + 1/12)
carr$Season <- factor(format(yq, "%q"), levels = 1:4, 
                      labels = c("Winter", "Spring", "Summer", "Fall"))
carr <- carr %>% separate(Date, sep="-", into = c("Year"))

carr$ID <- paste(carr$Year, carr$Season, carr$Buoy)

# creating summary table 
# carr.summ <- carr %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# carr.summ$ID <- paste(carr.summ$Year, carr.summ$Season, carr.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(carr, aes(x= Season, y= Salinity, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18) 

###########################################################
### Cherry Point
###########################################################
cherry <- read.table("cherry point/CSVs/cherry point SST.csv", header=T,sep=",")
colnames(cherry) <- c('Date','SST')
cherry <- mutate(cherry, Buoy = "Cherry Point") # adding col for buoy
cherry$Date <- mdy(cherry$Date) 

# splitting into seasons and year
cherry <- as.data.frame(cherry)
yq <- as.yearqtr(as.yearmon(cherry$Date, "%m/%d/%Y") + 1/12)
cherry$Season <- factor(format(yq, "%q"), levels = 1:4, 
                        labels = c("Winter", "Spring", "Summer", "Fall"))
cherry <- cherry %>% separate(Date, sep="-", into = c("Year"))
cherry$ID <- paste(cherry$Year, cherry$Season, cherry$Buoy)

# creating summary table 
# cherry.summ <- cherry %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# cherry.summ$ID <- paste(cherry.summ$Year, cherry.summ$Season, cherry.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(cherry, aes(x= Season, y= SST, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18) 

###########################################################
### Hoodsport
###########################################################
hoodsport <- read.table("Hoodsport/CSVs/Hoodsport.csv", header=T, sep=",")
colnames(hoodsport) <- c('Date','SST', 'Salinity', 'pH')
hoodsport <- mutate(hoodsport, Buoy = "Hoodsport") # adding col for buoy
hoodsport$Date <- mdy(hoodsport$Date) 

# splitting into seasons and year
hoodsport <- as.data.frame(hoodsport)
yq <- as.yearqtr(as.yearmon(hoodsport$Date, "%m/%d/%Y") + 1/12)
hoodsport$Season <- factor(format(yq, "%q"), levels = 1:4, 
                           labels = c("Winter", "Spring", "Summer", "Fall"))
hoodsport <- hoodsport %>% separate(Date, sep="-", into = c("Year"))
hoodsport$ID <- paste(hoodsport$Year, hoodsport$Season, hoodsport$Buoy)

# creating summary table 
# hoodsport.summ <- hoodsport %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# hoodsport.summ$ID <- paste(hoodsport.summ$Year, hoodsport.summ$Season, hoodsport.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(hoodsport, aes(x= Season, y= SST, color=Season, show.legend= FALSE, fill= Season))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE) 
# plot + theme_classic(base_size = 18) 

###########################################################
### Port Townsend
###########################################################
ptownsend <- read.table("port townsend/CSVs/PortTownsend.csv", header=T,sep=",")
colnames(ptownsend) <- c('Date','SST')
ptownsend <- mutate(ptownsend, Buoy = "Port Townsend") # adding col for buoy
ptownsend$Date <- mdy(ptownsend$Date) 

# splitting into seasons and year ## FIX
ptownsend <- as.data.frame(ptownsend)
yq <- as.yearqtr(as.yearmon(ptownsend$Date, "%m/%d/%Y") + 1/12)
ptownsend$Season <- factor(format(yq, "%q"), levels = 1:4, 
                           labels = c("Winter", "Spring", "Summer", "Fall"))
ptownsend <- ptownsend %>% separate(Date, sep="-", into = c("Year"))
ptownsend$ID <- paste(ptownsend$Year, ptownsend$Season, ptownsend$Buoy)

# creating summary table 
# ptownsend.summ <- ptownsend %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# ptownsend.summ$ID <- paste(ptownsend.summ$Year, ptownsend.summ$Season, ptownsend.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(ptownsend, aes(x= Season, y= SST, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### Tokeland
###########################################################
tokeland <- read.table("tokeland/CSVs/Tokeland.csv", header=T, sep=",")
colnames(tokeland) <- c('Date','SST', 'Salinity', 'DO', 'pH')
tokeland <- mutate(tokeland, Buoy = "Tokeland") # adding col for buoy
tokeland$Date <- mdy(tokeland$Date) 

# splitting into seasons and year
tokeland <- as.data.frame(tokeland)
yq <- as.yearqtr(as.yearmon(tokeland$Date, "%m/%d/%Y") + 1/12)
tokeland$Season <- factor(format(yq, "%q"), levels = 1:4, 
                          labels = c("Winter", "Spring", "Summer", "Fall"))
tokeland <- tokeland %>% separate(Date, sep="-", into = c("Year"))
tokeland$ID <- paste(tokeland$Year, tokeland$Season, tokeland$Buoy)

# creating summary table 
# tokeland.summ <- tokeland %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# tokeland.summ$ID <- paste(tokeland.summ$Year, tokeland.summ$Season, tokeland.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(tokeland, aes(x= Season, y= pH, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### Bayview
###########################################################
bayview <- read.table("Bayview channel/CSVs/Bay view channel.csv", header=T, sep=",")
colnames(bayview) <- c('Date','SST', 'Salinity', 'DO', 'depth','pH')
bayview <- mutate(bayview, Buoy = "Bayview") # adding col for buoy
bayview$Date <- mdy(bayview$Date) 

# splitting into seasons and year 
bayview <- as.data.frame(bayview)
yq <- as.yearqtr(as.yearmon(bayview$Date, "%m/%d/%Y") + 1/12)
bayview$Season <- factor(format(yq, "%q"), levels = 1:4, 
                         labels = c("Winter", "Spring", "Summer", "Fall"))
bayview <- bayview %>% separate(Date, sep="-", into = c("Year"))
bayview$ID <- paste(bayview$Year, bayview$Season, bayview$Buoy)
todrop <- which(bayview$Salinity < 15) 
bayview <- bayview[-todrop,]

# plotting env data just to check
# plot <- ggplot(bayview, aes(x= Season, y= Salinity, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### COOS BAY BUOY
###########################################################
coos <- read.table("Coos Bay buoy/CSVs/Coos Bay buoy.csv", header=T, sep=",")
colnames(coos) <- c('Date','SST', 'Salinity', 'DO', 'depth','pH', 'Turbidity')
coos <- mutate(coos, Buoy = "Coos") # adding col for buoy
coos$Date <- mdy(coos$Date) 

# splitting into seasons and year
coos <- as.data.frame(coos)
yq <- as.yearqtr(as.yearmon(coos$Date, "%m/%d/%Y") + 1/12)
coos$Season <- factor(format(yq, "%q"), levels = 1:4, 
                      labels = c("Winter", "Spring", "Summer", "Fall"))
coos <- coos %>% separate(Date, sep="-", into = c("Year"))
coos$ID <- paste(coos$Year, coos$Season, coos$Buoy)

# creating summary table 
# coos.summ <- coos %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# coos.summ$ID <- paste(coos.summ$Year, coos.summ$Season, coos.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(coos, aes(x=Season, y=pH, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### BODEGA BAY BUOY
###########################################################
mydir <- 'bodega bay/CSVs'
env.files <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
env.files # check the list of files is okay

bodega <- ldply(env.files, read_csv) # read all the files 
colnames(bodega) <- c('Date','Salinity','SST')
bodega <- mutate(bodega, Buoy = "Bodega") # adding col for buoy
bodega$Date <- as.Date(bodega$Date) 

# splitting into seasons and year ## FIX
bodega <- as.data.frame(bodega)
yq <- as.yearqtr(as.yearmon(bodega$Date, "%m/%d/%Y") + 1/12)
bodega$Season <- factor(format(yq, "%q"), levels = 1:4, 
                        labels = c("Winter", "Spring", "Summer", "Fall"))
bodega <- bodega %>% separate(Date, sep="-", into = c("Year"))
bodega$ID <- paste(bodega$Year, bodega$Season, bodega$Buoy)
todrop <- which(bodega$SST < 5) 
bodega <- bodega[-todrop,]

# creating summary table 
# bodega.summ <- bodega %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# bodega.summ$ID <- paste(bodega.summ$Year, bodega.summ$Season, bodega.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(bodega, aes(x=Season, y=SST, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### yaquina BAY BUOY
###########################################################
yaquina <- read.table("yaquina/CSVs/yaquina SST.csv", header=T, sep=",")
yaquina <- mutate(yaquina, Buoy = "Yaquina") # adding col for buoy
yaquina$Date <- mdy(yaquina$Date) 

# splitting into seasons and year
yaquina <- as.data.frame(yaquina)
yq <- as.yearqtr(as.yearmon(yaquina$Date, "%m/%d/%Y") + 1/12)
yaquina$Season <- factor(format(yq, "%q"), levels = 1:4, 
                         labels = c("Winter", "Spring", "Summer", "Fall"))
yaquina <- yaquina %>% separate(Date, sep="-", into = c("Year"))
yaquina$ID <- paste(yaquina$Year, yaquina$Season, yaquina$Buoy)

# # creating summary table 
# yaquina.summ <- yaquina %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# yaquina.summ$ID <- paste(yaquina.summ$Year, yaquina.summ$Season, yaquina.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(yaquina, aes(x=Season, y=SST, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### netarts BAY BUOY: only annual data
###########################################################
netarts <- read.table("netarts bay/CSVs/Netarts bay.csv", header=T, sep=",")
netarts <- netarts[,(5:8)] # read all the files 

colnames(netarts) <- c('pH','Salinity','SST','Year')
netarts <- mutate(netarts, Buoy = "Netarts") # adding col for buoy
netarts <- as.data.frame(netarts)
netarts$ID <- paste(netarts$Year, netarts$Season, netarts$Buoy)

# # creating summary table 
# netarts.summ <- netarts %>% group_by(Date, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# netarts.summ$ID <- paste(netarts.summ$Date)

# plotting env data just to check
# plot <- ggplot(netarts, aes(x=Year, y=pH, color=Year, show.legend=FALSE, fill= Year))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         #scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### ketchikan BAY BUOY
###########################################################
ketchikan <- read.table("Ketchikan/CSVs/ketchikan temp.csv", header=T, sep=",")
colnames(ketchikan) <- c('Date','SST')
ketchikan <- mutate(ketchikan, Buoy = "Ketchikan") # adding col for buoy
ketchikan$Date <- as.Date(ketchikan$Date) # dumping time bc I dont need it

# splitting into seasons and year ## FIX
ketchikan <- as.data.frame(ketchikan)
yq <- as.yearqtr(as.yearmon(ketchikan$Date, "%m/%d/%Y") + 1/12)
ketchikan$Season <- factor(format(yq, "%q"), levels = 1:4, 
                           labels = c("Winter", "Spring", "Summer", "Fall"))
ketchikan <- ketchikan %>% separate(Date, sep="-", into = c("Year"))
ketchikan$ID <- paste(ketchikan$Year, ketchikan$Season, ketchikan$Buoy)

# creating summary table 
# ketchikan.summ <- ketchikan %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# ketchikan.summ$ID <- paste(ketchikan.summ$Year, ketchikan.summ$Season, ketchikan.summ$Buoy)
# 
# plotting env data just to check
# plot <- ggplot(ketchikan, aes(x=Season, y=SST, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### tillamook BAY BUOY
###########################################################
tillamook <- read.table("tillamook/CSVs/tillamook SST.csv", header=T, sep=",")
tillamook <- mutate(tillamook, Buoy = "Tillamook") # adding col for buoy
tillamook$Date <- mdy(tillamook$Date) 

# splitting into seasons and year
tillamook <- as.data.frame(tillamook)
yq <- as.yearqtr(as.yearmon(tillamook$Date, "%m/%d/%Y") + 1/12)
tillamook$Season <- factor(format(yq, "%q"), levels = 1:4, 
                           labels = c("Winter", "Spring", "Summer", "Fall"))
tillamook <- tillamook %>% separate(Date, sep="-", into = c("Year"))
tillamook$ID <- paste(tillamook$Year, tillamook$Season, tillamook$Buoy)

# creating summary table 
# tillamook.summ <- tillamook %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# tillamook.summ$ID <- paste(tillamook.summ$Year, tillamook.summ$Season, tillamook.summ$Buoy)
# 
# plotting env data just to check
# plot <- ggplot(tillamook, aes(x=Season, y=SST, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### tomales BAY BUOY
###########################################################
mydir <- 'tomales/CSVs'
env.files <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
env.files # check the list of files is okay

tom <- ldply(env.files, read_csv) # read all the files 
tom <- tom[,(-3)]
colnames(tom) <- c('Date','Chlorophyll', 'pH', 'Salinity', 'SST')
tom <- mutate(tom, Buoy = "Tomales") # adding col for buoy
tom$Date <- as.Date(tom$Date) # dumping time bc I dont need it

# splitting into seasons and year
tom <- as.data.frame(tom)
yq <- as.yearqtr(as.yearmon(tom$Date, "%m/%d/%Y") + 1/12)
tom$Season <- factor(format(yq, "%q"), levels = 1:4, 
                     labels = c("Winter", "Spring", "Summer", "Fall"))
tom <- tom %>% separate(Date, sep="-", into = c("Year"))
tom$ID <- paste(tom$Year, tom$Season, tom$Buoy)

# removing weird values
todrop <- which(tom$pH < 7) 
tom <- tom[-todrop,]
todrop <- which(tom$SST < 5) 
tom <- tom[-todrop,]

# creating summary table 
# tom.summ <- tom %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# tom.summ$ID <- paste(tom.summ$Year, tom.summ$Season, tom.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(tom, aes(x=Season, y=Salinity, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18) 

###########################################################
### humboldt BAY BUOY
###########################################################
mydir <- 'humboldt/CSVs'
env.files <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
env.files # check the list of files is okay

humboldt <- ldply(env.files, read_csv) # read all the files 
colnames(humboldt) <- c('Date','Chlorophyll','pH','Salinity','SST')
humboldt <- mutate(humboldt, Buoy = "Humboldt") # adding col for buoy

# dumping time bc I dont need it
humboldt$Date <- as.Date(humboldt$Date) 
todrop <- which(humboldt$SST >50) 
humboldt <- humboldt[-todrop,]

humboldt$ID <- paste(humboldt$Year, humboldt$Season, humboldt$Buoy)

todrop <- which(humboldt$pH < 7) 
humboldt <- humboldt[-todrop,]

# splitting into seasons and year
humboldt <- as.data.frame(humboldt)
yq <- as.yearqtr(as.yearmon(humboldt$Date, "%m/%d/%Y") + 1/12)
humboldt$Season <- factor(format(yq, "%q"), levels = 1:4, 
                          labels = c("Winter", "Spring", "Summer", "Fall"))
humboldt <- humboldt %>% separate(Date, sep="-", into = c("Year"))

# creating summary table 
# humboldt.summ <- humboldt %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# humboldt.summ$ID <- paste(humboldt.summ$Year, humboldt.summ$Season, humboldt.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(humboldt, aes(x=Season, y=SST, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### homer BAY BUOY
###########################################################
homer <- read.table("Homer surface/CSVs/Homer surface.csv", header=T, sep=",")
colnames(homer) <- c('Date','SST','Salinity','DO','Depth','pH', 'Turbidity')
homer <- mutate(homer, Buoy = "Homer") # adding col for buoy
homer$Date <- mdy(homer$Date) 

# splitting into seasons and year 
homer <- as.data.frame(homer)
yq <- as.yearqtr(as.yearmon(homer$Date, "%Y/%m/%d") + 1/12)
homer$Season <- factor(format(yq, "%q"), levels = 1:4, 
                       labels = c("Winter", "Spring", "Summer", "Fall"))
homer <- homer %>% separate(Date, sep="-", into = c("Year"))
homer$ID <- paste(homer$Year, homer$Season, homer$Buoy)

# creating summary table 
# homer.summ <- homer %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# homer.summ$ID <- paste(homer.summ$Year, homer.summ$Season, homer.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(homer, aes(x=Season, y=pH, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

###########################################################
### PT ALEXANDER BUOY
###########################################################
portalex <- read.table("port alexander/CSVs/PtAlexander_SST.csv", header=T, sep=",")
colnames(portalex) <- c('Date','SST')
portalex <- mutate(portalex, Buoy = "P Alexander") # adding col for buoy
portalex$Date <- as.Date(portalex$Date) # dumping time bc I dont need it

# splitting into seasons and year
portalex <- as.data.frame(portalex)
yq <- as.yearqtr(as.yearmon(portalex$Date, "%m/%d/%Y") + 1/12)
portalex$Season <- factor(format(yq, "%q"), levels = 1:4, 
                          labels = c("Winter", "Spring", "Summer", "Fall"))
portalex <- portalex %>% separate(Date, sep="-", into = c("Year"))
portalex$ID <- paste(portalex$Year, portalex$Season, portalex$Buoy)

# creating summary table 
# portalex.summ <- portalex %>% group_by(Year, Season, Buoy) %>% summarise_all(funs(mean), na.rm=T)
# portalex.summ$ID <- paste(portalex.summ$Year, portalex.summ$Season, portalex.summ$Buoy)

# plotting env data just to check
# plot <- ggplot(portalex, aes(x=Season, y=SST, color=Season, show.legend=FALSE, fill= Season, na.rm = TRUE))  +
#         geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) + 
#         scale_fill_brewer(palette="YlGnBu") +
#         geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE) 
# plot + theme_classic(base_size = 18)

########################
## checking class for each dataset bc I cant bind them
sapply(yaquina, class)
yaquina$Year <- as.numeric(yaquina$Year)
sapply(portalex, class)
portalex$Year <- as.numeric(portalex$Year)
sapply(homer, class)
homer$Year <- as.numeric(homer$Year)
sapply(humboldt, class)
humboldt$Year <- as.numeric(humboldt$Year)
sapply(tillamook, class)
tillamook$Year <- as.numeric(tillamook$Year)
sapply(tom, class)
tom$Year <- as.numeric(tom$Year)
sapply(data, class)
data$Year <- as.numeric(data$Year)
sapply(carr, class)
carr$Year <- as.numeric(carr$Year)
sapply(cherry, class)
cherry$Year <- as.numeric(cherry$Year)
sapply(hoodsport, class)
hoodsport$Year <- as.numeric(hoodsport$Year)       
sapply(ptownsend, class)
ptownsend$Year <- as.numeric(ptownsend$Year)
sapply(tokeland, class)
tokeland$Year <- as.numeric(tokeland$Year)
sapply(bayview, class)
bayview$Year <- as.numeric(bayview$Year)
sapply(coos, class)
coos$Year <- as.numeric(coos$Year)
sapply(bodega, class)
bodega$Year <- as.numeric(bodega$Year)       
sapply(netarts, class)
netarts$Year <- as.numeric(netarts$Year)     
sapply(ketchikan, class)
ketchikan$Year <- as.numeric(ketchikan$Year)           

########################
## binding all the buoys
buoys <- bind_rows(data, carr, cherry, hoodsport, ptownsend, tokeland, bayview, coos, 
        bodega, yaquina, netarts, ketchikan, tillamook, tom, humboldt, homer, portalex)

########################
# CHECKING FOR CORRELATION IN ENV VARIABLES
buoys <- as.data.frame(buoys) # turning tibble into df
buoys.mat <- buoys[, c(2:7,11:14)] # selecting the cols with numbers

matrix <- cor(buoys.mat, use="pairwise.complete.obs")
par(xpd = TRUE)
corrplot(matrix,method="number",type="lower",tl.col="black",tl.srt=45,mar=c(0,0,0,0))

# 3 diff kinds of DO.... will keep DOsat bc I don't understand the rest
mean(buoys.mat$DOconc, na.rm=T) # 6.923538
mean(buoys.mat$DOsat, na.rm=T) # 80.66294
mean(buoys.mat$DO, na.rm=T) # 8.828491

buoys <- buoys[, c(1:4,6:11)]

#### CREATING TIBBLE W/AVERAGES PER YEAR
###########################################################
buoys_averaged <- buoys %>%
  group_by(ID) %>%
  summarize_at(vars(SST,Salinity,DOsat,Turbidity,Chlorophyll,pH),funs(mean = mean))

###########################################################
## ADDING prev DATASET
###########################################################
prev <- read.table("master_spreadsheet_all.csv", header=T, sep=",")
# remove L valves to avoid duplication of weights etc
prev <- subset(prev, prev$Valve =='R') # only keep R valves
# splitting and creating dates
prev <- prev %>% mutate(Date1 = Date)
prev$Date1 <- dmy(prev$Date1)
prev <- prev %>% separate(Date1, sep="-", into = c('Year', 'Day', 'Month'))
prev$Date <- dmy(prev$Date)
prev$Year <- as.numeric(prev$Year)
# dim: 4085, 21

prev$Shell_g <- as.numeric(prev$Shell_g)     
prev$Tissue_g <- as.numeric(prev$Tissue_g)
prev$Thick <- as.numeric(prev$Thick)
prev$x <- as.numeric(prev$x)
prev$y <- as.numeric(prev$y)
prev$z <- as.numeric(prev$z)
prev$Year <- as.numeric(prev$Year)
prev$Lat_sc <- scale(prev$Lat, center = TRUE, scale = TRUE) # scaling lat
str(prev)

### MERGING DATASETS
###########################################################
fulldata <- merge(prev, buoys_averaged, merge.by=ID, all.x = TRUE)

summarytab <- fulldata %>%
  group_by(Year, Bay, Farm) %>%
  summarize_at(vars(SST_mean,Salinity_mean,DOsat_mean,Turbidity_mean,Chlorophyll_mean,pH_mean),funs(mean = mean))

### MODEL TESTING FOR ALL STATES
###########################################################
mod <- glmer(Infested ~ pH_mean + SST_mean + Salinity_mean + (1|Year) + (1|State/Bay/Farm), family="binomial", data = fulldata)
summary(mod)
anova(mod)
vif(mod)
# + DOsat_mean + Turbidity_mean + Chlorophyll_mean

mod2 <- glmer(Infested ~ pH_mean + Salinity_mean + pH_mean*State + Salinity_mean*State + (1|Year) + (1|State/Bay/Farm), family="binomial", data = fulldata)
summary(mod2)


### MODEL TESTING FOR each STATE
###########################################################
wafulldata <- subset(fulldata, fulldata$State =='WA')
modwa <- glmer(Infested ~ pH_mean + SST_mean + Salinity_mean + (1|Year) + (1|Bay/Farm), family="binomial", data = wafulldata)
summary(modwa)

###########################################################
akfulldata <- subset(fulldata, fulldata$State =='AK')
modak <- glmer(Infested ~ pH_mean + SST_mean + Salinity_mean + (1|Year) + (1|Bay/Farm), family="binomial", data = akfulldata)
summary(modak)

###########################################################
orfulldata <- subset(fulldata, fulldata$State =='OR')
modor <- glmer(Infested ~ pH_mean + SST_mean + Salinity_mean + (1|Year) + (1|Bay), family="binomial", data = orfulldata)
summary(modor)

###########################################################
cafulldata <- subset(fulldata, fulldata$State =='CA')
modca <- glmer(Infested ~ pH_mean + SST_mean + Salinity_mean + (1|Year) + (1|Bay), family="binomial", data = cafulldata)
summary(modca)


