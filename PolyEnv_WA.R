
### PACKAGES
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
library(wesanderson)
library(viridis)
library(ggeffects)

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


# plotting env data just to check
plot <-  ggplot(data, aes(x= Season, y = Salinity, color=Salinity, show.legend = FALSE, fill=Season), na.rm=T)  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm=T) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm=T)
plot + theme_classic(base_size = 18)

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

# plotting env data just to check
plot <- ggplot(carr, aes(x= Season, y= Salinity, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE)
plot + theme_classic(base_size = 18)

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

# plotting env data just to check
plot <- ggplot(cherry, aes(x= Season, y= SST, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE)
plot + theme_classic(base_size = 18)

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

# plotting env data just to check
plot <- ggplot(hoodsport, aes(x= Season, y= SST, color=Season, show.legend= FALSE, fill= Season))  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE)
plot + theme_classic(base_size = 18)

###########################################################
### Port Townsend
###########################################################
ptownsend <- read.table("port townsend/CSVs/PortTownsend.csv", header=T,sep=",")
colnames(ptownsend) <- c('Date','SST')
ptownsend <- mutate(ptownsend, Buoy = "Port Townsend") # adding col for buoy
ptownsend$Date <- mdy(ptownsend$Date) 

# splitting into seasons and year
ptownsend <- as.data.frame(ptownsend)
yq <- as.yearqtr(as.yearmon(ptownsend$Date, "%m/%d/%Y") + 1/12)
ptownsend$Season <- factor(format(yq, "%q"), levels = 1:4, 
                           labels = c("Winter", "Spring", "Summer", "Fall"))
ptownsend <- ptownsend %>% separate(Date, sep="-", into = c("Year"))
ptownsend$ID <- paste(ptownsend$Year, ptownsend$Season, ptownsend$Buoy)

# plotting env data just to check
plot <- ggplot(ptownsend, aes(x= Season, y= SST, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE)
plot + theme_classic(base_size = 18)

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

# plotting env data just to check
plot <- ggplot(tokeland, aes(x= Season, y= pH, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE)
plot + theme_classic(base_size = 18)

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
plot <- ggplot(bayview, aes(x= Season, y= Salinity, color=Season, show.legend= FALSE, fill= Season, na.rm = TRUE))  +
        geom_boxplot(alpha=0.7, lwd=1, show.legend = FALSE, color='black', na.rm = TRUE) +
        scale_fill_brewer(palette="YlGnBu") +
        geom_point(size= 4, shape = 21, color= 'black', show.legend = FALSE, na.rm = TRUE)
plot + theme_classic(base_size = 18)


########################
## checking class for each dataset bc I cant bind them
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

########################
## binding all the buoys
buoys <- bind_rows(data, carr, cherry, hoodsport, ptownsend, tokeland,bayview)

########################
# CHECKING FOR CORRELATION IN ENV VARIABLES
buoys <- as.data.frame(buoys) # turning tibble into df
buoys.mat <- buoys[, c(2:7,11:13)] # selecting the cols with numbers

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
        group_by(Year,Buoy,ID,Season) %>% #
        summarize_at(vars(SST,Salinity,pH), list(mean = mean))

# here, to get the summary for the 2 years prior I'd have to subset
# only 2017 & 2018 and then merge it to the full dataset to have the
# prior env included

#buoys_subset <- subset(buoys_averaged, buoys_averaged$Year == "2017" | buoys_averaged$Year== "2018")
write.csv(buoys_averaged,"buoys_averaged.csv", row.names = FALSE)

###########################################################
## ADDING prev DATASET
###########################################################
prev <- read.table("master_spreadsheet_WA.csv", header=T, sep=",")
# remove L valves to avoid duplication of weights etc
prev <- subset(prev, prev$Valve =='R') # only keep R valves
# splitting and creating dates
prev <- prev %>% mutate(Date1 = Date)
prev$Date1 <- mdy(prev$Date1)
prev <- prev %>% separate(Date1, sep="-", into = c('Year', 'Month', 'Day'))
prev$Date <- mdy(prev$Date)
prev$Year <- as.numeric(prev$Year)
prev$Year_sc <- scale(prev$Year, center = TRUE, scale = TRUE) # scaling 

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
        group_by(ID) %>%
        summarize_at(vars(SST_mean,Salinity_mean,pH_mean),list(mean = mean))

fulldata$Salinity_mean_sc <- scale(fulldata$Salinity_mean, center = TRUE, scale = TRUE) # scaling lat
fulldata$SST_mean_sc <- scale(fulldata$SST_mean, center = TRUE, scale = TRUE) # scaling lat
fulldata$pH_mean_sc <- scale(fulldata$pH_mean, center = TRUE, scale = TRUE) # scaling lat
# fulldata$DOsat_mean_sc <- scale(fulldata$DOsat_mean, center = TRUE, scale = TRUE) # scaling lat
# fulldata$Turbidity_mean_sc <- scale(fulldata$Turbidity_mean, center = TRUE, scale = TRUE) # scaling lat
# fulldata$Chlorophyll_mean_sc <- scale(fulldata$Chlorophyll_mean, center = TRUE, scale = TRUE) # scaling lat

#write.csv(fulldata,"Desktop\\fulldata.csv", row.names = FALSE)

### MODEL TESTING FOR ALL STATES
###########################################################
# above I summarized the env data we used for years 2017 and 2018
# that was summarized in a csv called "buoys_averaged"
# now I'm going to merge that csv with "fulldata", merging by buoys and 
# adding 6 new cols

todrop <- which(fulldata$pH_mean_sc < -3)
fulldata <- fulldata[-todrop,]

todrop <- which(fulldata$Salinity_mean < 10)
fulldata <- fulldata[-todrop,]

### MERGING DATASETS
buoys_prior <- read.csv("buoys_prior.csv", header=T, sep=",")
fulldata2 <- merge(fulldata, buoys_prior, merge.by= Buoy, all.x = TRUE)

### SUBSETTING DATASET TO INCLUDE ONLY BUOYS WITH 2017-18 DATA
# these buoys are: Bayview, Bellingham, Bodega, Carr Inlet, Coos, Homer

dataprior <- subset(fulldata2, fulldata2$Buoy =='Bayview'|fulldata2$Buoy =='Bellingham'|
                            fulldata2$Buoy =='Carr Inlet') 

# scaling data prior
dataprior$Salinity_mean_2017_sc <- scale(dataprior$Salinity_mean_2017, center = TRUE, scale = TRUE) # scaling lat
dataprior$Salinity_mean_2018_sc <- scale(dataprior$Salinity_mean_2018, center = TRUE, scale = TRUE) # scaling lat
dataprior$SST_mean_2017_sc <- scale(dataprior$SST_mean_2017, center = TRUE, scale = TRUE) # scaling lat
dataprior$SST_mean_2018_sc <- scale(dataprior$SST_mean_2018, center = TRUE, scale = TRUE) # scaling lat
dataprior$pH_mean_2017_sc <- scale(dataprior$pH_mean_2017, center = TRUE, scale = TRUE) # scaling lat
dataprior$pH_mean_2018_sc <- scale(dataprior$pH_mean_2018, center = TRUE, scale = TRUE) # scaling lat

## plotting pH FULLDATA
phplot <- ggplot(fulldata,aes(x=Year, y=pH_mean, fill=Year, group=Year)) +
        #scale_color_manual(values=wes_palette("GrandBudapest1", n = 3)) + 
        geom_boxplot(alpha=0.7, lwd=1, outlier.shape = NA, show.legend = FALSE) + 
        geom_point(stat = "identity", size= 4, shape = 21, lwd=2, show.legend = FALSE) +
        scale_fill_viridis(discrete = FALSE, option = "D") +
        theme_classic() +
        theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
              axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
              panel.grid.minor=element_line(color=NA))
phplot

## plotting Salinity FULLDATA
salinityplot <-ggplot(fulldata,aes(x=Year, y=Salinity_mean, fill=Year, group=Year)) +
        geom_boxplot(alpha=0.7, lwd=1, outlier.shape = NA, show.legend = FALSE) + 
        geom_point(stat = "identity", size= 4, shape = 21, lwd=2, show.legend = FALSE) +
        scale_fill_viridis(discrete = FALSE, option = "D") +
        theme_classic() +
        theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
              axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
              panel.grid.minor=element_line(color=NA))
salinityplot

## plotting SST FULLDATA
sstplot <-ggplot(fulldata,aes(x=Year, y=SST_mean, fill=Year, group=Year)) +
        geom_boxplot(alpha=0.7, lwd=1, outlier.shape = NA, show.legend = FALSE) + 
        geom_point(stat = "identity", size= 4, shape = 21, lwd=2, show.legend = FALSE) +
        scale_fill_viridis(discrete = FALSE, option = "D") +
        theme_classic() +
        theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
              axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
              panel.grid.minor=element_line(color=NA))
sstplot


### MODEL TESTING FOR WASHINGTON
###########################################################
mod <- glmer(Infested ~ pH_mean + SST_mean + Salinity_mean + (1|Year) + (1|Farm), family="binomial", data = fulldata)
summary(mod)
anova(mod)
vif(mod)
car::Anova(mod, type=3) # getting p-values 

envmod <- ggpredict(mod,"pH_mean")

envmod_plot <- plot(envmod) +
  scale_fill_manual(values=wes_palette("GrandBudapest1", n = 2)) + 
  geom_point(size=4) +
  ylab(expression(paste("Predicted infestation"))) +
  xlab(expression(paste("Mean pH"))) +
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
        axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.minor=element_line(color="white")) +
  theme(plot.title=element_blank())
envmod_plot

envmod_plot<-ggplot(envmod,aes(x,predicted,color=group), color=group) +
  scale_color_manual(values=wes_palette("GrandBudapest1", n = 1)) + 
  geom_point(size=4.5, show.legend = FALSE) +
  geom_errorbar(data=envmod, mapping=aes(x=x, ymin=conf.low, ymax=conf.high), width=0.03, show.legend = FALSE) +
  geom_line(aes(group=group), show.legend = FALSE) +
  xlab("Mean pH") +
  ylab(expression(paste("Predicted infestation"))) +
  theme_classic() +
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
        axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.minor=element_line(color=NA))
envmod_plot

## for the next models I'll use only the subset  for the buoys with 2017-18 data
mod2 <- glmer(Infested ~ pH_mean_2017 + SST_mean_2017 + Salinity_mean_2017 + (1|Farm), family="binomial", data = dataprior)
summary(mod2)
anova(mod2)
car::Anova(mod2, type=3) # getting p-values 

envmod_2017 <- ggpredict(mod2,"pH_mean_2017")

envmod_2017 <- plot(mod2) +
  scale_color_manual(values=wes_palette("GrandBudapest1", n = 2)) + 
  geom_point(size=4) +
  ylab(expression(paste("Predicted infestation"))) +
  xlab(expression(paste("Mean pH 2017"))) +
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
        axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.minor=element_line(color="white")) +
  theme(plot.title=element_blank())
envmod_2017


mod4 <- glmer(Infested ~ pH_mean_2018 + SST_mean_2018 + Salinity_mean_2018 + (1|Farm), family="binomial", data = dataprior)
summary(mod4)
anova(mod4)
car::Anova(mod4, type=3) # getting p-values 

envmod_2018 <- ggpredict(mod4,"pH_mean_2018")

envmod_2018 <- plot(mod4) +
  scale_color_manual(values=wes_palette("GrandBudapest1", n = 2)) + 
  geom_point(size=4) +
  ylab(expression(paste("Predicted infestation"))) +
  xlab(expression(paste("Mean pH 2017"))) +
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"), axis.text.y=element_text(size=14), 
        axis.title.y=element_text(size=14), axis.text.x=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.minor=element_line(color="white")) +
  theme(plot.title=element_blank())
envmod_2017
