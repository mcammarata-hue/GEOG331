##########Project Code###########
#Load in csv files
bryometa <- read.csv("BryoMetadata.csv", header = T)
dbryo <- read.csv("DeepBryo.csv", header = T)

#merge data
x <- merge(dbryo, bryometa, by.x = "image.id", by.y = "Specimen_ID")

#########First Analysis#########
#load packages
library(lubridate)
library(ggplot2)    
library(dplyr)
library(tidyr)
library(ggpubr)

#####Creat Boxplot for D.depressa Reproductive Mode
#filter data for D.depressa reproductive mode by state
filterDR_AL <- x %>%
  filter(Genus == "Discoporella", State == 'AL', category == 'autozooid', 
         Reproductive_Mode %in% c('Aclonal', 'Clonal'))
filterDR_FL <- x %>%
  filter(Genus == "Discoporella", State == 'FL', category == "autozooid",
         Reproductive_Mode %in% c('Aclonal', 'Clonal'))

#make boxplot for D.depressa AL
ggplot(data= filterDR_AL, aes(x = Reproductive_Mode, y = area, fill = Reproductive_Mode)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Reproductive Mode AL", 
       x = "Reproductive Mode",
       y = "Area") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  )+
  ylim(0,0.15)
#make boxplot for D.depressa FL
ggplot(data= filterDR_FL, aes(x = Reproductive_Mode, y = area, fill = Reproductive_Mode)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Reproductive Mode FL", 
       x = "Reproductive Mode",
       y = "Area") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  )+
  ylim(0,0.15)
#####Create box Plot for D.depressa + R.doma in FL and AL
#filter data for D.depressa
filterD <- x %>% 
  filter(Genus == "Discoporella", State %in% c('AL','FL'), category == "autozooid")
#filter data
filterR <- x %>% 
  filter(Genus == "Reussirella", State %in% c('AL','FL'), category == "autozooid") 

#make boxplot plot for D.depressa
ggplot(data= filterD, aes(x = State, y = area, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Autozooid Area", 
       x = "State",
       y = "Area") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  )+
  ylim(0,0.15)
#make violin plot for R.doma
ggplot(data= filterR, aes(x = State, y = area, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "R.doma Autozooid Area", 
       x = "State",
       y = "Area") +
  stat_compare_means(
    methods = "t.test",
    label = "p.format"
  ) +
  ylim(0,0.075)

######Create violin plot for d.depressa and r.doma based on reproductive mode 
#####Create Plot for D.depressa + R.doma in FL and AL
#filter data for D.depressa
filter1 <- x %>% 
  filter(Genus == "Discoporella", 
         Reproductive_Mode == "Aclonal",
         State %in% c("AL", "FL"), category == 'autozooid')
#filter data for R.doma
filter2 <- x %>% 
  filter(Genus == "Reussirella", 
         Reproductive_Mode == "Aclonal",
         State %in% c("AL", "FL"), category == 'autozooid')

#make violin plot for D.depressa
ggplot(data= filter1, aes(x = State, y = area, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Autozooid Area by Aclonal Reproduction", 
       x = "State",
       y = "Area",
       fill = "State")+
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  ) +
  scale_fill_manual(values = c("FL" = "green4",
                               "AL" = "orange3"))+
  ylim(0,0.13)

#make violin plot for R.doma
ggplot(data= filter2, aes(x = State, y = area, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "R.doma Autozooid Area by Aclonal Reproduction", 
       x = "State",
       y = "Area",
       fill = "State")+ 
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  ) +
  stat_compare_means(method = "t-test", label = "p.format")+
  scale_fill_manual(values = c("AL" = "green4",
                               "FL" = "orange3"))+
  ylim(0,0.075)

########Create Violin Plot for D.depressa and R.doma Max Colony Size
########in AL and FL with Wilcox 
#filter data for D.depressa
metaD <- bryometa %>% 
  filter(State %in% c('AL','FL')) %>%
  mutate(MaxColonyDiameter_mm = as.numeric(gsub("[^0-9\\.]", "",
                                                MaxColonyDiameter_mm))) %>%
  filter(Genus == "Discoporella")
#filter data for R.doma
metaR <- bryometa %>% 
  filter(State %in% c('AL','FL')) %>%
  mutate(MaxColonyDiameter_mm = as.numeric(gsub("[^0-9\\.]", "",
                                                MaxColonyDiameter_mm))) %>%
  filter(Genus == "Reussirella") 

#filter na 
metaD <- metaD %>% filter(!is.na(MaxColonyDiameter_mm))
metaR <- metaR %>% filter(!is.na(MaxColonyDiameter_mm))

#make violin plot for D.depressa
ggplot(data= metaD, aes(x = State, y = MaxColonyDiameter_mm, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values = c("AL" = "green4", "FL" = "magenta4")) +
  labs(title = "D.depressa Max Colony Diameter", 
       x = "State",
       y = "Colony Diameter") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  ) +
  ylim(2,7.5)
 
#make violin plot for R.doma
ggplot(data= metaR, aes(x = State, y = MaxColonyDiameter_mm, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values = c("AL" = "green4", "FL" = "magenta4")) +
  labs(title = "R.doma Max Colony Diameter", 
       x = "State",
       y = "Colony Diameter")+
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  ) +
  ylim(1.5,3.5)

#########Second Analysis########
#median 
m <- function(x) mean(x, na.rm = TRUE)
#find average autozooid area per colony 
avg_a <- tapply(dbryo$area[which(dbryo$category=="autozooid")], 
                dbryo$image.id[which(dbryo$category=="autozooid")], m)
b=match(names(avg_a),bryometa$Specimen_ID)
percol=cbind(bryometa[b,],avg_a)
write.csv(percol,'PerColonyData.csv')


#load packages
library(lubridate)
library(ggplot2)    
library(dplyr)
library(tidyr)
library(ggpubr)

#####Creat Boxplot for D.depressa Reproductive Mode
#filter data for D.depressa reproductive mode by state
filterDR_AL <- percol %>%
  filter(Genus == "Discoporella", State == 'AL', 
         Reproductive_Mode %in% c('Aclonal', 'Clonal'))
filterDR_FL <- percol %>%
  filter(Genus == "Discoporella", State == 'FL',
         Reproductive_Mode %in% c('Aclonal', 'Clonal'))

#make boxplot for D.depressa AL
ggplot(data= filterDR_AL, aes(x = Reproductive_Mode, y = avg_a, fill = Reproductive_Mode)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Reproductive Mode AL (Using Mean)", 
       x = "Reproductive Mode",
       y = "Area",
       fill = "Reproductive Mode") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  )+
  scale_fill_manual(values = c("Clonal" = "yellow3",
                               "Aclonal" = "plum3"))+
  ylim(0.03,0.11)
#make boxplot for D.depressa FL
ggplot(data= filterDR_FL, aes(x = Reproductive_Mode, y = avg_a, fill = Reproductive_Mode)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Reproductive Mode FL (Using Mean)", 
       x = "Reproductive Mode",
       y = "Area", 
       fill = "Reproductive Mode") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  )+
  scale_fill_manual(values = c("Clonal" = "yellow3",
                               "Aclonal" = "plum3"))+
  ylim(0.03,0.11)

#####Create Violin Plot for D.depressa + R.doma in FL and AL
#filter data for D.depressa
filterD <- percol %>% 
  filter(Genus == "Discoporella", State %in% c('AL','FL'))
#filter data
filterR <- percol %>% 
  filter(Genus == "Reussirella", State %in% c('AL','FL')) 

#make violin plot for D.depressa
ggplot(data= filterD, aes(x = State, y = avg_a, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Mean Autozooid Area", 
       x = "State",
       y = "Area") +
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  )+
  ylim(0,0.125)
t.test(avg_a ~ State, data = filterD)
#make violin plot for R.doma
ggplot(data= filterR, aes(x = State, y = avg_a, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "R.doma Mean Autozooid Area", 
       x = "State",
       y = "Area") +
  stat_compare_means(
    methods = "t.test",
    label = "p.format"
  ) +
  ylim(0,0.075)

######Create violin plot for d.depressa and r.doma based on reproductive mode 
#####Create Plot for D.depressa + R.doma in FL and AL
#filter data for D.depressa
filter1 <- percol %>% 
  filter(Genus == "Discoporella", 
         Reproductive_Mode == "Aclonal",
         State %in% c("AL", "FL"))
#filter data for R.doma
filter2 <- percol %>% 
  filter(Genus == "Reussirella", 
         Reproductive_Mode == "Aclonal",
         State %in% c("AL", "FL"))

#make violin plot for D.depressa
ggplot(data= filter1, aes(x = State, y = avg_a, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Mean Autozooid Area by Aclonal Reproduction", 
       x = "State",
       y = "Area",
       fill = "State")+
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  ) +
  scale_fill_manual(values = c("FL" = "green4",
                               "AL" = "orange3"))+
  ylim(0,0.13)
#make violin plot for R.doma
ggplot(data= filter2, aes(x = State, y = avg_a, fill = State)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "R.doma Mean Autozooid Area by Aclonal Reproduction", 
       x = "State",
       y = "Area",
       fill = "State")+ 
  stat_compare_means(
         method = "t.test",
         label = "p.format"
       ) +
  scale_fill_manual(values = c("FL" = "green4",
                               "AL" = "orange3"))+
  ylim(0,0.075)

#########Map#######
library(terra)
library(tidyterra)
library(FedData)
library(raster)

#read in data
dat <- raster("cafe.2023121.hdf")

#assign extent 
extent(dat) <- extent(-180, 180, -90, 90)
#projection 
crs(dat) <- "+proj=longlat +datum=WGS84"

#restrict y 
lat_min <- 26
lat_max <- 32
#plot only GOM 
gom <- extent(-94, -82, lat_min, lat_max)
#flip data
flip_dat <- flip(dat, direction = "y")
gom_dat <- crop(flip_dat, gom)

#get rid of NAs 
gom_dat[gom_dat <= 0] <- NA

##add stations 
stations <- read.csv("Harnik NSF stations.csv")
head(stations)
#remove LA stations 
stations_new <- subset(stations, State != "LA")
#assign coordinates 
coordinates(stations_new) <- ~ Longitude + Latitude
proj4string(stations_new) <- CRS("+proj=longlat +datum=WGS84")

#correct axis 
par(xaxs = "i", yaxs = "i")

#plot
library(maps)
library(viridis)
plot(gom_dat, main = "Net Primary Productivity GOM May 2024", 
     legend.args = list(
       text = "g C m-² day-¹", 
       side = 3, 
       line = 1, 
       cex = 0.9,
       adj = 0),
     xlim = c(-94,-82),
     col = viridis(100)
)
map("state",
    xlim = c(-94,-81),
    ylim = c(26, 32),
    add = TRUE, 
    col = "black", 
    lwd = 1)
points(stations_new, 
       pch = 21,
       bg = "red",
       col = "red",
       cex = 0.5)
#add points legend
par(xpd = NA)
usr <- par("usr")

legend( x= usr[2] + 0.3,
        y = usr[4] -5,
        legend = "Stations",
        pch = 21,
        pt.bg = "red",
        col = "red",
        pt.cex = 1,
        bty = "n")


######reproductive mode 
filter1 <- x %>% 
  filter(Taxon == "D.depressa", 
         category == "autozooid",
         Reproductive_Mode %in% c("Aclonal", "Clonal"),
         State %in% c("AL", "FL"))
#filter data for R.doma
filter2 <- x %>% 
  filter(Taxon == "R.doma", 
         category == "autozooid", 
         Reproductive_Mode %in% c("Aclonal", "Clonal"),
         State %in% c("AL", "FL"))

#make violin plot for D.depressa
ggplot(data= filter1, aes(x = Reproductive_Mode, y = area, fill = Reproductive_Mode)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = "D.depressa Autozooid Area by Aclonal Reproduction", 
       x = "Reproductive Mode",
       y = "Area",
       fill = "Reproductive Mode")+
  stat_compare_means(
    method = "t.test",
    label = "p.format"
  ) +
  scale_fill_manual(values = c("Aclonal" = "green4",
                               "Clonal" = "orange3"))+
  ylim(0,0.13)



