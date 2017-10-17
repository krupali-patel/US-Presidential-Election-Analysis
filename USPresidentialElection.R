
library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)
library(fiftystater)
library(mapproj)
usa.dat <- read.csv("us_2016_election_data.csv", header = T, sep = ",")
usa.dat$State <- tolower(usa.dat$State)
usa.dat$Clinton..<- as.numeric(gsub("%", "", usa.dat$Clinton..))
usa.dat$Trump..<- as.numeric(gsub("%", "", usa.dat$Trump..))
usa.dat <- usa.dat[,c("State", "Clinton..", "Trump..")]
usa.Hillary <- usa.dat[usa.dat$Clinton.. < 50 , c('State','Clinton..')]
usa.Donald <- usa.dat[usa.dat$Trump..>=50, c('State','Trump..')]
usa.df <- fifty_states
colnames(usa.df)[6] <-"State"

usa.final <- c(usa.Hillary,usa.Donald)

usa.df <- join(usa.df, usa.final, by = "State", type= "inner")
states <- data.frame(state.center, state.abb)
#states <- data.frame(state.center, state.abb) # centers of states and abbreviations
#subset <- fifty_states  %in% tolower(state.name)  # exclude Hawaii as there is no data for this state
#states <- states[subset, ]
aindex = which(usa.df$State == "alaska")
hindex = which(usa.df$State == "hawaii")
alat = usa.df[aindex, 2]
alon = usa.df[aindex, 1]
hlat = usa.df[hindex, 2]
hlon = usa.df[hindex, 1]
states[which(states$state.abb == "AK"),2] = mean(alat)+2.5
states[which(states$state.abb == "AK"),1] = mean(alon)
states[which(states$state.abb == "HI"),2] = mean(hlat)
states[which(states$state.abb == "HI"),1] = mean(hlon)



p <- function(data, title) {
  
  ggp <- ggplot()+geom_polygon(data = data, aes(x = long, y = lat, group = group, fill = usa.final)) + 
    
    # map points to the fifty_states shape data
     
    scale_fill_gradient2(midpoint=50,low="red3", mid="aliceblue",high="deepskyblue3",breaks=c(0,25,50,75,100),labels=c("Trump..","75","50","75","Clinton.."),limits=c(0,100)) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    labs( title=title, fill="",x = "", y = "") +
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3) +
    theme(legend.position = "bottom", 
          panel.background = element_blank())
  return (ggp)
}
figure.title <- "Election Result"
ggsave(p(usa.df, figure.title), height = 4, width = 4*1.9,
       file = "ElectionResult2016.jpg")
