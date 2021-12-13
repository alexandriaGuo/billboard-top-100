library(tidyverse)
library(GGally)
library(vcd)
setwd("/Users/alexandriaguo/Documents/2021-2022/STATW 5702/Final Project/billboard-top-100")
source("plot_missing.R")

###############################
### DATA CLEANING & LOADING ###
###############################

rawdata <- readr::read_csv("billboard_spotify_cleaned_kpop.csv")
rawdata$spotify_genre_clean <- as.factor(rawdata$spotify_genre_clean)
rawdata$week_id <- as.Date(rawdata$week_id, format="%m/%d/%Y")

data <- rawdata
data$missing <-ifelse(rowSums(is.na(data)) >= 1, TRUE, FALSE)
# sort by (week?), song, then artist
data <- data[order(data$performer,
                   data$song,
                   data$week_id),]
summdata <- data %>% 
  group_by(song, performer) %>%
  summarise(first_week = min(week_id),
            last_week = max(week_id),
            total_peak = min(peak_position),
            total_instances = max(instance),
            total_weeks=n()) %>% # found to be more accurate than max(weeks_on_chart)
  ungroup()

uniqdata <- data %>%
  select(c("spotify_genre_clean", "song", "performer",
           "key", "mode", "tempo", "time_signature",
           "danceability", "energy", "loudness", 
           "speechiness", "acousticness", "instrumentalness",
           "liveness", "valence",
           "missing", # custom column
           "spotify_genre", "spotify_track_popularity")) %>% 
  # keeping original genre to check
  unique() %>%
  inner_join(summdata, by=c("song", "performer"))

# manually alter 'CRUSH' to non-kpop
uniqdata[which(uniqdata$performer=="CRUSH"),1] <- "other"

############################################
### ADDITIONAL DATA MISSINGNESS ANALYSIS ###
############################################

uniqdata %>%
  mutate(year = as.numeric(format(last_week, format="%Y"))) %>%
  group_by(year) %>%
  summarise(Total = sum(missing)/n(),
            Genre = sum(is.na(spotify_genre_clean))/n()) %>%
  # need percentages due to non-standard number of songs/year
  ungroup() %>%
  pivot_longer(cols=c(Total, Genre),
               names_to = "type",
               values_to = "perc") %>% 
  mutate(perc = 100*perc,
         type = as.factor(type)) %>%
  ggplot(mapping=aes(x=year, y=perc, 
                     color=fct_reorder(type, perc), 
                     group=fct_reorder(type, perc))) +
  geom_line() +
  scale_color_discrete(name="Name type") +
  scale_x_continuous(limits = c(1958, 2021)) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x="Year", y="% Missing", 
       title="Missingness by year")
# FIX FCT_REORDER TO MATCH FIGURE
# add annotated lines of best fit

temp <- uniqdata %>%
  mutate(feat = str_detect(performer, regex('[:space:]ft[:space:]', ignore_case=TRUE)) |
           str_detect(performer, regex('[:space:]ft.[:space:]', ignore_case=TRUE)) |
           str_detect(performer, regex('[:space:]feat[:space:]', ignore_case=TRUE)) |
           str_detect(performer, regex('[:space:]feat.[:space:]', ignore_case=TRUE)) |
           str_detect(performer, regex('[:space:]featuring[:space:]', ignore_case=TRUE)) |
           str_detect(song, regex('[:space:]featuring[:space:]', ignore_case=TRUE))) %>%
  # almost all are of the form "[:space:]featuring[:space:]" in performer
  # many features are actually recorded in the form "[artist] & [ft. artist]"
  # sometimes "with" or "x" or other choices, as well
  # but these cannot be easily distinguished from bands with "&" in their names
  group_by(feat, missing) %>%
  summarise(Freq = n()) %>% # vcd::mosaic convention
  ungroup() # relevel factors 

mosaic(missing~feat, direction = c("v", "h"), temp, 
       highlighting_fill = c("#FDAE61","#D53E4F"),
       main = "Missingness in songs w/ featured artists")
# add chi-squared?

#################################
### 1. UNIQUE SONGS OVER TIME ###
#################################

first <- uniqdata %>%
  mutate(year = as.numeric(format(first_week, format="%Y"))) %>%
  group_by(year) %>%
  summarise(First = n()) %>%
  ungroup()

last <- uniqdata %>%
  mutate(year = as.numeric(format(last_week, format="%Y"))) %>%
  group_by(year) %>%
  summarise(Last = n()) %>%
  ungroup() 

cbind(first, last[,-1]) %>%
  pivot_longer(cols=c(First, Last),
               names_to = "Year_bin",
               values_to = "Freq") %>%
  ggplot(mapping=aes(x=year, y=Freq,
                     color=Year_bin,
                     group=Year_bin)) +
  geom_line() +
  scale_color_discrete(name="Year Charted") +
  scale_x_continuous(limits = c(1958, 2021)) +
  scale_y_continuous(limits = c(0, 800)) +
  labs(x="Year", y="Frequency",
       title="Number of unique songs in the Top 100")  +
  theme_gray()
# FIX FCT_REORDER TO MATCH FIGURE
# add annotated lines of best fit

###################################
### 2. MUSICAL FEATURE ANALYSIS ###
###################################

fulldata <- na.omit(uniqdata)

# histogram of song popularity
fulldata %>%
  pivot_longer(cols=c(total_peak,
                      total_instances,
                      total_weeks,
                      spotify_track_popularity),
               names_to = "feature",
               values_to = "value") %>%
  ggplot(mapping=aes(value)) +
  geom_histogram() +
  facet_wrap(~feature, scales="free") +
  labs(x="Value", y="Count",
       title="Spreads of song popularity measures") +
  theme_gray()

# histogram of all features
fulldata %>%
  pivot_longer(cols=key:valence,
               names_to = "feature",
               values_to = "value") %>%
  ggplot(mapping=aes(value)) +
  geom_histogram() +
  facet_wrap(~feature, scales="free") +
  labs(x="Value", y="Count",
       title="Spreads of musical features") +
  theme_gray()
# thus choose acousticness (12), danceability (8), energy (9), 
# liveness (14), loudness (10), tempo (6), and valence (15)
# removed speechiness (11) after looking at the pca summary
featcols <- c(6,8,9,10,11,12,14,15)

pcs <- prcomp(fulldata[,featcols], scale.=TRUE)
expvar <- 100*(pcs$sdev^2)/sum(pcs$sdev^2)

# https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
datax <- data.frame(pcs$x)
datapc <- data.frame(varnames=rownames(pcs$rotation), pcs$rotation)
mult <- min(
  (max(datax[,"PC2"]) - min(datax[,"PC2"])/(max(datax[,"PC2"])-min(datax[,"PC2"]))),
  (max(datax[,"PC1"]) - min(datax[,"PC1"])/(max(datax[,"PC1"])-min(datax[,"PC1"])))
)
datapc <- transform(datapc,
                    v1 = .8 * mult * (get("PC1")),
                    v2 = .8 * mult * (get("PC2")))

temp <- cbind(data.frame(pcs$x), fulldata) %>%
  filter(total_peak < 50) 

temp %>% 
  ggplot(aes(x=PC1, y=PC2)) +
  geom_point(aes(color = spotify_genre_clean, 
                 alpha = 0.01, stroke = 0)) +
  geom_point(subset(temp, spotify_genre_clean == 'kpop'),
             mapping=aes(color = spotify_genre_clean)) +
  geom_text(data=datapc, aes(x=v1, y=v2, label=varnames),
            nudge_x=ifelse(datapc$v1 > 0, 0.3, -0.3),
            nudge_y=ifelse(datapc$v2 > 0, 0.4, -0.4), 
            angle=0,
            size=4, color="black") +
  geom_segment(data=datapc, mapping=aes(x=0, y=0, xend=v1, yend=v2), 
               arrow=arrow(length=unit(0.2,"cm")), 
               alpha=0.75, color="black") +
  scale_alpha(guide="none") +
  scale_size(guide="none") +
  scale_color_discrete(name="Genre") +
  labs(x=paste("PC 1 (",format(expvar[1], digits=3),"%)"),
       y=paste("PC 2 (",format(expvar[2], digits=3),"%)"),
       title="Biplot of musical features") +
  theme_gray()

##################################
### 3. GENRE COMPARISON: K-POP ###
##################################

fulldata %>%
  mutate(year = as.numeric(format(first_week, format="%Y"))) %>%
  filter(((year >= 2009) & (total_peak <= 5)) | 
           (spotify_genre_clean == "kpop")) %>% 
  ggparcoord(columns = featcols, scale="uniminmax", alphaLines=0.1, splineFactor=10,
             groupColumn = 'spotify_genre_clean') +
  labs(title="Auditory features", x="Auditory features", y="Count") +
  theme(legend.position = "none") 

##################################
### 4. CHART MOVEMENT/VELOCITY ###
##################################

# removes debut ranks
# total sum of rank diff is -73897, which may be due to removing debut rank movement
rankdata <- data %>%
  mutate(previous_week_position = replace(previous_week_position,
                                          which(previous_week_position=="debut"), NA)) %>%
  na.omit() %>%
  mutate(rankdiff = week_position - as.numeric(previous_week_position))
  
rankdata %>%
  ggplot(aes(x=rankdiff)) +
    geom_histogram(breaks=c(seq(-80.5,-0.5,1),seq(0.5,80,1))) +
  labs(x="Difference in rank", y="Frequency", 
       title="Difference in rank for songs in Top 100, between weeks")
