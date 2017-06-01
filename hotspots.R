# Packages
install.packages("RCurl")
install.packages("car")
library(RCurl)
library(car)
#

# Get data
x <- getURL("https://raw.githubusercontent.com/luisra/hotspots/master/conflictdata.csv")
conflict <- read.csv(text = x, header = TRUE, sep = ",")
#

# Summary of events
summary(conflict$EventType)
#

# Frequency of events
hist(conflict$Year, main = "Number of Political Conflicts", xlab = "Year")
#

# Randomized Block Design
year <- conflict$Year
event_type <- conflict$EventType
check <- table(year, event_type)

events_y <- c(24, 44, 42, 35,
              33, 40, 32, 22,
              28, 26, 28, 32, 
              28,14,
              13, 5, 15, 18,
              10, 18, 5, 13,
              18, 11, 20, 11,
              17, 9,
              15, 20, 22, 29,
              36, 53, 39, 21,
              28, 12, 30, 43,
              34, 7)

types_e <- c(rep("Battles", 14), rep("Riots", 14), rep("Violence", 14))
types_e <- factor(types_e)

years_e <- c(rep(seq(from=1997, to=2010), 3))
years_e <- factor(years_e)

boxplot(events_y ~ years_e, xlab = "Year", ylab = "Number of Conflicts",
        main = "Violent Political Conflicts from 1997-2010")
boxplot(events_y ~ types_e, ylab ="Number of Conflicts", xlab = "Type of Event",
        main = "Violent Political Conflicts from 1997-2010")

type <- types_e
interaction.plot(years_e, type, events_y, xlab = "Year", ylab = "Number of Conflicts",
                 main = "Violent Political Conflicts from 1997-2010", legend = TRUE) 

aov.Events_Year <- aov(events_y ~ types_e + years_e)
summary(aov.Events_Year)
TukeyHSD(aov.Events_Year)
#

# Verify ANOVA assumptions
leveneTest(events_y ~ types_e)
hist(aov.Events_Year$residuals)
plot(aov.Events_Year)
#

# Nonparametric approach
friedman.test(events_y ~ types_e|years_e)
#

# Post-hoc test
download.file('https://www.r-statistics.com/wp-content/uploads/2010/02/Friedman-Test-with-Post-Hoc.r.txt', 
              'Desktop/post_hoc.R', 'auto', quiet = FALSE, 
              mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"))

source('Desktop/post_hoc.R')

Master <- data.frame(
  events_y = c(24, 44, 42, 35,
               33, 40, 32, 22,
               28, 26, 28, 32, 
               28,14,
               13, 5, 15, 18,
               10, 18, 5, 13,
               18, 11, 20, 11,
               17, 9,
               15, 20, 22, 29,
               36, 53, 39, 21,
               28, 12, 30, 43,
               34, 7),
  types_e = factor(c(rep("Battles", 14), rep("Riots", 14), rep("Violence", 14))),
  years_e = factor(c(rep(seq(from=1997, to=2010), 3))) )

friedman.test.with.post.hoc(events_y ~ types_e|years_e, data = Master)

# Categorical test (Independence)
country <- conflict$Country
event_type <- conflict$EventType
check <- table(country, event_type)

counts <- matrix(c(9, 7, 14,
                   16, 4, 9,
                   0, 1, 0,
                   0, 6, 0,
                   46, 0, 40,
                   1, 2, 1,
                   11, 3, 10,
                   4, 0, 3,
                   60, 8, 23,
                   1, 0, 0,
                   4, 9, 4,
                   0, 0, 1,
                   3, 0, 0,
                   16, 3, 8,
                   0, 3, 0,
                   0, 0, 1,
                   1, 2, 0,
                   4, 2, 0,
                   2, 0, 0,
                   7, 13, 10,
                   34, 14, 29,
                   3, 0, 0,
                   14, 2, 5,
                   0, 9, 1,
                   0, 1, 0,
                   0, 1, 0,
                   1, 1, 1,
                   0, 3, 1,
                   0, 2, 1,
                   5, 2, 2,
                   30, 17, 28,
                   1, 0, 0,
                   7, 0, 3,
                   10, 4, 5,
                   17, 0, 4,
                   22, 6, 15,
                   9, 20, 17,
                   42, 9, 29,
                   4, 0, 1,
                   0, 0, 4,
                   0, 1, 0,
                   43, 5, 32,
                   0, 10, 3,
                   1, 13, 84), nrow = 44, ncol = 3, byrow = T)

outer(rowSums(counts), colSums(counts))/sum(counts)
fisher.test(counts, simulate.p.value = TRUE)
# 

# Set up
remove(proportions)
proportions <- data.frame(Battles = counts[,1]/colSums(counts)[1], 
                          Riots = counts[,2]/colSums(counts)[2],
                          Violence = counts[,3]/colSums(counts)[3],
                          Country = sort(unique(country)))
#

# Hotspots of Activity
battles <- head(sort(proportions[,1], decreasing=TRUE), 5)
riots <- head(sort(proportions[,2], decreasing=TRUE), 5)
violence <- head(sort(proportions[,3], decreasing=TRUE), 5)

cat("Hotspots of Battles:")
m = 1
for (i in 1:5) {
  
  if (length(proportions[proportions$Battles == battles[i],"Country"]) > 1){
    
    l = length(proportions[proportions$Battles == battles[i],"Country"])
    
    if (m <= 5) {
      
      for (k in 1:l) {
        cat(as.character(proportions[proportions$Battles == battles[i],"Country"])[k], ' (',  round(battles[i]*100,0), '%)', '\n', sep='')
      }
      m = i + l
    } else {}
    
  } else {
    
    cat(as.character(proportions[proportions$Battles == battles[i],"Country"]), ' (',  round(battles[i]*100,0), '%)', '\n', sep='')
  } 
}

cat("Hotspots of Riots:")
m = 1
for (i in 1:5) {
  
  if (length(proportions[proportions$Riots == riots[i],"Country"]) > 1){

    l = length(proportions[proportions$Riots == riots[i],"Country"])
  
    if (m <= 5) {
     
      for (k in 1:l) {
        cat(as.character(proportions[proportions$Riots == riots[i],"Country"])[k], ' (',  round(riots[i]*100,0), '%)', '\n', sep='')
      }
    m = i + l
    } else {}

  } else {
  
  cat(as.character(proportions[proportions$Riots == riots[i],"Country"]), ' (',  round(riots[i]*100,0), '%)', '\n', sep='')
  } 
}

cat("Hotspots of Violence Against Civilians:")
m = 1
for (i in 1:5) {
  
  if (length(proportions[proportions$Violence == violence[i],"Country"]) > 1){
    
    l = length(proportions[proportions$Violence == violence[i],"Country"])
    
    if (m <= 5) {
      
      for (k in 1:l) {
        cat(as.character(proportions[proportions$Violence == violence[i],"Country"])[k], ' (',  round(violence[i]*100,0), '%)', '\n', sep='')
      }
      m = i + l
    } else {}
    
  } else {
    
    cat(as.character(proportions[proportions$Violence == violence[i],"Country"]), ' (',  round(violence[i]*100,0), '%)', '\n', sep='')
  } 
}
#