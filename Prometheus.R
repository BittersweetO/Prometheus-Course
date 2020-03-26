library(dplyr)
library(ggplot2)
library(lubridate)

moon <- read.csv("moon.csv")
crimes <- read.csv("crimes.csv")
crimes$Dates <- as.Date(ymd_hms(as.character(crimes$Dates)))
moon$date <- as.Date(moon$date, "%m/%d/%Y")
full <- inner_join(crimes, moon, by = c("Dates"= "date"))

phasedate <- full %>%
  group_by(Dates, phase) %>%
  count() %>%
  arrange(desc(n))

ggplot(phasedate, aes(Dates, n)) +
  geom_line(alpha= 0.5) + 
  labs(title = "San-Francisco crimes (2003-2015)",
       x = "Date",
       y = "Crimes count") +
  geom_point(data = phasedate[phasedate$phase == "Full Moon",], color = "red") +
  geom_smooth()

x  <- mean(phasedate$n[phasedate$phase == "Full Moon"]) 
mu <- mean(phasedate$n[phasedate$phase != "Full Moon"]) # :H0 / Ha: mu!=mu

# Alpha = 0.05
n  <- length(phasedate$n[phasedate$phase == "Full Moon"])# Sample lenght
sd <- sd(phasedate$n[phasedate$phase == "Full Moon"])    # Standart deviation

t <- (x-mu)*sqrt(n)/sd
Pval <- 2*pt(t,df = n-1, lower.tail = F)

# Pval > Alpha , Ha rejected

Sample <- phasedate$n[phasedate$phase == "Full Moon"]

t.test(Sample, mu = mu, alternative = "two.sided", conf.level = 0.95)

#_________________________________________________________________________
# Dy of week affect on count crimes, H0  
dwcrimes <- full %>%
  group_by(DayOfWeek) %>%
  count()
dwcrimes$DayOfWeek <- factor(dwcrimes$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data = dwcrimes, aes(x = DayOfWeek, y = n)) + geom_bar(stat = "identity", fill = "lightblue")


byday <- full %>%
  group_by(Dates, DayOfWeek) %>%
  count()

fridaysample <- byday$n[byday$DayOfWeek == "Friday"]

x1  <- mean(byday$n[byday$DayOfWeek == "Friday"]) # Sample mean
mu1 <- mean(byday$n[byday$DayOfWeek != "Friday"]) # Population mean
sd1 <- sd(byday$n[byday$DayOfWeek == "Friday"]) # Sample standart deviation 
t.test(fridaysample, mu = 391.75, alternative = "two.sided",  conf.level = 0.99 )