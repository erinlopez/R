library(dplyr)
data <- HRV_data_20201209

#compare mean hr of sepsis vs no sepsis groups
hr <- data %>% group_by(Sepsis3) %>%
  summarise(avg = mean(Mean.rate, na.rm = TRUE)) %>%
  arrange(avg)

shr <- subset(data, Sepsis3 == 1)$Mean.rate
nhr <- subset(data, Sepsis3 == 0)$Mean.rate

t.test(nhr, shr) #no sepsis group shows significantly higher hr

b <- boxplot(nhr, shr, names = c('No Sepsis', 'Sepsis'), ylab = 'Mean Heart Rate')

#compare hr CV
cv <- data %>% group_by(Sepsis3) %>%
  summarise(avg = mean(Coefficient.of.variation, na.rm = TRUE)) %>%
  arrange(avg)

scv <- subset(data, Sepsis3 == 1)$Coefficient.of.variation
ncv <- subset(data, Sepsis3 == 0)$Coefficient.of.variation

t.test(ncv, scv) #no sepsis group shows significantly higher hr

b <- boxplot(ncv, scv, names = c('No Sepsis', 'Sepsis'), ylab = 'Mean Heart Rate CV')
summary(scv)
summary(ncv)

######## DFA.Alpha.2
dfa2 <- data %>% group_by(Sepsis3) %>%
  summarise(avg = mean(DFA.Alpha.2, na.rm = TRUE)) %>%
  arrange(avg)

sdfa2 <- subset(data, Sepsis3 == 1)$DFA.Alpha.2
ndfa2 <- subset(data, Sepsis3 == 0)$DFA.Alpha.2

t.test(ndfa2, sdfa2) #no sepsis group shows significantly higher hr

dfab <- boxplot(ndfa2, sdfa2, names = c('No Sepsis', 'Sepsis'), ylab = 'DFA Alpha 2')
summary(sdfa2)
summary(ndfa2)

m <- glm(Sepsis3 ~ ., data= data)
summary(m)
