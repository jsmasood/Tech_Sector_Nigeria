# Import Datasets

library(haven)
library(readr)
library(ggplot2)
library(extrafont)

Gha_2013 <- read_dta("~/Desktop/Nigeria Report Charts/Nigeria Data and R files/Ghana 2013 Services.dta")
Nig_2014 <- read_dta("~/Desktop/Nigeria Report Charts/Nigeria Data and R files//Nigeria 2014 Services.dta")
SA_2007 <- read_dta("~/Desktop/Nigeria Report Charts/Nigeria Data and R files//South Africa 2007 Services.dta")
Tan_2013 <- read_dta("~/Desktop/Nigeria Report Charts/Nigeria Data and R files//Tanzania 2013 Services.dta")
Ken_2018 <- read_dta("~/Desktop/Nigeria Report Charts/Nigeria Data and R files//Kenya 2018 Services.dta")
Nig_Tech <- read_csv("~/Desktop/Nigeria Report Charts/Nigeria Data and R files//One campaign data ful set.csv")

library(dplyr)
library(spreadr)

setwd("~/Desktop/Nigeria Report Charts/")

# Subselet columns of interest

Gha_2013 <- Gha_2013 %>% 
  select(b4, b4a, b5, b6, b7, b7a, c7, c8, c9a, c10, h7, j3, j5, j6a, k8, k9, k13, k15d, k30, l1, l5, n2b, l30a, l30b, j30a, j30b, j30c, j30e, j30f, h30, d30a, d30b, c30a)

Nig_2014 <- Nig_2014 %>% 
  select(b4, b4a, b5, b6, b7, b7a, c7, c8a, c9a, c10, h7, j3, j5, j6a, k8, k9, k13, k15d, k30, l1, l5, n2b, l30a, l30b, j30a, j30b, j30c, j30e, j30f, h30, d30a, d30b, c30a)

SA_2007 <- SA_2007 %>% 
  select(b4, b5, b6, b7, c7, c8, c9a, c10, j3, j5, j6a, k8, k9, k13, k30, l1, n2b, l30a, l30b, j30a, j30b, j30c, j30e, j30f, h30, d30a, d30b, c30a)

Tan_2013 <- Tan_2013 %>% 
  select(b4, b4a, b5, b6, b7, b7a, c7, c8, c9a, c10, h7, j3, j5, j6a, k8, k9, k13, k15d, k30, l1, l5, n2b, l30a, l30b, j30a, j30b, j30c, j30e, j30f, h30, d30a, d30b, c30a)

Ken_2018 <- Ken_2018 %>% 
  select(b4, b4a, b5, b6, b7, b7a, c7, c8a, c9a, c10, h8, j3, j5, j6a, k8, k9, k13, k15d, k30, l1, l5, n2b, l30a, l30b, j30a, j30b, j30c, j30e, j30f, h30, d30a, d30b, c30a)

Nig_Tech <- Nig_Tech %>%
  select(colnames(Nig_Tech)[c(4:5, 7, 10:11, 15:17, 20, 41, 54:55, 58, 69, 72, 74:75, 84, 94, 105, 100:101, 78:83, 34: 35 )])

# Create a vector with all the names and order for the countries

names <- c("Nigeria Tech","Nigeria","Ghana","South Africa","Kenya","Tanzania")

# Create a seperate df for each question across the countries

# Variable b4 - Female Owners Y/N

b4 <- count(Nig_Tech, female_owners_B4)
b4$Gha <- count(Gha_2013, b4)[,2]
b4$SA <- count(SA_2007, b4)[,2]
b4$Ken <- count(Ken_2018, b4)[,2]
b4$Tan <- count(Tan_2013, b4)[,2]
b4$Nig <- count(Nig_2014, b4)[1:3,2]

b4 <- b4[-c(1),]

b4_t <- t(b4)
b4_t <- data.frame(B4= row.names(b4_t), b4_t, row.names=NULL) 

b4_t <- b4_t[-c(1),]

b4_t$Perc <- (b4_t$X1/(b4_t$X1 + b4_t$X2))*100

b4_t$B4 <- as.factor(b4_t$B4)

b4_t$B4 <- c("Nigeria\nTech", "Ghana Services", "South Africa\nServices", "Kenya Services", "Tanzania Services","Nigeria Services")

ggplot(b4_t, aes(x=B4, y=Perc)) + geom_bar(stat = "identity", fill="steelblue", width = 0.5) +
  geom_text(aes(label = round(Perc, 1), vjust = 1.5), colour = "white", size = 2) + theme_classic() +
  theme(
    text = element_text(family="Khmer Sangam MN", size=10),
    # axis.text.x = element_text(angle=45),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=11, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold")
  ) +
  labs(title = "Amongst the owners of the firm, are there any females? %",
       x = "Percentage of Firm",
       y = "Number of Firms")


## Business Climate Charts

# C30 - Electricity

# Read in frequency values for different options by df

c30<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(c30) <- c("Country","Levels","Freq","perc")

c30$Country[1:5] <- names[1]
c30$Country[6:10] <- names[2]
c30$Country[11:15] <- names[3]
c30$Country[16:20] <- names[4]
c30$Country[21:25] <- names[5]
c30$Country[26:30] <- names[6]


c30$Levels <- seq(0,4,1)

c30[1:5,3] <- count(Nig_Tech, impact_of_electricity_C30)[2:6,2]
c30[6:10,3] <- count(Nig_2014, c30a)[3:7,2]
c30[11:15,3] <- count(Gha_2013, c30a)[2:6,2]
c30[16:20,3] <- count(SA_2007, c30a)[,2]
c30[21:25,3] <- count(Ken_2018, c30a)[3:7,2]
c30[26:30,3] <- count(Tan_2013, c30a)[3:7,2]

for (i in 1:5) {
  c30$perc[i] <- c30$Freq[i]/sum(c30$Freq[1:5])
  c30$perc[i+5] <- c30$Freq[i+5]/sum(c30$Freq[6:10])
  c30$perc[i+10] <- c30$Freq[i+10]/sum(c30$Freq[11:15])
  c30$perc[i+15] <- c30$Freq[i+15]/sum(c30$Freq[16:20])
  c30$perc[i+20] <- c30$Freq[i+20]/sum(c30$Freq[21:25])
  c30$perc[i+25] <- c30$Freq[i+25]/sum(c30$Freq[26:30])
}

c30$Country = factor(c30$Country,levels = rev(names))
c30$Levels = factor(c30$Levels,levels = c("4","3","2","1","0"))

c30$perc <- c30$perc * 100

ggplot(data = c30, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
   scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is electricity an obstacle to the current operations of this\nestablishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(c30$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("C30.png",height = 6, width = 7, units = c("in"))


# D30a - Transport

# Read in frequency values for different options by df

d30a <- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(d30a) <- c("Country","Levels","Freq","perc")

d30a$Country[1:5] <- names[1]
d30a$Country[6:10] <- names[2]
d30a$Country[11:15] <- names[3]
d30a$Country[16:20] <- names[4]
d30a$Country[21:25] <- names[5]
d30a$Country[26:30] <- names[6]


d30a$Levels <- seq(0,4,1)

d30a[1:5,3] <- c(26,43,0,18,1)
  # count(Nig_Tech, current_obstacles_operations_D30)[2:6,2]
d30a[6:10,3] <- count(Nig_2014, d30a)[3:7,2]
d30a[11:15,3] <- count(Gha_2013, d30a)[2:6,2]
d30a[16:20,3] <- count(SA_2007, d30a)[,2]
d30a[21:25,3] <- count(Ken_2018, d30a)[3:7,2]
d30a[26:30,3] <- count(Tan_2013, d30a)[3:7,2]

for (i in 1:5) {
  d30a$perc[i] <- d30a$Freq[i]/sum(d30a$Freq[1:5])
  d30a$perc[i+5] <- d30a$Freq[i+5]/sum(d30a$Freq[6:10])
  d30a$perc[i+10] <- d30a$Freq[i+10]/sum(d30a$Freq[11:15])
  d30a$perc[i+15] <- d30a$Freq[i+15]/sum(d30a$Freq[16:20])
  d30a$perc[i+20] <- d30a$Freq[i+20]/sum(d30a$Freq[21:25])
  d30a$perc[i+25] <- d30a$Freq[i+25]/sum(d30a$Freq[26:30])
}

d30a$Country = factor(d30a$Country,levels = rev(names))
d30a$Levels = factor(d30a$Levels,levels = c("4","3","2","1","0"))

d30a$perc <- d30a$perc * 100

d30a[3,4] <- NA

ggplot(data = d30a, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is transport an obstacle to the current operations of\nthis establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(d30a$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  )

ggsave("D30a.png",height = 6, width = 7, units = c("in"))


# D30b - Customs & Trade Regulations

# Read in frequency values for different options by df

d30b <- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(d30b) <- c("Country","Levels","Freq","perc")

d30b$Country[1:5] <- names[1]
d30b$Country[6:10] <- names[2]
d30b$Country[11:15] <- names[3]
d30b$Country[16:20] <- names[4]
d30b$Country[21:25] <- names[5]
d30b$Country[26:30] <- names[6]


d30b$Levels <- seq(0,4,1)

d30b[1:5,3] <- c(61,9,0,8,2)
# count(Nig_Tech, current_obstacles_operations_D30)[2:6,2]
d30b[6:10,3] <- count(Nig_2014, d30b)[3:7,2]
d30b[11:15,3] <- count(Gha_2013, d30b)[2:6,2]
d30b[16:20,3] <- count(SA_2007, d30b)[,2]
d30b[21:25,3] <- count(Ken_2018, d30b)[3:7,2]
d30b[26:30,3] <- count(Tan_2013, d30b)[3:7,2]

for (i in 1:5) {
  d30b$perc[i] <- d30b$Freq[i]/sum(d30b$Freq[1:5])
  d30b$perc[i+5] <- d30b$Freq[i+5]/sum(d30b$Freq[6:10])
  d30b$perc[i+10] <- d30b$Freq[i+10]/sum(d30b$Freq[11:15])
  d30b$perc[i+15] <- d30b$Freq[i+15]/sum(d30b$Freq[16:20])
  d30b$perc[i+20] <- d30b$Freq[i+20]/sum(d30b$Freq[21:25])
  d30b$perc[i+25] <- d30b$Freq[i+25]/sum(d30b$Freq[26:30])
}

d30b$Country = factor(d30b$Country,levels = rev(names))
d30b$Levels = factor(d30b$Levels,levels = c("4","3","2","1","0"))

d30b$perc <- d30b$perc * 100

d30b[3,4] <- NA

ggplot(data = d30b, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree are customs and trade regulations an obstacle to the\ncurrent operations of this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(d30b$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  )

ggsave("D30b.png",height = 6, width = 7, units = c("in"))


# K30 - Access to Finance

# Read in frequency values for different options by df

k30<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(k30) <- c("Country","Levels","Freq","perc")

k30$Country[1:5] <- names[1]
k30$Country[6:10] <- names[2]
k30$Country[11:15] <- names[3]
k30$Country[16:20] <- names[4]
k30$Country[21:25] <- names[5]
k30$Country[26:30] <- names[6]


k30$Levels <- seq(0,4,1)

k30[1:5,3] <- count(Nig_Tech, acces_to_finance_K30)[1:5,2]
k30[6:10,3] <- count(Nig_2014, k30)[3:7,2]
k30[11:15,3] <- count(Gha_2013, k30)[2:6,2]
k30[16:20,3] <- count(SA_2007, k30)[,2]
k30[21:25,3] <- count(Ken_2018, k30)[3:7,2]
k30[26:30,3] <- count(Tan_2013, k30)[3:7,2]

for (i in 1:5) {
  k30$perc[i] <- k30$Freq[i]/sum(k30$Freq[1:5])
  k30$perc[i+5] <- k30$Freq[i+5]/sum(k30$Freq[6:10])
  k30$perc[i+10] <- k30$Freq[i+10]/sum(k30$Freq[11:15])
  k30$perc[i+15] <- k30$Freq[i+15]/sum(k30$Freq[16:20])
  k30$perc[i+20] <- k30$Freq[i+20]/sum(k30$Freq[21:25])
  k30$perc[i+25] <- k30$Freq[i+25]/sum(k30$Freq[26:30])
}

k30$Country = factor(k30$Country,levels = rev(names))
k30$Levels = factor(k30$Levels,levels = c("4","3","2","1","0"))

k30$perc <- k30$perc * 100

ggplot(data = k30, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is access to finance an obstacle to the current\noperations of this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(k30$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("K30.png",height = 6, width = 7, units = c("in"))


# J30a - Tax Rate

j30a<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(j30a) <- c("Country","Levels","Freq","perc")

j30a$Country[1:5] <- names[1]
j30a$Country[6:10] <- names[2]
j30a$Country[11:15] <- names[3]
j30a$Country[16:20] <- names[4]
j30a$Country[21:25] <- names[5]
j30a$Country[26:30] <- names[6]


j30a$Levels <- seq(0,4,1)

j30a[1:5,3] <- count(Nig_Tech, tax_rate_J30a)[1:5,2]
j30a[6:10,3] <- count(Nig_2014, j30a)[3:7,2]
j30a[11:15,3] <- count(Gha_2013, j30a)[2:6,2]
j30a[16:20,3] <- count(SA_2007, j30a)[,2]
j30a[21:25,3] <- count(Ken_2018, j30a)[3:7,2]
j30a[26:30,3] <- count(Tan_2013, j30a)[3:7,2]

for (i in 1:5) {
  j30a$perc[i] <- j30a$Freq[i]/sum(j30a$Freq[1:5])
  j30a$perc[i+5] <- j30a$Freq[i+5]/sum(j30a$Freq[6:10])
  j30a$perc[i+10] <- j30a$Freq[i+10]/sum(j30a$Freq[11:15])
  j30a$perc[i+15] <- j30a$Freq[i+15]/sum(j30a$Freq[16:20])
  j30a$perc[i+20] <- j30a$Freq[i+20]/sum(j30a$Freq[21:25])
  j30a$perc[i+25] <- j30a$Freq[i+25]/sum(j30a$Freq[26:30])
}

j30a$Country = factor(j30a$Country,levels = rev(names))
j30a$Levels = factor(j30a$Levels,levels = c("4","3","2","1","0"))

j30a$perc <- j30a$perc * 100

ggplot(data = j30a, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is the tax rate an obstacle to the current operations\nof this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(j30a$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("J30a.png",height = 6, width = 7, units = c("in"))


# J30b - Tax Rate

j30b<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(j30b) <- c("Country","Levels","Freq","perc")

j30b$Country[1:5] <- names[1]
j30b$Country[6:10] <- names[2]
j30b$Country[11:15] <- names[3]
j30b$Country[16:20] <- names[4]
j30b$Country[21:25] <- names[5]
j30b$Country[26:30] <- names[6]


j30b$Levels <- seq(0,4,1)

j30b[1:5,3] <- count(Nig_Tech, tax_adminstration_J30b)[1:5,2]
j30b[6:10,3] <- count(Nig_2014, j30b)[3:7,2]
j30b[11:15,3] <- count(Gha_2013, j30b)[2:6,2]
j30b[16:20,3] <- count(SA_2007, j30b)[,2]
j30b[21:25,3] <- count(Ken_2018, j30b)[3:7,2]
j30b[26:30,3] <- count(Tan_2013, j30b)[3:7,2]

for (i in 1:5) {
  j30b$perc[i] <- j30b$Freq[i]/sum(j30b$Freq[1:5])
  j30b$perc[i+5] <- j30b$Freq[i+5]/sum(j30b$Freq[6:10])
  j30b$perc[i+10] <- j30b$Freq[i+10]/sum(j30b$Freq[11:15])
  j30b$perc[i+15] <- j30b$Freq[i+15]/sum(j30b$Freq[16:20])
  j30b$perc[i+20] <- j30b$Freq[i+20]/sum(j30b$Freq[21:25])
  j30b$perc[i+25] <- j30b$Freq[i+25]/sum(j30b$Freq[26:30])
}

j30b$Country = factor(j30b$Country,levels = rev(names))
j30b$Levels = factor(j30b$Levels,levels = c("4","3","2","1","0"))

j30b$perc <- j30b$perc * 100

ggplot(data = j30b, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is the tax administration system an obstacle to the current\noperations of this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(j30b$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("J30b.png",height = 6, width = 7, units = c("in"))

# J30c - Business Licensing

j30c<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(j30c) <- c("Country","Levels","Freq","perc")

j30c$Country[1:5] <- names[1]
j30c$Country[6:10] <- names[2]
j30c$Country[11:15] <- names[3]
j30c$Country[16:20] <- names[4]
j30c$Country[21:25] <- names[5]
j30c$Country[26:30] <- names[6]


j30c$Levels <- seq(0,4,1)

j30c[1:5,3] <- count(Nig_Tech, business_licensing_J30c)[1:5,2]
j30c[6:10,3] <- count(Nig_2014, j30c)[3:7,2]
j30c[11:15,3] <- count(Gha_2013, j30c)[2:6,2]
j30c[16:20,3] <- count(SA_2007, j30c)[,2]
j30c[21:25,3] <- count(Ken_2018, j30c)[3:7,2]
j30c[26:30,3] <- count(Tan_2013, j30c)[3:7,2]

for (i in 1:5) {
  j30c$perc[i] <- j30c$Freq[i]/sum(j30c$Freq[1:5])
  j30c$perc[i+5] <- j30c$Freq[i+5]/sum(j30c$Freq[6:10])
  j30c$perc[i+10] <- j30c$Freq[i+10]/sum(j30c$Freq[11:15])
  j30c$perc[i+15] <- j30c$Freq[i+15]/sum(j30c$Freq[16:20])
  j30c$perc[i+20] <- j30c$Freq[i+20]/sum(j30c$Freq[21:25])
  j30c$perc[i+25] <- j30c$Freq[i+25]/sum(j30c$Freq[26:30])
}

j30c$Country = factor(j30c$Country,levels = rev(names))
j30c$Levels = factor(j30c$Levels,levels = c("4","3","2","1","0"))

j30c$perc <- j30c$perc * 100

j30c[11,2:4] <- NA

ggplot(data = j30c, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is the business licensing regime an obstacle to the current\noperations of this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(j30c$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("J30c.png",height = 6, width = 7, units = c("in"))


# J30e - Political Instability

j30e<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(j30e) <- c("Country","Levels","Freq","perc")

j30e$Country[1:5] <- names[1]
j30e$Country[6:10] <- names[2]
j30e$Country[11:15] <- names[3]
j30e$Country[16:20] <- names[4]
j30e$Country[21:25] <- names[5]
j30e$Country[26:30] <- names[6]


j30e$Levels <- seq(0,4,1)

j30e[1:5,3] <- count(Nig_Tech, current_operations_J30e)[1:5,2]
j30e[6:10,3] <- count(Nig_2014, j30e)[3:7,2]
j30e[11:15,3] <- count(Gha_2013, j30e)[2:6,2]
j30e[16:20,3] <- count(SA_2007, j30e)[,2]
j30e[21:25,3] <- count(Ken_2018, j30e)[3:7,2]
j30e[26:30,3] <- count(Tan_2013, j30e)[3:7,2]

for (i in 1:5) {
  j30e$perc[i] <- j30e$Freq[i]/sum(j30e$Freq[1:5])
  j30e$perc[i+5] <- j30e$Freq[i+5]/sum(j30e$Freq[6:10])
  j30e$perc[i+10] <- j30e$Freq[i+10]/sum(j30e$Freq[11:15])
  j30e$perc[i+15] <- j30e$Freq[i+15]/sum(j30e$Freq[16:20])
  j30e$perc[i+20] <- j30e$Freq[i+20]/sum(j30e$Freq[21:25])
  j30e$perc[i+25] <- j30e$Freq[i+25]/sum(j30e$Freq[26:30])
}

j30e$Country = factor(j30e$Country,levels = rev(names))
j30e$Levels = factor(j30e$Levels,levels = c("4","3","2","1","0"))

j30e$perc <- j30e$perc * 100

ggplot(data = j30e, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is political instability an obstacle to the current operations\nof this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(j30e$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("J30e.png",height = 6, width = 7, units = c("in"))


# J30f - Corruption

j30f<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(j30f) <- c("Country","Levels","Freq","perc")

j30f$Country[1:5] <- names[1]
j30f$Country[6:10] <- names[2]
j30f$Country[11:15] <- names[3]
j30f$Country[16:20] <- names[4]
j30f$Country[21:25] <- names[5]
j30f$Country[26:30] <- names[6]


j30f$Levels <- seq(0,4,1)

j30f[1:5,3] <- count(Nig_Tech, corruption_J30f)[1:5,2]
j30f[6:10,3] <- count(Nig_2014, j30f)[3:7,2]
j30f[11:15,3] <- count(Gha_2013, j30f)[2:6,2]
j30f[16:20,3] <- count(SA_2007, j30f)[,2]
j30f[21:25,3] <- count(Ken_2018, j30f)[3:7,2]
j30f[26:30,3] <- count(Tan_2013, j30f)[3:7,2]

for (i in 1:5) {
  j30f$perc[i] <- j30f$Freq[i]/sum(j30f$Freq[1:5])
  j30f$perc[i+5] <- j30f$Freq[i+5]/sum(j30f$Freq[6:10])
  j30f$perc[i+10] <- j30f$Freq[i+10]/sum(j30f$Freq[11:15])
  j30f$perc[i+15] <- j30f$Freq[i+15]/sum(j30f$Freq[16:20])
  j30f$perc[i+20] <- j30f$Freq[i+20]/sum(j30f$Freq[21:25])
  j30f$perc[i+25] <- j30f$Freq[i+25]/sum(j30f$Freq[26:30])
}

j30f$Country = factor(j30f$Country,levels = rev(names))
j30f$Levels = factor(j30f$Levels,levels = c("4","3","2","1","0"))

j30f$perc <- j30f$perc * 100

j30f[16,2:4] <- NA

ggplot(data = j30f, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is corruption an obstacle to the current operations\nof this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(j30f$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("J30f.png",height = 6, width = 7, units = c("in"))


# H30 - Courts

h30<- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(h30) <- c("Country","Levels","Freq","perc")

h30$Country[1:5] <- names[1]
h30$Country[6:10] <- names[2]
h30$Country[11:15] <- names[3]
h30$Country[16:20] <- names[4]
h30$Country[21:25] <- names[5]
h30$Country[26:30] <- names[6]


h30$Levels <- seq(0,4,1)

h30[1:5,3] <- count(Nig_Tech, courts_J30_h30)[1:5,2]
h30[6:10,3] <- count(Nig_2014, h30)[3:7,2]
h30[11:15,3] <- count(Gha_2013, h30)[2:6,2]
h30[16:20,3] <- count(SA_2007, h30)[,2]
h30[21:25,3] <- count(Ken_2018, h30)[3:7,2]
h30[26:30,3] <- count(Tan_2013, h30)[3:7,2]

for (i in 1:5) {
  h30$perc[i] <- h30$Freq[i]/sum(h30$Freq[1:5])
  h30$perc[i+5] <- h30$Freq[i+5]/sum(h30$Freq[6:10])
  h30$perc[i+10] <- h30$Freq[i+10]/sum(h30$Freq[11:15])
  h30$perc[i+15] <- h30$Freq[i+15]/sum(h30$Freq[16:20])
  h30$perc[i+20] <- h30$Freq[i+20]/sum(h30$Freq[21:25])
  h30$perc[i+25] <- h30$Freq[i+25]/sum(h30$Freq[26:30])
}

h30$Country = factor(h30$Country,levels = rev(names))
h30$Levels = factor(h30$Levels,levels = c("4","3","2","1","0"))

h30$perc <- h30$perc * 100

ggplot(data = h30, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree are courts an obstacle to the current operations of this\nestablishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(h30$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("H30.png",height = 6, width = 7,units = c("in"))


# L30a - Labor Regulations

# Read in frequency values for different options by df

l30a <- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(l30a) <- c("Country","Levels","Freq","perc")

l30a$Country[1:5] <- names[1]
l30a$Country[6:10] <- names[2]
l30a$Country[11:15] <- names[3]
l30a$Country[16:20] <- names[4]
l30a$Country[21:25] <- names[5]
l30a$Country[26:30] <- names[6]


l30a$Levels <- seq(0,4,1)

l30a[1:5,3] <- c(47,30,10,1,0)
# count(Nig_Tech, labor_regulation_L30a)[2:6,2]
l30a[6:10,3] <- count(Nig_2014, l30a)[3:7,2]
l30a[11:15,3] <- count(Gha_2013, l30a)[2:6,2]
l30a[16:20,3] <- count(SA_2007, l30a)[,2]
l30a[21:25,3] <- count(Ken_2018, l30a)[1:5,2]
l30a[26:30,3] <- count(Tan_2013, l30a)[3:7,2]

for (i in 1:5) {
  l30a$perc[i] <- l30a$Freq[i]/sum(l30a$Freq[1:5])
  l30a$perc[i+5] <- l30a$Freq[i+5]/sum(l30a$Freq[6:10])
  l30a$perc[i+10] <- l30a$Freq[i+10]/sum(l30a$Freq[11:15])
  l30a$perc[i+15] <- l30a$Freq[i+15]/sum(l30a$Freq[16:20])
  l30a$perc[i+20] <- l30a$Freq[i+20]/sum(l30a$Freq[21:25])
  l30a$perc[i+25] <- l30a$Freq[i+25]/sum(l30a$Freq[26:30])
}

l30a$Country = factor(l30a$Country,levels = rev(names))
l30a$Levels = factor(l30a$Levels,levels = c("4","3","2","1","0"))

l30a$perc <- l30a$perc * 100

l30a[5,4] <- NA

ggplot(data = l30a, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree are labor regulations an obstacle to the current operations of this\nestablishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(l30a$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("L30a.png",height = 6, width = 7, units = c("in"))

# L30b - Inadequately Educated Workforce

# Read in frequency values for different options by df

l30b <- as.data.frame(matrix(0, ncol = 4, nrow = 30))
names(l30b) <- c("Country","Levels","Freq","perc")

l30b$Country[1:5] <- names[1]
l30b$Country[6:10] <- names[2]
l30b$Country[11:15] <- names[3]
l30b$Country[16:20] <- names[4]
l30b$Country[21:25] <- names[5]
l30b$Country[26:30] <- names[6]


l30b$Levels <- seq(0,4,1)

l30b[1:5,3] <- c(47,26,9,8,0)
# count(Nig_Tech, labor_regulation_l30b)[2:6,2]
l30b[6:10,3] <- count(Nig_2014, l30b)[3:7,2]
l30b[11:15,3] <- count(Gha_2013, l30b)[2:6,2]
l30b[16:20,3] <- count(SA_2007, l30b)[,2]
l30b[21:25,3] <- count(Ken_2018, l30b)[2:6,2]
l30b[26:30,3] <- count(Tan_2013, l30b)[3:7,2]

for (i in 1:5) {
  l30b$perc[i] <- l30b$Freq[i]/sum(l30b$Freq[1:5])
  l30b$perc[i+5] <- l30b$Freq[i+5]/sum(l30b$Freq[6:10])
  l30b$perc[i+10] <- l30b$Freq[i+10]/sum(l30b$Freq[11:15])
  l30b$perc[i+15] <- l30b$Freq[i+15]/sum(l30b$Freq[16:20])
  l30b$perc[i+20] <- l30b$Freq[i+20]/sum(l30b$Freq[21:25])
  l30b$perc[i+25] <- l30b$Freq[i+25]/sum(l30b$Freq[26:30])
}

l30b$Country = factor(l30b$Country,levels = rev(names))
l30b$Levels = factor(l30b$Levels,levels = c("4","3","2","1","0"))

l30b$perc <- l30b$perc * 100

l30b[5,4] <- NA

ggplot(data = l30b, aes(x = Country, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree is an inadequately educated workforce an obstacle to the current \noperations of this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(l30b$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("L30b.png",height = 6, width = 7, units = c("in"))


# Nig_Tech Business Climate Chart

# Import df

df_30_comp <- read_csv("~/Desktop/Nigeria Report Charts/Nigeria Data and R files/df_30_comp.csv")

df_30_comp$Levels = factor(df_30_comp$Levels,levels = c("4","3","2","1","0"))

df_30_comp$Variable = factor(df_30_comp$Variable,levels = rev(c(
  df_30_comp[31,1],df_30_comp[46,1],df_30_comp[1,1],df_30_comp[36,1],df_30_comp[26,1],
  df_30_comp[16,1],df_30_comp[21,1],df_30_comp[6,1],df_30_comp[41,1],
  df_30_comp[11,1],df_30_comp[56,1],df_30_comp[51,1]
)))


ggplot(data = df_30_comp, aes(x = Variable, y = perc, fill = Levels)) + geom_bar(stat="identity", width = 0.4) + 
  #scale_x_discrete(labels = c("Business Licensing\n and Permits","Corruption","Courts","Political Instability","Tax Administration","Tax Rates")) +
  scale_fill_manual(
    labels = c("Very severe\nobstacle","Major\nobstacle","Moderate\nobstacle","Minor\nobstacle", "No\nobstacle"),
    values = c("#00333F", "#004859","#00677F", "#0086A6","#00A5CC")) +
  labs(title = "To what degree are each of the following factors an obstacle\nto the current operations of this establishment?", x = "", y = "Percentage", x = "", y = "Percentage",
       fill = "") +
  geom_text(aes(label=round(df_30_comp$perc, digits = 0)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + coord_flip() +
  theme_classic() + theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    #panel.background = element_rect(fill = "transparent"), # bg of the panel
    #plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    #panel.grid.major = element_blank(), # get rid of major grid
    #panel.grid.minor = element_blank(), # get rid of minor grid
    # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    legend.position="bottom"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("30_Comp.png",height = 6, width = 7 width = 7, units = c("in"))


## Redo_FDI Chart

redo_fdi <- as.data.frame(matrix(0, ncol = 3, nrow = 6))
names(redo_fdi) <- c("Subsector","FDI","lab_val")

redo_fdi[,1] <- c("Fintech","Cleantech","E-commerce","Edtech","Agritech","Healthcare")
redo_fdi[,2] <- c(284.6,143.5,97.7,32.3,20.2,15.3)

for (i in 1:6) {
  redo_fdi[i,3] <- paste("$",toString(redo_fdi[i,2])," million", sep = "")  
}


redo_fdi$Subsector = factor(redo_fdi$Subsector,levels = rev(c("Fintech","Cleantech",
                                                          "E-commerce","Edtech","Agritech","Healthcare")))


ggplot(data = redo_fdi,aes(x= Subsector,y = FDI)) + geom_bar(stat = "identity", fill="#00677F", width = 0.5) +
  geom_text(aes(label= lab_val),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + 
  coord_flip() + labs(title = "FDI by tech sub-sector in Africa", x = "", y = "") +
  theme_classic() +
  theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    #panel.background = element_rect(fill = "transparent"), # bg of the panel
    #plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    #panel.grid.major = element_blank(), # get rid of major grid
    #panel.grid.minor = element_blank(), # get rid of minor grid
    #legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    legend.position="bottom"
  )


ggsave("FDI.png",height = 6, width = 7 units = c("in"))

# c30 <- c30[-c(1),]  # delete first row of NA values and use only rows with answers 0 to 4 (ignore NA and no response values)
# 
# c30$Gha <- count(Gha_2013, c30a)[2:6,2]
# c30$SA <- count(SA_2007, c30a)[,2]
# c30$Ken <- count(Ken_2018, c30a)[3:7,2]
# c30$Tan <- count(Tan_2013, c30a)[3:7,2]
# c30$Nig <- count(Nig_2014, c30a)[3:7,2]
# 
# c30_t <- t(c30)
# c30_t <- data.frame(C30= row.names(c30_t), c30_t, row.names=NULL)
# 
# names(c30_t) <- c30_t[1,]
# names(c30_t)[1] <- "C30"
# c30_t <- c30_t[-c(1),] 
# 
# c30_t$C30 <- names
# c30_t$C30 = factor(c30_t$C30,levels = names)
# 

# Import internet datasets

library(readxl)
library(ggplot2)
Internet_Costs <- read_excel("~/Desktop/Nigeria Report Charts/Nigeria Data and R files/Internet Costs.xlsx")
Internet_Speeds <- read_excel("~/Desktop/Nigeria Report Charts/Nigeria Data and R files/Internet Speeds.xlsx")

# Costs

Internet_Costs$Country = factor(Internet_Costs$Country,levels = rev(c(Internet_Costs$Country[1],Internet_Costs$Country[2],
                                                                      Internet_Costs$Country[3],Internet_Costs$Country[4],Internet_Costs$Country[5],
                                                                      Internet_Costs$Country[6],Internet_Costs$Country[7],Internet_Costs$Country[8],
                                                                      Internet_Costs$Country[9],Internet_Costs$Country[10],Internet_Costs$Country[11])))

ggplot(data = Internet_Costs,aes(x= Country,y = Cost)) + geom_bar(stat = "identity", fill="#00677F", width = 0.5) +
  geom_text(aes(label= paste("$",Cost,sep="")),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + 
  coord_flip() + labs(title = "Average monthly cost of broadband", x = "", y = "") +
  theme(
    # text = element_text(family="Khmer Sangam MN", size=10),
    # axis.text.x = element_text(angle=45),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=11, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold")
  ) + theme_classic() +
  theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  ) 


ggsave("Cost_Net.png",height = 7, units = c("in"))

# Speed

Internet_Speeds$Country = factor(Internet_Speeds$Country,levels = rev(c(Internet_Speeds$Country[1],Internet_Speeds$Country[2],
                                                                        Internet_Speeds$Country[3],Internet_Speeds$Country[4],Internet_Speeds$Country[5],
                                                                        Internet_Speeds$Country[6],Internet_Speeds$Country[7],Internet_Speeds$Country[8],
                                                                        Internet_Speeds$Country[9],Internet_Speeds$Country[10],Internet_Speeds$Country[11],
                                                                        Internet_Speeds$Country[12],Internet_Speeds$Country[13],Internet_Speeds$Country[14],
                                                                        Internet_Speeds$Country[15])))


# Select same countries as above

ggplot(data = Internet_Speeds,aes(x= Country,y = speed)) + geom_bar(stat = "identity", fill="#00677F", width = 0.5) +
  geom_text(aes(label= round(speed, digits = 2)),  size = 3, color="white",  position = position_stack(vjust = 0.5)) + 
  coord_flip() + labs(title = "Mean download speed in megabits per second", x = "", y = "") +
  theme(
    # text = element_text(family="Khmer Sangam MN", size=10),
    # axis.text.x = element_text(angle=45),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=11, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold")
  ) + theme_classic() +
  theme(
    text = element_text(family="Arial", size=10),
    plot.title = element_text(color="#162d51", size=12, face="bold"),
    axis.title.x = element_text(color="#162d51", size=10, face="bold"),
    axis.title.y = element_text(color="#162d51", size=11, face="bold"),
    legend.position="bottom"
  )

ggsave("Speed_Net.png",height = 7, units = c("in"))
