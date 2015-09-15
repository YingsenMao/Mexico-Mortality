library(dplyr)
library(ggplot2)
library(xtable)
library(stringr)
library(gridExtra)
library(scales)

setwd('C:/Users/Yingsen/Documents/data visualization/tidyDataCopy')
deaths <- read.csv('deaths08.csv', header = TRUE)

deaths$hod[deaths$hod == 99] <- NA
deaths$hod[deaths$hod == 24] <- 0
deaths$hod[deaths$hod == 0] <- NA
deaths$hod <- as.integer(deaths$hod)  
deaths <- arrange(deaths, yod, mod, dod, hod, cod)
deaths <- deaths[c("yod", "mod", "dod", "hod", "cod")]

saveRDS(deaths, "deaths.rds") # Serialization version of save()
deaths <- readRDS("deaths.rds")
deaths <- filter(deaths, !is.na(hod))
head(deaths)
print(xtable(head(deaths)), type="html")
print(xtable(tail(deaths)), type="html")
#make a sample table in latex format
ok <- subset(deaths, yod == 2008 & mod != 0 & dod != 0)
xtable(ok[c(1, 1:14 * 2000), c("yod", "mod", "dod", "hod", "cod")], 
       "raw.tex")    #produce a sample table in latex

##input the diease description
codes <- read.csv("icd-main.csv")
saveRDS(codes, 'codes.rds')
codes <- readRDS('codes.rds')
print(xtable(head(codes)), type="html")
codes$disease <- sapply(codes$disease, function(x)
  str_c(strwrap(x, width = 30), collapse = '\n')) # format the long string
                                                  # str_c is equivalent to paste, 
                                                  # but it uses the
                                                  # empty string (¡°¡±) as the default separator

head(codes)
names(codes)[1] <- 'cod'
any(duplicated(codes$cod)) # is any duplicated code
codes[duplicated(codes$cod), ] # check all the duplicated code
codes <- codes[!duplicated(codes$cod), ]
head(codes)

# Display overall hourly deaths
res1 <- deaths %>%
  group_by(hod) %>%
  summarise(tot = n())
head(res1)
g1 <- ggplot(res1, aes(x = hod, y = tot)) + geom_line() +   
  labs(title = 'Deaths Pattern within a Day', x = 'Hour', y = 'Number of Deaths') +
  theme(plot.title = element_text(face="bold", vjust=1.5)) +
  expand_limits(x=c(0,23))
ggsave("overall.png", width = 10, height = 6)
(gg <- ggplotly(g1))
res1$p_hod <- res1$tot / sum(res1$tot) 
head(res1)
library(scales)
ggplot(res1, aes(x = hod, y = p_hod)) + geom_line() +   
  labs(title = 'Deaths Pattern within a Day (measured by percentage)', x = 'Hour', y = 'Percentage') +
  theme(plot.title = element_text(face="bold", vjust=1.5)) +
  expand_limits(x=c(0,23)) + scale_y_continuous(labels = percent)
ggsave("overall_percentage.png", width = 10, height = 6)


# merge the two table
head(deaths)
head(codes)
res2 <- merge(deaths, codes, by = 'cod')
head(res2)

## get the deviation between percent of EACH death within each
## hour over all THIS SPECIFIC DEATH and percent of death of this 
## specific hour over a day
res3 <- res2 %>%
  group_by(hod, cod) %>%
  summarise(hod_cod_tot = n()) 
head(res3)

res4 <- res3 %>%
  group_by(cod) %>%
  mutate(p_hod_cod = hod_cod_tot / sum(hod_cod_tot)) # mutate keep the 
                                         # original table
                                         # unchanged
head(res4)


# get a example of pattern of one death
# I21 is the Acute myocardial infarction
codes[grepl('myocardial infarction', codes$disease), ]
codes[grepl('drowning and submersion', codes$disease), ]

drowning <- subset(res4, cod == 'W74', select = c('p_hod_cod'))
infarction <- subset(res4, cod == 'I21', select = c('p_hod_cod'))
overall <- res1['p_hod']
Category <- factor(rep(c('Overall Death Pattern', 
                 'Drowning and Submersion in Water',
                 'Myocardial Infarction'), 
               each = 23), levels = c('Overall Death Pattern',
                                      'Drowning and Submersion in Water',
                                      'Myocardial Infarction'))
compare_df <- data.frame(Category = Category,
                         Hours = rep(1:23, 3),
                         Percentage = c(overall[[1]], drowning[[1]], 
                                        infarction[[1]]))

g1 <- ggplot(compare_df, aes(x = Hours, y = Percentage, color = Category)) + 
  geom_line() +
  labs(title = 'Comparison between Overall Death and Two particular Deaths') +
  theme(plot.title = element_text(face="bold"), 
        legend.position="top", legend.title=element_blank()) +
  scale_fill_discrete(breaks=c('Overall Death Pattern',
                               'Drowning and Submersion in Water',
                               'Myocardial Infarction'))
(gg <- ggplotly(g1))

head(res1) #res1 is hourly number of deaths

res5 <- merge(res4, res1, by = 'hod')
head(res5)
##
res_show <- res5[, c(1, 2, 4, 6)]
head(res_show)
names(res_show) <- c('hour', 'death code', 'proportion of code death', 'proportion of overall death')
print(xtable(head(res_show)), type = 'html')
res_show2 <- head(filter(res_show, hour == 2))
print(xtable(res_show2), type = 'html')
##

res6 <- res5 %>%
  group_by(cod) %>%
  summarise(n = sum(hod_cod_tot), dist = mean((p_hod_cod - p_hod)^2)) %>%
  filter(n > 50)
head(res6)
res6$dist
as.character(res6$dist)
substr(as.character(res6$dist), 1, 6)
##
res_ex <- head(res6)
char_num <- c("0.000891", "0.000738", "0.000198",
              "0.000439",  "0.000028", "0.000030")
names(res_ex) <- c('death code', 'count', 'distance')
res_ex$distance <- char_num
print(xtable(res_ex), type = 'html')
##
res6 <- merge(res6, codes, by = 'cod')
head(res6)
res6$resCondi <- ifelse(res6$dist > 1.5, 'Abnormal Death', 'Normal Death') 
res6$resCondi <- factor(res6$resCondi, levels = c('Abnormal Death','Normal Death'))
g1 <- ggplot(data = res6, aes(x = n, y = dist)) + geom_point() + 
  labs(title = 'Deviation vs. Freq of Each Death', x = 'Count', 
       y = 'Deivation')
ggplotly(g1)  

g1 + scale_x_log10() + scale_x_log10() +
  geom_smooth(method = 'lm', se = F)

res6$res <- resid(lm(log(dist) ~ log(n), data = res6))
coef(lm(log(dist) ~ log(n), data = res6))
res6$resCondi <- ifelse(res6$res > 1.5, 'Abnormal Death', 'Normal Death') 
res6$resCondi <- factor(res6$resCondi, levels = c('Abnormal Death','Normal Death'))
g1 <- ggplot(data = res6, aes(x = n, y = res, color = resCondi)) + 
  geom_hline(yintercept = 1.5, colour = "grey50") +
  geom_point() + 
  scale_x_log10() +
  labs(x = 'Number of Obs.', y = 'Residuals of Obs.') +
  theme(plot.title = element_blank(), 
        legend.position="top") +
  scale_color_manual(name = '', labels = c("Death Deviating from Normal Pattern", "Death following Normal Pattern"),
                     values = c("red", "black"))
ggplotly(g1)
ggsave(file="residualPlot.jpg", 
       path = 'C:/Users/Yingsen/Documents/data visualization/tidyDataCopy',
       width=9, height=6)

res7 <- res6[res6$res > 1.5, ]
res7 <- arrange(res7, desc(n))
dim(res7)
res7$d_code <- str_c('Code.', 1:13)
res7
g2 <- ggplot(data = res7, aes(x = n, y = res, color = d_code)) + 
  geom_point()+
  labs(title = 'Residuals vs. Frequency of Abnormal Death', 
       x = 'Number of Obs.', y = 'Residuals of Obs.') +
  theme(plot.title = element_text(face="bold", vjust=1.5),
        legend.position="none")

ggplotly(g2)


d_des <- res7[, c(7, 4)]
names(d_des) <- c('Death Code', 'Cause of Death')
print(xtable(d_des), type = 'html')

head(res5)
head(res7)
res8 <- res7[, c(1, 2, 4)]
res8$disease <- sapply(res8$disease, function(x)
  str_c(strwrap(x, width = 30), collapse = "\n"))
res9 <- res5[, c(1,2,4)]
res10 <- merge(res8, res9, by = 'cod')
head(res10)
res1
# Visualise unusual causes of death
ggplot(subset(res10, n > 350), aes(hod, p_hod_cod)) + 
  geom_line(aes(y = p_hod), data = res1, colour = "grey50") +
  geom_line() + 
  facet_wrap(~ disease, ncol = 3) +
  labs(x = 'Hours', y = 'Proportion of Death')
ggsave("unusual-big.png", width = 9, height = 6)
ggplot(subset(res10, n <= 350), aes(hod, p_hod_cod)) + 
  geom_line(aes(y = p_hod), data = res1, colour = "grey50") +
  geom_line() + 
  facet_wrap(~ disease, ncol = 3) +
  labs(x = 'Hours', y = 'Proportion of Death')
ggsave("unusual-sml.png", width = 9, height = 4)

