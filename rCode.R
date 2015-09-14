library(dplyr)
library(ggplot2)
library(xtable)
library(stringr)

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
ggplot(res1, aes(x = hod, y = tot)) + geom_line() +   
  labs(title = 'Deaths Pattern within a Day', x = 'Hour', y = 'Number of deaths') +
  theme(plot.title = element_text(face="bold", vjust=1.5)) +
  expand_limits(x=c(0,23))
ggsave("overall.png", width = 10, height = 6)

# merge the two table
head(deaths)
head(codes)
res2 <- merge(deaths, codes, by = 'cod')
head(res2)

# get the difference
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

head(res1) #res1 is hourly number of deaths
res1$p_hod <- res1$tot / sum(res1$tot) 

res5 <- merge(res4, res1, by = 'hod')
head(res5)

res6 <- res5 %>%
  group_by(cod) %>%
  summarise(n = sum(hod_cod_tot), dist = mean((p_hod_cod - p_hod)^2)) %>%
  filter(n > 50)
head(res6)
res6 <- merge(res6, codes, by = 'cod')
res6$resCondi <- ifelse(res6$res > 1.5, 'Abnormal Death', 'Normal Death') 
res6$resCondi <- factor(res6$resCondi, levels = c('Abnormal Death','Normal Death'))
ggplot(data = res6, aes(x = n, y = dist)) + geom_point()
last_plot() + scale_x_log10() + 
  scale_y_log10() +
  geom_smooth(method = 'lm', se = F)

res6$res <- resid(lm(log(dist) ~ log(n), data = res6))
coef(lm(log(dist) ~ log(n), data = res6))

g1 <- ggplot(data = res6, aes(x = n, y = res, color = resCondi)) + 
  geom_hline(yintercept = 1.5, colour = "grey50") +
  geom_point() + 
  scale_x_log10() +
  labs(title = 'Residuals vs. Frequency of Death', 
       x = 'Number of Obs.', y = 'Residuals of Obs.') +
  theme(plot.title = element_text(face="bold"), 
        legend.position="top") +
  scale_color_manual(name = '', labels = c("Death Deviating from Normal Pattern", "Normal Death"),
                     values = c("red", "black"))
ggplotly(g1)

res7 <- res6[res6$res > 1.5, ]
res7 <- arrange(res7, desc(res))
dim(res7)
res7$d_code <- str_c('d', 1:13)
res7
g2 <- ggplot(data = res7, aes(x = n, y = res, color = d_code)) + 
  geom_point()+
  labs(title = 'Residuals vs. Frequency of Abnormal Death', 
       x = 'Number of Obs.', y = 'Residuals of Obs.') +
  theme(plot.title = element_text(face="bold", vjust=1.5),
        legend.position="none")

ggplotly(g2)

d_des <- res7[, c(7, 5)]

ggplot(data = res6, aes(x = n, y = res, color = resCondi)) + 
  geom_hline(yintercept = 1.5, colour = "grey50") +
  geom_point() + 
  scale_x_log10() +
  labs(title = 'Residuals vs. Number of Observation', 
       x = 'Number of Obs.', y = 'Residuals of Obs.') +
  theme(plot.title = element_text(face="bold"), 
        legend.position="top") +
  scale_color_manual(name = '', labels = c("Death Deviate from Normal Pattern"),
                     values = c("red"))
res_p <- res6[, c(1, 2, 4, 5, 6)]
head(res_p)
library(xlsx)
write.xlsx(res7, "C:/Users/Yingsen/Documents/data visualization/tidyDataCopy/mydata2.xlsx")
