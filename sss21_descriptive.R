
#----------------------setting up-------------------------
setwd("-/Documents/Rprojects/data/student_satisfaction_survey")
getwd()
list.files()

#load package
library(readxl) #for read_excel
library(readr) #for read_csv
library(dplyr) #for select, rename, join, etc.
library(tidyverse) #for pipes
library(ggplot2)
library(stringr) 
#-------------------------data file prep-------------------------------------------------
#read data files, initial exploration
sss21<-read_excel("2021 SSS Raw.xlsx")
names(sss21)
#glimpse(sss21$`Invite Custom Field 1`)

#select (and rename) variables to build separate data frames
sss21.qual<-select(sss21, starts_with("Please explain"))
names(sss21.qual)

sss21.slct<-sss21 %>% select(`Invite Custom Field 1`, contains(":Please rate")) 
names(sss21.slct) [1]<-"pc_id"
names(sss21.slct)<- gsub(":[A-Za-z]*.*$", "", names(sss21.slct))
names(sss21.slct)

#revert to data frame to make variables and detect values for each variable
df_sss21<-as.data.frame(sss21.slct)
str(df_sss21)
names(df_sss21)



#=============================descriptive analysis=================================
#==================================================================================

#use df_sss21 to go through each item [2-25] 

#-------------------------------- Overall facilities--------------------------
#1.as factor
df_sss21 <- df_sss21 %>% mutate (facilities_fac = factor(df_sss21$`Overall facilities (i.e. academic, residential and recreational spaces)`, levels = c('Poor', 'Acceptable', 'Good', 'Excellent','NA')) )
df_sss21$facilities_fac #好像的确没有na/ no usage的选项？

#2.pivot table
facilities21_tab <- df_sss21 %>% 
  filter(!is.na(facilities_fac)) %>% 
  group_by(facilities_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
facilities21_tab

#3.bar plot
facilities21_tab %>% 
  ggplot(aes(x =facilities_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Facilities (i.e. academic, residential and recreational spaces) Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('facilities21.jpg', width = 6, height = 6, path="pic")

#--------------------------------Overall Services--------------------------------
#1.as factor
df_sss21 <- df_sss21 %>% mutate (service_fac = factor(df_sss21$`Overall Services`, levels = c('Poor', 'Acceptable', 'Good', 'Excellent','NA')) )
df_sss21$service_fac #好像的确没有na/ no usage的选项？

#2.pivot table
service21_tab  <- df_sss21 %>% 
  filter(!is.na(service_fac )) %>% 
  group_by(service_fac ) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
service21_tab

#3.bar plot
service21_tab %>% 
  ggplot(aes(x =service_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Overall Services Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('service21.jpg', width = 6, height = 6, path="pic")

#-------------------------------- Overall Experiences---------------------------
#0.investige factor names
df_sss21 %>% group_by (df_sss21[4]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (exp_fac = factor(df_sss21$`My overall experience`, levels = c('Poor', 'Acceptable', 'Good', 'Excellent')) )
df_sss21$exp_fac #好像的确没有na/ no usage的选项？

#2.pivot table
experience_tab  <- df_sss21 %>% 
  filter(!is.na(exp_fac)) %>% 
  group_by(exp_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
experience_tab

#3.bar plot
experience_tab %>% 
  ggplot(aes(x =exp_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Overall Experiences Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('experience21.jpg', width = 6, height = 6, path="pic")


#--------------------------------`Office of Student Accounts`--------------------------------
#0.investige factor names
df_sss21 %>% group_by (df_sss21[5]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (account_fac = factor(df_sss21$`Office of Student Accounts`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (account_fac) %>% count()

#2.pivot table
account_tab  <- df_sss21 %>% 
  filter(!is.na(account_fac)) %>% 
  group_by(account_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
account_tab

#3.bar plot
account_tab %>% 
  ggplot(aes(x =account_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Office of Student Accounts Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('account21.jpg', width = 6, height = 6, path="pic")

#---------------------------------`Health Center`-------------------------------------
#0.investige factor names
df_sss21 %>% group_by (df_sss21[6]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (health_fac = factor(df_sss21$`Health Center`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (health_fac) %>% count()

#2.pivot table
account_tab  <- df_sss21 %>% 
  filter(!is.na(health_fac)) %>% 
  group_by(health_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
account_tab

#3.bar plot
account_tab %>% 
  ggplot(aes(x =health_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Health Center Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('health21.jpg', width = 6, height = 6, path="pic")


#---------------------------`I.T. Helpdesk`---------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[7]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (IT_fac = factor(df_sss21$`I.T. Helpdesk`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (IT_fac) %>% count()

#2.pivot table
IT_tab  <- df_sss21 %>% 
  filter(!is.na(IT_fac)) %>% 
  group_by(IT_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
IT_tab

#3.bar plot
IT_tab %>% 
  ggplot(aes(x =IT_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I.T. Helpdesk Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('IT21.jpg', width = 6, height = 6, path="pic")


#----------------------------------`Campus Shuttle`----------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[8]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (shuttle_fac = factor(df_sss21$`Campus Shuttle`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (shuttle_fac) %>% count()

#2.pivot table
shuttle_tab  <- df_sss21 %>% 
  filter(!is.na(shuttle_fac)) %>% 
  group_by(shuttle_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
shuttle_tab

#3.bar plot
shuttle_tab %>% 
  ggplot(aes(x =shuttle_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Campus Shuttle Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('shuttle21.jpg', width = 6, height = 6, path="pic")



#-----------------------------------`Library's online databases`---------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[9]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (lib_fac = factor(df_sss21$`Library's online databases`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (lib_fac) %>% count()

#2.pivot table
lib_tab  <- df_sss21 %>% 
  filter(!is.na(lib_fac)) %>% 
  group_by(lib_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
lib_tab

#3.bar plot
lib_tab %>% 
  ggplot(aes(x =lib_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Library's online databases Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('lib21.jpg', width = 6, height = 6, path="pic")

#--------------------------------`Mail Services`------------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[10]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (mail_fac = factor(df_sss21$`Mail Services`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (mail_fac) %>% count()

#2.pivot table
mail_tab  <- df_sss21 %>% 
  filter(!is.na(mail_fac)) %>% 
  group_by(mail_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
mail_tab

#3.bar plot
mail_tab %>% 
  ggplot(aes(x =mail_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Mail Services Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('mail21.jpg', width = 6, height = 6, path="pic")



#--------------------------------------Academic Achievement Center----------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[11]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (aca_fac = factor(df_sss21$`Academic Achievement Center`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (aca_fac) %>% count()

#2.pivot table
aca_tab  <- df_sss21 %>% 
  filter(!is.na(aca_fac)) %>% 
  group_by(aca_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
aca_tab

#3.bar plot
aca_tab %>% 
  ggplot(aes(x =aca_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Academic Achievement Center Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('acacenter21.jpg', width = 6, height = 6, path="pic")


#-------------------------Office of Student Financial Planning (Financial Aid)-----------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[12]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (financeaid_fac = factor(df_sss21$`Office of Student Financial Planning (Financial Aid)`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (financeaid_fac) %>% count()

#2.pivot table
financeaid_tab  <- df_sss21 %>% 
  filter(!is.na(financeaid_fac)) %>% 
  group_by(financeaid_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
financeaid_tab

#3.bar plot
financeaid_tab %>% 
  ggplot(aes(x =financeaid_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Office of Student Financial Planning (Financial Aid) Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('financeaid21.jpg', width = 6, height = 6, path="pic")



#--------------------------------`Registrar's Office`---------------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[13]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (registar_fac = factor(df_sss21$`Registrar's Office`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (registar_fac) %>% count()

#2.pivot table
registar_tab  <- df_sss21 %>% 
  filter(!is.na(registar_fac)) %>% 
  group_by(registar_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
registar_tab

#3.bar plot
registar_tab %>% 
  ggplot(aes(x =registar_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Registrar's Office Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('registar21.jpg', width = 6, height = 6, path="pic")



#-----------------------------------`Career Development Center`--------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[14]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (career_fac = factor(df_sss21$`Career Development Center`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (career_fac) %>% count()

#2.pivot table
career_tab  <- df_sss21 %>% 
  filter(!is.na(career_fac)) %>% 
  group_by(career_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
career_tab

#3.bar plot
career_tab %>% 
  ggplot(aes(x =career_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Career Development Center Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('career21.jpg', width = 6, height = 6, path="pic")


#-------------------------------`Counseling Center (in Mott House or virtual)`-----------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[15]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (counsel_fac = factor(df_sss21$ `Counseling Center (in Mott House or virtual)`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (counsel_fac) %>% count()

#2.pivot table
counsel_tab  <- df_sss21 %>% 
  filter(!is.na(counsel_fac)) %>% 
  group_by(counsel_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
counsel_tab

#3.bar plot
counsel_tab %>% 
  ggplot(aes(x =counsel_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Counseling Center (in Mott House or virtual) Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('counseling21.jpg', width = 6, height = 6, path="pic")



#----------------------------------Intercultural Center & Commuter Cottage (IC3)----------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[16]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (iccc_fac = factor(df_sss21$ `Intercultural Center & Commuter Cottage (IC3)`, levels = c('Extremely Dissatisfied', 'Dissatisfied', 'Satisfied', 'Extremely Satisfied', 'Not Used')) )
#check
df_sss21 %>% group_by (iccc_fac) %>% count()

#2.pivot table
iccc_tab  <- df_sss21 %>% 
  filter(!is.na(iccc_fac)) %>% 
  group_by(iccc_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
iccc_tab

#3.bar plot
iccc_tab %>% 
  ggplot(aes(x =iccc_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Intercultural Center & Commuter Cottage (IC3) Satisfaction"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('iccc21.jpg', width = 6, height = 6, path="pic")



#------------------------------------`Lasell students have a strong sense of school spirit.`------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[17]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (spirit_fac = factor(df_sss21$`Lasell students have a strong sense of school spirit.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (spirit_fac) %>% count()

#2.pivot table
spirit_tab  <- df_sss21 %>% 
  filter(!is.na(spirit_fac)) %>% 
  group_by(spirit_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
spirit_tab

#3.bar plot
spirit_tab %>% 
  ggplot(aes(x =spirit_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Lasell students have a strong sense of school spirit"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('spirit21.jpg', width = 6, height = 6, path="pic")



#------------------------There is a strong sense of academic integrity at Lasell.`------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[18]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (integrity_fac = factor(df_sss21$`There is a strong sense of academic integrity at Lasell.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (integrity_fac) %>% count()

#2.pivot table
integrity_tab  <- df_sss21 %>% 
  filter(!is.na(integrity_fac)) %>% 
  group_by(integrity_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
integrity_tab

#3.bar plot
integrity_tab %>% 
  ggplot(aes(x =integrity_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("There is a strong sense of academic integrity at Lasell"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('integrity21.jpg', width = 6, height = 6, path="pic")


#------------------------- `I have had positive experiences with individuals different than myself.`---------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[19]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (diverse_fac = factor(df_sss21$`I have had positive experiences with individuals different than myself.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (diverse_fac) %>% count()

#2.pivot table
diverse_tab  <- df_sss21 %>% 
  filter(!is.na(diverse_fac)) %>% 
  group_by(diverse_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
diverse_tab

#3.bar plot
diverse_tab %>% 
  ggplot(aes(x =diverse_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I have had positive experiences with individuals different than myself"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('diverse21.jpg', width = 6, height = 6, path="pic")



#-----------------------------`I am able to register for classes I need with few conflicts.-------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[20]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (registerclass_fac = factor(df_sss21$`I am able to register for classes I need with few conflicts.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (registerclass_fac) %>% count()

#2.pivot table
registerclass_tab  <- df_sss21 %>% 
  filter(!is.na(registerclass_fac)) %>% 
  group_by(registerclass_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
registerclass_tab

#3.bar plot
registerclass_tab %>% 
  ggplot(aes(x =registerclass_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I am able to register for classes I need with few conflicts"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('registerclass21.jpg', width = 6, height = 6, path="pic")



#----------------------------`Living conditions in the residence halls are comfortable.`--------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[21]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (living_fac = factor(df_sss21$`Living conditions in the residence halls are comfortable.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (living_fac) %>% count()

#2.pivot table
living_tab  <- df_sss21 %>% 
  filter(!is.na(living_fac)) %>% 
  group_by(living_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
living_tab

#3.bar plot
living_tab %>% 
  ggplot(aes(x =living_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Living conditions in the residence halls are comfortable"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('living21.jpg', width = 6, height = 6, path="pic")



#---------------------------`Living conditions in the residence halls are comfortable.`---------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[22]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (acacommit_fac = factor(df_sss21$`There is a commitment to academic excellence.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (acacommit_fac) %>% count()

#2.pivot table
acacommit_tab  <- df_sss21 %>% 
  filter(!is.na(acacommit_fac)) %>% 
  group_by(acacommit_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
acacommit_tab

#3.bar plot
acacommit_tab %>% 
  ggplot(aes(x =acacommit_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("There is a commitment to academic excellence"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('acacommit21.jpg', width = 6, height = 6, path="pic")



#----------------------------`Faculty care about me as an individual.`--------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[23]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (faccare_fac = factor(df_sss21$`Faculty care about me as an individual.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (faccare_fac) %>% count()

#2.pivot table
faccare_tab  <- df_sss21 %>% 
  filter(!is.na(faccare_fac)) %>% 
  group_by(faccare_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
faccare_tab

#3.bar plot
faccare_tab %>% 
  ggplot(aes(x =faccare_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Faculty care about me as an individual"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('faccare21.jpg', width = 6, height = 6, path="pic")



#---------------------------`Faculty care about me as an individual.`---------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[24]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (community_fac = factor(df_sss21$ `I feel like an integral part of the Lasell University community.` , levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (community_fac) %>% count()

#2.pivot table
community_tab  <- df_sss21 %>% 
  filter(!is.na(community_fac)) %>% 
  group_by(community_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
community_tab

#3.bar plot
community_tab %>% 
  ggplot(aes(x =community_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I feel like an integral part of the Lasell University community"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('community21.jpg', width = 6, height = 6, path="pic")



#-----------------------------`Tuition paid here is a worthwhile investment.`-------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[25]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (tuitionworth_fac = factor(df_sss21$`Tuition paid here is a worthwhile investment.` , levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (tuitionworth_fac) %>% count()

#2.pivot table
tuitionworth_tab  <- df_sss21 %>% 
  filter(!is.na(tuitionworth_fac)) %>% 
  group_by(tuitionworth_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
tuitionworth_tab

#3.bar plot
tuitionworth_tab %>% 
  ggplot(aes(x =tuitionworth_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Tuition paid here is a worthwhile investment"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('tuitionworth21.jpg', width = 6, height = 6, path="pic")



#-------------------------`I feel my voice is heard as a student.`-----------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[26]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (voice_fac = factor(df_sss21$`I feel my voice is heard as a student.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (voice_fac) %>% count()

#2.pivot table
voice_tab  <- df_sss21 %>% 
  filter(!is.na(voice_fac)) %>% 
  group_by(voice_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
voice_tab

#3.bar plot
voice_tab %>% 
  ggplot(aes(x =voice_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I feel my voice is heard as a student"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('voice21.jpg', width = 6, height = 6, path="pic")



#----------------------------`Faculty are usually available after class and during office hours.`--------------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[27]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (facavailable_fac = factor(df_sss21$`Faculty are usually available after class and during office hours.`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (facavailable_fac) %>% count()

#2.pivot table
facavailable_tab  <- df_sss21 %>% 
  filter(!is.na(facavailable_fac)) %>% 
  group_by(facavailable_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
facavailable_tab

#3.bar plot
facavailable_tab %>% 
  ggplot(aes(x =facavailable_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("Faculty are usually available after class and during office hours"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('facavailable21.jpg', width = 6, height = 6, path="pic")



#-------------`I was able to register for the course types that I wanted (on-campus, online, or flex)`-------------------``
#0.investige factor names
df_sss21 %>% group_by (df_sss21[28]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (registerwant_fac = factor(df_sss21$`I was able to register for the course types that I wanted (on-campus, online, or flex)`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (registerwant_fac) %>% count()

#2.pivot table
registerwant_tab  <- df_sss21 %>% 
  filter(!is.na(registerwant_fac)) %>% 
  group_by(registerwant_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
registerwant_tab

#3.bar plot
registerwant_tab %>% 
  ggplot(aes(x =registerwant_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I was able to register for the course types that I wanted (on-campus, online, or flex)"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('registerwant21.jpg', width = 6, height = 6, path="pic")



#-------------`I was able to register for the course types that I wanted (on-campus, online, or flex)`---------------`
#0.investige factor names
df_sss21 %>% group_by (df_sss21[29]) %>% count()

#1.as factor
df_sss21 <- df_sss21 %>% mutate (registerwant_fac = factor(df_sss21$`I was able to register for the course types that I wanted (on-campus, online, or flex)`, levels = c('Strongly Disagree', 'Disagree', 'Agree', 'Strongly Agree')) )
#check
df_sss21 %>% group_by (registerwant_fac) %>% count()

#2.pivot table
registerwant_tab  <- df_sss21 %>% 
  filter(!is.na(registerwant_fac)) %>% 
  group_by(registerwant_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#0.1f%%" )) #'%#.2f'
#check
registerwant_tab

#3.bar plot
registerwant_tab %>% 
  ggplot(aes(x =registerwant_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = str_wrap("I was able to register for the course types that I wanted (on-campus, online, or flex)"))+
  theme(plot.title = element_text(color = "#003057", size = 10, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  guides(y = "none")+
  theme(axis.ticks = element_blank())+
  geom_text(aes(label = percentage), vjust = 2, size = 3, color="#ffffff") 

# save the plot 
ggsave('registerwant21.jpg', width = 6, height = 6, path="pic")
