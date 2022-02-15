setwd("~/Documents/Rprojects/student_satisfaction_survey")
list.files()
source("set up full data.R") #run code from saved r script

#--------------------------------------------------------
#go through each item [1-24]
#[1] Overall facilities
#1.as factor
df_sss21 <- df_sss21 %>% mutate (facilities_fac = factor(df_sss21$`Overall facilities (i.e. academic, residential and recreational spaces)`, levels = c('Poor', 'Acceptable', 'Good', 'Excellent','NA')) )
df_sss21$facilities_fac #好像的确没有na/ no usage的选项？
#2.pivot table
facilities21_tab <- df_sss21 %>% 
  filter(!is.na(facilities_fac)) %>% 
  group_by(facilities_fac) %>% 
  summarize(cnt = n()) %>%
  mutate(percentage = sprintf(cnt/sum(cnt)*100, fmt= "%#1.0f%%" )) #'%#.2f'
facilities21_tab
#3.bar plot
facilities21_tab %>% 
  ggplot(aes(x =facilities_fac , y=percentage)) + 
  geom_col(fill = "#003057", color ="#ffffff") +
  theme_classic()+
  labs(title = "Facilities (i.e. academic, residential and recreational spaces) Satisfaction (2021)")+
  theme(plot.title = element_text(color = "#003057", size = 12, hjust = 0.5, face="bold"))+
  labs(x = "", y = "")+
  geom_text(aes(label = cnt), vjust = 2, size = 3, color="#ffffff") #vjust=-0.5 lable up
