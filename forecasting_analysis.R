setwd("C:/Users/HP/Desktop/FORECASTING--R")
library(tidyverse)
final_dataset<-read_csv("Data.csv")
long_panel <- final_dataset %>%
  pivot_longer(
    cols = c(EUR, GBP, SEK, NOK, CHF),
    names_to = "Currency",
    values_to = "value"
  ) %>%
  pivot_longer(
    cols = c(EUR_a, GBP_a, SEK_a, NOK_a, CHF_a),
    names_to = "Currency_a",
    values_to = "Actual"
  ) %>%
  filter(str_remove(Currency_a, "_a") == Currency) %>%
  select(-Currency_a) %>% rename(AI_Forecast=value)%>%
  filter(AI %in% c("COPILOT", "GEMINI"))

long_panel<-long_panel %>% mutate(Horizon=case_when(Horizon=="h=1"~1,
                                          Horizon=="h=2"~2,
                                          Horizon=="h=3"~3,
                                          Horizon=="h=4"~4,
                                          Horizon=="h=5"~5,
                                          Horizon=="h=6"~6),
                        AI=case_when( AI=="COPILOT"~1,
                                      AI=="GEMINI"~2),
                        Currency=case_when(Currency=="EUR"~1,
                                           Currency=="GBP"~2,
                                           Currency=="SEK"~3,
                                           Currency=="NOK"~4,
                                           Currency=="CHF"~5)) %>% select(-day)

write_csv(long_panel,"panlong.csv")




###Q4 a and b Calculating the raw forecast errors and percent forecast errors.
long_panel1<-long_panel %>% mutate(FE=Actual-as.numeric(AI_Forecast),
                         PFE=FE/Actual)

##Question 4 c Calculating Average Forecast Errors and the RMSPE
rmse <- function(actual, predicted, na.rm = TRUE) {
  sqrt(mean((actual - predicted)^2, na.rm = na.rm))
}

tab<-long_panel1 %>% 
  group_by(Horizon,AI,Prompt, Currency) %>% 
  summarize(AFE=mean(FE),
            RMSE=rmse(Actual, AI_Forecast)) %>% mutate(AFE=round(AFE,3))

## Generating the AFE tables for Copilot and Gemini Under Prompt 1 and 2

##Generating a table for Copilot Prompt 1 Average Forecast Errors
Copilot_Prompt1_Afe <- tab %>% ungroup() %>%  
  filter(Prompt == 1, AI == 1) %>% 
  select(Currency, Horizon, AFE) %>%
  pivot_wider(names_from = Horizon,
              values_from = AFE,
              names_prefix = "H=") %>% ungroup()



##Generating a table for Gemini Prompt 1 Average Forecast Errors
Gemini_Prompt1_Afe <- tab %>% ungroup() %>%  
  filter(Prompt == 1, AI == 2) %>% 
  select(Currency, Horizon, AFE) %>%
  pivot_wider(names_from = Horizon,
              values_from = AFE,
              names_prefix = "H=") %>% ungroup()



#####Generating a table for Copilot Prompt 2 Average Forecast Errors
Copilot_Prompt2_Afe <- tab %>% ungroup() %>%  
  filter(Prompt == 2, AI == 1) %>% 
  select(Currency, Horizon, AFE) %>%
  pivot_wider(names_from = Horizon,
              values_from = AFE,
              names_prefix = "H=") %>% ungroup()


####Generating a table for Gemini Prompt 1 Average Forecast Errors
Gemini_Prompt2_Afe <- tab %>% ungroup() %>%  
  filter(Prompt == 2, AI == 2) %>% 
  select(Currency, Horizon, AFE) %>%
  pivot_wider(names_from = Horizon,
              values_from = AFE,
              names_prefix = "H=") %>% ungroup()


####Generating a table for Copilot Prompt 1 Root Mean Square Prediction Forecast Error
Copilot_Prompt1_RMSPE <- tab %>% ungroup() %>%  
  filter(Prompt == 1, AI == 1) %>% 
  select(Currency, Horizon, RMSE) %>%
  pivot_wider(names_from = Horizon,
              values_from = RMSE,
              names_prefix = "H=") %>% ungroup()


######Generating a table for Gemini Prompt 1 Root Mean Square Prediction Forecast Error
Gemini_Prompt1_RMSPE <- tab %>% ungroup() %>%  
  filter(Prompt == 1, AI == 2) %>% 
  select(Currency, Horizon, RMSE) %>%
  pivot_wider(names_from = Horizon,
              values_from = RMSE,
              names_prefix = "H=") %>% ungroup()

#########Generating a table for Copilot Prompt 2 Root Mean Square Prediction Forecast Error
Copilot_P2_RMSPE <- tab %>% ungroup() %>%  
  filter(Prompt == 2, AI == 1) %>% 
  select(Currency, Horizon, RMSE) %>%
  pivot_wider(names_from = Horizon,
              values_from = RMSE,
              names_prefix = "H=") %>% ungroup()

#####Generating a table for Gemini Prompt 2 Root Mean Square Prediction Forecast Error
Gemini_Prompt2_RMPSE <- tab %>% ungroup() %>%  
  filter(Prompt == 2, AI == 2) %>% 
  select(Currency, Horizon, RMSE) %>%
  pivot_wider(names_from = Horizon,
              values_from = RMSE,
              names_prefix = "H=") 

##Q4d Calculating the Mean Directional Error
##For Horizon 1
Mean_directional1<-long_panel  %>% 
  group_by(Currency, Prompt, AI)%>%
  filter(Horizon==1) %>%
  mutate(ActualD=sign(Actual - lag(Actual)),
         PredictedD=sign(as.numeric(AI_Forecast)-lag(Actual)),
         Direction=ifelse(ActualD==PredictedD,0,1)
  ) %>% ungroup()

MeanDirectionalError <- Mean_directional1 %>% group_by(Currency, Prompt, AI) %>%
  summarize(MeanDirectionalError=mean(Direction, na.rm=TRUE))

##Generating the tables for the mean directional errors
##MDE for Copilot 
Copilot_MeanDirectionalError<-MeanDirectionalError %>% filter(AI==1) %>% select(-AI) %>% 
  pivot_wider(names_from = Prompt,
              values_from = MeanDirectionalError,
              names_prefix = "Prompt=") 


##Mean Directional Error For Gemini
Gemini_MeanDirectionalError<-MeanDirectionalError %>% filter(AI==2) %>% select(-AI) %>% 
  pivot_wider(names_from = Prompt,
              values_from = MeanDirectionalError,
              names_prefix = "Prompt=") 

##Q5 Producing benchmark forecasts for each assigned currency using AR1 Model and Random Walk
##AR1 Model

NOK1<-read_csv("C:\\Users\\HP\\Desktop\\FORECASTING--R\\DEXNOUS.csv")%>%
  mutate(Actual2=DEXNOUS,
         Currency=4) %>%
  select(-DEXNOUS)
SEK1<-read_csv("C:\\Users\\HP\\Desktop\\FORECASTING--R\\DEXSDUS.csv")%>%
  mutate(Actual2=DEXSDUS,
         Currency=3) %>%
  select(-DEXSDUS)
CHF1<-read_csv("C:\\Users\\HP\\Desktop\\FORECASTING--R\\DEXSZUS.csv")%>%
  mutate(Actual2=DEXSZUS,
         Currency=5) %>%
  select(-DEXSZUS)
EUR1<-read_csv("C:\\Users\\HP\\Desktop\\FORECASTING--R\\DEXUSEU.csv")%>%
  mutate(Actual2=1/DEXUSEU,
         Currency=1) %>%
  select(-DEXUSEU)
GBP1<-read_csv("C:\\Users\\HP\\Desktop\\FORECASTING--R\\DEXUSUK.csv")%>%
  mutate(Actual2=1/DEXUSUK,
         Currency=2) %>%
  select(-DEXUSUK)


Actual2<-bind_rows(NOK1,SEK1,CHF1,EUR1,GBP1)
write_csv(Actual2,"Actual2.csv")



## Generating AR(1) models for each horizon
Act<-read_csv('Actual2.csv')
##AR1 Model horizon 1
summary(reg1_5<-lm(Actual2~lag(Actual2),data=filter(Act, Currency==5)))
summary(reg1_4<-lm(Actual2~lag(Actual2),data=filter(Act, Currency==4)))
summary(reg1_2<-lm(Actual2~lag(Actual2),data=filter(Act, Currency==2)))
summary(reg1_1<-lm(Actual2~lag(Actual2),data=filter(Act, Currency==1)))
summary(reg1_3<-lm(Actual2~lag(Actual2),data=filter(Act, Currency==3)))

library(stargazer)


stargazer(reg1_5, reg1_4, reg1_2, reg1_1, reg1_3,
          type = "text",
          out = "Ar1_horizon_1.doc",
          title = "AR1 Model - Horizon 1",
          column.labels = c("CHF", "NOK", "GBP", "EUR", "SEK"),
          covariate.labels = "Lag 1 Actual",
          dep.var.labels = "Actual",
          star.cutoffs = c(0.05, 0.01, 0.001))


##Generating the AR1 Model for Horizon 2
summary(reg2_5<-lm(Actual2~lag(Actual2,2),data=filter(Act, Currency==5)))
summary(reg2_4<-lm(Actual2~lag(Actual2,2),data=filter(Act, Currency==4)))
summary(reg2_2<-lm(Actual2~lag(Actual2,2),data=filter(Act, Currency==2)))
summary(reg2_1<-lm(Actual2~lag(Actual2,2),data=filter(Act, Currency==1)))
summary(reg2_3<-lm(Actual2~lag(Actual2,2),data=filter(Act, Currency==3)))

stargazer(reg2_5, reg2_4, reg2_2, reg2_1, reg2_3,
          type = "text",
          out = "Ar1_horizon2.doc",
          title = "AR1 Model - Horizon 2",
          column.labels = c("CHF", "NOK", "GBP", "EUR", "SEK"),
          covariate.labels = "Lag 1 Actual",
          dep.var.labels = "Actual",
          star.cutoffs = c(0.05, 0.01, 0.001))


##Generating the AR1 Model for Horizon 3
summary(reg3_5<-lm(Actual2~lag(Actual2,3),data=filter(Act, Currency==5)))
summary(reg3_4<-lm(Actual2~lag(Actual2,3),data=filter(Act, Currency==4)))
summary(reg3_2<-lm(Actual2~lag(Actual2,3),data=filter(Act, Currency==2)))
summary(reg3_1<-lm(Actual2~lag(Actual2,3),data=filter(Act, Currency==1)))
summary(reg3_3<-lm(Actual2~lag(Actual2,3),data=filter(Act, Currency==3)))


stargazer(reg3_5, reg3_4, reg3_2, reg3_1, reg3_3,
          type = "text",
          out = "Ar1_Horizon3.doc",
          title = "AR1 Model - Horizon 3",
          column.labels = c("CHF", "NOK", "GBP", "EUR", "SEK"),
          covariate.labels = "Lag 1 Actual",
          dep.var.labels = "Actual",
          star.cutoffs = c(0.05, 0.01, 0.001))



##Generating the AR1 Model for Horizon 4
summary(reg4_5<-lm(Actual2~lag(Actual2,4),data=filter(Act, Currency==5)))
summary(reg4_4<-lm(Actual2~lag(Actual2,4),data=filter(Act, Currency==4)))
summary(reg4_2<-lm(Actual2~lag(Actual2,4),data=filter(Act, Currency==2)))
summary(reg4_1<-lm(Actual2~lag(Actual2,4),data=filter(Act, Currency==1)))
summary(reg4_3<-lm(Actual2~lag(Actual2,4),data=filter(Act, Currency==3)))

stargazer(reg4_5, reg4_4, reg4_2, reg4_1, reg4_3,
          type = "text",
          out = "Ar1_Horizon4.doc",
          title = "AR1 Model - Horizon 4",
          column.labels = c("CHF", "NOK", "GBP", "EUR", "SEK"),
          covariate.labels = "Lag 1 Actual",
          dep.var.labels = "Actual",
          star.cutoffs = c(0.05, 0.01, 0.001))



##AR1 Model horizon 5
summary(reg5_5<-lm(Actual2~lag(Actual2,5),data=filter(Act, Currency==5)))
summary(reg5_4<-lm(Actual2~lag(Actual2,5),data=filter(Act, Currency==4)))
summary(reg5_2<-lm(Actual2~lag(Actual2,5),data=filter(Act, Currency==2)))
summary(reg5_1<-lm(Actual2~lag(Actual2,5),data=filter(Act, Currency==1)))
summary(reg5_3<-lm(Actual2~lag(Actual2,5),data=filter(Act, Currency==3)))


stargazer(reg5_5, reg5_4, reg5_2, reg5_1, reg5_3,
          type = "text",
          out = "Ar1_Horizon5.doc",
          title = "AR1 Model - Horizon 5",
          column.labels = c("CHF", "NOK", "GBP", "EUR", "SEK"),
          covariate.labels = "Lag 1 Actual",
          dep.var.labels = "Actual",
          star.cutoffs = c(0.05, 0.01, 0.001))


##AR1 Model horizon 6
summary(reg6_5<-lm(Actual2~lag(Actual2,6),data=filter(Act, Currency==5)))
summary(reg6_4<-lm(Actual2~lag(Actual2,6),data=filter(Act, Currency==4)))
summary(reg6_2<-lm(Actual2~lag(Actual2,6),data=filter(Act, Currency==2)))
summary(reg6_1<-lm(Actual2~lag(Actual2,6),data=filter(Act, Currency==1)))
summary(reg6_3<-lm(Actual2~lag(Actual2,6),data=filter(Act, Currency==3)))


stargazer(reg6_5, reg6_4, reg6_2, reg6_1, reg6_3,
          type = "text",
          out = "AR1_Horizon6.doc",
          title = "AR1 Model - Horizon 6",
          column.labels = c("CHF", "NOK", "GBP", "EUR", "SEK"),
          covariate.labels = "Lag 1 Actual",
          dep.var.labels = "Actual",
          star.cutoffs = c(0.05, 0.01, 0.001))


#### Generating AR(1) Forecasts for each currency across horizons using the estimated AR(1)
Act1<-Act %>% mutate(AR_h1=case_when(Currency==5~(0.001616 + 0.998045 *lag(Actual2)),
                                  Currency==4~(0.054526+0.994615*lag(Actual2)),
                                  Currency==2~(0.003965+0.994894*lag(Actual2)),
                                  Currency==1~(0.004867 +0.994611*lag(Actual2)),
                                  Currency==3~(0.049425+0.995072*lag(Actual2))),
                  AR_h2=case_when(Currency==5~(0.003786+0.995522*lag(Actual2,2)),
                                  Currency==4~(0.107725+0.989424*lag(Actual2,2)),
                                  Currency==2~(0.007978 +0.989809*lag(Actual2,2)),
                                  Currency==1~(0.008821 +0.990260*lag(Actual2,2)),
                                  Currency==3~(0.096339 +0.990426*lag(Actual2,2))),
                  AR_h3=case_when(Currency==5~(0.004738+0.994396*lag(Actual2,3)),
                                  Currency==4~(0.151968+0.985138*lag(Actual2,3)),
                                  Currency==2~(0.012458+0.984121*lag(Actual2,3)),
                                  Currency==1~(0.012840 +0.985889*lag(Actual2,3)),
                                  Currency==3~( 0.141181+0.986009*lag(Actual2,3))),
                  AR_h4=case_when(Currency==5~(0.007352+0.991372*lag(Actual2,4)),
                                  Currency==4~(0.216203 +0.978873 *lag(Actual2,4)),
                                  Currency==2~(0.018263+0.976717*lag(Actual2,4)),
                                  Currency==1~(0.017689 +0.980583*lag(Actual2,4)),
                                  Currency==3~(0.195251 +0.980686*lag(Actual2,4))),
                  AR_h5=case_when(Currency==5~( 0.008718+0.989804*lag(Actual2,5)),
                                  Currency==4~(0.272028+0.973488 *lag(Actual2,5)),
                                  Currency==2~(0.020035 +0.974546*lag(Actual2,5)),
                                  Currency==1~( 0.020475+0.977632*lag(Actual2,5)),
                                  Currency==3~( 0.230456+0.977351*lag(Actual2,5))),
                  AR_h6=case_when(Currency==5~(0.011460 +0.986607*lag(Actual2,6)),
                                  Currency==4~(0.32350+0.96842*lag(Actual2,6)),
                                  Currency==2~(0.024508 +0.968767*lag(Actual2,6)),
                                  Currency==1~(0.025129 +0.972512*lag(Actual2,6)),
                                  Currency==3~(0.276796 +0.972756*lag(Actual2,6))))


##Random walk
Act1<-Act1 %>%group_by(Currency) %>%  
  mutate(RW1=case_when(Currency==5~(lag(Actual2)),
                       Currency==4~(lag(Actual2)),
                       Currency==2~(lag(Actual2)), 
                       Currency==1~(lag(Actual2)), 
                       Currency==3~(lag(Actual2))),
         RW2=case_when(Currency==5~(lag(Actual2,2)),
                       Currency==4~(lag(Actual2,2)),
                       Currency==2~(lag(Actual2,2)), 
                       Currency==1~(lag(Actual2,2)), 
                       Currency==3~(lag(Actual2,2))),
         RW3=case_when(Currency==5~(lag(Actual2,3)),
                       Currency==4~(lag(Actual2,3)),
                       Currency==2~(lag(Actual2,3)), 
                       Currency==1~(lag(Actual2,3)), 
                       Currency==3~(lag(Actual2,3))),
         RW4=case_when(Currency==5~(lag(Actual2,4)),
                       Currency==4~(lag(Actual2,4)),
                       Currency==2~(lag(Actual2,4)), 
                       Currency==1~(lag(Actual2,4)), 
                       Currency==3~(lag(Actual2,4))),
         RW5=case_when(Currency==5~(lag(Actual2,5)),
                       Currency==4~(lag(Actual2,5)),
                       Currency==2~(lag(Actual2,5)), 
                       Currency==1~(lag(Actual2,5)), 
                       Currency==3~(lag(Actual2,5))),
         RW6=case_when(Currency==5~(lag(Actual2,6)),
                       Currency==4~(lag(Actual2,6)),
                       Currency==2~(lag(Actual2,6)), 
                       Currency==1~(lag(Actual2,6)), 
                       Currency==3~(lag(Actual2,6)))) %>% ungroup()

##Computing forecasts errors of AR(1) horizons and Random Walk

Act1<-Act1 %>% mutate(AR_FE1=Actual2-AR_h1,
                  AR_FE2=Actual2-AR_h2,
                  AR_FE3=Actual2-AR_h3,
                  AR_FE4=Actual2-AR_h4,
                  AR_FE5=Actual2-AR_h5,
                  AR_FE6=Actual2-AR_h6,
                  RW1_FE=Actual2-RW1,
                  RW2_FE=Actual2-RW2,
                  RW3_FE=Actual2-RW3,
                  RW4_FE=Actual2-RW4,
                  RW5_FE=Actual2-RW5,
                  RW6_FE=Actual2-RW6)



##Testing if the average forecast errors are zero
##For GBP
summary(m1<-lm(AR_FE1~1, data=filter(Act1, Currency==2)))
summary(m2<-lm(AR_FE2~1, data=filter(Act1, Currency==2)))
summary(m3<-lm(AR_FE3~1, data=filter(Act1, Currency==2)))
summary(m4<-lm(AR_FE4~1, data=filter(Act1, Currency==2)))
summary(m5<-lm(AR_FE5~1, data=filter(Act1, Currency==2)))
summary(m6<-lm(AR_FE6~1, data=filter(Act1, Currency==2)))
summary(m7<-lm(RW1_FE~1, data=filter(Act1, Currency==2)))
summary(m8<-lm(RW2_FE~1, data=filter(Act1, Currency==2)))
summary(m9<-lm(RW3_FE~1, data=filter(Act1, Currency==2)))
summary(m10<-lm(RW4_FE~1, data=filter(Act1, Currency==2)))
summary(m11<-lm(RW5_FE~1, data=filter(Act1, Currency==2)))
summary(m12<-lm(RW6_FE~1, data=filter(Act1, Currency==2)))


stargazer(m1, m2, m3, m4, m5,m6,
          type = "text",
          out = "GBP1_AR.doc",
          digits=2,
          title = "AFE From AR1 Forecast Of GBP ",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(m7, m8, m9, m10, m11,m12,
          type = "text",
          out = "GBP1_RW.doc",
          digits=2,
          title = "AFE From RW1 Forecast Of GBP ",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))


##For CHF
summary(ch1<-lm(AR_FE1~1, data=filter(Act1, Currency==5)))
summary(ch2<-lm(AR_FE2~1, data=filter(Act1, Currency==5)))
summary(ch3<-lm(AR_FE3~1, data=filter(Act1, Currency==5)))
summary(ch4<-lm(AR_FE4~1, data=filter(Act1, Currency==5)))
summary(ch5<-lm(AR_FE5~1, data=filter(Act1, Currency==5)))
summary(ch6<-lm(AR_FE6~1, data=filter(Act1, Currency==5)))
summary(ch7<-lm(RW1_FE~1, data=filter(Act1, Currency==5)))
summary(ch8<-lm(RW2_FE~1, data=filter(Act1, Currency==5)))
summary(ch9<-lm(RW3_FE~1, data=filter(Act1, Currency==5)))
summary(ch10<-lm(RW4_FE~1, data=filter(Act1, Currency==5)))
summary(ch11<-lm(RW5_FE~1, data=filter(Act1, Currency==5)))
summary(ch12<-lm(RW6_FE~1, data=filter(Act1, Currency==5)))

stargazer(ch1, ch2, ch3, ch4, ch5,ch6,
          type = "text",
          out = "CHF1_AR.doc",
          digits=2,
          title = "AFE From AR1 Forecast Of CHF",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(ch7, ch8, ch9, ch10, ch11,ch12,
          type = "text",
          out = "CHF1_RW.doc",
          digits=2,
          title = "AFE From RW1 Forecast Of CHF ",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

##For SEK
summary(sk1<-lm(AR_FE1~1, data=filter(Act1, Currency==3)))
summary(sk2<-lm(AR_FE2~1, data=filter(Act1, Currency==3)))
summary(sk3<-lm(AR_FE3~1, data=filter(Act1, Currency==3)))
summary(sk4<-lm(AR_FE4~1, data=filter(Act1, Currency==3)))
summary(sk5<-lm(AR_FE5~1, data=filter(Act1, Currency==3)))
summary(sk6<-lm(AR_FE6~1, data=filter(Act1, Currency==3)))
summary(sk7<-lm(RW1_FE~1, data=filter(Act1, Currency==3)))
summary(sk8<-lm(RW2_FE~1, data=filter(Act1, Currency==3)))
summary(sk9<-lm(RW3_FE~1, data=filter(Act1, Currency==3)))
summary(sk10<-lm(RW4_FE~1, data=filter(Act1, Currency==3)))
summary(sk11<-lm(RW5_FE~1, data=filter(Act1, Currency==3)))
summary(sk12<-lm(RW6_FE~1, data=filter(Act1, Currency==3)))


stargazer(sk1, sk2, sk3, sk4, sk5,sk6,
          type = "text",
          out = "SEK1_AR.doc",
          digits=2,
          title = "AFE From AR1 Forecast Of SEK",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(sk7, sk8, sk9, sk10, sk11,sk12,
          type = "text",
          out = "SEK1_RW.doc",
          digits=2,
          title = "AFE From RW1 Forecast Of SEK ",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

##For EUR
summary(eu1<-lm(AR_FE1~1, data=filter(Act1, Currency==1)))
summary(eu2<-lm(AR_FE2~1, data=filter(Act1, Currency==1)))
summary(eu3<-lm(AR_FE3~1, data=filter(Act1, Currency==1)))
summary(eu4<-lm(AR_FE4~1, data=filter(Act1, Currency==1)))
summary(eu5<-lm(AR_FE5~1, data=filter(Act1, Currency==1)))
summary(eu6<-lm(AR_FE6~1, data=filter(Act1, Currency==1)))
summary(eu7<-lm(RW1_FE~1, data=filter(Act1, Currency==1)))
summary(eu8<-lm(RW2_FE~1, data=filter(Act1, Currency==1)))
summary(eu9<-lm(RW3_FE~1, data=filter(Act1, Currency==1)))
summary(eu10<-lm(RW4_FE~1, data=filter(Act1, Currency==1)))
summary(eu11<-lm(RW5_FE~1, data=filter(Act1, Currency==1)))
summary(eu12<-lm(RW6_FE~1, data=filter(Act1, Currency==1)))


stargazer(eu1, eu2, eu3, eu4, eu5,eu6,
          type = "text",
          out = "EUR1_AR.doc",
          digits=2,
          title = "AFE From AR1 Forecast Of EUR",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(eu7, eu8, eu9, eu10, eu11,eu12,
          type = "text",
          out = "EUR1_RW.doc",
          digits=2,
          title = "AFE From RW1 Forecast Of EUR",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))


##For NOK
summary(nk1<-lm(AR_FE1~1, data=filter(Act1, Currency==4)))
summary(nk2<-lm(AR_FE2~1, data=filter(Act1, Currency==4)))
summary(nk3<-lm(AR_FE3~1, data=filter(Act1, Currency==4)))
summary(nk4<-lm(AR_FE4~1, data=filter(Act1, Currency==4)))
summary(nk5<-lm(AR_FE5~1, data=filter(Act1, Currency==4)))
summary(nk6<-lm(AR_FE6~1, data=filter(Act1, Currency==4)))
summary(nk7<-lm(RW1_FE~1, data=filter(Act1, Currency==4)))
summary(nk8<-lm(RW2_FE~1, data=filter(Act1, Currency==4)))
summary(nk9<-lm(RW3_FE~1, data=filter(Act1, Currency==4)))
summary(nk10<-lm(RW4_FE~1, data=filter(Act1, Currency==4)))
summary(nk11<-lm(RW5_FE~1, data=filter(Act1, Currency==4)))
summary(nk12<-lm(RW6_FE~1, data=filter(Act1, Currency==4)))


stargazer(nk1, nk2, nk3, nk4, nk5,nk6,
          type = "text",
          out = "NOK1_AR.doc",
          digits=2,
          title = "AFE From AR1 Forecast Of NOK",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(nk7, nk8, nk9, nk10, nk11,nk12,
          type = "text",
          out = "NOK1_RW.doc",
          digits=2,
          title = "AFE From RW1 Forecast Of NOK ",
          column.labels = c("Horizon1", "Horizon2", "Horizon3","Horizon4","Horizon5","Horizon6"),
          dep.var.labels = c("AFE","AFE","AFE","AFE","AFE","AFE") ,
          star.cutoffs = c(0.05, 0.01, 0.001))



Act1$observation_date <- format(as.Date(Act1$observation_date), "%m/%d/%Y")

### Conducting the Diebold Mariano West Test Using Harvey Correction to compare the AI models to AR(1) and RW

### Copilot vs Random Walk DM Tests — All Horizons, All Currencies
library(forecast)

dmvresults1 <- list()

for (h in 1:6) {
  Copilot_RW <- long_panel1 %>%
    filter(AI == 1, Prompt == 1, Horizon == h) %>%
    select(-Actual)
  
  c_h <- Copilot_RW %>%
    left_join(Act1, by = c("Horizon_Date" = "observation_date", "Currency"))
  
  rw_fe_col <- paste0("RW", h, "_FE")
  
  for (curr in 1:5) {
    df_curr <- filter(c_h, Currency == curr)
    
    test <- dm.test(
      df_curr$FE,
      df_curr[[rw_fe_col]],
      alternative = "two.sided",
      h = 1,
      power = 2
    )
    
    dmvresults1[[paste0("H", h, "_C", curr)]] <- list(
      horizon   = h,
      currency  = curr,
      statistic = test$statistic,
      p_value   = test$p.value
    )
  }
}

## Summarizing as a tidy data frame
dmvresults_cr <- do.call(rbind, lapply(dmvresults1, as.data.frame))
rownames(dmvresults_cr) <- NULL


cr<-dmvresults_cr %>% select(-statistic) %>% mutate(p_value=round(p_value,2)) %>% 
  pivot_wider(names_from = horizon,
              values_from = p_value,
              names_prefix = "H") %>% arrange(currency)
cr_t <- flextable(cr) %>% set_caption("DMW Test P-Values: Copilot vs Random Walk")
doc <- read_docx() %>% body_add_flextable(cr_t)
print(doc, target = "cr1.docx")



### Gemini vs Random Walk DM Tests — All Horizons, All Currencies
library(forecast)

dmvresults2 <- list()

for (h in 1:6) {
  Gem_rw <- long_panel1 %>%
    filter(AI == 2, Prompt == 1, Horizon == h) %>%
    select(-Actual)
  
  c_h <- Gem_rw %>%
    left_join(Act1, by = c("Horizon_Date" = "observation_date", "Currency"))
  
  rw_fe_col <- paste0("RW", h, "_FE")
  
  for (curr in 1:5) {
    df_curr <- filter(c_h, Currency == curr)
    
    test <- dm.test(
      df_curr$FE,
      df_curr[[rw_fe_col]],
      alternative = "two.sided",
      h = 1,
      power = 2
    )
    
    dmvresults2[[paste0("H", h, "_C", curr)]] <- list(
      horizon   = h,
      currency  = curr,
      statistic = test$statistic,
      p_value   = test$p.value
    )
  }
}

## Summarize as a tidy data frame
results_gr <- do.call(rbind, lapply(dmvresults2, as.data.frame))
rownames(results_gr) <- NULL

##Now, I get a table in word for it. Check your folder after running to see it
gr<-results_gr %>% select(-statistic) %>% mutate(p_value=round(p_value,2)) %>% 
  pivot_wider(names_from = horizon,
              values_from = p_value,
              names_prefix = "H") %>% arrange(currency)
gr_t <- flextable(gr) %>% set_caption("DMW Test P-Values: Gemini Vs Random Walk")
doc <- read_docx() %>% body_add_flextable(gr_t)
print(doc, target = "gr1.docx")


### Copilot vs AR1 DM Tests — All Horizons, All Currencies
library(forecast)

dmvresults3 <- list()

for (h in 1:6) {
  cop_ar <- long_panel1 %>%
    filter(AI == 1, Prompt == 1, Horizon == h) %>%
    select(-Actual)
  
  c_h <- cop_ar %>%
    left_join(Act1, by = c("Horizon_Date" = "observation_date", "Currency"))
  
  ar_fe_col <- paste0("AR_FE", h)
  
  for (curr in 1:5) {
    df_curr <- filter(c_h, Currency == curr)
    
    test <- dm.test(
      df_curr$FE,
      df_curr[[ar_fe_col]],
      alternative = "two.sided",
      h = 1,
      power = 2
    )
    
    dmvresults3[[paste0("H", h, "_C", curr)]] <- list(
      horizon   = h,
      currency  = curr,
      statistic = test$statistic,
      p_value   = test$p.value
    )
  }
}

results_ca <- do.call(rbind, lapply(dmvresults3, as.data.frame))
rownames(results_ca) <- NULL


##Getting the table
ca<-results_ca %>% select(-statistic) %>% mutate(p_value=round(p_value,2)) %>% 
  pivot_wider(names_from = horizon,
              values_from = p_value,
              names_prefix = "H") %>% arrange(currency)

ca_t <- flextable(ca) %>% set_caption("DMW Test P-Values: Copilot vs AR1")
doc <- read_docx() %>% body_add_flextable(ca_t)
print(doc, target = "ca1.docx")



### Gemini vs AR1 DM Tests — All Horizons, All Currencies
dmvresults4 <- list()

for (h in 1:6) {
  Gem_ar <- long_panel1 %>%
    filter(AI == 2, Prompt == 1, Horizon == h) %>%
    select(-Actual)
  
  c_h <- Gem_ar %>%
    left_join(Act1, by = c("Horizon_Date" = "observation_date", "Currency"))
  
  ar_fe_col <- paste0("AR_FE", h)
  
  for (curr in 1:5) {
    df_curr <- filter(c_h, Currency == curr)
    
    test <- dm.test(
      df_curr$FE,
      df_curr[[ar_fe_col]],
      alternative = "two.sided",
      h = 1,
      power = 2
    )
    
    dmvresults4[[paste0("H", h, "_C", curr)]] <- list(
      horizon   = h,
      currency  = curr,
      statistic = test$statistic,
      p_value   = test$p.value
    )
  }
}

results_ga <- do.call(rbind, lapply(dmvresults4, as.data.frame))
rownames(results_ga) <- NULL

##Getting the table
ga<-results_ga %>% select(-statistic) %>% mutate(p_value=round(p_value,2)) %>% 
  pivot_wider(names_from = horizon,
              values_from = p_value,
              names_prefix = "H") %>% arrange(currency)
ga_t <- flextable(ga) %>% set_caption("DMW Test P-Values: Gemini Vs AR1")
doc <- read_docx() %>% body_add_flextable(ga_t)
print(doc, target = "ga1.docx")



##Q8 Running Panel Regression with square percent error as the dependent variable to check accuracy between AI tools
##For Copilot and Prompt 1
CP1<-long_panel1 %>% filter(AI==1 & Prompt==1) %>% select(-c(AI_Forecast, Actual, FE, AI)) %>% mutate(PFE_G=PFE)
##For Gemini and Prompt 1
GP1<-long_panel1 %>% filter(AI==2 & Prompt==1) %>% select(PFE_P=PFE)

##Combined files for prompt 1
comb1<-bind_cols(CP1,GP1)
comb1<-comb1 %>% mutate(Change_SPE=((PFE_G)^2-(PFE_P)^2))

pm1<-lm(Change_SPE~factor(day_id)+factor(Horizon)+factor(Currency), data=comb1)

library(sandwich)
library(lmtest)
pm11<-coeftest(pm1, vcov = NeweyWest(pm1))



##For Copilot and Prompt 2
CP2<-long_panel1 %>% filter(AI==1 & Prompt==2) %>% select(-c(AI_Forecast, Actual, FE, AI)) %>% mutate(PFE_G=PFE)
##For Gemini  and Prompt 2
GP2<-long_panel1 %>% filter(AI==2 & Prompt==2) %>% select(PFE_P=PFE)






##Combined files for prompt 2
comb2<-bind_cols(CP2,GP2)
comb2<-comb2 %>% mutate(Change_SPE=((PFE_G)^2-(PFE_P)^2))

pm2<-lm(Change_SPE~factor(day_id)+factor(Horizon)+factor(Currency), data=comb2)
pm2<-lm(Change_SPE~factor(day_id)+factor(Horizon)+factor(Currency), data=comb2)
pm22<-coeftest(pm2, vcov = NeweyWest(pm2))


##Getting the table
library(stargazer)
stargazer(pm11, pm22,
          type = "text",
          out = "NWwpanel.doc",
          column.labels = c("Prompt 1", "Prompt 2"),
          covariate.labels = c(
            # rename day_id levels (adjust numbers to match your data)
            2, 3, 4, 5,
            # rename Horizon levels
            2, 3, 4, 5,6,
            # rename Currency levels
            "GBP", "SEK", "NOK", "CHF"
          ),
          dep.var.labels = "Change in SPE",
          star.cutoffs = c(0.05, 0.01, 0.001))

