library(tidyverse)
library(psych)
library(reshape2)

#####
##AAPECS data
#####
#setwd("~/Dropbox/GPT Personality/AAPECS Data and Output")
aapecs_sr = read.csv("selfReport.csv")
aapecs_sr_neo = aapecs_sr %>%
        select(participantID,
          neo1:neo120_R)
for (i in 1:120) {
  col = paste0("neo",i,sep="")
  colr = paste0("neo",i,"_R",sep="")
  if (colr %in% names(aapecs_sr_neo)) {
     aapecs_sr_neo[col] = aapecs_sr_neo[colr]
  }
}

aapecs_sr_neo_r = aapecs_sr_neo %>%
  select(participantID,neo1:neo120)
names(aapecs_sr_neo_r) = gsub("neo","question_",names(aapecs_sr_neo_r))
write.csv(aapecs_sr_neo_r,"llm_outputs/aapecs/per_question_responses/aapecs_self_report_text_per_question_responses_int.csv",row.names=F)

sst_sr_neo = read.csv("SST_round1_round2_items_scales.csv")
sst_sr_neo = sst_sr_neo %>%
  select(subject_num,Q1:Q9)
names(sst_sr_neo) = gsub("Q","question_",names(sst_sr_neo))
names(sst_sr_neo)[1] = "subject"
write.csv(sst_sr_neo,"llm_outputs/sst/per_question_responses/sst_self_report_text_per_question_responses_int.csv",row.names=F)

models = c("self_report","claude_sonnet","gemini_flash","gpt_41_mini","gpt_41","grok3_beta","llama_maverick","qwen3_235b","llms_avg")

outputA = data.frame(model=NA,N=NA,E=NA,A=NA,O=NA,C=NA,n1=NA,n2=NA,n3=NA,n4=NA,n5=NA,n6=NA,e1=NA,e2=NA,e3=NA,e4=NA,e5=NA,e6=NA,
                    o1=NA,o2=NA,o3=NA,o4=NA,o5=NA,o6=NA,a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,
                    c1=NA,c2=NA,c3=NA,c4=NA,c5=NA,c6=NA)
outputS = data.frame(model=NA,N=NA,E=NA,O=NA,A=NA,C=NA)

i = 1
for (model in models) {
  outputA[i,"model"] = model
  outputS[i,"model"] = model
#Load .csv file
aapecs_df_item<-read.csv(paste0("llm_outputs/aapecs/per_question_responses/aapecs_", model, "_text_per_question_responses_int.csv",sep=""), header=T)

###Psychometrics 


#Domains
aapecs_df_item<-aapecs_df_item%>%mutate(ipip_N=rowMeans(.[,c("question_1","question_31","question_61","question_91",
                                                             "question_6",  "question_36","question_66","question_96",
                                                              "question_11", "question_41","question_71","question_101",
                                                              "question_16","question_46","question_76","question_106",
                                                              "question_21","question_51","question_81","question_111",
                                                             "question_26","question_56","question_86","question_116")], na.rm=TRUE))

aapecs_df_item<-aapecs_df_item%>%mutate(ipip_E=rowMeans(.[,c("question_2","question_32","question_62","question_92",
                                                            "question_7","question_37","question_67","question_97",
                                                            "question_12","question_42","question_72","question_102",
                                                            "question_17","question_47","question_77","question_107",
                                                            "question_22","question_52","question_82","question_112",
                                                            "question_27","question_57","question_87","question_117")], na.rm=TRUE))

aapecs_df_item<-aapecs_df_item%>%mutate(ipip_O=rowMeans(.[,c("question_3","question_33","question_63","question_93",
                                                        "question_8","question_38","question_68","question_98",
                                                        "question_13","question_43","question_73","question_103",
                                                        "question_18","question_48","question_78","question_108",
                                                        "question_23","question_53","question_83","question_113",
                                                        "question_28","question_58","question_88","question_118")], na.rm=TRUE))


aapecs_df_item<-aapecs_df_item%>%mutate(ipip_A=rowMeans(.[,c("question_4","question_34","question_64","question_94",
                                                        "question_9","question_39","question_69","question_99",
                                                        "question_14","question_44","question_74","question_104",
                                                        "question_19","question_49","question_79","question_109",
                                                        "question_24","question_54","question_84","question_114",
                                                        "question_29","question_59","question_89","question_119")], na.rm=TRUE))

aapecs_df_item<-aapecs_df_item%>%mutate(ipip_C=rowMeans(.[,c("question_5",  "question_35","question_65","question_95",
                                                             "question_10","question_40","question_70","question_100",
                                                             "question_15","question_45","question_75","question_105",
                                                             "question_20","question_50","question_80","question_110",
                                                             "question_25","question_55","question_85","question_115",
                                                            "question_30","question_60","question_90","question_120")], na.rm=TRUE))
##
## Neuroticism
##

a = aapecs_df_item %>%
  select("question_1","question_31","question_61","question_91",
         "question_6",  "question_36","question_66","question_96",
         "question_11", "question_41","question_71","question_101",
         "question_16","question_46","question_76","question_106",
         "question_21","question_51","question_81","question_111",
         "question_26","question_56","question_86","question_116") %>%
  alpha(title = "N Domain")
outputA[i,"N"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_1","question_31","question_61","question_91") %>%
  alpha(title = "n1-anxiety")
outputA[i,"n1"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_6", "question_36","question_66","question_96") %>%
  alpha(title = "n2-anger")
outputA[i,"n2"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_11", "question_41","question_71","question_101") %>%
  alpha(title = "n3-depression")
outputA[i,"n3"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_16","question_46","question_76","question_106") %>%
  alpha(title = "n4-self-consciousness")
outputA[i,"n4"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_21","question_51","question_81","question_111") %>%
  alpha(title = "n5-impulsivity")
outputA[i,"n5"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_26","question_56","question_86","question_116") %>%
  alpha(title = "n6-vulnerability")
outputA[i,"n6"] = a$total$raw_alpha

##
## Extraversion
##

a = aapecs_df_item %>%
  select("question_2","question_32","question_62","question_92",
          "question_7","question_37","question_67","question_97",
          "question_12","question_42","question_72","question_102",
          "question_17","question_47","question_77","question_107",
          "question_22","question_52","question_82","question_112",
          "question_27","question_57","question_87","question_117") %>%
  alpha(title = "E Domain") 
outputA[i,"E"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_2","question_32","question_62","question_92") %>%
  alpha(title = "e1-warmth")
outputA[i,"e1"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_7","question_37","question_67","question_97") %>%
  alpha(title = "e2-gregariousness")
outputA[i,"e2"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_12","question_42","question_72","question_102") %>%
  alpha(title = "e3-assertiveness")
outputA[i,"e3"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_17","question_47","question_77","question_107") %>%
  alpha(title = "e4-activity")
outputA[i,"e4"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_22","question_52","question_82","question_112") %>%
  alpha(title = "e5-excitement-seeking")
outputA[i,"e5"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_27","question_57","question_87","question_117") %>%
  alpha(title = "e6-positive emotions")
outputA[i,"e6"] = a$total$raw_alpha

##Openness

a = aapecs_df_item %>%
  select("question_3","question_33","question_63","question_93",
              "question_8","question_38","question_68","question_98",
              "question_13","question_43","question_73","question_103",
              "question_18","question_48","question_78","question_108",
              "question_23","question_53","question_83","question_113",
              "question_28","question_58","question_88","question_118") %>%
  alpha(title = "O Domain")
outputA[i,"O"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_3","question_33","question_63","question_93") %>%
  alpha(title = "o1-fantasy")
outputA[i,"o1"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_8","question_38","question_68","question_98") %>%
  alpha(title = "o2-aesthetics")
outputA[i,"o2"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_13","question_43","question_73","question_103") %>%
  alpha(title = "o3-feelings")
outputA[i,"o3"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_18","question_48","question_78","question_108") %>%
  alpha(title = "o4-actions")
outputA[i,"o4"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_23","question_53","question_83","question_113") %>%
  alpha(title = "o5-ideas")
outputA[i,"o5"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_28","question_58","question_88","question_118") %>%
  alpha(title = "o6-values")
outputA[i,"o6"] = a$total$raw_alpha

##Agreeableness

a = aapecs_df_item %>%
  select("question_4","question_34","question_64","question_94",
         "question_9","question_39","question_69","question_99",
         "question_14","question_44","question_74","question_104",
         "question_19","question_49","question_79","question_109",
         "question_24","question_54","question_84","question_114",
         "question_29","question_59","question_89","question_119") %>%
  alpha(title = "A Domain")
outputA[i,"A"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_4","question_34","question_64","question_94") %>%
  alpha(title = "a1-trust")
outputA[i,"a1"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_9","question_39","question_69","question_99") %>%
  alpha(title = "a2-straightforwardness")
outputA[i,"a2"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_14","question_44","question_74","question_104") %>%
  alpha(title = "a3-altruism")
outputA[i,"a3"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_19","question_49","question_79","question_109") %>%
  alpha(title = "a4-compliance")
outputA[i,"a4"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_24","question_54","question_84","question_114") %>%
  alpha(title = "a5-modesty")
outputA[i,"a5"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_29","question_59","question_89","question_119") %>%
  alpha(title = "a6-tender-mindedness")
outputA[i,"a6"] = a$total$raw_alpha

##Conscientiousness

a = aapecs_df_item %>%
  select("question_5",  "question_35","question_65","question_95",
         "question_10","question_40","question_70","question_100",
         "question_15","question_45","question_75","question_105",
         "question_20","question_50","question_80","question_110",
         "question_25","question_55","question_85","question_115",
         "question_30","question_60","question_90","question_120")%>%
  alpha(title = "C Domain")
outputA[i,"C"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_5",  "question_35","question_65","question_95") %>%
  alpha(title = "c1-competence")
outputA[i,"c1"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_10","question_40","question_70","question_100") %>%
  alpha(title = "c2-order")
outputA[i,"c2"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_15","question_45","question_75","question_105") %>%
  alpha(title = "c3-dutifulness")
outputA[i,"c3"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_20","question_50","question_80","question_110") %>%
  alpha(title = "c4-achievement striving")
outputA[i,"c4"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_25","question_55","question_85","question_115") %>%
  alpha(title = "c5-self-discipline")
outputA[i,"c5"] = a$total$raw_alpha

a = aapecs_df_item %>%
  select("question_30","question_60","question_90","question_120") %>%
  alpha(title = "c6-deliberation")
outputA[i,"c6"] = a$total$raw_alpha


#####
##SST data
#####


#Load .csv file
file = paste0("llm_outputs/sst/per_question_responses/sst_",model,"_text_per_question_responses_int.csv",sep="")
if (file.exists(file)) {
  SST_df_item<-read.csv(file, header=T)
} else {
  
  i = i + 1
  next
}
#SST_df_item<-read.csv("q1_ratings_SST_GPT_4o_all.csv")

#Move ID to beginning
SST_df_item<-SST_df_item %>% 
  relocate(subject)

a = SST_df_item %>%
  select("question_1","question_31",
         "question_6",  "question_36",
         "question_11", "question_41",
         "question_16","question_46",
         "question_21","question_51",
         "question_26","question_56",) %>%
  alpha(title = "N Domain")
outputS[i,"N"] = a$total$raw_alpha

a = SST_df_item %>%
  select("question_2","question_32",
         "question_7","question_37",
         "question_12","question_42",
         "question_17","question_47",
         "question_22","question_52",
         "question_27","question_57") %>%
  alpha(title = "E Domain") 
outputS[i,"E"] = a$total$raw_alpha

a = SST_df_item %>%
  select("question_3","question_33",
         "question_8","question_38",
         "question_13","question_43",
         "question_18","question_48",
         "question_23","question_53",
         "question_28","question_58") %>%
  alpha(title = "O Domain")
outputS[i,"O"] = a$total$raw_alpha

a = SST_df_item %>%
  select("question_4","question_34",
         "question_9","question_39",
         "question_14","question_44",
         "question_19","question_49",
         "question_24","question_54",
         "question_29","question_59") %>%
  alpha(title = "A Domain")
outputS[i,"A"] = a$total$raw_alpha

a = SST_df_item %>%
  select("question_5",  "question_35",
         "question_10","question_40",
         "question_15","question_45",
         "question_20","question_50",
         "question_25","question_55",
         "question_30","question_60")%>%
  alpha(title = "C Domain")
outputS[i,"C"] = a$total$raw_alpha


i = i + 1
}

write.csv(outputA,"aapecs_cronbach2.csv",row.names=F)
write.csv(outputS,"sst_cronbach2.csv",row.names=F)

print(outputA[,1:6],digits=2)
print(outputS[,1:6],digits=2)
