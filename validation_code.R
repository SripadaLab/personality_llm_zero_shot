##### load packages
library(dplyr)
library(openxlsx)
library(lineup)

#### load data
aapecs_sr = read.csv("selfReport.csv")

#### #### #### #### #### #### #### #### #### #### #### #### 
#### #### correlations for comparing methods #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### 

aapecs_v = aapecs_sr %>%
  select(participantID,
         catAffectiveLability:catWorkaholism,
         iipscPA_Z:iipscNO_Z) %>% 
  select(-contains("_R"))


#### #### self-report #### #### 
sr_neo = aapecs_sr %>%
  select(neoOpenness, neoConscientiousness, neoExtraversion, neoAgreeableness, neoNeuroticism,
         catAffectiveLability:catWorkaholism,
         iipscPA_Z:iipscNO_Z) %>% 
  select(-contains("_R"))

sr_neo_matrix = round(cor(sr_neo),2)
sr_neo_matrix[upper.tri(sr_neo_matrix)] = ""
sr_neo_rdf = sr_neo_matrix %>% 
  as.data.frame() %>%
  select(starts_with("neo")) %>%
  mutate(var = rownames(sr_neo_matrix)) %>%
  filter(!grepl("neo", var)) %>%
  select(-var)

#### #### select self-report vars #### #### 
aapecs_sr_r = aapecs_sr %>%
  select(participantID,
         neoOpenness:neoVulnerability,
         catAffectiveLability:catPsychoticism,
         iipscPA_Z:iipscELEV)

#loop over LLM models

models = c("claude_sonnet","gemini_flash","gpt_41_mini","gpt_41","grok3_beta","llama_maverick","qwen3_235b","llms_avg")

output = list()
profile = data.frame(model=NA,neoOpenness=NA,neoConscientiousness=NA,neoExtraversion=NA,neoAgreeableness=NA,neoNeuroticism=NA)

for (model in models) {
  aapecs_q1 = read.csv(paste0("llm_outputs/aapecs/per_trait_scores/aapecs_",model,"_text_per_question_scores.csv",sep=""))
  aapecs_q1 = aapecs_q1[,names(aapecs_q1) != "X"]
  aapecs_q1_combined = aapecs_q1 %>%
    rename_with(~paste0(.,"_gpt"), -participantID) %>%
    left_join(aapecs_v) %>%
    rename_with(~gsub("neo","",.x)) %>%
    select(-participantID)

  aapecs_q1_matrix = round(cor(aapecs_q1_combined),2)
  aapecs_q1_matrix[upper.tri(aapecs_q1_matrix)] = ""
  aapecs_q1_rdf = aapecs_q1_matrix %>%
    as.data.frame() %>%
    select(ends_with("gpt")) %>%
    mutate(var = rownames(aapecs_q1_matrix)) %>%
    filter(!grepl("gpt", var)) %>%
    select(Openness_gpt, Conscientiousness_gpt, Extraversion_gpt, Agreeableness_gpt, Neuroticism_gpt)

  # #clean and combine gpt and self-report
  # aapecs_q1_combined = aapecs_q1 %>%
  #   rename_with(~paste0(.,"_gpt"), -participantID) %>%
  #   left_join(aapecs_sr_r) %>%
  #   rename_with(~gsub("neo","",.x)) %>%
  #   select(-participantID)
  # 
  # #get correlation matrix and make into nice df
  # aapecs_q1_matrix = round(cor(aapecs_q1_combined),2)
  # aapecs_q1_matrix[upper.tri(aapecs_q1_matrix)] = ""
  # aapecs_q1_rdf = aapecs_q1_matrix %>% 
  #   as.data.frame() %>%
  #   select(ends_with("gpt")) %>%
  #   mutate(var = rownames(aapecs_q1_matrix)) %>%
  #   filter(!grepl("gpt", var)) %>%
  #   select(var, everything())
  tmp = aapecs_q1_rdf
  tmp$measure = row.names(aapecs_q1_rdf)
  output[[model]] = tmp #aapecs_q1_rdf
  profile = rbind(profile,c(model,corbetw2mat(sr_neo_rdf,aapecs_q1_rdf)))
}

tmp = sr_neo_rdf
tmp$measure = row.names(sr_neo_rdf)
output[["self-report"]] = tmp

write.xlsx(output, file = 'llm_correlations.xlsx')
profile = profile[-1,]
profile
write.csv(profile,'llm_profile.csv',row.names=F)

df = profile
df[,-1] = lapply(df[,-1],function(x) as.numeric(as.character(x)))
options(digits=2)
print(df)

df = profile
formatted_df <- df
numeric_columns <- sapply(df, is.numeric)
formatted_df[numeric_columns] <- lapply(df[numeric_columns], function(x) format(x, nsmall = 2, digits = 2))

print(formatted_df, quote = FALSE, right = TRUE)
 

