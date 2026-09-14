#QuantAnalysisReplication.R
#This includes the quantitative analyses for the replication of the paper
#Perspectives on Projective Techniques in Consumer Research: A Review, Primary Analysis, and Methodological Guidance
#It includes the topic modeling, chi-squared tests, and correspondence analysis for the word association and sentence completion data.
#Copyright Stephen France 2026: sfrance@business.msstate.edu
#Please see license.txt in Github for the license for this code and the data files.  

#Ensure that any data files and LLMTextHelper.R are in the working directory
#Set working directory (you may need to change this to your own working director)
setwd("~/R/LLMProjective")

#The helper functions I wrote for the STM topic modeling
source("ProjectiveHelper.R")

#Load in libraries
if (!require("FactoMineR")) install.packages("FactoMineR")
library(FactoMineR)
if (!require("factoextra")) install.packages("factoextra")
library(factoextra)
if (!require("survey")) install.packages("survey")
library(survey)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("ggrepel")) install.packages("ggrepel")
library(ggrepel)
if (!require("grid")) install.packages("grid")
library(grid)
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)

#################################################################
#File preprocessing for Word Association
#################################################################

#Analysis for the word association
InWA<-read.csv("WordAssociationData.csv",stringsAsFactors = TRUE)
InWA<-InWA[complete.cases(InWA),]
WAMeltData<-reshape2::melt(InWA,id.vars=c("ID"),variable.name="City",value.name="Sentence")
WAMeltData$IDCity<-paste(WAMeltData$ID,WAMeltData$City,sep="_")
levels(WAMeltData$City)<-c(rep("LasVegas",4),
                           rep("NewYorkCity",4),
                           rep("Nashville",4),
                           rep("LosAngeles",4),
                           rep("NewOrleans",4))


#################################################################
#Topic modeling for Word Association
#################################################################

#Word Association Run analysis across all k to understand and evaluate
#solutions with respect to the number of topics
kVector<-seq(5,15,1)
k_result<-STMTopicModelSearch(WAMeltData,kVector)
PlotTopicResults(k_result)

#For the paper, we picked the nine topic solution
STMAll<-STMTopicModel(WAMeltData,Maxk=9)
stm_fit<-STMAll$stm_fit
stm_input<-STMAll$stm_input
#We used 
plot(stm_fit, n = 5)
print(labelTopics(stm_fit, n = 10))
theta_df <- as.data.frame(stm_fit$theta)
colnames(theta_df) <- paste0("Topic", 1:ncol(theta_df))
theta_df$City <- stm_input$meta$City
theta_df$Person <- stm_input$meta$ID
theta_df$Topics<-apply(theta_df[,1:9],1,which.max)
table1<-table(theta_df$City,theta_df$Topics)
TopicList<-c("Street Food & Entertainment","Casinos & Celebrities",
             "Walking & Crowds","Gambling & Bright Lights",
             "People of the City","Music & Concerts",
             "Partying & Casinos","Expensive & Crowded",
             "Big City Issues")
colnames(table1)<-TopicList
#The following code allows us to save and reload contingency tables
#without rerunning the topic modeling
#write.csv(theta_df,"WAHumanSolution.csv",row.names=FALSE)
#write.csv(table1,"WAHumanSolutionTable.csv",row.names=TRUE)
#table1<-read.csv("WAHumanSolutionTable.csv",stringsAsFactors = TRUE)
#rownames(table1)<-table1[,1]
#table1<-table1[,-1]

#################################################################
#Chi-squared tests for Word Association
#################################################################

#Basic chi-squared test
CSQContingency<-table1
CSQResults<-chisq.test(CSQContingency)
CSQResults
CSQResults$expected
#Manual calculation of effect size (Cramer's V)
CramersV<- sqrt(
  as.numeric(CSQResults$statistic) /
    (sum(CSQContingency) * min(nrow(CSQContingency) - 1, ncol(CSQContingency) - 1))
)
CramersV
theta_df$Topics <- factor(theta_df$Topics)

cluster_design <- svydesign(
  ids     = ~Person,
  weights = ~1,
  data    = theta_df
)

#Rao-Scott adjustment
SPrao_scott <- svychisq(
  ~City + Topics,
  design    = cluster_design,
  statistic = "F"
)
SPrao_scott

#################################################################
#Correspondence analysis for Word Association
#################################################################

CA <- CA(table1, graph = FALSE)
fviz_ca_biplot(CA, repel = TRUE, map="symbiplot",
               col.row = "red", 
               col.col = "blue",
               title = "Word Association", 
               arrow = c(TRUE, TRUE)) +
  coord_equal() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
top_n <- 10

#################################################################
#Correspondence analysis plot (with repelling labels)
#for Word Association
#################################################################

# Symbiplot scaling factors for Dimensions 1 and 2
sym_scale <- CA$eig[1:2, 1]^(1/4)

# Extract and rescale row coordinates
row_df <- as.data.frame(
  sweep(CA$row$coord[, 1:2], 2, sym_scale, "/")
)
colnames(row_df) <- c("Dim1", "Dim2")
row_df$label <- rownames(CA$row$coord)
row_df$type <- "Row"

# Extract and rescale column coordinates
col_df <- as.data.frame(
  sweep(CA$col$coord[, 1:2], 2, sym_scale, "/")
)
colnames(col_df) <- c("Dim1", "Dim2")
col_df$label <- rownames(CA$col$coord)
col_df$type <- "Column"

# Combine rows and columns so all labels repel one another
label_df <- rbind(row_df, col_df)
CurTitle<-"Word Association"
gg_hybrid <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Row arrows
  geom_segment(
    data = row_df,
    aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
    color = "red",
    linewidth = 0.5,
    alpha = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  
  # Column arrows
  geom_segment(
    data = col_df,
    aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
    color = "blue",
    linewidth = 0.5,
    alpha = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  
  # One label layer so everything repels everything else
  geom_text_repel(
    data = label_df,
    aes(x = Dim1, y = Dim2, label = label, color = type),
    size = 3.5,
    force = 10,
    force_pull = 0.15,
    box.padding = 1,
    point.padding = 0.4,
    max.overlaps = Inf,
    max.iter = 50000,
    max.time = 15,
    min.segment.length = 0,
    seed = 123,
    segment.color = "gray60",
    segment.alpha = 0.7
  ) +
  
  scale_color_manual(
    values = c(Row = "red", Column = "blue"),
    guide = "none"
  ) +
  
  coord_equal(clip = "off") +
  scale_x_continuous(expand = expansion(mult = 0.30)) +
  scale_y_continuous(expand = expansion(mult = 0.30)) +
  
  labs(
    title = CurTitle,
    x = paste0("Dimension 1 (", round(CA$eig[1, 2], 1), "%)"),
    y = paste0("Dimension 2 (", round(CA$eig[2, 2], 1), "%)")
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

gg_hybrid

#################################################################
#Top 10 terms for each topic
#################################################################

#Plot the top 10 terms for each topic
top_n <- 10
beta <- exp(stm_fit$beta$logbeta[[1]])
vocab <- stm_fit$vocab
terms_df <- as.data.frame(beta) %>%
  setNames(vocab) %>%
  mutate(topic_num = row_number()) %>%
  pivot_longer(
    cols = -topic_num,
    names_to = "token",
    values_to = "probability"
  ) %>%
  group_by(topic_num) %>%
  slice_max(probability, n = top_n, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    topic = TopicList[topic_num],
    topic = factor(topic, levels = TopicList),
    token = reorder_within(token, probability, topic)
  )

ggplot(terms_df, aes(x = token, y = probability, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "Term", y = "Probability") +
  theme_minimal()

#################################################################
#File preprocessing for Sentence Completion
#################################################################

#Now add the two SCs
InSC<-read.csv("SentenceCompletionData.csv",stringsAsFactors = TRUE)
InSCPos<-InSC[,c(1,2,4,6,8,10)]
InSCNeg<-InSC[,c(1,3,5,7,9,11)]
SPMeltData<-reshape2::melt(InSCPos,id.vars=c("ID"),variable.name="City",value.name="Sentence")
SPMeltData$IDCity<-paste(SPMeltData$ID,SPMeltData$City,sep="_")
levels(SPMeltData$City)<-c("LasVegas","NewYorkCity","Nashville","LosAngeles","NewOrleans")

SNMeltData<-reshape2::melt(InSCNeg,id.vars=c("ID"),variable.name="City",value.name="Sentence")
SNMeltData$IDCity<-paste(SNMeltData$ID,SNMeltData$City,sep="_")
levels(SNMeltData$City)<-c("LasVegas","NewYorkCity","Nashville","LosAngeles","NewOrleans")

#################################################################
#Topic modeling for Sentence Positive
#################################################################

#Sentence positive Run analysis across all k to understand and evaluate
#solutions with respect to the number of topics
kVector<-seq(5,15,1)
k_result<-STMTopicModelSearch(SPMeltData,kVector)
SPCompare<-PlotTopicResults(k_result,ChooseMetrics=c("semcoh","heldout"))

#For the paper we picked the 7 topic solution
STMAll<-STMTopicModel(SPMeltData,Maxk=7)
stm_fit<-STMAll$stm_fit
stm_input<-STMAll$stm_input
plot(stm_fit, n = 5)
print(labelTopics(stm_fit, n = 10))
theta_df <- as.data.frame(stm_fit$theta)
colnames(theta_df) <- paste0("Topic", 1:ncol(theta_df))
theta_df$City <- stm_input$meta$City
theta_df$Person <- stm_input$meta$ID
theta_df$Topics<-apply(theta_df[,1:7],1,which.max)
table1<-table(theta_df$City,theta_df$Topics)
TopicList<-c("Country Music","Fun in the City","Casinos & Gambling",
             "Walking the Streets","Celebrities & Sights",
             "Shopping, Beach, & Concerts","Great Food")
colnames(table1)<-TopicList
#write.csv(theta_df,"SPHumanSolution.csv",row.names=FALSE)
#write.csv(table1,"SPHumanSolutionTable.csv",row.names=TRUE)
#table1<-read.csv("SPHumanSolutionTable.csv",stringsAsFactors = TRUE)
#rownames(table1)<-table1[,1]
#table1<-table1[,-1]

#################################################################
#Chi-squared tests for Sentence Positive
#################################################################

#Basic chi-squared test
CSQContingency<-table1
CSQResults<-chisq.test(CSQContingency)
CSQResults
CSQResults$expected
#Manual calculation of effect size (Cramer's V)
CramersV<- sqrt(
  as.numeric(CSQResults$statistic) /
    (sum(CSQContingency) * min(nrow(CSQContingency) - 1, ncol(CSQContingency) - 1))
)
CramersV
theta_df$Person<-as.factor(theta_df$Person)
theta_df$Topics <- factor(theta_df$Topics)

cluster_design <- svydesign(
  ids     = ~Person,
  weights = ~1,
  data    = theta_df
)

#Rao-Scott adjustment
SPrao_scott <- svychisq(
  ~City + Topics,
  design    = cluster_design,
  statistic = "F"
)
SPrao_scott

#################################################################
#Correspondence analysis for Sentence Positive
#################################################################

library(FactoMineR)
library(factoextra)
CA <- CA(table1, graph = FALSE)
ggpossen<-fviz_ca_biplot(CA, repel = TRUE, map="symbiplot",
                         col.row = "red", 
                         col.col = "blue",
                         title = "Positive Sentence Completion", 
                         arrow = c(TRUE, TRUE)) +
  coord_equal() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggpossen

#################################################################
#Correspondence analysis plot (with repelling labels)
#for Sentence Positive
#################################################################

# Symbiplot scaling factors for Dimensions 1 and 2
sym_scale <- CA$eig[1:2, 1]^(1/4)

# Extract and rescale row coordinates
row_df <- as.data.frame(
  sweep(CA$row$coord[, 1:2], 2, sym_scale, "/")
)
colnames(row_df) <- c("Dim1", "Dim2")
row_df$label <- rownames(CA$row$coord)
row_df$type <- "Row"

# Extract and rescale column coordinates
col_df <- as.data.frame(
  sweep(CA$col$coord[, 1:2], 2, sym_scale, "/")
)
colnames(col_df) <- c("Dim1", "Dim2")
col_df$label <- rownames(CA$col$coord)
col_df$type <- "Column"

# Combine rows and columns so all labels repel one another
label_df <- rbind(row_df, col_df)
CurTitle<-"Sentence Positive"
gg_hybrid <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Row arrows
  geom_segment(
    data = row_df,
    aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
    color = "red",
    linewidth = 0.5,
    alpha = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  
  # Column arrows
  geom_segment(
    data = col_df,
    aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
    color = "blue",
    linewidth = 0.5,
    alpha = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  
  # One label layer so everything repels everything else
  geom_text_repel(
    data = label_df,
    aes(x = Dim1, y = Dim2, label = label, color = type),
    size = 3.5,
    force = 10,
    force_pull = 0.15,
    box.padding = 1,
    point.padding = 0.4,
    max.overlaps = Inf,
    max.iter = 50000,
    max.time = 15,
    min.segment.length = 0,
    seed = 123,
    segment.color = "gray60",
    segment.alpha = 0.7
  ) +
  
  scale_color_manual(
    values = c(Row = "red", Column = "blue"),
    guide = "none"
  ) +
  
  coord_equal(clip = "off") +
  scale_x_continuous(expand = expansion(mult = 0.30)) +
  scale_y_continuous(expand = expansion(mult = 0.30)) +
  
  labs(
    title = CurTitle,
    x = paste0("Dimension 1 (", round(CA$eig[1, 2], 1), "%)"),
    y = paste0("Dimension 2 (", round(CA$eig[2, 2], 1), "%)")
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

gg_hybrid

#################################################################
#Top 10 terms for each topic
#################################################################

#Plot the top 10 terms for each topic
top_n <- 10
beta <- exp(stm_fit$beta$logbeta[[1]])
vocab <- stm_fit$vocab
terms_df <- as.data.frame(beta) %>%
  setNames(vocab) %>%
  mutate(topic_num = row_number()) %>%
  pivot_longer(
    cols = -topic_num,
    names_to = "token",
    values_to = "probability"
  ) %>%
  group_by(topic_num) %>%
  slice_max(probability, n = top_n, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    topic = TopicList[topic_num],
    topic = factor(topic, levels = TopicList),
    token = reorder_within(token, probability, topic)
  )

ggplot(terms_df, aes(x = token, y = probability, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "Term", y = "Probability") +
  theme_minimal()

#################################################################
#Topic modeling for Sentence Negative
#################################################################

#Sentence Negative Human
kVector<-seq(5,15,1)
k_result<-STMTopicModelSearch(SNMeltData,kVector)
SNCompare<-PlotTopicResults(k_result,ChooseMetrics=c("semcoh","heldout"))

STMAll<-STMTopicModel(SNMeltData,Maxk=7)
stm_fit<-STMAll$stm_fit
stm_input<-STMAll$stm_input
plot(stm_fit, n = 5)
print(labelTopics(stm_fit, n = 10))
theta_df <- as.data.frame(stm_fit$theta)
colnames(theta_df) <- paste0("Topic", 1:ncol(theta_df))
theta_df$City <- stm_input$meta$City
theta_df$Person <- stm_input$meta$ID
theta_df$Topics<-apply(theta_df[,1:7],1,which.max)
table1<-table(theta_df$City,theta_df$Topics)
TopicList<-c("Lost Money & Gambling","Homeless & Drunk","Crowded & Dirty",
             "Big City Dangers","Traffic & Homelessness",
             "Terrible Time","Crazy People")
colnames(table1)<-TopicList
#write.csv(theta_df,"SNHumanSolution.csv",row.names=FALSE)
#write.csv(table1,"SNHumanSolutionTable.csv",row.names=TRUE)
#table1<-read.csv("SNHumanSolutionTable.csv",stringsAsFactors = TRUE)
#rownames(table1)<-table1[,1]
#table1<-table1[,-1]

#################################################################
#Chi-squared tests for Sentence Negative
#################################################################

#Basic chi-squared test
CSQContingency<-table1
CSQResults<-chisq.test(CSQContingency)
CSQResults
CSQResults$expected
#Manual calculation of effect size (Cramer's V)
CramersV<- sqrt(
  as.numeric(CSQResults$statistic) /
    (sum(CSQContingency) * min(nrow(CSQContingency) - 1, ncol(CSQContingency) - 1))
)
CramersV
theta_df$Person<-as.factor(theta_df$Person)
theta_df$Topics <- factor(theta_df$Topics)

cluster_design <- svydesign(
  ids     = ~Person,
  weights = ~1,
  data    = theta_df
)

#Rao-Scott adjustment
SPrao_scott <- svychisq(
  ~City + Topics,
  design    = cluster_design,
  statistic = "F"
)
SPrao_scott

#################################################################
#Correspondence analysis for Sentence Positive
#################################################################

CA <- CA(table1, graph = FALSE)
ggnegsen<-fviz_ca_biplot(CA, repel = TRUE, map="symbiplot",
                         col.row = "red", 
                         col.col = "blue",
                         title = "Negative Sentence Completion", 
                         arrow = c(TRUE, TRUE)) +
  coord_equal() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggnegsen

CurTitle="Sentence Negative"

#################################################################
#Correspondence analysis plot (with repelling labels)
#for Sentence Negative
#################################################################

# Symbiplot scaling factors for Dimensions 1 and 2
sym_scale <- CA$eig[1:2, 1]^(1/4)

# Extract and rescale row coordinates
row_df <- as.data.frame(
  sweep(CA$row$coord[, 1:2], 2, sym_scale, "/")
)
colnames(row_df) <- c("Dim1", "Dim2")
row_df$label <- rownames(CA$row$coord)
row_df$type <- "Row"

# Extract and rescale column coordinates
col_df <- as.data.frame(
  sweep(CA$col$coord[, 1:2], 2, sym_scale, "/")
)
colnames(col_df) <- c("Dim1", "Dim2")
col_df$label <- rownames(CA$col$coord)
col_df$type <- "Column"

# Combine rows and columns so all labels repel one another
label_df <- rbind(row_df, col_df)
CurTitle="Sentence Negative"

gg_hybrid <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Row arrows
  geom_segment(
    data = row_df,
    aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
    color = "red",
    linewidth = 0.5,
    alpha = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  
  # Column arrows
  geom_segment(
    data = col_df,
    aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
    color = "blue",
    linewidth = 0.5,
    alpha = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  
  # One label layer so everything repels everything else
  geom_text_repel(
    data = label_df,
    aes(x = Dim1, y = Dim2, label = label, color = type),
    size = 3.5,
    force = 10,
    force_pull = 0.15,
    box.padding = 1,
    point.padding = 0.4,
    max.overlaps = Inf,
    max.iter = 50000,
    max.time = 15,
    min.segment.length = 0,
    seed = 123,
    segment.color = "gray60",
    segment.alpha = 0.7
  ) +
  
  scale_color_manual(
    values = c(Row = "red", Column = "blue"),
    guide = "none"
  ) +
  
  coord_equal(clip = "off") +
  scale_x_continuous(expand = expansion(mult = 0.30)) +
  scale_y_continuous(expand = expansion(mult = 0.30)) +
  
  labs(
    title = CurTitle,
    x = paste0("Dimension 1 (", round(CA$eig[1, 2], 1), "%)"),
    y = paste0("Dimension 2 (", round(CA$eig[2, 2], 1), "%)")
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

gg_hybrid
