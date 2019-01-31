
# library(devtools)
# install_github("bstewart/stm",dependencies=TRUE)

library(devtools)
# install_github("mroberts/stmBrowser",dependencies=TRUE)
library(stmBrowser)

library(tm)
library(topicmodels)
library(data.table)
library(slam)
library(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(gender)
library(zoo)
library(Hmisc)
library(lmtest)
library(plm)
library(vars)
library(mice)
library(e1071)
library(SparseM)
library(caret)
library(Matrix)
library(stm)
library(igraph)
library(wordcloud)
library(Rmpfr)
library(quanteda)
library(SnowballC)
library(RTextTools)
library(ROCR)

data <- fread("~/Documents/2.ABA/Data/speech_data_h109.csv", stringsAsFactors = T, strip.white = T, fill = T)

data$text <- gsub("[^a-zA-Z+]", " ", data$speech)
data$nchar <- nchar(data$text)
data <- data[data$nchar>999,]
data$text <- gsub('[[:punct:]]', "", data$text)
data$text <- tolower(data$text)
stopwords_regex = paste(stopwords('english'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
data$text = stringr::str_replace_all(data$text, stopwords_regex, '')

all_data <- data.frame(text=data$text, doc_id=data$speech_id
                       , dmeta1 = data$gender
                       , dmeta2 = data$party  
                       , stringsAsFactors = F
)

## STM:
sub_data <- all_data[,c("text","dmeta1","dmeta2")]
sub_data$dmeta3 <- ifelse(sub_data$dmeta1=="F", 
                          ifelse(sub_data$dmeta2=="D","FD",ifelse(sub_data$dmeta2=="I","FI","FR")),ifelse(sub_data$dmeta2=="D","MD",ifelse(sub_data$dmeta2=="I","MI","MR")) )
sub_data$dmeta3 <- as.factor(sub_data$dmeta3)

processed <- textProcessor(sub_data$text, metadata = sub_data, sparselevel = .95)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 20)


storage <- searchK(out$documents, out$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80, 90), data = meta)

plot(storage)

out$meta$dmeta1 <- as.factor(out$meta$dmeta1)
out$meta$dmeta2 <- as.factor(out$meta$dmeta2)
out$meta$dmeta3 <- as.factor(out$meta$dmeta3)

stm_fit <- stm(documents = out$documents, vocab = out$vocab,
               K = 60, prevalence =~ dmeta3, 
               max.em.its = 50, data = out$meta,
               init.type = "Spectral")

plot(stm_fit)

poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,
                              max.em.its = 75,
                              data = out$meta, runs = 20)

plotModels(poliblogSelect, pch=c(1,2,3,4), legend.position="bottomright")
selectedmodel <- poliblogSelect$runout[[3]]

plot(stm_fit, type = "summary", xlim = c(0, .4))

# install.packages("igraph")
library(igraph)
mod.out.corr <- topicCorr(stm_fit)
plot(mod.out.corr)

library(wordcloud)
cloud(stm_fit, topic = 7, scale = c(6,.5))
cloud(stm_fit, topic = 2, scale = c(6,.5))
cloud(stm_fit, topic = 4, scale = c(6,.5))
cloud(stm_fit, topic = 11, scale = c(6,.5))

topicQuality(stm_fit, out$documents, xlab = "Semantic Coherence",
             ylab = "Exclusivity", labels = 1:ncol(stm_fit$theta))

length(processed$documents)

poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                       K = 60, prevalence =~ dmeta3,
                       max.em.its = 50, data = out$meta,
                       init.type = "Spectral")

plot(poliblogPrevFit)
summary(poliblogPrevFit)

prep <- estimateEffect(1:60 ~ dmeta3, poliblogPrevFit,
                       meta = out$meta, uncertainty = "Global")


summary(prep)