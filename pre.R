#install.packages('pheatmap')
#install.packages('ggcorrplot')
library(pheatmap)
library(ggplot2)
library(dplyr)
library(ggcorrplot)

data <- read.csv("dataset_phishing.csv", header = T)

temp <- subset(data, select = -url)

zero_mean <- c()

temp %>% 
  mutate_if(is.integer, .funs = "as.numeric") -> temp
sapply(temp, "class")

for(i in 1:length(temp$status)) {
  if(temp$status[i] == 'legitimate') {
    temp$status[i] <- 0
  }
  else {
    temp$status[i] <- 1
  }
}

temp %>%
  mutate_if(is.character, .funs = "as.numeric") -> temp
sapply(temp, 'class')

for(i in 1:length(temp)) {
  if(mean(temp[, i]) == 0) {
    zero_mean <- c(zero_mean, i)
  }
}

temp <- subset(temp, select = -zero_mean)
sapply(temp, 'class')

corr <- round(cor(temp[,c(1:82)], use = "all.obs", method = "pearson"), 4)
p_value <- round(cor_pmat(temp), 4)
ggcorrplot(corr, method = 'square', type = 'lower', show.legend = F, show.diag = F, outline.color = 'gray',
           lab = F, lab_col = 'black', lab_size = 3.5, p.mat = p_value, sig.level = 0.05, pch.col = 'black',
           tl.cex = 10, tl.col = 'white', tl.srt = 0, ggtheme = ggplot2::theme_test())

subv <- c()
for(i in 1:length(p_value[, 1])) {
  if(p_value[i, 82] >= 0.05) {
    subv <- c(subv, i)
  }
}
subv
temp <- subset(temp, select = -subv)

corr2 <- round(cor(temp[,c(1:74)], use = "all.obs", method = "pearson"), 4)
p_value2 <- round(cor_pmat(temp), 4)
ggcorrplot(corr2, method = 'square', type = 'lower', show.legend = F, show.diag = F, outline.color = 'gray',
           lab = F, lab_col = 'black', lab_size = 3.5, p.mat = p_value2, sig.level = 0.05, pch.col = 'black',
           tl.cex = 10, tl.col = 'white', tl.srt = 0, ggtheme = ggplot2::theme_test())
feature <- c()
length(corr2)

for(i in 1:length(corr2[, 1])) {
  if(abs(corr2[74, i]) >= 0.2) {
    feature <- c(feature, i)
  }
}
length(feature)
feature <- feature[-24]

feature <- temp[, feature]
target <- temp[, 74]

phishing.df <- cbind(feature, target)

corr3 <- round(cor(phishing.df, use = "all.obs", method = "pearson"), 4)
p_value3 <- round(cor_pmat(phishing.df), 4)
ggcorrplot(corr3, method = 'square', type = 'lower', show.legend = F, show.diag = F, outline.color = 'gray',
           lab = F, lab_col = 'black', lab_size = 3.5, p.mat = p_value3, sig.level = 0.05, pch.col = 'black',
           tl.cex = 10, tl.col = 'white', tl.srt = 0, ggtheme = ggplot2::theme_test())

#write.csv(temp[, feature], "feature.csv", row.names = F)
#write.csv(temp[, 74], "target.csv", row.names = F)