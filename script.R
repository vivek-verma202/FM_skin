library(tidyverse)
library(readxl)
library(arsenal)
df <- read_excel("df.xlsx", sheet = "Sheet1")
glimpse(df)
df[,c(2:4,6)] <- lapply(df[,c(2:4,6)], factor)

df <- df[,-c(1,2)]
table_one <- tableby(group ~ ., data = df)
summary(table_one)

df$mean_pc_ULBP_SEP <- df$mean_pc_ULBP_SEP*100
df$mean_pc_NK_SEP <- df$mean_pc_NK_SEP*100

ggplot(data = df,aes(x = group, y = mean_pc_ULBP_SEP)) +
  stat_boxplot(geom = "errorbar", width = 0.2, size = 1.2) +
  geom_boxplot(aes(fill = group), 
               width = 0.8, size = 1.2, fatten = 1.5, colour = "black",
               outlier.alpha = 0) +
  geom_point(aes(fill = group), size = 4, alpha = 0.5, position = 
               position_jitterdodge(dodge.width = 1.5)) +
  scale_fill_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title = element_text(color = "white", size = 0),
        legend.position = "none")

ggplot(data = df,aes(x = group, y = mean_pc_NK_SEP)) +
  stat_boxplot(geom = "errorbar", width = 0.2, size = 1.2) +
  geom_boxplot(aes(fill = group), 
               width = 0.8, size = 1.2, fatten = 1.5, colour = "black",
               outlier.alpha = 0) +
  geom_point(aes(fill = group), size = 4, alpha = 0.5, position = 
               position_jitterdodge(dodge.width = 1.5)) +
  scale_fill_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title = element_text(color = "white", size = 0),
        legend.position = "none")

# Compute the analysis of variance
res.aov <- aov(mean_pc_ULBP_SEP ~ group, data = df)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)
res.aov <- aov(mean_pc_NK_SEP ~ group, data = df)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(df, aes(x=FQS, y=IENFD_f_mm)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_point(aes(col = group), size = 4) +
  scale_color_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  labs(x="FSQ Score", y="IENF density (fibres/mm)") +
  theme(plot.title = element_text(size = 0),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

ggplot(df, aes(x=FQS, y=mean_pc_ULBP_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_point(aes(col = group), size = 4) +
  scale_color_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  labs(x="FSQ Score", y="ULBP+ SEP (%)") +
  theme(plot.title = element_text(size = 0),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")


ggplot(df, aes(x=FQS, y=mean_pc_NK_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_point(aes(col = group), size = 4) +
  scale_color_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  labs(x="FSQ Score", y="NK cell+ SEP (%)") +
  theme(plot.title = element_text(size = 0),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

ggplot(df, aes(x=IENFD_f_mm, y=mean_pc_ULBP_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_point(aes(col = group), size = 4) +
  scale_color_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  labs(x="IENF density (fibres/mm)", y="ULBP+ SEP (%)") +
  theme(plot.title = element_text(size = 0),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

ggplot(df, aes(x=IENFD_f_mm, y=mean_pc_NK_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_point(aes(col = group), size = 4) +
  scale_color_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  labs(x="IENF density (fibres/mm)", y="NK cell+ SEP (%)") +
  theme(plot.title = element_text(size = 0),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

ggplot(df, aes(x=mean_pc_ULBP_SEP, y=mean_pc_NK_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_point(aes(col = group), size = 4) +
  scale_color_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  labs(x="ULBP+ SEP (%)", y="NK cell+ SEP (%)") +
  theme(plot.title = element_text(size = 0),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

cor.test(df$FQS, df$IENFD_f_mm, method=c("pearson", "kendall", "spearman"))
cor.test(df$mean_pc_ULBP_SEP,
         df$mean_pc_NK_SEP, method="pearson", "kendall", "spearman")
cor.test(df$mean_pc_ULBP_SEP,
         df$mean_pc_NK_SEP, method="spearman")


library(PerformanceAnalytics)
chart.Correlation(Data.num, method="spearman")

library(psych)
ct <- corr.test(Data.num,method = "pearson")
ct1 <- corr.test(Data.num,method = "spearman")
print(ct, short = F)
print(ct1, short = F)




#-----------------------------------------------------------

df <- read_csv("nk_skin_fm.csv")
glimpse(df)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],as.factor)
df$Group <- factor(df$Group, levels = c("HC","FMS"))
df$IENFD <- factor(df$IENFD, levels = c("normal","abnormal"))
sapply(df,function(x) sum(is.na(x)))
df$cond <- factor(ifelse(df$Group == "HC","HC",
                         ifelse(df$Group == "FMS"
                                & df$IENFD == "normal", "FM",
                                "FM+IENFD")),
                  levels = c("HC","FM","FM+IENFD"))
table(df$cond)

ggplot(data = df,aes(x = cond, y = ULBP_SEP_pc)) +
  stat_boxplot(geom = "errorbar", width = 0.2, size = 1.2) +
  geom_boxplot(aes(fill = cond), 
               width = 0.8, size = 1.2, fatten = 1.5, colour = "black",
               outlier.alpha = 0) +
  geom_point(aes(fill = cond), size = 4, alpha = 0.5, position = 
               position_jitterdodge(dodge.width = 1.5)) +
  scale_fill_manual(values = c("#4BB269","#FFA500","#BC3A3A")) +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title = element_text(color = "white", size = 0),
        legend.position = "none")

plot(df$IENF_density,df$ULBP_SEP_pc)
geom_text(data = means, aes(label = weight, y = weight + 0.08))