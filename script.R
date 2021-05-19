library(ggplot2)
library(readxl)
library(arsenal)
library(ggstatsplot)
library(gridExtra)
library(dplyr)
df <- read_excel("df.xlsx", sheet = "Sheet1")
glimpse(df)
df$mean_pc_ULBP_SEP <- df$mean_pc_ULBP_SEP*100
df$mean_pc_NK_SEP <- df$mean_pc_NK_SEP*100
df[,c(2:4,6)] <- lapply(df[,c(2:4,6)], factor)
table(df$FM,df$group)
levels(df$FM)[levels(df$FM)=="0"] ="Control"
levels(df$FM)[levels(df$FM)=="1"] ="FMS"
levels(df$group)[levels(df$group)=="1"] ="Control"
levels(df$group)[levels(df$group)=="2"] ="FMS"
levels(df$group)[levels(df$group)=="3"] ="FMS with IENFD"
#df1 <- df[,-c(1,2)]
#table_one <- tableby(group ~ ., data = df1)
#summary(table_one)

#supplementary 11
ggplot(data = df,aes(x = group, y = mean_pc_ULBP_SEP)) +
  stat_boxplot(geom = "errorbar", width = 0.5, size = 2) +
  geom_boxplot(aes(fill = group),colour = "black",lwd=3) + 
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  geom_dotplot(binaxis = "y", binwidth = 4,
               stackdir = "center",aes(fill = group), 
               stroke = 4,color = "black") +
  theme_classic() + ylab("Mean ULBP+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text.x = element_text(color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title.x = element_text(color = "black", size = 0),
        axis.title.y = element_text(color = "black", size = 25),
        legend.position = "none")

ggplot(data = df,aes(x = group, y = mean_pc_NK_SEP)) +
  stat_boxplot(geom = "errorbar", width = 0.5, size = 2) +
  geom_boxplot(aes(fill = group),colour = "black",lwd=3) + 
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  geom_dotplot(binaxis = "y", binwidth = 2,
               stackdir = "center",aes(fill = group), 
               stroke = 4,color = "black") +
  theme_classic() + ylab("Mean NK cell+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text.x = element_text(color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title.x = element_text(color = "black", size = 0),
        axis.title.y = element_text(color = "black", size = 25),
        legend.position = "none")

# Compute the analysis of variance
res.aov <- aov(mean_pc_ULBP_SEP ~ group, data = df)
summary(res.aov)
TukeyHSD(res.aov)
res.aov <- aov(mean_pc_NK_SEP ~ group, data = df)
summary(res.aov)
TukeyHSD(res.aov)

p1 <- ggplot(df, aes(x=FQS, y=IENFD_f_mm)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_jitter(aes(fill = group), shape = 21,
             size = 4, color = "black", stroke = 4, width = 2) +
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  labs(x="FSQ Score", y="IENF density (fibers/mm)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 18),
         legend.position = "none")
p2 <- ggplot(df, aes(x=FQS, y=mean_pc_ULBP_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_jitter(aes(fill = group), shape = 21,
              size = 4, color = "black", stroke = 4, width = 2) +
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  labs(x="FSQ Score", y="Mean ULBP+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 18),
        legend.position = "none")
p3 <- ggplot(df, aes(x=FQS, y=mean_pc_NK_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_jitter(aes(fill = group), shape = 21,
              size = 4, color = "black", stroke = 4, width = 2) +
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  labs(x="FSQ Score", y="Mean NK cell+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 18),
        legend.position = "none")
p4 <- ggplot(df, aes(x=mean_pc_ULBP_SEP, y=mean_pc_NK_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_jitter(aes(fill = group), shape = 21,
              size = 4, color = "black", stroke = 4, width = 2) +
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  labs(x="Mean ULBP+ SEP (%)", y="Mean NK cell+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 18),
        legend.position = "none")
p5 <- ggplot(df, aes(x=IENFD_f_mm, y=mean_pc_ULBP_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_jitter(aes(fill = group), shape = 21,
              size = 4, color = "black", stroke = 4, width = 2) +
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  labs(x="Mean ULBP+ SEP (%)", y="Mean NK cell+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 18),
        legend.position = "none")
p6 <- ggplot(df, aes(x=IENFD_f_mm, y=mean_pc_NK_SEP)) + 
  theme_classic() +
  geom_smooth(method="lm", col="black", size=2) + 
  geom_jitter(aes(fill = group), shape = 21,
              size = 4, color = "black", stroke = 4, width = 2) +
  scale_fill_manual(values = c("#5e3c99","#f7f7f7","#fdb863")) +
  labs(x="Mean ULBP+ SEP (%)", y="Mean NK cell+ SEP (%)") +
  theme(axis.line = element_line(colour = 'black', size = 2),
        axis.ticks = element_line(colour = "black", size = 2),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 18),
        legend.position = "none")
grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2)






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
saveRDS(df,"df.RDS")
write.csv(df,"df.csv")


devtools::install_github("rsquaredacademy/xplorerr")
library("xplorerr")
app_visualizer()

devtools::install_github("krupanss/IEDA")

library(IEDA)

runIEDA()





