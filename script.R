library(tidyverse)
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