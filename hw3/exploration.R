####
# Setup ----
####
rm(list=ls())
source("../utils/source_me.R", chdir = T)
CreateDefaultPlotOpts()

require(plyr)

load("hw3.rdat")
# Gender should be a factor variable!
RelevelGender <- function(x) {
  x <- factor(x)
  x <- revalue(x, c('1'='female', '2'='male'))
  return(x)
}
df$SenderGender <- RelevelGender(df$SenderGender)
df$ReceiverGender <- RelevelGender(df$ReceiverGender)


####
# Heatmap of Data by Gender ----
####
col.theme <- gg_color_hue(3)
g <- ggplot(data=df, aes(x=SenderLooks, y=ReceiverLooks)) + 
  stat_bin2d(aes(fill=..count..), drop=T) +
  scale_fill_gradient("Sample\nCount") +
  facet_wrap("SenderGender", labeller =labeller(
    SenderGender=c(female="Female Sender", male="Male Sender"))) +
  coord_equal() + theme_bw() + 
  labs(x="Sender Looks", y="Receiver Looks")
GGPlotSave(g, "q0_data_heat")


####
# Heatmap of Contacts by Gender ----
####
g <- ggplot(data=df[df$y==1,], aes(x=SenderLooks, y=ReceiverLooks)) +
  stat_bin2d(aes(fill=..count..), drop=T) +
  scale_fill_gradient("Sample\nCount") + 
  facet_wrap("SenderGender", labeller =labeller(
    SenderGender=c(female="Female Sender", male="Male Sender"))) +
  coord_equal() + theme_bw() + 
  labs(x="Sender Looks", y="Receiver Looks")
GGPlotSave(g, "q0_contact_heat")
