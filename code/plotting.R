# 20250114 R Carpenter workshop
# name is case sensitive
#don't start a name with a number

library(tidyverse)

sample_data <-read_csv("sample_data.csv")
read.csv(file="sample_data.csv")

Xgetwd()

ggplot(data = sample_data) + 
  aes(x=temperature)+
  labs(x="Temperature (C)")+
  aes(y=cells_per_ml)+
  labs(y="Cells per mL")+
  geom_point()+
  labs(title = "Does temperature affect microbial abundance?")+
  aes(color=env_group)+
  aes(size = chlorophyll)+
  aes(shape = env_group)+
  labs(size="Chlorophll (ug/L)",
       color="Environmental Group",
       shape="Environmental Group")

#combined ver
ggplot(data = sample_data)+
  aes(x=temperature,
      y=cells_per_ml/100000,
      color=env_group,
      size = chlorophyll)+
  geom_point()+
  labs(x="Temperature (C)",
       y="Cells(millions/ml)",
       title = "Does temperautre affect microbial abundance?",
       size= "Chlorophll (ug/L)",
       color= "Environmental Group")



buoy_data = read_csv("buoy_data.csv")
dim(buoy_data)
head(buoy_data)

ggplot(data = buoy_data)+
  aes(x= day_of_year,
      y= temperature,
      group=sensor,
      color = depth)+
  geom_line()+
  facet_wrap(~buoy, scales= "free_y")

buoy_data = read_csv("buoy_data.csv")
dim(buoy_data)
head(buoy_data)

ggplot(data = buoy_data)+
  aes(x= day_of_year,
      y= temperature,
      group=sensor,
      color = depth)+
  geom_line()+
  facet_grid(rows=vars(buoy))

str(buoy_data)


#discrete lots
#box plot
#change transparency



#box plot with palette
ggplot(data = sample_data)+
  aes(x=env_group,
      y=cells_per_ml)+
  geom_boxplot(aes(fill = env_group))+
  scale_fill_brewer(palette = "Set1")

#box plot with custom palette
install.packages("wesanderson")
install.packages("ggsci")
library(wesanderson)
library(ggsci)

ggplot(data = sample_data)+
  aes(x=env_group,
      y=cells_per_ml)+
  geom_boxplot(aes(fill = env_group))+
  scale_fill_manual(values = wes_palette("Cavalcanti1"))


ggplot(data = sample_data)+
  aes(x=env_group,
      y=cells_per_ml)+
  geom_boxplot(fill = "darkblue",
               alpha=0.3)


#univariate plts
box_plot = 
  ggplot(sample_data)+
  aes(x= env_group,
      y= cells_per_ml)+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))

#saving
ggsave("awesome_plot.jpg", width=6, height=4, dpi = 500)


#add changes to the plot for black and white
box_plot = box_plot + theme_bw()
box_plot

ggsave("awesome_plot_Example.jpg", plot = box_plot, width=6, height=4, dpi = 500)

