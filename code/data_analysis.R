library(tidyverse)
sample_data = read.csv("data/sample_data.csv")
summarize(sample_data,average_cells=mean(cells_per_ml))
sample_data %>% 
  summarise((average_cells=mean(cells_per_ml)))

#Filtering rows
#column name does not need a quotation mark
# value should be in quotation mark
# != means everything but [regular expression] string detect is really useful 

sample_data %>% 
  filter(env_group == "Deep") %>% 
  summarise(average_cells = mean(cells_per_ml))

#ave chlorophll in entire

sample_data %>% 
  summarize(average=mean(chlorophyll))

#ave chlorophll in shallow september
sample_data %>% 
  filter(env_group == "Shallow_September") %>% 
  summarize(average=mean(chlorophyll))

#group by!

sample_data %>% 
  group_by(env_group) %>% 
  summarise(average_cell = mean(cells_per_ml),
            min_cells=min(cells_per_ml))

#average temp
sample_data %>% 
  group_by(env_group) %>% 
  summarise(ave_tem = mean(temperature))


#mutate
sample_data %>% 
  mutate(temp_is_hot = temperature > 8) %>% 
  group_by(env_group, temp_is_hot) %>% 
  summarize(avg_temp=mean(temperature),
            ave_cell =mean(cells_per_ml))

#select
sample_data %>% 
  select(sample_id,depth)


sample_data %>% 
  select(env_group)

#from sample id to temperature
sample_data %>% 
  select(sample_id:temperature)

sample_data %>% 
  select(starts_with("total"))


#select can reorder column
sample_data %>% 
  select(sample_id:temperature)

#or 
sample_data %>% 
  select(1:5)

#or 
sample_data %>% 
  select(-(total_nitrogen:chlorophyll))



#CLEANING DATA, skip=2 it automatically know skip2 means 2 rows not columns

taxon_clean = read_csv("data/taxon_abundance.csv",skip=2) %>% 
  select(-...10) %>% 
  rename(sequencer=...9) %>% 
  select(-Lot_Number, -sequencer)

#if column has the same unit, can pivot to long format
taxon_long = 
  taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria,
               names_to = "Phylum",
               values_to = "Abundance")


taxon_long %>% 
  group_by(Phylum) %>% 
  summarise(avg_abund = mean(Abundance))


taxon_long %>% 
  ggplot()+
  aes(x=sample_id,
      y=Abundance,
      fill = Phylum)+
  geom_col()+
  theme(axis.text.x = element_text(angle=90))




#making long data wide
taxon_long %>% 
  pivot_wider(names_from = "Phylum",
              values_from = "Abundance")




#JOINING DATAFRAME

head(sample_data)
head(taxon_clean)


inner_join(sample_data,taxon_clean, by = "sample_id")

anti_join(sample_data,taxon_clean, by="sample_id")


sample_data$sample_id

taxon_clean$sample_id

taxon_clean_goodSep = 
  taxon_clean %>% 
  mutate(sample_id= str_replace(sample_id, pattern = "Sep", replacement = "September"))


sample_and_taxon = 
  inner_join(sample_data, taxon_clean_goodSep, by = "sample_id")

write_csv(sample_and_taxon,file = "data/sample_and_taxon.csv")

library(ggpubr)
#ggpubr
#where does chloroflexi like to live

sample_and_taxon %>% 
  ggplot()+
  aes(x=depth,
      y=Chloroflexi)+
  geom_point()+
  labs(x="Depth(m)",
       y="Chloroflexi relative abundance")+
  geom_smooth(method = lm)+
  stat_cor()+
  annotate(geom = "text",
           x=25,
           y=0.3,
           label="This is text label")


#averqage abundance and stdev of chloroflexi in three env group

sample_and_taxon %>% 
  group_by(env_group) %>% 
  summarise(avg_chloro = mean(Chloroflexi),
            sd_chlo=sd(Chloroflexi))

taxon_long %>% 
  arrange(desc(Abundance))







