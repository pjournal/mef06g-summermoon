
install.packages("lubridate")
install.packages("plyr")

library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringi)
library(hrbrthemes)
library(viridis)
library(scales)
library(plyr)

options(dplyr.summarise.inform = FALSE)


deals<-read_excel("/Users/berkozcan/Desktop/MEF_BDA/BDA_503_R/Untitled/mef06g-summermoon/docs/startup_deals_2021.xlsx")

head(deals)

col_names <- colnames(deals)

new_cols <- c("ID","target_company","sector","investor","announcement_date","financial_investor"
              ,"investor_origin","stake","deal_value","investment_stage")

colnames(deals) <- new_cols

head(deals)

deals$announcement_date<-my(deals$announcement_date)
#not : neden ymd kullanmadık, farklı bir çıkarım var mı

deals$stake<-as.numeric(sub("%","",deals$stake))

deals$deal_value<-as.numeric(deals$deal_value)


to_be_adjusted <- c("Telecpm", "Cybersec urity", "B lockchain", "Artificial intelligence"
                    ,"Diğital Comparison","I mage process")

assigning_values_for_adj <- c("Telecom", "Cybersecurity", "Blockchain", "Artificial Intelligence"
                              ,"Digital Comparison","Image process")

deals$sector <- mapvalues(deals$sector, from = to_be_adjusted, to = assigning_values_for_adj)

#böyle ekleyelim

deals$org_count <- lengths(strsplit(deals$investor_origin, ","))
deals$inv_count <- lengths(strsplit(deals$investor, ","))

deals$deal_value <- deals$deal_value %>% replace_na(0)
#null değerleri böyle değiştirelim

origins <- deals %>% select(ID, investor_origin)
origins <- separate_rows(origins, investor_origin, sep = ", ")

investors <- deals %>% select(ID, investor)

investors <- separate_rows(investors, investor, sep = ", ")
##investores sep için , yanında boşluk olmalı


df0 <- deals %>% 
  group_by(investment_stage) %>% 
  dplyr::summarise(count = n()  , total_deal = sum(deal_value))

df0 %>%
  ggplot(aes(x = "", y = format(total_deal, suffix = "M", scale = 1e-6), fill= investment_stage)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "Deal(USD) by Investment Stage") +
  xlab("") +
  ylab("")



df1 <- deals %>% group_by(announcement_date, investment_stage) %>% 
  summarise(total_deal = sum(deal_value))




print(origins)
head(deals)

glimpse(deals)

head(deals)

head(investors)

df_count_of_investors <- investors %>% 
                               group_by(ID) %>%
                                  dplyr::summarise(Nb_of_Investors = n())

deals %>%
  inner_join(df_count_of_investors, by = "ID") %>%
  filter(deal_value != 0) %>%
  group_by(sector) %>%  
  dplyr::summarise(nb_of_investors = sum(Nb_of_Investors), total_deal_value = sum(deal_value)) %>%
  arrange()
  top_n(10)
  


1-

  
  
  deals %>%
    filter(deal_value != 0) %>%
    group_by(sector) %>%  
    dplyr::summarise(nb_of_investment = n(), total_deal_value = sum(deal_value)) %>%
    arrange(desc(total_deal_value)) %>%
    top_n(5)  %>%
    ggplot(aes(fill = sector, y = format(total_deal_value, scientific = FALSE, big.mark = ",") , x = reorder(sector, -total_deal_value)) ) + 
    geom_bar(position="stack", stat = "identity") +
    labs(title = "Top 5 Sectors Receiving Most Investments And Number of Investments") +
    xlab("Sector") +
    ylab("Deal(USD)") +
    geom_text(aes(label = nb_of_investment), vjust = - 1) 

  
2-
  

deals %>%
  filter(deal_value != 0) %>%
  group_by(target_company) %>%  
  dplyr::summarise(nb_of_investment = n(), total_deal_value = sum(deal_value)) %>%
  arrange(desc(total_deal_value)) %>%
  top_n(10)  %>%
  ggplot(aes(fill = target_company, y = format(total_deal_value, scientific = FALSE, big.mark = ",") , x = reorder(target_company, -total_deal_value)) ) + 
    geom_bar(position="stack", stat = "identity") +
    labs(title = "Top 10 Companies Receiving Most Investments And Number of Investments") +
    xlab("Target Company") +
    ylab("Deal(USD)") +
      geom_text(aes(label = nb_of_investment), vjust = - 1) 

    geom_text(
      aes(label = Weight),
      colour = "white", size = 3,
      vjust = 1.5, position = position_dodge(.9)
?format  



6-
  
  df_sector_by_inv <- deals %>%
    select(ID, target_company, sector) %>%
    full_join(investors, by = "ID") %>%
     group_by(investor) %>%  
     dplyr::summarise(nb_of_sector = n_distinct(sector)) %>%
     arrange(desc(nb_of_sector)) %>%
      top_n(20)


df_dealvalue_by_sector <- deals %>%
  group_by(sector) %>%  
  dplyr::summarise(total_deal_value = sum(deal_value)) %>%
  arrange(desc(total_deal_value))  


inner_join(df_investor_by_sector, df_dealvalue_by_sector, by="sector") %>% top_n(40)



```{r}
df5 %>%  + 
  theme(axis.text.x = element_text(angle=90,vjust=1,hjust=1)) +
  labs(title = "Monthly Investor Origin Percentage by Investment Stage") +
  xlab("Investor Origin") +
  ylab("Investor Origin Percentage")


comments :
  1) sektörlere göre en çok yatırım alanlar, zaman bazlı da olabilir, bar chat
  2) en çok yatırım alan şirketler
  3) en çok sayıda yatırımcı hangi sektöre ve şirkete yatırım yaptı
  4) financial investor olmayanlar kimler
  5) farklı zamanlarda yatırım alan şirketler
  6) hangi sektörede kaç şirket var 
  7) birden fazla sektöre yatırım yapan yatırımcılar 
  8) yatırımcıların totalde harcadığı para
  9) bir yatırımcı diğer yatırımcıları etkiliyor mu ?
  )
