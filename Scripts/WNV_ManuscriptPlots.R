library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(ggrepel)
library(compare)
library(cowplot)
library(ggpubr)
library(gtable)
library(ggExtra)
library(gridExtra)
## load data file 
load("WNV_OutputData.RData")

Fig1A_MainText <-  ggplot(data=df_Fig1A_MainText,  aes(x=year, y= totl_pos_regs)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_line(df_Fig1A_MainText, mapping= aes(x= year, y=as.numeric(total_cases/15)), color = "black", alpha=0.8)+
  geom_point(df_Fig1A_MainText, mapping= aes(x= year, y=as.numeric(total_cases/15)), color = "black",alpha=0.8)+#+
  
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right  = element_text(margin = margin(r = .3, unit = "cm"),
                                           size=10)
  )+
  scale_fill_viridis_c()+
  theme_minimal()+
  theme_classic()+
  theme(axis.title.x = element_blank(), 
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black",fill=NA, size=0.5),
        panel.background =element_blank(),# element_rect(fill = 'white', colour = 'white'),
        legend.position =  "none", #c(0.5, 0.94),#
        legend.direction = "horizontal",
        axis.text.x = element_text(margin = margin(t = .5, unit = "cm"), angle=45, size = 10),
        axis.text.y = element_text(margin = margin(r = .3, unit = "cm"),
                                   size=10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        legend.key = element_blank(),#element_rect(fill = NA),
        legend.box = "vertical",
        axis.line.y.right = element_line(color = "black"), 
        axis.ticks.y.right = element_line(color = "black"),
        axis.text.y.right = element_text(
          color = "black", size = 10),
        axis.title.y.right = element_text(color = "black", size = 10)
        
  )+
  scale_y_continuous( limits=c(0, 150), 
                      breaks = c(0,50,100,150),
                      expand = c(0, 0),
                      sec.axis = sec_axis( trans=~.*15, 
                                           name="Annual Cases"
                                           #labels = scales::percent
                      )
                      
  ) +
  
  scale_x_continuous(  expand = c(0, 0),
                       breaks = c(2010:2019),
  ) +
  labs(y= "Number of WNV\n Affected Regions")






df_outbreakYears <- df_6Sep%>%filter(wnf_case==1)%>%
  filter(year %in% c(2010:2017))%>%
  group_by(NUTS_ID)%>%
  summarise(total_years=n())%>%
  mutate(total_years= as.factor(total_years))

df_fig1B1 <- left_join(mapdata[,c("NUTS_ID", "geometry")],df_outbreakYears , by="NUTS_ID", all.x= TRUE)%>%
  # mutate(year= NA)%>%
  mutate(FigType="B1")%>%
  select(c(1,2,4,3))
# select(NUTS_ID, geometry, total_years,year)

df_fig1B2 <- map_yrs%>%select(c(1,3))%>%mutate(total_years= as.factor(year))%>%
  mutate(FigType="B2")%>%select(c(1,4,5,3))

df_fig1 <- rbind(df_fig1B1, df_fig1B2)#%>%

FIG1_B <- df_fig1B1%>%
  ggplot(aes(fill = total_years))+
  geom_sf(aes(geometry = geometry))+
  #theme_map()+
  geom_sf(color = alpha("white", 1/5), alpha=1)+
  facet_wrap("FigType")+
  #labs(fill="Total WNV-outbreak \n years (2010-2017)")+
  theme( #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    #text = element_text(size=10,  family= "Microsoft Sans Serif") ,
    panel.border = element_rect(colour = "black",fill=NA, size=0.5),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "bottom" ,#c(0.85, 0.5), #
    legend.direction = "horizontal",
    legend.title = element_text(size=10),
    legend.text=element_text(size=10),
    legend.box = "horizontal")+
  scale_y_continuous( limits=c(34,71), 
                      expand = c(0, 0)
  ) +
  scale_x_continuous(  limits=c(-10,45),
                       expand = c(0, 0)) 


Fig1_B1_MainText <-df_fig1B1 %>%
  ggplot(aes(fill = total_years))+
  geom_sf(aes(geometry = geometry))+
  #theme_map()+
  geom_sf(color = alpha("white", 1/5), alpha=1)+
  labs(fill="Total WNV-outbreak \n years (2010-2017)")+
  theme( 
    panel.border = element_rect(colour = "black",fill=NA, size=0.5),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "bottom" ,#c(0.85, 0.5), #
    legend.direction = "horizontal",
    legend.title = element_text(size=10),
    legend.text=element_text(size=10),
    axis.text.x = element_blank(),
    axis.text.y =  element_blank(), 
    legend.box = "horizontal")+
  scale_y_continuous( limits=c(34,71), 
                      # breaks = c(0,5,10, 15,30),
                      expand = c(0, 0)
  ) +
  scale_x_continuous(  limits=c(-10,45),
                       expand = c(0, 0)) 



bitmap("WNV_Fig1B_MainTxt.png",width=4,height=6,units='in',res=300,type="pngalpha",pointsize=13)
Fig1_B1_MainText
dev.off()


df_fig1B2 <- map_yrs


Fig1_B2_MainText  <- df_fig1B2%>%
  ggplot(aes(fill = year))+
  geom_sf(aes(geometry = geometry))+
  #theme_map()+
  #scale_fill_viridis() +
  #scale_color_discrete()+
  #scale_colour_viridis_d(option = "A")+
  #scale_fill_brewer(pallette = "RdYlBu")+ #
  geom_sf(color = alpha("black", 1/2), alpha=5)+
  #xlim() + ylim(c(35,60))+
  labs(fill="2018-outbreak \n NUTS3")+
  theme( #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    #text = element_text(size=10,  family= "Microsoft Sans Serif") ,
    panel.border = element_rect(colour = "black",fill=NA, size=0.5),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "bottom" ,#c(0.85, 0.5), #
    legend.direction = "horizontal",
    #legend.title = element_text(size=10),
    legend.text=element_text(size=10),
    #legend.text=element_text(size=10),
    legend.title=element_blank(),
    #axis.ticks.x  = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    #axis.ticks.length.y =unit(-0.1, "cm"),
    # axis.text.x = element_text(margin = margin(t = .5, unit = "cm"), angle=45),
    # axis.text.x = element_text(margin = margin(t = .5, unit = "cm")),
    axis.text.y =  element_blank(), #element_text(margin = margin(r = .5, unit = "cm"),
    #size=10),
    legend.key = element_blank(), #element_rect(fill = NA),
    legend.box = "horizontal")+
  scale_y_continuous( limits=c(34,71), 
                      # breaks = c(0,5,10, 15,30),
                      expand = c(0, 0)
  ) +
  scale_x_continuous(  limits=c(-10,45),
                       expand = c(0, 0)) 




bitmap("WNV_Fig1B2_MainTxt.png",width=4,height=6,units='in',res=300,type="pngalpha",pointsize=13)
Fig1_B2_MainText
dev.off()






df_dmy = df_wnf_all%>%dplyr::select(c(1:8,131))%>%
  filter(wnf_case=="1")%>%
  #filter( year!=2018)%>%
  filter(year !=2019)#%>%

group_by(NUTS_ID,year, CNTR_CODE, NUTS_NAME)%>%
  #summarise(across(c(1:16), list(mean)))%>%na.omit()%>%
  group_by(NUTS_ID,CNTR_CODE)%>%summarise(across(c(1:14), list(mean)))%>%
  #`colnames<-`(c(names(df_wnf_all)[1:20]))
  df_dmy %>%na.omit()%>%dplyr::filter(CNTR_CODE=="IT")%>%
  pivot_longer(c(5:8), names_to= "var",
               values_to="value" )%>%
  ggplot()+
  geom_point(aes(year, value ,color = var))+
  facet_wrap(.~NUTS_ID)




df.train2017 <- train_wnv%>%filter(NUTS_ID %in% df_regs_8plusyearsWNV$NUTS_ID)%>% #dplyr::select(-c(1:2))%>%
  group_by(NUTS_ID)%>%
  summarise(across(everything(),list(mean = mean)) )%>%
  mutate(data= rep("2010-2017", length(levels(train_wnv$wnf_case))-1 ) )%>%
  `colnames<-`(names(df.test2018))


df.test2018 <- test_2018 %>% #dplyr::select(-c(1:2))%>%
  group_by(wnf_case)%>%
  summarise(across(everything(),list(mean = mean)) )%>%
  mutate(data= rep("2018", length(levels(train_wnv$wnf_case)) ) )


df.test2019 <- test_2019 %>% #dplyr::select(-c(1:2))%>%
  group_by(wnf_case)%>%
  summarise(across(everything(),list(mean = mean)) )%>%
  mutate(data= rep("2019", length(levels(train_wnv$wnf_case)) ) )






df.test2018 <- test_2018 %>%filter(wnf_case==1)%>%
  #filter(NUTS_ID %in% df_regs_8plusyearsWNV$NUTS_ID)%>%
  mutate(data= rep("2018", 88)) #dim(test_2018)[1]

df.test2019 <- test_2019 %>%filter(wnf_case==1)%>%
  #filter(NUTS_ID %in% df_regs_8plusyearsWNV$NUTS_ID)%>%
  mutate(data= rep("2019",52 ))#dim(test_2019)[1]

newRegs1819=union(df.test2018$NUTS_ID,df.test2019$NUTS_ID)

df.train2017 <- train_wnv%>%filter(wnf_case==0)%>%
  filter(NUTS_ID %in% df.test2018$NUTS_ID)%>%
  #mutate(data= rep("2010-2017", dim(train_wnv)[1]) )
  mutate(data= rep("2010-2017", 460) )

df_new <- rbind(df.train2017,df.test2018 )#, df.test2019)


df_6Sep <- readxl::read_xlsx('WNV_data_06Sep21.xlsx') #'WNV_Imputed_20Aug21.xlsx'

df_17_neg <- df_6Sep%>% filter(year==2017 & wnf_case==0)

df_18_pos <- df_6Sep%>% filter(year==2018 & wnf_case==1)


df_17n_18p <- df_17_neg%>%filter(NUTS_ID %in% df_18_pos$NUTS_ID)%>%
  mutate(data= rep("2017", 88) )

df_18common17 <- df_18_pos%>%filter(NUTS_ID %in% df_17n_18p$NUTS_ID)%>%
  mutate(data= rep("2018", 88) )


df_new1718_p <- rbind(df_17n_18p,df_18common17)

#feature_indices =c(8,17,7,22) 
#feature_indices =c(21,5,15,6,11,10) 
#feature_indices =c(23,7,17,8,13,12) 
#feature_indices =c(22,21,7,16,11,12,32,33) 
#feature_indices =c(32,4,26, 8, 43,42,46, 47) 



feature_indices =c(34,6,28, 10, 45,44) 

regs_naive <- setdiff(unique(Regs_2018$NUTS_ID), unique(Regs_2010to17$NUTS_ID))




my3cols1 <- c( '#117733' , '#88CCEE', '#CC6677')


fig2A_MainText <- df_Fig2A%>%dplyr::select(c(1,3,2,4,5))%>%
  pivot_longer(c(2:4), names_to= "data", values_to= "value")%>%
  mutate(data= factor(data, levels= c("train_logloss", "test2018_logloss", "test2019_logloss")))%>%
  ggplot(aes(x= Model, y= value, color = data) )  +
  geom_point(size= 3)+
  theme_classic()+
  scale_color_manual(labels = c("Train:2010-2017","Test-2018", "Test-2019"),#,
                     values = c(my3cols1[1], my3cols1[2], my3cols1[3]),
                     guide = "legend" 
  )+
  
  
  theme( #axis.title.x = element_blank(), 
    strip.background = element_blank(),
    # text = element_text(size=10, family = "Helvetica" ),
    panel.border = element_rect(colour = "black",fill=NA, size=1),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "bottom", # c(0.9, 0.92),## 
    legend.direction = "horizontal",
    #axis.ticks.length=unit(-0.0, "cm"),
    #axis.ticks.length=unit(-0.1, "cm"),
    axis.text.x = element_text(margin = margin(t = 0.8, unit = "cm"), angle=45, size = 10),
    axis.text.y = element_text(margin = margin(r = .2, unit = "cm"),
                               size=10),
    legend.title = element_blank(),
    legend.key = element_blank(),#element_rect(fill = NA),
    legend.box = "vertical") +
  labs(x=" Model", 
       #title = "Logloss comparison of scaled features Models and original Model Q2",
       #subtitle = "Scaling of top-6 SHAP predicted Features",
       y= "Logloss" 
  )+
  
  scale_y_continuous(limits=c(0.0, 0.3), 
                     breaks = c(0.0, 0.15, 0.25), 
                     expand = c(0,0))



#df_Fig2B <- df_metrics_AllModels

Figure2B_MainText <- df_Fig2B%>%#dplyr::select(c(1,3,2,4,5))%>%
  filter(Model=="Q1-Q2")%>%
  pivot_longer(c(2:7), names_to= "Metric", values_to= "value")%>%
  ggplot(aes(x= as.factor(Threshold), y= value*100, color = Test.Year) )  +
  geom_point(size= 3)+
  theme_classic()+
  facet_wrap(vars(Metric))+
  scale_color_manual(labels = c("Test-2018", "Test-2019"),#,
                     values = c(my3cols1[2], my3cols1[3]),
                     guide = "legend" 
  )+
  
  theme( #axis.title.x = element_blank(), 
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black",fill=NA, size=1),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "bottom", # c(0.9, 0.92),## 
    legend.direction = "horizontal",
    axis.text.x = element_text(margin = margin(t = 0.8, unit = "cm"), angle=45, size = 10),
    axis.text.y = element_text(margin = margin(r = .2, unit = "cm"),
                               size=10),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.box = "vertical") +
  labs(x=" Classification Threshold", 
       y= "Score[%]" 
  )



ggarrange(fig2A_MainText, Figure2B_MainText, 
          nrow=2,
          labels= c("A","B"))






## Figure 3 Main Text


x_bound <- NULL
x_bound <- if (is.null(x_bound)){ 
  max(abs(data_long$value)) * 1.1
  
  dilute = FALSE
  scientific = FALSE
  my_format = NULL}

if (scientific) {
  label_format = "%.1e"
}
else {
  label_format = "%.3f"
}
if (!is.null(my_format)) 
  label_format <- my_format
N_features <- setDT(df_fig3A)[, uniqueN(variable)]
if (is.null(dilute)) 
  dilute = FALSE
nrow_X <- nrow(df_fig3A)/N_features
if (dilute != 0) {
  dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute))))
  set.seed(1234)
  df_fig3A <- df_fig3A[sample(nrow(df_fig3A), min(nrow(df_fig3A)/dilute, 
                                                     nrow(df_fig3A)/2))]
}

#

#data_long <- shap.prep(xgb_model = model.xgb, X_train = as.matrix(train_wnv[,-dim(train_wnv)[2]]), top_n = 10)

Fig3A_MainText <- ggplot(data = df_fig3A ) + 
  #coord_flip(ylim = c(-x_bound, x_bound)) + 
  coord_flip()+
  geom_hline(yintercept = 0) + 
  ggforce::geom_sina(aes(x = variable,  y = value, color = stdfvalue),
                     method = "counts", maxwidth = 0.7, alpha = 0.7) +
  geom_text(data = unique(data_long[, c("variable", "mean_value")]), 
            aes(x = variable, y = -Inf, label = sprintf(label_format, mean_value)),
            size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold") +
  scale_color_viridis( option = "D",
                       #low = "#FFCC33", high = "#6600CC", 
                       breaks = c(0,1), labels = c(" Low","High "), 
                       guide = guide_colorbar(barwidth = 12, 
                                              barheight = 0.3)) + 
  theme_classic()+
  theme( 
    axis.line.y = element_line(size=0.3), #element_blank(), 
    legend.text = element_text(size = 8), 
    axis.title.x = element_text(size = 10),
    #axis.title.x = element_blank(), 
    plot.title = element_text(hjust = 0.5, vjust = 0.2, size = 12),
    plot.subtitle = element_text(margin = margin(t = 10, b = -40), size=10),
    strip.background = element_blank(),
    # text = element_text(size=10, family = "Helvetica" ),
    panel.border = element_rect(colour = "black",fill=NA, size=1.2),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position =  "bottom", #c(0.2, 0.9),#"none", #
    legend.direction = "horizontal",
    #axis.ticks.length=unit(-0.1, "cm"),
    axis.text.x = element_text(margin = margin(t = .8, unit = "cm"), angle=45, size = 10),
    axis.text.y = element_text(margin = margin(r = .2, unit = "cm"),
                               size=10),
    legend.box = "vertical")+
  scale_x_discrete(limits = rev(levels(data_long$variable))#, 
                   # labels = label.feature(rev(levels(data_long$variable)))
  ) +
  labs(y = "SHAP value (impact on model output)", x = "", 
       color = "Feature value ")




MeanShap_train <- MeanShap_train%>% mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                                                     c(paste("bio", 1:19, sep='')), 
                                                                   "Bioclimatic"))%>%
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(1:3,5:8,10)], 
                                 "Climate"))%>%
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(4,9,11,12)], 
                                 "Environmental"))%>%
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(32,33)], 
                                 "Vectors"))%>%
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(34:41)], 
                                 "Demographic"))%>%
  
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(42)], 
                                 "Economic"))%>%
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(43:44)], 
                                 "Trade"))%>%
  mutate(Feature_Class = replace(Feature_Class, Feature %in% 
                                   names(shap_values$shap_score)[c(45:108)], 
                                 "Birds"))%>%
  mutate(Feature_Class = as.factor(Feature_Class))



MST= MeanShap_train%>%mutate(Feature_Class= replace(as.character(Feature_Class), Feature_Class=="Mobility", "Trade"))

MST$Feature_Class[c(9,13,19,25)] <- "Bioclimatic"


#MeanShap_train%>%mutate(Feature_Class= if_else(Feature_Class=="Mobility", "Trade"))

Figure3B_MainText <- df_Fig3B%>%
  #MeanShap_train%>% 
  group_by(Feature_Class)%>%
  #summarise(Class_Contrb= round(sum(Gain)*100,1))%>%arrange(desc(Class_Contrb))%>%
  summarise(Class_Contrb= (sum(Percent_Contr)))%>%arrange(desc(Class_Contrb))%>%
  ggplot(aes(x = reorder(Feature_Class, Class_Contrb),y= (Class_Contrb),
             fill= as.factor(Feature_Class))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_flip()+
  #scale_fill_viridis(discrete = TRUE)+
  theme_minimal()+
  theme_classic()+
  theme( #axis.title.x = element_blank(), 
    strip.background = element_blank(),
    # text = element_text(size=10, family = "Helvetica" ),
    panel.border = element_rect(colour = "black",fill=NA, size=0.5),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "none", # c(0.5, 0.1),## 
    legend.direction = "horizontal",
    #axis.ticks.length=unit(-0.0, "cm"),
    axis.text.x = element_text(margin = margin(t = .5, unit = "cm"), angle=45, size = 10),
    axis.text.y = element_text(margin = margin(r = .3, unit = "cm"),
                               size=10),
    #axis.text.x = element_text(angle=45),
    legend.text=element_text(size=10),
    #legend.text.align =  0,
    #legend.title = element_blank(),
    legend.key = element_blank(),#element_rect(fill = NA),
    legend.box = "vertical") +
  scale_x_discrete(  expand = c(0, 0)
                     #breaks = c(2010:2019),
                     # limits= c(0, 45)
                     
  )+
  scale_y_continuous(  expand = c(0, 0),
                       limits= c(0,52)
                       # breaks=c(), #c(10, 20, 30,40),
  )+
  labs(x="Feature Class ", 
       # y= "Percent contribution by each feature class based on 'Gain'",
       y= "SHAP - Percent Contribution",
       fill ="Feature \n Class")


ggarrange(Fig3A_MainText, Figure3B_MainText, 
          nrow=2,
          labels= c("A","B"))

#df_new %>%
#dplyr::select(c(1,feature_indices,123,124))%>%

Fig4_MainText <- df_fig4%>%
  mutate(wnf_case= fct_rev(as.factor(wnf_case)))%>%
  dplyr::select(c(1:3,feature_indices,124,125))%>%
  pivot_longer(c("bio10", "max_temp_02","mean_temp_02", 
                 "bio4","mndwi_q2", "mndwi_q1"
                 #, "dist_Culex.modestus","dist_Culex.pipiens"
  ))%>%
  mutate(name = factor(name, 
                       levels = c("bio10", "max_temp_02", 
                                  "bio4","mean_temp_02","mndwi_q2", "mndwi_q1"#, 
                                  #"dist_Culex.modestus", "dist_Culex.pipiens"
                       ))) %>%
  
  ggplot(aes(wnf_case, value, color = data))+ 
  geom_jitter(size=1, alpha = 0.8, width = 0.2)+
  geom_boxplot()+
  stat_summary(fun = mean, geom = "point", size = 3)+
  facet_wrap(.~name, scales = "free_y", ncol=3)+
  theme_minimal()+
  theme_classic()+
  scale_color_manual(labels = c("2017", "2018"),#,
                     values = c(my3cols1[1], my3cols1[2]),
                     guide = "legend" )+
  theme( 
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black",fill=NA, size=0.5),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    axis.text.x = element_blank(),
    axis.text.y = element_text(margin = margin(r = .1, unit = "cm"),
                               size=10),
    legend.text=element_text(size=10),
    legend.key = element_blank(),
    legend.box = "vertical") +
  labs( 
    x='',
    y= "Feature value"#,
  )

