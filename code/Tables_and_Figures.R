#### Tables and Figures ####

# upload code
source('code/packages_upload.R')


# upload data

load('data/model_male_data.RData')
load('data/model_female_data.RData')
load('results/california_estimates.RData')
load('results/utah_estimates.RData')
load('results/australia_sim.RData')

#### Figure 1 ####

data_svd = data.frame(m=c(m_male,m_female),
                      X1=c(X_male[1,],X_female[1,]),
                      X2=c(X_male[2,],X_female[2,]),
                      age=c(seq(15,55,5),seq(15,45,5)),
                      sex=c(rep('Male',9),rep('Female',7))) %>%
  pivot_longer(!c('sex','age'),names_to = 'svd',values_to = 'svd_est')

data_svd$svd = factor(data_svd$svd,
                      levels = c('m','X1','X2'),
                      labels=c(expression(italic(m)[x]^s),
                               expression(italic(y)[1*","*x]^s),
                               expression(italic(y)[2*","*x]^s)))
y_labels <- c(
  m = "Average Age Fertility Schedule",
  X1 = "Principal Component 1",
  X2 = "Principal Component 2"
)


plot_svd = ggplot(data_svd)+
  geom_line(aes(x=age,y=svd_est,color=sex),size=1)+
  geom_point(aes(x=age,y=svd_est,color=sex,shape=sex),size=5)+
  scale_colour_manual(values = c("#990099",
                                 "#009900"),
                      name="",
                      labels=c("Female","Male"))+
  scale_shape_manual(
    values = c(0, 2),  # 16 = solid circle, 17 = solid triangle
    name = "",
    labels = c("Female", "Male")
  ) +
  scale_x_continuous(breaks=seq(15,55,5),
                     labels=seq(15,55,5))+
  facet_wrap(~svd,
             scales = "free_y",
             labeller = label_parsed) +
  #scale_y_continuous(labels = function(x) parse(text = y_labels[data_svd$svd])) + 
  theme_minimal(base_family = "serif")+
  xlab("Age")+
  ylab("") +  # Remove the default y-axis label
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=14),
        axis.text.y = element_text(size=14),
        legend.position = "bottom",
        legend.text = element_text(face="bold",size=18),
        legend.title  = element_text(face="bold",size=12),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=18),
        strip.text.y = element_text(face="bold",size=18),
        axis.title.y = element_text(face="bold",size=18),
        axis.title.x = element_text(face="bold",size=18),
        plot.title = element_text(hjust=0.5,face="bold"))

ggsave("figures/plot1_paper.pdf",plot_svd,height = 10, width = 20, units = "cm")


#### Figure 3 ####


plot_sim_data = rbind(australia_TFR_final_male %>%
                        dplyr::select(code,Year,TFR_median,TFR_lower,TFR_upper,TFR_nat) %>%
                        mutate(sex='Male'),
                      australia_TFR_final_female %>%
                        dplyr::select(code,Year,TFR_median,TFR_lower,TFR_upper,TFR_nat) %>%
                        mutate(sex='Female'))



plot_sim = ggplot(plot_sim_data) +
  geom_line(aes(Year,TFR_median,color='Model'),size=1) +
  geom_point(aes(x=Year,y=TFR_nat,colour = "True"),shape=23,size=5)+
  #geom_point(mapping=aes(x=Year,y=iTFR,colour = "Indirect"))+
  geom_ribbon(aes(x=Year,ymin = TFR_lower, ymax = TFR_upper), alpha = 0.5, show.legend = F,fill="#d73027")+
  scale_colour_manual(values = c("Model" = "#d73027", "True" = "#009999"),
                      name = "",
                      breaks = c("Model", "True"), # This ensures the order of the legend
                      labels = c("Model", "True")) +
  theme_minimal(base_family = "serif")+
  ylim(c(1,3))+
  xlab('Year')+
  ylab('TFR')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=15),
        axis.text.y = element_text(size=15),
        legend.position = "bottom",
        aspect.ratio = 0.7,
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(face="bold",size=20),
        legend.title  = element_text(face="bold",size=20),
        strip.text.x = element_text(face="bold",size=15),
        strip.text.y = element_text(face="bold",size=15),
        axis.title.y = element_text(face="bold",size=15),
        axis.title.x = element_text(face="bold",size=15))+
  
  #scale_y_continuous(breaks=seq(0,3,.5),labels=seq(0,3,.5))+
  scale_x_continuous(breaks=c(2001,seq(2005,2020,5)),
                     labels=c(2001,seq(2005,2020,5))) +
  facet_wrap(sex~code,nrow=2)


ggsave("figures/plot3_paper.pdf",plot_sim,height = 15, width = 30, units = "cm")

#### Table 1 ####

data_table_men = iTFR_data_men  %>%
  group_by(code) %>%
  summarise(indirect_male=sqrt(mean((TFR_state-iTFR)^2))) %>%
  left_join(australia_TFR_final_male %>%
              group_by(code) %>%
              summarise(model_male=sqrt(mean((TFR_nat-TFR_median)^2))))




data_table_women = iTFR_data_women  %>%
  group_by(code) %>%
  summarise(indirect_fem=sqrt(mean((TFR-iTFR)^2))) %>%
  left_join(australia_TFR_final_female %>%
              group_by(code) %>%
              summarise(model_fem=sqrt(mean((TFR_nat-TFR_median)^2))))



data_table = data_table_men %>%
  left_join(data_table_women)


kable(data_table, "latex", booktabs = TRUE,
      col.names = c("State", "Model", "Indirect", "Model", "Indirect"),
      align = c("l", "r", "r", "r", "r"),
      caption = "") %>%
  add_header_above(c(" " = 1, "Male" = 2, "Female" = 2)) %>%
  kable_styling(latex_options = c("hold_position", "striped"),
                position = "center",
                full_width = FALSE)


#### Figure 4 ####

california_males = california_TFR_final_male %>%
  rename(fips=Fips) %>%
  left_join(Fips_to_County) %>%
  filter(county_name %in% c("Yolo County","San Francisco County","Tulare County")) %>%
  mutate(sex="Male",
         state="California",
         county_name=factor(county_name,levels=c("Yolo County","Tulare County","San Francisco County"),
                            labels=c('Yolo','Tulare','San Francisco')))


california_females = california_TFR_final_female %>% 
  select(-TFR_nat) %>%
  rename(fips=Fips) %>%
  left_join(Fips_to_County) %>%
  filter(county_name %in% c("Yolo County","San Francisco County","Tulare County")) %>%
  mutate(sex="Female",
         state="California",
         county_name=factor(county_name,levels=c("Yolo County","Tulare County","San Francisco County"),
                            labels=c('Yolo','Tulare','San Francisco')))


Fips_to_County <- read.table("~/Library/Mobile Documents/com~apple~CloudDocs/PHD STAT/PhD Dissertation/Male Fertility/Fips_to_County.txt")
Fips_to_County$fips = sprintf("%05d",Fips_to_County$fips)
Fips_to_County = Fips_to_County %>%
  rename(county_name=name)

utah_males = utah_TFR_final_male %>%
  rename(fips=Fips) %>%
  left_join(Fips_to_County) %>%
  filter(county_name %in% c("Duchesne County","Cache County",
                            "Sanpete County")) %>%
  mutate(sex="Male",
         state="Utah",
         county_name=factor(county_name,levels=c("Duchesne County","Cache County",
                                                 "Sanpete County"),
                            labels=c('Duchesne','Cache','Sanpete'))) %>%
  select(-TFR_nat)

utah_females = utah_TFR_final_female %>%
  rename(fips=Fips) %>%
  left_join(Fips_to_County) %>%
  filter(county_name %in% c("Duchesne County","Cache County",
                            "Sanpete County")) %>%
  mutate(sex="Female",
         state="Utah",
         county_name=factor(county_name,levels=c("Duchesne County","Cache County",
                                                 "Sanpete County"),
                            labels=c('Duchesne','Cache','Sanpete'))) %>%
  select(-TFR_nat)

plot_time_series = rbind(utah_males,utah_females,
                         california_females,california_males) %>%
  ggplot()+
  geom_line(aes(x=Year,y=TFR_median,color=sex),size=2)+
  geom_ribbon(aes(x=Year,ymin = TFR_lower, ymax = TFR_upper,color=sex,fill=sex), alpha = 0.5, show.legend = F)+
  #geom_text(aes(x=2005,y=2.6,label = label),color="black",check_overlap = TRUE,size=4)+
  scale_colour_manual(values = c("#990099",
                                 "#009900"),
                      name="",
                      labels=c("Female","Male"))+
  coord_cartesian(ylim=c(0.5,4.5))+
  scale_x_continuous(breaks = c(1982,seq(1985,2015,5),2019),
            labels = c(1982,seq(1985,2015,5),2019)) +
  scale_y_continuous(breaks = seq(1,5,1),
                     labels = seq(1,5,1)) +
  geom_hline(aes(yintercept = 2.1),linetype='dotted')+
  facet_wrap(state~county_name,nrow=2)+
  theme_minimal()+
  ylab("TFR estimate")+
  xlab("Year")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=10),
        axis.text.y = element_text(size=10),
        legend.position = "bottom",
        aspect.ratio = 0.66,
        #legend.key.size = unit(0.7, 'cm'),
        legend.text = element_text(face="bold",size=14),
        legend.title  = element_text(face="bold",size=14),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=14),
        strip.text.y = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.title.x = element_text(face="bold",size=14),
        plot.title = element_text(hjust=0.5,face="bold"))

ggsave("figures/plot4_paper.pdf",plot_time_series,height = 15, width = 30, units = "cm")

#### Figure 5 ####

data_utah_map_male = filter(utah_TFR_final_male,Year==2019) %>%
  mutate(counties=substr(Fips,3,5)) %>%
  rename(fips=Fips) %>%
  mutate(TFR_cat = case_when(TFR_median<1.3~"<1.3",
                             TFR_median>=1.3 & TFR_median<=1.6~"1.3-1.6",
                             TFR_median>1.6 & TFR_median<=2~"1.7-2",
                             TFR_median>2 & TFR_median<=2.5 ~"2.1-2.5",
                             TFR_median>2.5 ~ ">2.5"),
         TFR_cat = factor(TFR_cat,levels=c("<1.3","1.3-1.6","1.7-2","2.1-2.5",">2.5"))) %>%
  select(-TFR_nat)



data_california_map_male = filter(california_TFR_final_male,Year==2019) %>%
  mutate(counties=substr(Fips,3,5)) %>%
  rename(fips=Fips) %>%
  mutate(TFR_cat = case_when(TFR_median<1.3~"<1.3",
                             TFR_median>=1.3 & TFR_median<=1.6~"1.3-1.6",
                             TFR_median>1.6 & TFR_median<=2~"1.7-2",
                             TFR_median>2 & TFR_median<=2.5 ~"2.1-2.5",
                             TFR_median>2.5 ~ ">2.5"),
         TFR_cat = factor(TFR_cat,levels=c("<1.3","1.3-1.6","1.7-2","2.1-2.5",">2.5"))) 


data_utah_map_female = filter(utah_TFR_final_female,Year==2019) %>%
  mutate(counties=substr(Fips,3,5)) %>%
  rename(fips=Fips) %>%
  mutate(TFR_cat = case_when(TFR_median<1.3~"<1.3",
                             TFR_median>=1.3 & TFR_median<=1.6~"1.3-1.6",
                             TFR_median>1.6 & TFR_median<=2~"1.7-2",
                             TFR_median>2 & TFR_median<=2.5 ~"2.1-2.5",
                             TFR_median>2.5 ~ ">2.5"),
         TFR_cat = factor(TFR_cat,levels=c("<1.3","1.3-1.6","1.7-2","2.1-2.5",">2.5"))) %>%
  select(-TFR_nat)


data_california_map_female = filter(california_TFR_final_female,Year==2019) %>%
  mutate(counties=substr(Fips,3,5)) %>%
  rename(fips=Fips) %>%
  mutate(TFR_cat = case_when(TFR_median<1.3~"<1.3",
                             TFR_median>=1.3 & TFR_median<=1.6~"1.3-1.6",
                             TFR_median>1.6 & TFR_median<=2~"1.7-2",
                             TFR_median>2 & TFR_median<=2.5 ~"2.1-2.5",
                             TFR_median>2.5 ~ ">2.5"),
         TFR_cat = factor(TFR_cat,levels=c("<1.3","1.3-1.6","1.7-2","2.1-2.5",">2.5"))) %>%
  select(-TFR_nat)

                             
data_utah_map = rbind(data_utah_map_female %>%
                        mutate(sex='Female'),
                      data_utah_map_male %>%
                        mutate(sex='Male')) %>%
  mutate(state='Utah')


data_california_map = rbind(data_california_map_female %>%
                              mutate(sex='Female'),
                            data_california_map_male %>%
                              mutate(sex='Male')) %>%
  mutate(state='California')

                             
data_map <- rbind(data_utah_map, data_california_map)

# Define TFR categories
TFR_cat_1 <- c("<1.3", "1.3-1.6", "1.7-2", "2.1-2.5", ">2.5")
custom_colors <- setNames(scico(5, palette = "vik"), TFR_cat_1)

plot_utah = plot_usmap(data= data_map,values = "TFR_cat",regions = "counties",include = c("UT")) + 
  ggrepel::geom_label_repel(transformed_counties %>% 
                              filter(region=="utah",subregion!="lake") %>%
                              mutate(subregion=str_to_title(subregion)),mapping=aes(label = subregion, geometry = geometry),
                            stat = "sf_coordinates", size = 8,color="red")+
  scale_fill_manual(name = "TFR", values = custom_colors, drop = FALSE)+
  facet_wrap(state~sex)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(face="bold",size=16),
        legend.title  = element_text(face="bold",size=16),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=16),
        strip.text.y = element_text(face="bold",size=16),
        plot.title = element_text(hjust=0.5,face="bold",size=22))    



plot_california = plot_usmap(data=data_map,values = "TFR_cat",regions = "counties",include = c("CA")) + 
  scale_fill_manual(name = "TFR", 
                    values = custom_colors, 
                    limits = TFR_levels,
                    drop = FALSE)+
  ggrepel::geom_label_repel(transformed_counties %>% 
                              filter(region=="california",subregion!="lake") %>%
                              mutate(subregion=str_to_title(subregion)),mapping=aes(label = subregion, geometry = geometry),
                            stat = "sf_coordinates", size = 8,color="red")+
  scale_fill_manual(name = "TFR", values = custom_colors, drop = FALSE)+
  facet_wrap(state~sex)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(face="bold",size=16),
        legend.title  = element_text(face="bold",size=16),
        #legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid"),
        strip.text.x = element_text(face="bold",size=16),
        strip.text.y = element_text(face="bold",size=16),
        plot.title = element_text(hjust=0.5,face="bold",size=22))            



# Define your color mapping manually
TFR_levels <- c("<1.3", "1.3−1.6", "1.7−2", "2.1−2.5", ">2.5")
TFR_colors <- c(
  "<1.3"    = "#001260",
  "1.3−1.6" = "#2F7CA5",
  "1.7−2"   = "#EBE5E0",
  "2.1−2.5" = "#C27142",
  ">2.5"    = "#590007" 
)

# Dummy data for legend generation
legend_data <- data.frame(
  TFR_cat = factor(TFR_levels, levels = TFR_levels),
  y = 1
)

# Build a dummy plot that *produces* the legend
legend_plot <- ggplot(legend_data, aes(x = TFR_cat, y = y, fill = TFR_cat)) +
  geom_tile() +
  scale_fill_manual(
    name = "TFR",
    values = TFR_colors,
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1   # horizontal legend
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.7, "cm"),
    plot.margin = margin(0, 0, 0, 0)
  )

# Wrap legend into a standalone patchwork object
legend_only <- wrap_elements(get_legend(legend_plot))
g <- ggplotGrob(legend_plot)
idx <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
# If multiple, pick the first fill legend; you can also inspect g$grobs[idx]
legend_grob <- g$grobs[[idx[1]]]
legend <- grid.draw(legend_grob)

plot_map_TFR = plot_california/plot_utah/legend

ggsave("figures/plot5_paper.pdf",plot_map_TFR,height = 15, width = 30, units = "cm")


#### Figure 6 #### 

data_ratio_map_utah = utah_TFR_final_male %>%
  filter(Year==2019) %>%
  dplyr::select(Year,Fips,TFR_male=TFR_median) %>%
  left_join(utah_TFR_final_female %>%
              filter(Year==2019) %>%
              dplyr::select(Year,Fips,TFR_female=TFR_median)) %>%
  mutate(ratio=TFR_male/TFR_female,
         ratio_cat = case_when(ratio<0.95~'<0.95',
                               ratio>=0.95 & ratio<=1.05 ~ '0.95-1.05',
                               ratio>=1.05 ~ '>1.05'),
         ratio_cat = factor(ratio_cat,levels=c('<0.95','0.95-1.05','>1.05'))) %>%
  rename(fips=Fips)


data_ratio_map_california = california_TFR_final_male %>%
  filter(Year==2019) %>%
  dplyr::select(Year,Fips,TFR_male=TFR_median) %>%
  left_join(california_TFR_final_female %>%
              filter(Year==2019) %>%
              dplyr::select(Year,Fips,TFR_female=TFR_median)) %>%
  mutate(ratio=TFR_male/TFR_female,
         ratio_cat = case_when(ratio<0.95~'<0.95',
                               ratio>=0.95 & ratio<=1.05 ~ '0.95-1.05',
                               ratio>=1.05 ~ '>1.05'),
         ratio_cat = factor(ratio_cat,levels=c('<0.95','0.95-1.05','>1.05'))) %>%
  rename(fips=Fips)



data_ratio_map_utah = data_ratio_map_utah %>%
  mutate(state='Utah')


data_ratio_map_california = data_ratio_map_california %>%
  mutate(state='California')

plot_TFR_ratio_utah = plot_usmap(data=data_ratio_map_utah,values = "ratio_cat",regions = "counties",include = c("UT")) + 
  scico::scale_fill_scico_d(name="TFR ratio",palette = "vik")+
  ggrepel::geom_label_repel(transformed_counties %>% 
                              filter(region=="utah") %>%
                              mutate(subregion=str_to_title(subregion)),mapping=aes(label = subregion, geometry = geometry),
                            stat = "sf_coordinates", size = 4,color="red")+
  geom_sf(transformed_counties %>% filter(region=="utah"),mapping=aes(geometry = geometry),color="red") +
  facet_wrap(~state)+
  theme_void()+
  theme(legend.position = "none",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(face="bold",size=14),
        legend.title  = element_text(face="bold",size=14),
        strip.text.x = element_text(face="bold",size=14),
        strip.text.y = element_text(face="bold",size=14),
        plot.title = element_text(hjust=0.5,face="bold",size=22))



plot_TFR_ratio_california = plot_usmap(data=data_ratio_map_california,values = "ratio_cat",regions = "counties",include = c("CA")) + 
  scico::scale_fill_scico_d(name="TFR ratio",palette = "vik")+
  ggrepel::geom_label_repel(transformed_counties %>% 
                              filter(region=="california") %>%
                              mutate(subregion=str_to_title(subregion)),mapping=aes(label = subregion, geometry = geometry),
                            stat = "sf_coordinates", size = 4,color="red")+
  geom_sf(transformed_counties %>% filter(region=="california"),mapping=aes(geometry = geometry),color="red") +
  facet_wrap(~state)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        legend.text  = element_text(face="bold",size=14),
        legend.title  = element_text(face="bold",size=14),
        strip.text.x = element_text(face="bold",size=14),
        strip.text.y = element_text(face="bold",size=14),
        plot.title = element_text(hjust=0.5,face="bold",size=22))




plot_map_TFR_ratio = (plot_TFR_ratio_california+plot_TFR_ratio_utah)


ggsave("figures/plot6_paper.pdf",plot_map_TFR_ratio,height = 15, width = 30, units = "cm")

  
