### ASSESSMENT OF RED SWAMP CRAYFISH PRESENCE, DISTRIBUTION AND ABUNDANCE WITHIN THE FIDDIEN VALLEY SYSTEM
### Data Analysis code written by Alex Caruana

        ## Setting-up R
                # Package Installation & Loading
                install.packages("tidyverse")
                install.packages("readxl")
                install.packages('devtools')
                install_github("thomasp85/patchwork")
                install.packages("magrittr")
                install.packages("dplyr")   
                install.packages("ggpmisc")
                remotes::install_github("coolbutuseless/ggpattern")
                
                library(ggpattern)
                library(ggplot2)
                library(readxl)
                library(devtools)
                library(patchwork)
                library(magrittr)
                library(dplyr)  
                library(ggpmisc)

                # Set working directory with all the data-sets present
                getwd()
                setwd('C:/Users/Alex/Desktop/Masters Thesis/Fieldwork Data')

        ## Crayfish carapace distribution graphs
        CF <- read_excel("Fiddien Valley Assessment - Fieldwork Data.xlsx", sheet = "3. Crayfish_Data")
        Female<-CF[CF$Sex=="Female",]
        Male<-CF[CF$Sex=="Male",]

        CF_Distribution <- ggplot() +
                geom_histogram(data = CF[CF$ Sex == "Female",], aes(x = Carapace_Length, y = -..count..,), 
                               bins=50, color = '#d95f02', fill = '#d95f02', alpha = 0.5) +
                geom_histogram(data = CF[CF$ Sex == "Male",], aes(x = Carapace_Length, y = ..count..,), 
                               bins=50, color = '#1b9e77', fill = '#1b9e77', alpha = 0.5) +
                scale_x_continuous(breaks=seq(0,75,2.5)) +
                scale_y_continuous(breaks=seq(-30,30,1),
                                   labels = paste0(as.character(c(30:0, 1:30)), "")) +
                xlab("Carapace Length (mm)") +
                ylab("Frequency") +
                coord_flip() +
                annotate("text", x = 70, y = -7, label = "Female", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = -7, label = "(n = 313)", fontface="bold", size = 3) +
                annotate("text", x = 70, y = 7, label = "Male", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = 7, label = "(n = 343)", fontface="bold", size = 3) +
                theme_bw() +
                theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))
        #66A391", "#1d3830"
        CF_Distribution_Density <- ggplot() +
                geom_density(data = CF[CF$ Sex == "Female",], aes(x = Carapace_Length, y = -..count..,), 
                               color = '#2b2d42', fill = '#2b2d42', alpha = 0.5) +
                geom_density(data = CF[CF$ Sex == "Male",], aes(x = Carapace_Length, y = ..count..,), 
                               color = '#8d99ae', fill = '#8d99ae', alpha = 0.5) +
                scale_x_continuous(breaks=seq(0,75,2.5)) +
                scale_y_continuous(breaks=seq(-30,30,1),
                                   labels = paste0(as.character(c(30:0, 1:30)), "")) +
                xlab("Carapace Length (mm)") +
                ylab("Density") +
                coord_flip() +
                annotate("text", x = 70, y = -7, label = "Female", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = -7, label = "(n = 313)", fontface="bold", size = 3) +
                annotate("text", x = 70, y = 7, label = "Male", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = 7, label = "(n = 343)", fontface="bold", size = 3) +
                theme_bw() +
                theme(axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

        CF_Distribution + CF_Distribution_Density + plot_annotation(tag_levels = 'A')

        ## Crayfish sex pie chart & mean statistics
        gender_summary <- CF %>%
                group_by(Sex) %>%
                summarise(Percent = n()/nrow(.) * 100)
        
        label <- round(gender_summary$Percent, digits = 1)
        
        CF_Gender <- ggplot(data = gender_summary, mapping = aes(x = "", y = Percent, fill = Sex)) + 
                geom_bar(width = 1, stat = "identity", alpha = 0.7, color="white") + 
                scale_y_continuous(breaks = round(cumsum(rev(gender_summary$Percent)), 1)) +
                coord_polar("y", start = 0) +
                scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
                geom_text(aes(label = paste0(label, "%")), position = position_stack(vjust=0.5)) + 
                guides(fill = guide_legend(title = "Gender")) +
                theme_void()
        
        ## Environmental Data Analysis
        EnvData <- read_excel("Site Environmental Data.xlsx", sheet = 2)
        str(EnvData)
        EnvData$Stream_Depth <- as.numeric(EnvData$Stream_Depth)   

                # Stream Depth & Crayfish CPUE scatter plot with regression
                Depth <- ggplot(EnvData, aes(x=Stream_Depth, y=Crayfish_CPUE, color = Site_Classification)) + 
                        geom_point() +
                        geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
                                    aes(group=1),colour="black") +
                        scale_color_manual(breaks = c("Excavated Area", "Non-excavated Area"),
                                           values=c("#66A391", "#999999")) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Stream Depth (cm)") +
                        ylab("Crayfish CPUE") +
                        theme_bw() +
                        xlim(0, 250) +
                        scale_y_continuous(breaks=seq(0,20,2.5)) +
                        #geom_text(x = 55, y = 15.7, label = eq(EnvData$Stream_Depth,EnvData$Crayfish_CPUE), parse = TRUE) +
                        theme(legend.position = "none")

                # Stream Width & Crayfish CPUE scatter plot with regression
                Width <- ggplot(EnvData, aes(x=Stream_Width, y=Crayfish_CPUE, color = Site_Classification)) + 
                        geom_point()+
                        #stat_poly_line(color = "#448471") +
                        #stat_poly_eq(use_label(c("eq", "R2"))) +
                        geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
                                    aes(group=1),colour="black") +
                        scale_color_manual(breaks = c("Excavated Area", "Non-excavated Area"),
                                           values=c("#66A391", "#999999")) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Stream Width (cm)") +
                        ylab("Crayfish CPUE") +
                        theme_bw() +
                        xlim(0, 2000) +
                        scale_y_continuous(breaks=seq(0,17.5,2.5)) +
                        #geom_text(x = 500, y = 16.5, label = eq(EnvData$Stream_Width,EnvData$Crayfish_CPUE), parse = TRUE) +
                        theme(legend.position = "none")

                # Riparian Vegetation & Crayfish CPUE scatter plot with regression
                EnvData$Riparian_Vegetation_Height <- as.numeric(EnvData$Riparian_Vegetation_Height) 
                Riparian <- ggplot(EnvData, aes(x=Riparian_Vegetation_Height, y=Crayfish_CPUE, color = Site_Classification)) + 
                        geom_point()+
                        #stat_poly_line(color = "#448471") +
                        #stat_poly_eq(use_label(c("eq", "R2"))) +
                        geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
                                    aes(group=1),colour="black") +
                        scale_color_manual(breaks = c("Excavated Area", "Non-excavated Area"),
                                           values=c("#66A391", "#999999")) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Riparian Vegetation Height (cm)") +
                        ylab("Crayfish CPUE") +
                        theme_bw()+
                        xlim(0, 150) +
                        scale_y_continuous(breaks=seq(0,20,2.5)) +
                        #geom_text(x = 50, y = 15.7, label = eq(EnvData$Riparian_Vegetation_Height,EnvData$Crayfish_CPUE), parse = TRUE) +
                        theme(legend.position = "none")
                
                eq(EnvData$Riparian_Vegetation_Height,EnvData$Crayfish_CPUE)
                # Aquatic Vegetation Bar Plot with normalized CPUE (CPUE was calculated based on the total trapping effort used in all sites, grouped by the aquatic
                # vegetation cover breaks)
                EnvData$Aquatic_Vegetation_Cover <- as.numeric(EnvData$Aquatic_Vegetation_Cover)  
                EnvData$Aquatic_Vegetation_Cover_Breaks <- cut(EnvData$Aquatic_Vegetation_Cover, breaks=5, include.lowest=TRUE)
                
                
                Aquatic_Veg_Breaks <- c("0-20","0-20",
                           "20-40","20-40",
                           "40-60","40-60",
                           "60-80","60-80",
                           "80-100","80-100",
                           "NA","NA")
                
                Excavation_Classification_Veg <- c("Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site")
                
                Summed_Crayfish_Veg <- c(76,7,
                                         0,5,
                                         0,20,
                                         64,4,
                                         0,16,
                                         396,14)
                
                Summed_Effort <- c(6,18,
                                   0,3,
                                   0,9,
                                   6,12,
                                   0,12,
                                   36,48)
                
                CPUE_Veg <- c(76/24,7/24,
                              0,5/3,
                              0,20/9,
                              64/12,4/12,
                              0,16/12,
                              396/60,14/60)
                
                Aquatic_CPUE_2 <- data.frame(Aquatic_Veg_Breaks, Excavation_Classification_Veg, Summed_Crayfish_Veg, Summed_Effort, CPUE_Veg) 
                Vegetation_2 <- ggplot(Aquatic_CPUE_2, aes(x = Aquatic_Veg_Breaks, y = CPUE_Veg, fill = Excavation_Classification_Veg)) +
                        geom_col_pattern(
                                aes(pattern = Excavation_Classification_Veg),
                                colour = "black",
                                pattern_fill = "#66A391",
                                pattern_alpha = 0.1,
                                pattern_angle = 45,
                                pattern_density = 1,
                                pattern_spacing = 0.05,
                                position = "stack",
                        ) +
                        scale_pattern_manual(
                                values = c("stripe", "none"),
                                guide = guide_legend(override.aes = list(fill = "white", alpha = 0.1)) # <- make lighter
                        ) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Aquatic Vegeation Cover (%)") +
                        ylab("Crayfish CPUE") +
                        scale_fill_manual(values= alpha(c("#66A391", "#1d3830"), 0.7)) +
                        guides(fill = FALSE)+
                        scale_y_continuous(breaks=seq(0,7,0.5)) +
                        theme_bw() +
                        theme(legend.position = "none")
                #66A391", "#1d3830"

                                # Original CPUE (Vegetation) - No excavation classification
                                AquaticCPUE <- EnvData %>% 
                                        group_by(Aquatic_Vegetation_Cover_Breaks) %>% 
                                        summarise(summedCrayfish = sum(Total_Crayfish)) 
                                
                                TrappingSitesByGroupAquatic = c(24,3,9,12,12,60) #The amount of sites was manually counted and multiplied depending on the trapping protocol used.
                                AquaticCPUE$effort = TrappingSitesByGroupAquatic
                                AquaticCPUE$CPUE = AquaticCPUE$summedCrayfish/AquaticCPUE$effort
                                
                                Vegetation <- ggplot(data = AquaticCPUE, aes(x = Aquatic_Vegetation_Cover_Breaks, y = CPUE, fill = Aquatic_Vegetation_Cover_Breaks, pattern = Excavation)) +
                                        geom_bar(stat = "identity", position = "dodge") +
                                        geom_bar_pattern(position = position_dodge(preserve = "single"),
                                                         color = "black", 
                                                         pattern_fill = "black",
                                                         pattern_angle = 45,
                                                         pattern_density = 0.1,
                                                         pattern_spacing = 0.025,
                                                         pattern_key_scale_factor = 0.6)+
                                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                                        xlab("Aquatic Vegeation Cover (%)") +
                                        ylab("Crayfish CPUE") +
                                        scale_x_discrete(labels=c("0-20", "20-40","40-60", "60-80","80-100"))+
                                        scale_fill_manual(values=c("#61bca1", "#448471", "#315e51", "#1d3830","#0a1310")) +
                                        guides(fill = FALSE)+
                                        scale_y_continuous(breaks=seq(0,7,0.5)) +
                                        theme_bw()

                # Stream Substrate Bar Plot with normalized CPUE (CPUE was calculated based on the total trapping effort used in all sites, grouped by the stream
                # substrate)
                                
                Substrate_Classification <- c("Bedrock","Bedrock",
                                        "Cobble","Cobble",
                                        "Sand","Sand",
                                        "Silt","Silt")
                                
                Excavation_Classification_Sub <- c("Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site",
                                                   "Excavated Site","Non-excavated Site")
                                
                Summed_Crayfish_Substrate <- c(0,7,
                                         0,13,
                                         145,5,
                                         391,41)
                                
                Summed_Effort_Substrate <- c(0,21,
                                   0,12,
                                   12,9,
                                   36,30)
                                
                CPUE_Substrate <- c(0,7/21,
                              0,13/12,
                              145/21,5/21,
                              391/66,41/66)                
                                
                                
                Substrate_CPUE_2 <- data.frame(Substrate_Classification, Excavation_Classification_Sub, Summed_Crayfish_Substrate, Summed_Effort_Substrate, CPUE_Substrate) 
                Substrate_2 <- ggplot(Substrate_CPUE_2, aes(x = Substrate_Classification, y = CPUE_Substrate, fill = Excavation_Classification_Sub)) +
                        geom_col_pattern(
                                aes(pattern = Excavation_Classification_Sub),
                                colour = "black",
                                pattern_fill = "#66A391",
                                pattern_alpha = 0.1,
                                pattern_angle = 45,
                                pattern_density = 1,
                                pattern_spacing = 0.05,
                                position = "stack") +
                        scale_pattern_manual(
                                values = c("stripe", "none")) + # <- make lighter
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Substrate Type") +
                        ylab("Crayfish CPUE") +
                        scale_fill_manual(values= alpha(c("#66A391", "#1d3830"), 0.7)) +
                        scale_y_continuous(breaks=seq(0,7,0.5)) +
                        theme_bw() +
                        theme(legend.position = "none")
                
                                # Original CPUE (Substrate) - No excavation classification
                                df <- EnvData %>% 
                                        group_by(Stream_Substrate) %>% 
                                        summarise(Total_Crayfish = sum(Total_Crayfish))
                                
                                TrappingSitesByGroup = c(21,12,21,66) #The amount of sites was manually counted and multiplied depending on the trapping protocol used.
                                df$effort = TrappingSitesByGroup
                                df$CPUE = df$Total_Crayfish/df$effort
                                
                                Substrate <- ggplot(data = df, aes(x = Stream_Substrate, y = CPUE, fill = Stream_Substrate)) +
                                        geom_bar(stat = "identity", position = "dodge") +
                                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                                        xlab("Stream Substrate Type") +
                                        ylab("Crayfish CPUE") +
                                        scale_fill_manual(values=c("#61bca1", "#448471", "#315e51","#0a1310")) +
                                        guides(fill = FALSE)+
                                        scale_y_continuous(breaks=seq(0,7,0.5)) +
                                        theme_bw()

                Depth + Width + Vegetation_2 + Substrate_2 + Riparian + plot_annotation(tag_levels = 'A') +   plot_layout(ncol = 2)
                
        ## Crayfish Whisper Plots per site
        WS <- read_excel("Fiddien Valley Assessment - Fieldwork Data.xlsx", sheet = "3. Crayfish_Data")
        
        
        str(WS)
        WS_Plot <- ggplot(WS, aes(x=Site_Classification, y=Carapace_Length, color=Site_Classification)) +
                geom_boxplot(alpha = 0.7, show.legend = FALSE) +
                theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                ylab("Carapace Length") +
                xlab("") +
                scale_color_manual(values=c("#66A391", "#1d3830"))+
                scale_y_continuous(breaks=seq(0,75,5)) +
                theme_bw()
        
        WS_CPUE <- read_excel("Fiddien Valley Assessment - Fieldwork Data.xlsx", sheet = "5. Environmental_Data_Average")
        
        WS_Plot_CPUE <- ggplot(WS_CPUE, aes(x=Site_Classification, y=Crayfish_CPUE, color=Site_Classification)) +
                geom_boxplot(alpha = 0.7, show.legend = FALSE) +
                theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                ylab("Crayfish CPUE") +
                xlab("") +
                scale_color_manual(values=c("#66A391", "#1d3830"))+
                scale_y_continuous(breaks=seq(0,15,2.5)) +
                theme_bw()
                
        WS_Plot + WS_Plot_CPUE + plot_annotation(tag_levels = 'A') +   plot_layout(ncol = 2)
        
        layer_data(WS_Plot_CPUE)
        
        ## General Statistics on whisker plot
                # Means
                Mean_Male <- WS[WS$Sex=="Male",]
                Mean_Male_Results <- mean(Mean_Male$Carapace_Length)
                print(Mean_Male_Results)
                
                Mean_Female <- WS[WS$Sex=="Female",]
                Mean_Female_Results <- mean(Mean_Female$Carapace_Length)
                print(Mean_Female_Results)
                
                # CPUE Quantiles
                Excavated_Quantile <- WS_CPUE[WS_CPUE$Site_Classification=="Excavated Area",]
                Excavated_Quantile_CPUE <- Excavated_Quantile$Crayfish_CPUE
                quantile(Excavated_Quantile_CPUE)
                
                Non_excavated_Quantile <- WS_CPUE[WS_CPUE$Site_Classification=="Non-Excavated Area",]
                Non_excavated_Quantile_CPUE <- Non_excavated_Quantile$Crayfish_CPUE
                quantile(Non_excavated_Quantile_CPUE)
        
        ## T-Test
        T_Test_Excavated <- WS[WS$Site_Classification=="Excavated Area",]
        T_Test_Non_Excavated <- WS[WS$Site_Classification=="Non-Excavated Area",]
        
        sd(T_Test_Excavated$Carapace_Length)
        sd(T_Test_Non_Excavated$Carapace_Length)
        
        t.test(T_Test_Excavated$Carapace_Length, T_Test_Non_Excavated$Carapace_Length, var.equal = FALSE)
### END