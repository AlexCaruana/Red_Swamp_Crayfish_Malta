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
        CF <- read_xlsx("Crayfish Capture Data.xlsx")
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
                annotate("text", x = 68.5, y = -7, label = "(n = 287)", fontface="bold", size = 3) +
                annotate("text", x = 70, y = 7, label = "Male", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = 7, label = "(n = 316)", fontface="bold", size = 3) +
                theme_bw() +
                theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

        CF_Distribution_Density <- ggplot() +
                geom_density(data = CF[CF$ Sex == "Female",], aes(x = Carapace_Length, y = -..count..,), 
                               color = '#d95f02', fill = '#d95f02', alpha = 0.5) +
                geom_density(data = CF[CF$ Sex == "Male",], aes(x = Carapace_Length, y = ..count..,), 
                               color = '#1b9e77', fill = '#1b9e77', alpha = 0.5) +
                scale_x_continuous(breaks=seq(0,75,2.5)) +
                scale_y_continuous(breaks=seq(-30,30,1),
                                   labels = paste0(as.character(c(30:0, 1:30)), "")) +
                xlab("Carapace Length (mm)") +
                ylab("Density") +
                coord_flip() +
                annotate("text", x = 70, y = -7, label = "Female", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = -7, label = "(n = 287)", fontface="bold", size = 3) +
                annotate("text", x = 70, y = 7, label = "Male", fontface="bold", size = 5) +
                annotate("text", x = 68.5, y = 7, label = "(n = 316)", fontface="bold", size = 3) +
                theme_bw() +
                theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

        CF_Distribution + CF_Distribution_Density + plot_annotation(tag_levels = 'A')

        ## Crayfish sex pie chart
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
                Depth <- ggplot(EnvData, aes(x=Stream_Depth, y=Crayfish_CPUE)) + 
                        geom_point()+
                        stat_poly_line(color = "#448471") +
                        stat_poly_eq(use_label(c("eq", "R2"))) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Stream Depth (cm)") +
                        ylab("Crayfish CPUE") +
                        theme_bw() +
                        xlim(0, 250) +
                        scale_y_continuous(breaks=seq(0,20,2.5))

                # Stream Width & Crayfish CPUE scatter plot with regression
                Width <- ggplot(EnvData, aes(x=Stream_Width, y=Crayfish_CPUE)) + 
                        geom_point()+
                        stat_poly_line(color = "#448471") +
                        stat_poly_eq(use_label(c("eq", "R2"))) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Stream Width (cm)") +
                        ylab("Crayfish CPUE") +
                        theme_bw() +
                        xlim(0, 2000) +
                        scale_y_continuous(breaks=seq(0,17.5,2.5))

                # Riparian Vegetation & Crayfish CPUE scatter plot with regression
                EnvData$Riparian_Vegetation_Height <- as.numeric(EnvData$Riparian_Vegetation_Height) 
                Riparian <- ggplot(EnvData, aes(x=Riparian_Vegetation_Height, y=Crayfish_CPUE)) + 
                        geom_point()+
                        stat_poly_line(color = "#448471") +
                        stat_poly_eq(use_label(c("eq", "R2"))) +
                        theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
                        theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                        theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                        theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
                        xlab("Riparian Vegetation Height (cm)") +
                        ylab("Crayfish CPUE") +
                        theme_bw()+
                        xlim(0, 150) +
                        scale_y_continuous(breaks=seq(0,20,2.5))

                # Aquatic Vegetation Bar Plot with normalized CPUE (CPUE was calculated based on the total trapping effort used in all sites, grouped by the aquatic
                # vegetation cover breaks)
                EnvData$Aquatic_Vegetation_Cover <- as.numeric(EnvData$Aquatic_Vegetation_Cover)  
                EnvData$Aquatic_Vegetation_Cover_Breaks <- cut(EnvData$Aquatic_Vegetation_Cover, breaks=5, include.lowest=TRUE)
                AquaticCPUE <- EnvData %>% 
                        group_by(Aquatic_Vegetation_Cover_Breaks) %>% 
                        summarise(summedCrayfish = sum(Total_Crayfish)) 
                
                TrappingSitesByGroupAquatic = c(24,3,9,12,12,60) #The amount of sites was manually counted and multiplied depending on the trapping protocol used.
                AquaticCPUE$effort = TrappingSitesByGroupAquatic
                AquaticCPUE$CPUE = AquaticCPUE$summedCrayfish/AquaticCPUE$effort
                
                Vegetation <- ggplot(data = AquaticCPUE, aes(x = Aquatic_Vegetation_Cover_Breaks, y = CPUE, fill = Aquatic_Vegetation_Cover_Breaks)) +
                        geom_bar(stat = "identity", position = "dodge") +
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

                Depth + Width + Vegetation + Substrate + Riparian + plot_annotation(tag_levels = 'A') +   plot_layout(ncol = 2)

### END