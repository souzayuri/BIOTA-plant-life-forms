### title: GLMM Estimates graphic ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 01/02/2020
### Description: graphics representing life form effects


# load packages and table -------------------------------------------------

rm(list = ls())

library(tidyverse)
library(ggpubr)


life_forms <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/glmm/life_forms_competition_estimates_27-04-20.csv")
life_forms


# Pointrange plots --------------------------------------------------------


# trees -------------------------------------------------------------------

trees <- life_forms %>% 
  filter(life_form == "trees")
trees

trees$competitors <- factor(trees$competitors, levels = unique(trees$competitors))


trees_graph <- ggplot(trees, aes(x = reorder(competitors, desc(competitors)), y = estimate)) +
  geom_errorbar(aes(ymin = estimate-sd, ymax = estimate+sd, fill = treatment), 
    position = position_dodge(0.5), width = 0.5, size = 0.5) + 
  geom_point(aes(fill = treatment), position = position_dodge(0.5), 
             size = 4, shape = 21, color = "black", stroke = 1.0) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype=2) +
  theme_classic() + 
  ylab("") + xlab("Trees competitors") +
  scale_x_discrete(labels=c("Bamboos", "Herbs", "Shrubs", "Palms", "Lianas")) +
  scale_fill_manual(values = c("tomato4", "seagreen"), labels = c("Closed","Open"), name = "Treatment") +
  theme(
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = c(0.93, .8),
    legend.background = element_rect(fill = "azure2", color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "dimgrey"),
    panel.background = element_rect(fill = "darkseagreen3", colour = "dimgrey",
                                          size = 0.5, linetype = "solid")) +
  annotate("text", label = "Positive", parse = FALSE, size = 8, x = 5.0, y = 0.015, fontface="bold", color = "gray17") +
  annotate("text", label = "Negative", parse = FALSE, size = 8, x = 5.0, y = -0.02, fontface="bold", color = "gray17") +  
  expand_limits(y = 0.18) +
  scale_y_continuous(breaks = c(-0.05,0.0, 0.05, 0.1, 0.15))
trees_graph

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/projeto/artigo/02_figuras/competiton_glmm/trees_vs_life-form.png", width = 25, height = 8, units = "cm", dpi = 500)


# lianas -------------------------------------------------------------------


lianas <- life_forms %>% 
  filter(life_form == "lianas")
lianas

lianas$competitors <- factor(lianas$competitors, levels = unique(lianas$competitors))


lianas_graph <- ggplot(lianas, aes(x = reorder(competitors, desc(competitors)), y = estimate)) +
  geom_errorbar(aes(ymin = estimate-sd, ymax = estimate+sd, fill = treatment), 
                position = position_dodge(0.5), width = 0.5, size = 0.5) + 
  geom_point(aes(fill = treatment), position = position_dodge(0.5), 
             size = 4, shape = 21, color = "black", stroke = 1.0) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype=2) +
  theme_classic() + 
  ylab("") + xlab("Lianas competitors") +
  scale_x_discrete(labels=c("Bamboos", "Herbs", "Shrubs", "Palms", "Trees")) +
  scale_fill_manual(values = c("tomato4", "seagreen"), labels = c("Closed","Open"), name = "Treatment") +
  theme(
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.background = element_rect(fill = "azure2", color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "dimgrey"),
    panel.background = element_rect(fill = "aquamarine3", colour = "dimgrey",
                                    size = 0.5, linetype = "solid")) +
  #annotate("text", label = 'atop(bold("A"))', parse = TRUE, size = 7, x = 4.8, y = 0.37) +
  expand_limits(y=0.6)

lianas_graph

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/projeto/artigo/02_figuras/competiton_glmm/lianas_vs_life-form.png", width = 25, height = 8, units = "cm", dpi = 500)

# Palms -------------------------------------------------------------------


palms <- life_forms %>% 
  filter(life_form == "palms")
palms

palms$competitors <- factor(palms$competitors, levels = unique(palms$competitors))


palms_graph <- ggplot(palms, aes(x = reorder(competitors, desc(competitors)), y = estimate)) +
  geom_errorbar(aes(ymin = estimate-sd, ymax = estimate+sd, fill = treatment), 
                position = position_dodge(0.5), width = 0.5, size = 0.5) + 
  geom_point(aes(fill = treatment), position = position_dodge(0.5), 
             size = 4, shape = 21, color = "black", stroke = 1.0) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype=2) +
  theme_classic() + 
  ylab("") + xlab("Palms competitors") +
  scale_x_discrete(labels=c("Bamboos", "Herbs", "Shrubs", "Lianas", "Trees")) +
  scale_fill_manual(values = c("tomato4", "seagreen"), labels = c("Closed","Open"), name = "Treatment") +
  theme(
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "none",
    legend.background = element_rect(fill = "azure2", color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "dimgrey"),
    panel.background = element_rect(fill = "antiquewhite2", colour = "dimgrey",
                                    size = 0.5, linetype = "solid")) +
  #annotate("text", label = 'atop(bold("A"))', parse = TRUE, size = 7, x = 4.8, y = 0.37) +
  expand_limits(y=0.75)

palms_graph

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/projeto/artigo/02_figuras/competiton_glmm/palms_vs_life-form.png", width = 25, height = 8, units = "cm", dpi = 500)

# Shrubs -------------------------------------------------------------------


shrubs <- life_forms %>% 
  filter(life_form == "shrubs")
shrubs

shrubs$competitors <- factor(shrubs$competitors, levels = unique(shrubs$competitors))


shrubs_graph <- ggplot(shrubs, aes(x = reorder(competitors, desc(competitors)), y = estimate)) +
  geom_errorbar(aes(ymin = estimate-sd, ymax = estimate+sd, fill = treatment), 
                position = position_dodge(0.5), width = 0.5, size = 0.5) + 
  geom_point(aes(fill = treatment), position = position_dodge(0.5), 
             size = 4, shape = 21, color = "black", stroke = 1.0) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype=2) +
  theme_classic() + 
  ylab("") + xlab("Shrubs competitors") +
  scale_x_discrete(labels=c("Bamboos", "Herbs", "Palms", "Lianas", "Trees")) +
  scale_fill_manual(values = c("tomato4", "seagreen"), labels = c("Closed","Open"), name = "Treatment") +
  theme(
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "none",
    legend.background = element_rect(fill = "azure2", color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "dimgrey"),
    panel.background = element_rect(fill = "peachpuff1", colour = "dimgrey",
                                    size = 0.5, linetype = "solid")) #+
  #annotate("text", label = 'atop(bold("A"))', parse = TRUE, size = 7, x = 4.8, y = 0.37) +
  scale_y_continuous(limits = c(-0.55, 0.50))

shrubs_graph

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/projeto/artigo/02_figuras/competiton_glmm/shrubs_vs_life-form.png", width = 25, height = 8, units = "cm", dpi = 500)



# Herbs -------------------------------------------------------------------

herbs <- life_forms %>% 
  filter(life_form == "herbs")
herbs

herbs$competitors <- factor(herbs$competitors, levels = unique(herbs$competitors))


herbs_graph <- ggplot(herbs, aes(x = reorder(competitors, desc(competitors)), y = estimate)) +
  geom_errorbar(aes(ymin = estimate-sd, ymax = estimate+sd, fill = treatment), 
                position = position_dodge(0.5), width = 0.5, size = 0.5) + 
  geom_point(aes(fill = treatment), position = position_dodge(0.5), 
             size = 4, shape = 21, color = "black", stroke = 1.0) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype=2) +
  theme_classic() + 
  ylab("") + xlab("Herbs competitors") +
  scale_x_discrete(labels=c("Bamboos", "Shrubs", "Palms", "Lianas", "Trees")) +
  scale_fill_manual(values = c("tomato4", "seagreen"), labels = c("Closed","Open"), name = "Treatment") +
  theme(
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "none",
    legend.background = element_rect(fill = "azure2", color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "dimgrey"),
    panel.background = element_rect(fill = "burlywood2", colour = "dimgrey",
                                    size = 0.5, linetype = "solid")) +
  #annotate("text", label = 'atop(bold("A"))', parse = TRUE, size = 7, x = 4.8, y = 0.37) +
  expand_limits(y=0.32)

herbs_graph
  
#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/projeto/artigo/02_figuras/competiton_glmm/herbs_vs_life-form.png", width = 25, height = 8, units = "cm", dpi = 500)




# bamboos -------------------------------------------------------------------


bamboos <- life_forms %>% 
  filter(life_form == "bamboos")
bamboos
bamboos$competitors <- factor(bamboos$competitors, levels = unique(bamboos$competitors))


bamboos_graph <- ggplot(bamboos, aes(x = reorder(competitors, desc(competitors)), y = estimate)) +
  geom_errorbar(aes(ymin = estimate-sd, ymax = estimate+sd, fill = treatment), 
                position = position_dodge(0.5), width = 0.5, size = 0.5) + 
  geom_point(aes(fill = treatment), position = position_dodge(0.5), 
             size = 4, shape = 21, color = "black", stroke = 1.0) +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype=2) +
  theme_classic() + 
  ylab("Estimates") + xlab("Bamboos competitors") +
  scale_x_discrete(labels=c("Herbs", "Shrubs", "Palms", "Lianas", "Trees")) +
  scale_fill_manual(values = c("tomato4", "seagreen"), labels = c("Closed","Open"), name = "Treatment") +
  theme(
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "none",
    legend.background = element_rect(fill = "azure2", color = "black"),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "dimgrey"),
    panel.background = element_rect(fill = "burlywood3", colour = "dimgrey",
                                    size = 0.5, linetype = "solid")) +
  scale_y_continuous(limits = c(-0.43, 1.05))

bamboos_graph 

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/projeto/artigo/02_figuras/competiton_glmm/bamboos_vs_life-form.png", width = 25, height = 8, units = "cm", dpi = 500)




# all_graphics ------------------------------------------------------------

figure <- ggarrange(trees_graph, lianas_graph, palms_graph, 
                    shrubs_graph, herbs_graph, bamboos_graph,
                    ncol = 1, nrow = 6)
figure

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/competition_estimates/all_vs_life-form_27-04-20.png", width = 35, height = 60, units = "cm", dpi = 300)



