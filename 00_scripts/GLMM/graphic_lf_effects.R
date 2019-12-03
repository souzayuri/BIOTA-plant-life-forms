### title: GLMM graphic ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 12/23/2019
### Description: graphics to life form effects



teste <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/glmm/lf_effects.csv")
teste


ggplot(teste, aes(x = group, y = propor, fill = treat)) + geom_boxplot() + coord_flip() + 
  ylab("estimates") + geom_hline(yintercept = 0, linetype="dotted")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lf_effects.png", w = 15, h = 20, units = "cm", dpi = 300)
