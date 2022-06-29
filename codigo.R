
setwd("C:\\Users\\victo\\OneDrive\\Documents\\Vic\\arrete c'est ici l'empire de la mort\\Mestrado\\Tabelas")

getwd()

install.packages("ggplot2")

library("ggplot2")

tabela <- read.csv2("TPI.csv")


grafico <- ggplot(data=tabela, aes(x=Forma, fill=Período))
grafico + geom_bar(position = "dodge", color="black", alpha=0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                         position = position_dodge(width = 1), size = 3.5) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição da tipologia de formas de 
amuletos em cada período") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))


grafico2 <- ggplot(subset(tabela, Forma%in%c("Hathor", "Thoth", "Ísis e Hórus", "Nefertum", 
                                             "Sekhmet", "Taweret", "Bes", "Hórus", "Ísis",
                                             "Nehebkau", "Hórus criança", "Pataikos", "Néftis",
                                             "Mut", "Hetmehyt", "Nekhbet", "Divindade")), aes(x=Forma, fill=Período))
grafico2 + geom_bar(position = "dodge", color="black", alpha=0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                         position = position_dodge(width = 1), size = 3.5) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição de amuletos em forma de divindades 
em cada período") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))


grafico3 <- ggplot(subset(tabela, !Material%in%c("N/A")), aes(x=Forma, fill=Material)) +
  grafico3 + geom_bar(position = "dodge", color="black", alpha=0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                         position = position_dodge(width = 1), size = 4) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans", size = 15)) +
  ggtitle("Distribuição de formas de amuletos por
material no Terceiro Período Intermediário") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))
  

grafico4 <- ggplot(subset(tabela, !Sexo.Idade%in%c( "N/A")), aes(x=Forma, fill=Sexo.Idade))
grafico4 + geom_bar(position = "stack", color="black", alpha= 0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = 0.5,
                         position=position_stack(vjust=0.5), size = 3.5) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição de formas de amuletos por sexo
e idade no Terceiro Período Intermediário") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size = 10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))



  grafico4 <- ggplot(subset(tabela, Posição %in% c("Cabeça", "Mãos", "Peito", "Pés", "Pescoço", "Tornozelos")), aes(x=Posição, y=Forma, color=Tipo))
grafico4 + geom_count() +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição de formas por tipologia 
          e posição em Matmar") + 
  scale_color_manual(values=c("#F34213", "#38E4AE", "#E06C9F", "#B14AED", "#454ADE", "#FF7F11")) +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_size(range = c(2, 6))
  
grafico <- ggplot(data=tabela, aes(x=Forma, y=Sexo.Idade))
grafico + geom_bar(position = "dodge", color="black", alpha=0.8, width = 0.5) + theme_bw() + 
  geom_text(aes(label=..count..), stat="count", position=position_dodge(width=0.5), vjust = -1, size = 4) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  scale_fill_manual(values=c("#D49297", "#8EAEBD", "#AFD5AA", "#A491D3", "#818AA3")) +
  ggtitle("Distribuição de formas de divindades por sexo e idade em Lahun") + 
  theme(plot.title=element_text(face="bold")) + coord_flip()

scale_fill_manual(values=c("#D49297", "#8EAEBD", "#AFD5AA", "#A491D3", "#818AA3", "#FFBF00", "#34113F", "#0B6E4F", "#FA9F42", "#18FF6D", "#FD151B", "#FD151B", "#01295F", "#FFB30F", "#A50104", "#E365C1", "#2274A5")) +

 
  grafico5 <- ggplot(subset(tabela, Forma%in%c("Sapo", "Crocodilo", "Tartaruga", "Lebre", "Porca", "Falcão", "Pato",
                                               "Cosca", "Babuíno", "Peixe", "Gato", "Chacal", "Íbis", "Serpente", 
                                               "Carneiro", "Leão", "Golfinho", "Mosca")), aes(x=Forma, fill=Período))
  grafico5 + geom_bar(position = "dodge", color="black", alpha=0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                         position = position_dodge(width = 1), size = 3.5) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição de amuletos em forma de animais 
em cada período") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))

grafico6 <- ggplot(subset(tabela, Forma%in%c("Nefer", "Udjat", "Pilar djed", "Lua crescente",
                                             "Menat", "Égide", "Tyt", "Uraeus", "Pássaro-ba", 
                                             "Cetro-was")), aes(x=Forma, fill=Período))
  grafico6 + geom_bar(position = "dodge", color="black", alpha=0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                         position = position_dodge(width = 1), size = 3.5) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição de amuletos em forma de símbolos 
divinos em cada período") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))

grafico7 <- ggplot(subset(tabela, Forma%in%c("Coluna de papiro", "Flor de lótus", "Perna", "Cabeça",
                                             "Rosto", "Coração", "Mão", "Não identificada",
                                             "Mesa de oferenda", "Sítula", "Lira")), aes(x=Forma, fill=Período))
  grafico7 + geom_bar(position = "dodge", color="black", alpha=0.8) +
  theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                         position = position_dodge(width = 1), size = 3.5) +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribuição de amuletos em formas naturais, partes 
do corpo e outros em cada período") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))
  
  grafico8 <- ggplot(subset(tabela, Forma%in%c("Thoth", "Taweret", "Sekhmet", "Nefertum", "Ísis e Hórus", "Hathor", "Bes",
                                               "Sapo", "Porca", "Peixe", "Gato", "Falcão", "Babuíno", "Mosca", "Uraeus",
                                               "Udjat", "Menat", "Égide", "Coração")), aes(x=Forma, fill=Período))
  grafico8 + geom_bar(position = "dodge", color="black", alpha=0.8) +
    theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                           position = position_dodge(width = 1), size = 3.5) +
    theme(axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(),
          legend.background = element_blank(),
          legend.direction="vertical",
          legend.title = element_blank(),
          text = element_text(family = "sans")) +
    ggtitle("Distribuição de amuletos com formas em comum nos dois períodos") +
    theme(plot.title=element_text(face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=10))) +
    coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
    scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                                 "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                                 "#FF8484", "#47682C", "#9A031E", "#565554"))
  
  grafico9 <- ggplot(subset(tabela, Forma%in%c("Tartaruga", "Pato", "Lebre", "Crocodilo", "Tyt", "Pilar djed", "Nefer", "Lua crescente", "Rosto", "Perna", "Flor de lótus", "Coluna de papiro", "Cabeça")), (aes(x=Forma)))
  grafico9 + geom_bar(position = "dodge", color="black", alpha=0.3) +
    theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                           position = position_dodge(width = 1), size = 3.5) +
    theme(axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(),
          legend.background = element_blank(),
          legend.direction="vertical",
          legend.title = element_blank(),
          text = element_text(family = "sans")) +
    ggtitle("Formas de amuletos exclusivas do Reino Novo") +
    theme(plot.title=element_text(face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=10))) +
    coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
    scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                                 "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                                 "#FF8484", "#47682C", "#9A031E", "#565554"))
  
  grafico10 <- ggplot(subset(tabela, Forma%in%c("Pataikos", "Nekhbet", "Néftis", "Nehebkau", "Mut", 
                                                "Ísis", "Hórus criança", "Hórus", "Hetmehyt", 
                                                "Serpente", "Leão", "Íbis", "Golfinho", "Chacal",
                                                "Carneiro", "Pássaro-ba", "Cetro-was",
                                                "Sítula", "Mesa de oferenda", "Mão", "Lira")), (aes(x=Forma)))
  grafico10 + geom_bar(position = "dodge", color="black", alpha=0.3) +
    theme_bw() + geom_text(aes(label=..count..), stat="count", hjust = -0.5,
                           position = position_dodge(width = 1), size = 3.5) +
    theme(axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(),
          legend.background = element_blank(),
          legend.direction="vertical",
          legend.title = element_blank(),
          text = element_text(family = "sans")) +
    ggtitle("Formas de amuletos exclusivas do
Terceiro Período Intermediário") +
    theme(plot.title=element_text(face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=10))) +
    coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
    scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                                 "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                                 "#FF8484", "#47682C", "#9A031E", "#565554"))


#scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#3D348B", "#729B79", "#861657", "#D56AA0", "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86", "#FF8484", "#47682C", "#9A031E", "#565554"))
