
setwd("C:\\Users\\victo\\OneDrive\\Documents\\Vic\\arrete c'est ici l'empire de la mort\\Mestrado\\Tabelas")

getwd()

install.packages("ggplot2")

library("ggplot2")

tabela <- read.csv2("TPI.csv")


grafico <- ggplot(data=tabela, aes(x=Forma, fill=Per�odo))
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
  ggtitle("Distribui��o da tipologia de formas de 
amuletos em cada per�odo") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))


grafico2 <- ggplot(subset(tabela, Forma%in%c("Hathor", "Thoth", "�sis e H�rus", "Nefertum", 
                                             "Sekhmet", "Taweret", "Bes", "H�rus", "�sis",
                                             "Nehebkau", "H�rus crian�a", "Pataikos", "N�ftis",
                                             "Mut", "Hetmehyt", "Nekhbet", "Divindade")), aes(x=Forma, fill=Per�odo))
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
  ggtitle("Distribui��o de amuletos em forma de divindades 
em cada per�odo") +
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
  ggtitle("Distribui��o de formas de amuletos por
material no Terceiro Per�odo Intermedi�rio") +
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
  ggtitle("Distribui��o de formas de amuletos por sexo
e idade no Terceiro Per�odo Intermedi�rio") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size = 10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))



  grafico4 <- ggplot(subset(tabela, Posi��o %in% c("Cabe�a", "M�os", "Peito", "P�s", "Pesco�o", "Tornozelos")), aes(x=Posi��o, y=Forma, color=Tipo))
grafico4 + geom_count() +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_blank(),
        legend.direction="vertical",
        legend.title = element_blank(),
        text = element_text(family = "sans")) +
  ggtitle("Distribui��o de formas por tipologia 
          e posi��o em Matmar") + 
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
  ggtitle("Distribui��o de formas de divindades por sexo e idade em Lahun") + 
  theme(plot.title=element_text(face="bold")) + coord_flip()

scale_fill_manual(values=c("#D49297", "#8EAEBD", "#AFD5AA", "#A491D3", "#818AA3", "#FFBF00", "#34113F", "#0B6E4F", "#FA9F42", "#18FF6D", "#FD151B", "#FD151B", "#01295F", "#FFB30F", "#A50104", "#E365C1", "#2274A5")) +

 
  grafico5 <- ggplot(subset(tabela, Forma%in%c("Sapo", "Crocodilo", "Tartaruga", "Lebre", "Porca", "Falc�o", "Pato",
                                               "Cosca", "Babu�no", "Peixe", "Gato", "Chacal", "�bis", "Serpente", 
                                               "Carneiro", "Le�o", "Golfinho", "Mosca")), aes(x=Forma, fill=Per�odo))
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
  ggtitle("Distribui��o de amuletos em forma de animais 
em cada per�odo") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))

grafico6 <- ggplot(subset(tabela, Forma%in%c("Nefer", "Udjat", "Pilar djed", "Lua crescente",
                                             "Menat", "�gide", "Tyt", "Uraeus", "P�ssaro-ba", 
                                             "Cetro-was")), aes(x=Forma, fill=Per�odo))
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
  ggtitle("Distribui��o de amuletos em forma de s�mbolos 
divinos em cada per�odo") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))

grafico7 <- ggplot(subset(tabela, Forma%in%c("Coluna de papiro", "Flor de l�tus", "Perna", "Cabe�a",
                                             "Rosto", "Cora��o", "M�o", "N�o identificada",
                                             "Mesa de oferenda", "S�tula", "Lira")), aes(x=Forma, fill=Per�odo))
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
  ggtitle("Distribui��o de amuletos em formas naturais, partes 
do corpo e outros em cada per�odo") +
  theme(plot.title=element_text(face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
  scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                               "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                               "#FF8484", "#47682C", "#9A031E", "#565554"))
  
  grafico8 <- ggplot(subset(tabela, Forma%in%c("Thoth", "Taweret", "Sekhmet", "Nefertum", "�sis e H�rus", "Hathor", "Bes",
                                               "Sapo", "Porca", "Peixe", "Gato", "Falc�o", "Babu�no", "Mosca", "Uraeus",
                                               "Udjat", "Menat", "�gide", "Cora��o")), aes(x=Forma, fill=Per�odo))
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
    ggtitle("Distribui��o de amuletos com formas em comum nos dois per�odos") +
    theme(plot.title=element_text(face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=10))) +
    coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
    scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                                 "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                                 "#FF8484", "#47682C", "#9A031E", "#565554"))
  
  grafico9 <- ggplot(subset(tabela, Forma%in%c("Tartaruga", "Pato", "Lebre", "Crocodilo", "Tyt", "Pilar djed", "Nefer", "Lua crescente", "Rosto", "Perna", "Flor de l�tus", "Coluna de papiro", "Cabe�a")), (aes(x=Forma)))
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
  
  grafico10 <- ggplot(subset(tabela, Forma%in%c("Pataikos", "Nekhbet", "N�ftis", "Nehebkau", "Mut", 
                                                "�sis", "H�rus crian�a", "H�rus", "Hetmehyt", 
                                                "Serpente", "Le�o", "�bis", "Golfinho", "Chacal",
                                                "Carneiro", "P�ssaro-ba", "Cetro-was",
                                                "S�tula", "Mesa de oferenda", "M�o", "Lira")), (aes(x=Forma)))
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
Terceiro Per�odo Intermedi�rio") +
    theme(plot.title=element_text(face="bold")) +
    guides(colour = guide_legend(override.aes = list(size=10))) +
    coord_cartesian(ylim=c(0, 5, 10, 15, 20)) + coord_flip() +
    scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#38D685", "#E5F169", "#F47993", "#ED7D34", 
                                 "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86",
                                 "#FF8484", "#47682C", "#9A031E", "#565554"))


#scale_fill_manual(values = c("#0E6BA8", "#EFE9F4", "#3D348B", "#729B79", "#861657", "#D56AA0", "#919091", "#F4AC45", "#9055A2", "#F40000", "#63D2FF", "#AC7B7D", "#EDFF86", "#FF8484", "#47682C", "#9A031E", "#565554"))
