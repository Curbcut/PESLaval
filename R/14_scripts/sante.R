source("R/01_startup.R")


# Degré de solitude -------------------------------------------------------

sol <- readxl::read_xlsx("data/axe2/Données-Infocentre à partager_CISSSL.xlsx", 
                         sheet = "Degré de solitude (EQSP)", skip = 3)
sol <- sol[1:30, c("Genre", "Groupe d'âge", "Région sociosanitaire", "Degré moyen")]
sol$Genre[1:10] <- "Masculin"
sol$Genre[11:20] <- "Féminin"
sol$Genre[21:30] <- "Total"

sol$`Région sociosanitaire` <- gsub("13 Laval", "Ville de Laval", sol$`Région sociosanitaire`)

sol$`Région sociosanitaire` <- factor(sol$`Région sociosanitaire`, levels = c("Ville de Laval",
                                                                              "Ensemble du Québec"))

for (i in which(is.na(sol$`Groupe d'âge`))) {
  sol$`Groupe d'âge`[[i]] <- sol$`Groupe d'âge`[[i - 1]]
}

sol <- sol[sol$Genre %in% c("Masculin", "Féminin"), ]
sol$pretty <- convert_number(sol$`Degré moyen`)

degre_solitude_graph <- ggplot(sol, aes(x = `Groupe d'âge`, y = `Degré moyen`, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = pretty), position = position_dodge(width = 0.9),
            vjust = 2.5, color = "black", size = 3) +
  scale_fill_manual(values = c("Féminin" = "#CD718C", "Masculin" = "#A3B0D1")) +
  facet_wrap(~ `Région sociosanitaire`) +
  labs(
    title = NULL,
    x = NULL,
    y = "Degré moyen"
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe2/degre_solitude_graph.pdf"), 
                plot = degre_solitude_graph, width = 7.5, height = 4)


# Statut pondéral ---------------------------------------------------------


stat <- readxl::read_xlsx("data/axe2/Données-Infocentre à partager_CISSSL.xlsx", 
                         sheet = "Statut pondéral ", skip = 4)
stat <- stat[1:36, c("Genre", "Région sociosanitaire", "Statut pondéral", "Proportion brute (%)")]
stat$`Proportion brute (%)` <- stat$`Proportion brute (%)` / 100
stat$Genre[1:12] <- "Masculin"
stat$Genre[13:24] <- "Féminin"
stat$Genre[35:36] <- "Total"
stat$`Région sociosanitaire` <- rep(c(rep(c("Ville de Laval"), 4), rep(c(NA), 4), rep(c("Ensemble du Québec"), 4)), 3)
stat <- stat[!is.na(stat$`Région sociosanitaire`), ]

stat <- stat[stat$Genre %in% c("Masculin", "Féminin"), ]
stat$pretty <- convert_pct(stat$`Proportion brute (%)`)

# degre_solitude_graph <- 
  ggplot(stat, aes(x = `Statut pondéral`, y = `Proportion brute (%)`, fill = Genre)) +
  geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = pretty, vjust = ifelse(`Statut pondéral` == "Poids insuffisant", -0.5, 2.5)),
              position = position_dodge(width = 0.9), color = "black", size = 3) +
  scale_fill_manual(values = c("Féminin" = "#CD718C", "Masculin" = "#A3B0D1")) +
    scale_y_continuous(labels = convert_pct) +
  facet_wrap(~ `Région sociosanitaire`) +
  labs(
    title = NULL,
    x = NULL,
    y = "Statut pondéral"
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe2/degre_solitude_graph.pdf"), 
                plot = degre_solitude_graph, width = 7.5, height = 4)
