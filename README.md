# Projet Ville de Laval

## Structure des dossiers

- **R/** : Contient tous les scripts R divisés par axe thématique, ainsi que les fichiers RMarkdown pour l'écriture de chacune des axes.
- **data/** : Dossiers `axe1`, `axe2`, `axe3` contenant les données spécifiques à chaque axe.
- **output/** : Dossiers `axe1`, `axe2`, `axe3` pour les graphiques et autres résultats visuels générés.

## Utilisation de Bookdown

Ce projet utilise `bookdown` pour générer un document structuré qui facilite la navigation et l'intégration des résultats d'analyses.

L'utilisation de `bookdown` pour ce projet présente plusieurs avantages surtout en termes de gestion gestion automatique de la numérotation des figures, des tableaux et des sections, simplifiant les révisions et les ajouts de contenu.

### Instructions pour générer le livre

Pour générer le livre à partir des fichiers sources, utilisez la commande suivante dans R :

```r
bookdown::render_book("R/index.Rmd", bookdown::gitbook())
```

Pour garantir la stabilité et la qualité du projet, il est essentiel que le `bookdown` soit capable de se compiler sans erreur à chaque mise à jour (push) sur GitHub.

### Ressources supplémentaire

Pour ceux moins familiers à bookdown, consultez le tutoriel officiel de bookdown (https://bookdown.org/yihui/bookdown/get-started.html) qui offre des instructions détaillées sur l'utilisation de bookdown.