Ce dépôt github contient tous les éléments pour reproduire intégralement le document accessible à <https://xtimbeau.github.io/travail>.

L'organisation est assez standard. Le fichier `qmd` principal est `index.qmd`, les codes produisant les données sont dans `R`. Les fichiers `R` sont exécutés par `ofce::source_data()` qui exécute en cachant les résultats d'exécution. Pour rafraîchir le cache il faut exécuter `source_data_refresh()` avant le rendu de `index.qmd`.

Le répertoire `data` contient quelques fichiers téléchargés (par les scripts), `divers` contient des éléments annexes, `ecopol` le code source de l'article d'Economie Politique.

La version de `R` employée est la 4.5.1 et la version de `quarto` doit être supérieure ou égale à 1.8.25.

Pour fonctionner, en plus des packages signalés par `renv::dependencies()`, les packages suivants doivent être installés depuis `devtools::install_github()` : "`ofce/ofce`", "`rstudio/gt`", "`nsgrantham/ggbraid`", "`expersso/OECD`", "`InseeFrLab/melodi`". Une alternative est d'utiliser `renv` qui est configuré sur le dépôt.

*Happy reproduce* !
