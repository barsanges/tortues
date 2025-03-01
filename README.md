# Le lièvre et les tortues

Un solveur pour le jeu du lièvre et des tortues.

`tortues` produit des résultats sur JSON, par défaut sur la sortie
standard. Ces résultats peuvent par exemple être manipulés avec
`jq`. Par exemple :
```
$ tortues | jq "map( select( .score > 19 and .score < 40 ) )"
```
sélectionne les puzzles qui se terminent en plus de 19 coups et en
moins de 40, ou :
```
$ tortues | jq "map( .score )"
```
permet d'avoir la distribution des scores des puzzles.
