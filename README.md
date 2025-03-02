# Le lièvre et les tortues

Un solveur pour le jeu du lièvre et des tortues. Le temps d'exécution
est de l'ordre de quelques minutes.

`tortues` produit des résultats en JSON, par défaut sur la sortie
standard. Ces résultats peuvent par exemple être manipulés avec
`jq` ; ainsi :
* `tortues | jq 'map( .score )'` permet d'avoir la distribution des
  scores des puzzles ;
* `tortues | jq 'map( select( .score > 19 and .score < 40 ) )'`
  sélectionne les puzzles qui se terminent en plus de 19 coups et en
  moins de 40 ;
* `tortues | jq 'map( select( .figures | has( "hare" ) ) )'` pour ne
  retenir que les puzzles qui font appel au lièvre ;
* `tortues | jq 'map( select( (.figures | has( "hare" ) | not ) and
  .score > 39 ) )'` sélectionne les puzzles qui n'utilisent pas le
  lièvre et se terminent en 40 coups ou plus ;
* `tortues | jq 'map( select( (.figures | has( "hare" ) | not ) and
  .score > 39 ) ) | sort_by( .score )'` sélectionne les mêmes et les
  trie par nombre de coups.

`to_latex` peut être utilisé pour passer les puzzles d'un format JSON
à une représentation TikZ pouvant être insérée dans un document LaTeX.
