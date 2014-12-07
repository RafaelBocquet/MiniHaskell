MiniHaskell - Partie 1
======================

Projet de Langages de programmation et compilation 2014-2015
Rafaël Bocquet

Mon compilateur est actuellement capable de compiler correctement tous les tests de ``tests/exec``, mais je ne commente dans ce rapport que la partie correspondant à la partie 1 du projet (Il y a plus de travail pour commenter la partie 2). 


Compilation
-----------
Le projet est écrit en Haskell et utilise Cabal pour la compilation.
Si les dépendances sont accessibles (après ``cabal sandbox init && cabal install --only-dependencies``), ``make`` permet de compiler le projet et crée un exécutable ``petitghc``.


Choix
-----
Les analyseurs lexical et grammatical pour PetitHaskell sont dans ``src/Syntax/Small/``. Ils n'utilisent pas d'outils générateurs, car la grammaire est suffisamment simple pour que cela soit faisable et que cela permet de gérer un peu plus simplement les erreurs. Leur correction est cependant plus difficile à vérifier, et il est très difficile d'agrandir le langage qu'ils reconnaissent, mais ils sont capables de lire correctement tous les fichiers de test.

Des analyseurs utilisant les outils ``alex`` et ``happy`` pour un sous ensemble plus grand de Haskell sont présents dans ``src/Syntax/Full/``.


Difficultés / Problèmes
-----------------------
Les filtrages imbriqués sur les types ```Either TrucError a``` dans ``src/Main.hs`` ne sont pas très élégants, et la gestion des annotations de position lorsque la construction est du sucre syntaxique est un peu lourde. Sinon, je n'ai pas rencontré de problème particulier au niveau des analyseurs lexical et grammatical.
