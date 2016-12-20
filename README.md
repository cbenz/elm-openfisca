# Elm-OpenFisca playground

This is highly experimental!

Demo: https://cbenz.github.io/elm-openfisca/

## Getting started

You need to have [Elm](http://elm-lang.org/) 0.18 installed on your machine.

Compile this project with:

    elm make src/Main.elm

Then view it:

    elm reactor

## Rationale

Sorry, this is in French!

Voici quelques éléments en vrac :

- Tout ceci est hautement expérimental et doit être considéré comme de la R&D
- Je m'entraîne à créer une implémentation minimale d'OpenFisca en partant d'exemples rencontrés dans la législation ou bien fictifs.
- La raison est d'explorer une piste alternative pour accroître la facilité de contribution au projet OpenFisca, autrement qu'en refactorant le code Python d'OpenFisca-Core
- J'essaie de designer l'ensemble avec une API la plus user-friendly possible et en me ramenant au maximum à la manière standard de développer (factorisation de code par des fonctions, utilisation de structures de données, etc)
- J'entends supporter les formules, les paramètres, les périodes et les entités dans un premier temps.
- Les paramètres sont embarqués dans le code sous forme de data, mais pas dans des fichiers à part.
    Je pense que c'est mieux ainsi car :
    - on profite du moteur de typage s'affranchissant donc d'un parser/validateur
    - on référence des paramètres non pas par nom symbolique (x.y.z) mais par nom de variable "normal"
    - on voit le paramètre en contexte par rapport à la formule
- Pour l'instant les formules sont du code exécuté en Elm (du coup en JS en fait), permettant d'utiliser n'importe quelle fonction du langage Elm.
Elles ne retournent pas de data (par ex. un AST d'opérations arithmétiques) ce qui a pour inconvénients :
    - la compilation vers le vectoriel est impossible
    - l'introspection est impossible car Elm n'est pas introspectable
- Une fois cette base bien assise, on pourra réfléchir aux performances (calcul vectoriel) et à l'interaction avec des environnements tiers tels que Python/NumPy.
- Je ne suis pas encore sûr de voir l'intérêt d'un DSL externe.