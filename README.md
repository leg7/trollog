# Installation

Il faut installer la dernière version de GHC pour compiler le programme.

Vous pouvez l'installer avec GHCup
https://www.haskell.org/ghcup/

Ensuite il faut compiler le programme avec `./build.sh`

Une fois compilé vous pouvez le lancer en mode interactif `./Main`
Ou lui passer un fichier par redirection du stdin `./Main < monFichier`

# Langage

Le langage est 0+.

Vous pouvez déclarer des atomes.

Toute instruction se termine par un .

Vous pouvez écrire une instruction sur plusieurs lignes.

```
a.
b.
```

Vous pouvez écrire des règles

```
a -> b.
```

Négation

```
\a.
\a -> b.
b -> \c.
```

Vous ne pouvez que utiliser le ET logique pour faire des règles:

```
a, b -> c, d.
c -> \e, f.
```

`<conjonction> -> <conjonction>`

## Typage et Cohérence

Le langage est fortement typé.
Pour typer un prédicat :

```
!pokemon(String, Int, feu | eau | terre | elec | psy).
```

Les prédicats de nom/type pokemon devront être cohérents avec leur définition de type
Les types énumérés sont définis comme `val1 | val2 | val3 ...`

Cohérent `pokemon("pikachu", 50, elec).`
Incohérent `pokemon(50, "raichu").`

La négation et les types énumérés permettent de maintenir la cohérence de la base.

```
!vitesse(rapide | lent)
vitesse(lent) -> \vitesse(rapide)
vitesse(rapide) -> \vitesse(lent)
```

Donnez des alias à vos prédicats !

```
pika = pokemon("pikachu", 50, elec).
mew = pokemon("mew", 30, psy).
```

Un type peut prendre d'autres types :

```
!combat(pokemon, pokemon).
combat(pika, mew) -> combat(mew, pika).
```

Les règles ne doivent pas forcément respecter un type.

```
combat(pika, mew) -> foobar("aosntuhoa").
```

Le type foobar n'est pas défini, rien ne sera déduit par chaînage si son type n'est pas défini

Incorrect (Rien ne peut être déduit car foobar incohérent (pas typé))
```
combat(pika, mew) -> foobar("aosntuhoa").
:forward.
```

Correct
```
combat(pika, mew) -> foobar("aosntuhoa").
!foobar(String).
:forward.
:facts.
```

# Chaînage arrière

Pour poser une question `?<conjonction>.` `?a, b, \c.`

# Commandes

Vous pouvez utiliser des commandes :
    - `:facts.` affiche la base de faits.
    - `:rules.` affiche les règles.
    - `:types.` affiche les types
    - `:forward` effectue un chaînage avant
    - `?<conjonction>.` effectue un chaînage arrière
    - `:strategyFirst` change la stratégie de résolution de conflit
    - `:strategyRecent` idem
    - `:quit` pour quitter le programme
