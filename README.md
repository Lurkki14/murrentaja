## About

This program applies dialectal features to standard Finnish text.

Currently available features are listed in the help text (`./Main -h`).


## Compiling


For compiling you need the `text` and `optparse-applicative` Haskell packages.

You can also use `nix-shell shell.nix` to enter the needed compilation environment.

Then compiling and running is `cd src; ghc Main.hs && ./Main`

## Usage

Interactive mode and reading from a file are currently available. More information in the help text.

## Examples

```
$ ./Main -i -F SavoReduction,SpecialGemination,Epenthesis
poika ajaa salmessa veneellä
-> poeka ajjaa salammeen venneellä
```

```
$ ./Main -i -F WesternReduction,CommonGemination,PohjanmaaEpenthesis
Enter a line of text:
työmies ajaa kotiin kahville
-> tyämiäs ajjaa kottiin kahaville
```
