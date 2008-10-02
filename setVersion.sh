#!/bin/sh

NewVer=$1

sed -ie "s/^\(Version:\s*\)\S*$/\1$NewVer/" Graphalyze.cabal
sed -ie "s/^\(version = \"\).*\(\"\)/\1$NewVer\2/" Data/Graph/Analysis.hs

darcs record && darcs tag $NewVer && echo Version set to: $NewVer

