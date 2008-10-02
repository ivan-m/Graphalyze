#!/bin/sh

NewVer=$1

sed -i -e "s/^\(Version:\s*\)\S*$/\1$NewVer/" Graphalyze.cabal
sed -i -e "s/^\(version = \"\).*\(\"\)/\1$NewVer\2/" Data/Graph/Analysis.hs

darcs record && darcs tag $NewVer && echo Version set to: $NewVer

