#!/bin/bash
set -e

mkdir -p obj
rm -f ./bin/*.gb

echo "#autocreated Linkfile" >> obj/$1.link
echo "#" >> obj/$1.link
echo "#" >> obj/$1.link
echo "[Objects]" >> obj/$1.link
echo "./obj/$1.obj" >> obj/$1.link
echo "#" >> obj/$1.link
echo "[Output]" >> obj/$1.link
echo "./obj/$1.gb" >> obj/$1.link

echo "assembling..."
./rgbasm -iinc/ -oobj/$1.obj src/$1.asm

echo "linking..."
./rgblink -o obj/$1.gb obj/$1.obj

echo "fixing..."
./rgbfix -v obj/$1.gb

rm -f ./obj/*.obj
