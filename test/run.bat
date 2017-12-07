@echo off
setlocal

cd /d %~dp0
cd bin
tester --all --format=plain --tzdata=..\..\tzdata --vectors=..\..\vectors
