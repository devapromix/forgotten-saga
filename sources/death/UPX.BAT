del *.~*
del /s *.dcu
del *.dsk
del *.dsm
del *.rsm
del *.ddp
del *.log
del map.txt

upx.exe *.exe
upx.exe *.dll

cd data
upx.exe *.dll