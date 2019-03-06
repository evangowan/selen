#! /bin/bash

n=22

./icosahedron ${n}
./hex_grid ${n}

plot="hemisphere.ps"
pscoast -Rg -JA280/30/3.5i -Bg -Dc -A1000 -Ggrey -P -K  > ${plot}

psxy points.txt -J -R -K -O -P -Sc0.01 -Gred -Wred >> ${plot}
psxy hexagons.gmt -R -J -O -P -Wthinnest,blue -L >> ${plot}
