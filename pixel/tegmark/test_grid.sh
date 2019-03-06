#! /bin/bash

# controls size of the hexagon shapes
resolution=6

./icosahedron ${resolution}
./hex_grid ${resolution}

plot="hemisphere.ps"
pscoast -Rg -JA280/30/3.5i -Bg -Dc -A1000 -Ggrey -P -K  > ${plot}

psxy points.txt -J -R -K -O -P -Sc0.01 -Gred -Wred >> ${plot}
psxy hexagons.gmt -R -J -O -K -P -Wthinnest,blue -L >> ${plot}
psxy points.txt -R -J -O -P -SE-   >> ${plot}
