#! /bin/bash

# controls size of the hexagon shapes
resolution=60

./icosahedron ${resolution}
./hex_grid ${resolution}

cp -f hexagons.gmt hexagons_${resolution}.gmt
cp -f points.txt points_${resolution}.txt

exit 0
plot="hemisphere.ps"
pscoast -Rg -JA280/30/3.5i -Bg -Dc -A1000 -Ggrey -P -K  > ${plot}

psxy points.txt -J -R -K -O -P -Sc0.01 -Gred -Wred >> ${plot}
psxy hexagons.gmt -R -J -O -K -P -Wthinnest,blue -L >> ${plot}
psxy points.txt -R -J -O -P -SE- -Wthinnest,green  >> ${plot}


