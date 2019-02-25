set term post eps color enh solid
proffile="fort.30"
ingridfile="fort.31"
finalgridfile="fort.32"
sflgridfile="fort.33"
qfile="fort.34"
axisprofile="fort.35"
psiprofile="fort.36"
set output "plot.eps"


############### PLOT PROFILE ##################
set size 0.8,0.8
set xlabel "{/Symbol Y}"
set ylabel "Normalized Value"
set xrange [0:1]

set multiplot layout 2,2
set size 0.4,0.4
set origin 0,0
set title "H({/Symbol Y})"
plot proffile using 1:2 w l title "H"
set size 0.4,0.4
set origin 0,0.4
set title "F({/Symbol Y})^2"
plot proffile using 1:3 w l title "F^2"
set size 0.4,0.4
set origin 0.4,0
set title "{/Symbol W}^2({/Symbol Y})"
plot proffile using 1:4 w l title "{/Symbol W}^2"
set size 0.4,0.4
set origin 0.4,0.4
set title "T({/Symbol Y})"
plot proffile using 1:5 w l title "T"
unset multiplot
set multiplot layout 2,2
set size 0.4,0.4
set origin 0.0,0.4
set title "{/Symbol Q}({/Symbol Y})"
plot proffile using 1:6 w l title "{/Symbol Q}"
set size 0.4,0.4
set origin 0.4,0.4
set title "{/Symbol c}({/Symbol Y})"
plot proffile using 1:6 w l title "{/Symbol c}"
unset multiplot
################# GRIDS #######################
set size ratio -1
set title "Initial Grid"
set xlabel "X"
set ylabel "Y"
set xrange [-1.2:1.2]
plot ingridfile lw 1 lc rgb "black" notitle w l


set title "Final Grid"
plot finalgridfile lw 1 lc rgb "black" notitle w l

set title "Straight Field Line Coordinate"
plot sflgridfile lw 1 lc rgb "black" notitle w l

################ Q PROFILE ####################
set size ratio 0
set title "Q Profile"
set xlabel "{/Symbol Y}"
set ylabel "Q"
set xrange [0:1]
plot qfile w l title "Q"

############### PSI AND PRESURE CONTOUR #########
#set size ratio -1
#set xlabel "x"
#set ylabel "y"
#set xrange [-1:1]
#set title "{/Symbol Y}"
#set cntrparam cubicspline
#set cntrparam levels auto 10
#set palette rgbformulae 33,13,10
#unset clabel
#set contour surface
#set pm3d map
#splot psiprofile u 1:2:3 notitle lw 0.3 lc rgb "black"

#set title "{/Symbol r}"
#splot psiprofile u 1:2:4 notitle lw 0.3 lc rgb "black"

#set title "P_/_/"
#splot psiprofile u 1:2:5 notitle lw 0.3 lc rgb "black"

#set title "P_|"
#splot psiprofile u 1:2:6 notitle lw 0.3 lc rgb "black"

#set title "{/Symbol D}"
#splot psiprofile u 1:2:7 notitle lw 0.3 lc rgb "black"

#unset pm3d
#set size ratio 0

############## PROFILE ON AXIS PLANE ############
set clabel
set xlabel "x"
set multiplot layout 2,2
set size 0.4,0.4
set origin 0.0,0.4
set xrange [-1:1]
set title "Q"
set ylabel "Q"
plot axisprofile using 1:2 w l title "Q"
set title "dQ/ds"
set size 0.4,0.4
set origin 0.4,0.4
set title "dQ/ds"
plot axisprofile using 1:3 w l title "dQ/ds"
set title "Flux"
set ylabel "{/Symbol Y}"
set size 0.4,0.4
set origin 0.0,0.0
plot axisprofile using 1:4 w l title "{/Symbol Y}"
set title "{/Symbol r}"
set ylabel "{/Symbol r}"
set size 0.4,0.4
set origin 0.4,0.0
plot axisprofile using 1:5 w l title "{/Symbol r}"
unset multiplot
set multiplot layout 2,2
set title "P_/_/"
set size 0.4,0.4
set ylabel "P_/_/"
set origin 0.0,0.4
plot axisprofile using 1:6 w l title "P_/_/"
set title "P_|"
set size 0.4,0.4
set ylabel "P_|"
set origin 0.4,0.4
plot axisprofile using 1:7 w l title "P_|"
set title "{/Symbol D}"
set size 0.4,0.4
set ylabel "{/Symbol D}"
set origin 0.0,0.0
plot axisprofile using 1:8 w l title "{/Symbol D}"
set title "J_{/Symbol f}"
set size 0.4,0.4
set ylabel "J_{/Symbol f}"
set origin 0.4,0.0
plot axisprofile using 1:9 w l title "J_{/Symbol f}"
unset multiplot

