#!/bin/bash

gnuplot << EOD
set term postscript eps enhanced color solid
set output "test.eps"
set pm3d
set yrange [] reverse

splot "output.csv" matrix with dots
EOD
