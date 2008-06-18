#!/bin/sh

infile=$1
outfile=$2

ncks -O -C -a -v XTIME $infile $outfile
ncrename -v XTIME,Time $outfile
ncwa -A -v ZNU,ZNW,HGT -a Time $infile $outfile

#ncks -A -a -v U $infile $outfile
#ncks -A -a -v V $infile $outfile
#ncks -A -a -v W $infile $outfile

#ncks -A -a -v PH,PHB,$infile $outfile
#ncks -A -a -v MU,MUB $infile $outfile
#ncks -A -a -v P,PB $infile $outfile

