# This AWK script is a part of TASS Reweighting Codes
# AWK script to read plumed input file and write fortran readable format
# --------------------------------------------------# 
#
# Authour: Anji Babu Kapakayala
#          C/O Prof. Nisanth N. Nair
#          Dept. Of Chemistry, IIT Kanpur, India.
#
# --------------------------------------------------# 
#
#!/bin/bash
#To print full input
#awk '/METAD/ {print $0} ' plumed.dat 
#sed -i "/^#/d" plumed.dat 
awk -F" " '{
       for(i=1;i<=NF;i++) {
{ split($i, a, "=")
  if(a[1] == "BIASFACTOR" || a[1] == "PACE" ||a[1] == "STRIDE" ) {
# 		print a[1] " = "  a[2]
		print a[1] " = "  a[2] >> "admin_input.inp"
} else if (a[1] == "METAD"){
print a[1] " = "  "ON" >> "admin_input.inp"
} else if (a[2] == "COLVAR") {
print "CVFILE" " = "  a[2] >> "admin_input.inp"
} else if (a[2] == "HILLS") {
print "HILLSFILE" " = "  a[2] >> "admin_input.inp"
} else if (a[1] == "TEMP") {
print "CV_TEMP" " = "  a[2] >> "admin_input.inp"
}

}
#-----------------------------------------#
#     switch (a[1]) {
#	 case "BIASFACTOR":
#	    print  a[1] " = "  a[2]
#	 case "TEMP":
#	    print  a[1] " = "  a[2]
#	 default:
#
#}
#-----------------------------------------#
        }
}' plumed.dat
#-------------

#ref: https://www.linuxcommands.site/linux-text-processing-commands/linux-awk-command/awk-loop-for-while/

