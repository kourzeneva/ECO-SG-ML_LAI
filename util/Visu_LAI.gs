reinit
'open Visu.ctl'
'set poli off'
'set gxout grfill'
#'set mproj robinson'
#'set clevs 0 1 2 3'
#'set ccols 12 11 4 9'
# Also for other covers
#'set clevs 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14'
#'set ccols 1 11 4 8 14 6 9 2 5 3 10 7 12 14 13'
# For LAI
'set clevs 0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0 6.5 7.0'
'set ccols 0 1 14 9 6 2 8 12 7 10 3 13 5 11 4'
'd type'
'run /usr/share/grads/scripts/cbar.gs'
'gxprint Visu.eps eps white'
quit
