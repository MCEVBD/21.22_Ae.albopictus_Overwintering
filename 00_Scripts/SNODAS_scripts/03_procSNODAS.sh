#! /bin/bash

# Stop of get any simple error
 set -e

# EA Burakowski 
# 2017-07-17

# Process NOHRSC SNODAS daily .dat files, convert to .nc following recommended procedures from NSIDC: https://nsidc.org/support/how/how-do-i-convert-snodas-binary-files-geotiff-or-netcdf


# (1) Create generic .hdr file:
## FOR MASKED
 #  ENVI
 #  samples = 6935
 #  lines = 3351
 #  bands = 1
 #  header offset = 0
 #  file type = ENVI Standard
 #  data type = 2
 #  interleave = bsq
 #  byte order = 1
## FOR UNMASKED
 #  ENVI
 #  samples = 8192
 #  lines = 4096
 #  bands = 1
 #  header offset = 0
 #  file type = ENVI Standard
 #  data type = 2
 #  interleave = bsq
 #  byte order = 1
# (2) gdal_translate: convert .dat (binary) to .nc
# (3) ncrename: change variable name from gdal band1 to appropriate variable name
# (4) ncatted: add long_name to variable
# (5) ncatted: add units to variable


# Assumes .dat files are stored in separate directories for each variable.  
# Sub-directory (SNODAS_DATA_DIR) names are 4-letter abbreviations. Feel free to change, however
# they should be consistent with untarSNODAS.sh
# Options include: 
	# PRLQ - liquid precip
	# PRSL - solid precip
	# SNWM - snow melt (from base of snowpack)
	# SNWT - snowpack integrated temperature
	# SNWZ - snow depth
	# SUBB - sublimation from blowing snow
	# SUBS - sublimation from snow pack
	# SWEM - snow water equivalent
	# convert .dat (binary) to .nc (netCDF)

##############################################################################
#
#	Main Code
#
##############################################################################

 # Make sure the SNODAS_DATA_DIR environment variable is set

 if [ -z "SNODAS/Data" ]; then
    echo "Need to set SNODAS_DATA_DIR"
    exit 1
 fi

 # Flag if using masked or unmasked SNODAS extent
 masked=true 

 # Flag to subset netcdf files (removes original one!)   
 subset=false 

 # Define sub Region Of Interest (ROI)
 # onlt used if subsetiting
 # CRHO - SnowCast

 lon_min=-98
 lon_max=-82
 lat_min=38
 lat_max=50

 # Define path to generic.hdr
 genhdr=/Users/kmilsong/Documents/CBS_PhD/Ae.albo_OW_2021/21.22_Ae.albopictus_Overwintering/00_Scripts/SNODAS_scripts/generic.hdr

 # Define out directory for .nc files
 odir=/SNODAS/snowdepth
 mkdir  $odir 
 
 # Define directory with variable subdirs of dat files
 cd /SNODAS/Data

 # Loop over .dat subdirs. 
 # Copy/pasta 'elif' block below and edit accordingly (e.g., units, long_name)
 # to include additional .dat subdirs.  Mind the character spacing on filenames! 
 # Filenames are not consistent across variables. 
 # PRLQ PRSL SWEM SNWZ
 for dirs in SNWZ
 do
	echo "Now in working in the $dirs directory"
 	FILES=${dirs}/*.dat

	# loop over fils in subdir
	for infiles in ${FILES}
	do
		echo "Working on $infiles"
		varn=`echo ${infiles} | cut -c 14-17`
		echo "varn = $varn"
		if [ $varn -eq 1036 ];

	
		# Snow depth
		then
			Hdrfile=`echo $infiles | cut -c 6-47`
			echo "Now working on : $Hdrfile"
			var=SNWZ
			echo "And this variable: $var"
			date=`echo $infiles | cut -c 33-40`
			echo "For this day: $date"

			# (1) copy generic ENVI header file to filename
			# place in working directory for gdal_translate

			echo "genhdr = $genhdr dirs/hdrfile = $dirs/$Hdrfile.hdr"
			cp $genhdr $dirs/$Hdrfile.hdr
			echo "created generic .hdr: $hdr"

			### ADDED -r ABOVE
			# (2) Generate command to convert binary dat (infile) to netCDF (outfile)

			ofile=`echo ${var}_snodas_${date}.nc`
			echo "Will use this for the .nc filename: $ofile"
			echo $infiles			
			echo $odir/$ofile

	                # Check if masked or not
                        if [ "$masked" = true ] ; then
                           gdal_translate -of NetCDF -a_srs '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' -a_nodata -9999 -a_ullr -124.73333333333333 52.87500000000000 -66.94166666666667 24.95000000000000 $infiles $odir/$ofile
                        else
                            gdal_translate -of NetCDF -a_srs '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' -a_nodata -9999 -a_ullr -130.51666666666667 58.23333333333333 -62.25000000000000 24.10000000000000 $infiles $odir/$ofile
                        fi

	
			# (3) Change name of variable in .nc file (defaults to band1)
			ncrename -v Band1,$var $odir/$ofile

			# (4) Change the long_name in .nc file (unfortunately doesn't look like spaces are allowed
			ncatted -O -a long_name,$var,o,c,"Snow_Depth" $odir/$ofile

			# (5) Add units to new variable
			ncatted -a units,${var},c,c,"mm" $odir/$ofile

		else
			echo "None of your desired variables were found; check sub-directory abbreviations"
		fi

                # Final option to subset netcdf file and remove original
                if [ $subset = true ]; then
                    echo "Subsetting netcdf file to user defined region"
                    sub_ofile="${ofile%.*}"
                    ncea -O -d lat,$lat_min,$lat_max -d lon,$lon_min,$lon_max $odir/$ofile $odir/$sub_ofile"_sub.nc"
                    # Remove full extent file
                    echo "Removing full extent netcdf file"
                    rm -f $odir/$ofile   
                fi

	done  # loop over files in subdir	

 done	# loop over subdirs
