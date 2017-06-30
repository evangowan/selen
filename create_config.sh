#! /bin/bash

# this script must be run prior to running all the other scripts

# copy the icesheet output file 
icesheet_out_file=/scratch/users/egowan-local/icesheet/github/global/selen_input/icesheet_Evan_ehgk_2_I_I

# purge working directory? y/n
purge="y"

####################################
# Sea level calculatio parameters
####################################

# number of interations for the solution of the sea level equation. From my experience 5 is the best for production runs, but 3 might be ok for topography.
iterations=3

# Calculation modes, 1 is likely the best
#Modes:  1= Gravitationally self-consistent (GSC)
#        2= Elastic GSC / 3="Eustatic" / 4="Woodward" / 5="No Ice" 

SL_mode=1

####################################
# Spatial resolution and reference frame
####################################

# maximum harmonic degrees

max_degree=256

# Reference frame

include_degree_one='y' # include the degree 1 love number y/n
frame_center="CM" # Reference frame center is center of mass "CM" or center of Earth "CE"

# "Tegmark resolution": basically "pixiation of a sphere", to discretize a sphere in an equal area sense

tegmark_resolution=44 # 44 is lots for degrees 128 and 256, might need to increase to about 48 if you go to 512 to meet the requirements of equation 34 in the SELEN manual

new_pixel_file="y"
pixel_file_name="px-table-r${tegmark_resolution}.dat"

####################################
# Earth model parameters
####################################

# Earth model files should be located in the folder "VSC"

number_mantle_layers=2 
# have to dig into SELEN more to see how this can be changed
code_value=0 # zero allows variable lithosphere thickness, PREM averaged values
code_value=1 # zero allows variable lithosphere thickness, PREM averaged values, adjustment to PREM actual values at 670 km discontinuity
# code values above this have fixed properties, probably not of interest


lithosphere_thickness=120 # km
lower_mantle_viscosity=10 # x 10^21
upper_mantle_viscosity=0.6331 # x 10^21

earth_model_filename="earth.dat"

cat << END_CAT > VSC/${earth_model_filename}
'${lithosphere_thickness}'
'${lower_mantle_viscosity}'
'${upper_mantle_viscosity}'
END_CAT

calculate_SH=y

####################################
# Ice model parameters
####################################

# ice models should be located in the folder "ICE-MODELS"

ice_file="icesheet"
cp -f ${icesheet_out_file} ICE-MODELS/${ice_file}

prepare_ice_SH_file="${calculate_SH}" # y/n - using an existing spherical harmonics file will decrease computation time significantly
ice_SH_file="${ice_file}-l${max_degree}.dat" 

ice_model_time_step="5.0" # in kyr, SELEN can only do constant steps in the ice model


####################################
# Compute spherical harmonics on pixel grid
####################################

prepare_pixel_SH_file="${calculate_SH}" # y/n - using an existing spherical harmonics file will decrease computation time significantly
pixel_SH_file="sh-r${tegmark_resolution}-l${max_degree}.bin" 


####################################
# Compute spherical harmonics decomposition of ocean
####################################

# don't need to calculate ocean here, can use different earth models in later scripts.
prepare_ocean_SH_file="n" # y/n - using an existing spherical harmonics file will decrease computation time significantly
ocean_SH_file="of-l${max_degree}.dat" 


####################################
# Label for run
####################################

# files are stored in a folder depot-xxxx where xxxx is a four character string given below

depot_name="TEST"

# so we don't get a massive file
rm DEPOTS/depot-${depot_name}/rsl/rsl-contours/rsl_spreadsheet.dat

####################################
# output options
####################################

# right now most things are turned off, except for the sea level grids

# files should be located in the "DATA" folder

Regional_RSL_contour="n" # y/n
Regional_RSL_contour_file="rsl-region.dat"


cat << END_CAT > DATA/${Regional_RSL_contour_file}
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     -- Configuration file for regional RSL analysis --    
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Please be careful with lon-lat bounds and increments 
since no systematic check is made by SELEN - Two-digits 
codes on left should not be modified - Negative values
of longitude will be converted within the range [0:360]
GS Aug 7 2008 

-------------------------------------------------------------
10 Name of region:                'North America RSL'
-------------------------------------------------------------
20 Time of analysis (BP, units of 1 ka):   '20.'
30 Range in longitude (deg)                      '-170' '-5'  
40 Range in latitude (deg)                        '35' '85'  
50 Lon-lat increment  (deg)                      '0.5' 
60 Bounds & contour interval (m):       '-1.' '1.' '0.25'  
-------------------------------------------------------------  
EOF
END_CAT

####################################
# create config file for SELEN
####################################

cat << END_CAT > config.dat

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  This is file "config.dat" for SELEN 2.9 - 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The user can configure SELEN by the switches below. Any option is to be written within primes 
(e. g., 'option'). A three-digits, left aligned numerical code is provided for each entry (e. 
g., 001). 

In section 1 (settings) the user supplies the spatial resolution, the ice sheets distribution, 
and the Earth model viscoelastic structure. This allows one to solve the Sea Level Equation but 
no graphical output is obtained. 

In section 2 (outputs), a number of optional outputs can be scheduled, including tables and plots 
of numerical results. The required GMT scripts are automatically generated according to the 
options chosen. 

For help, comments, or suggestion, you  can contact the authors at the addresses below or consult 
the SELEN web page at http://geodynamics.org/cig/software/selen

Contact: Giorgio Spada <giorgio DOT spada AT gmail DOT com> 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 This is SECTION (1) of "config.dat": SELEN settings
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

====> PURGING option ----------------------------------------------------------
110  Purging the wdir before & after execution                      '${purge}'
     [see config.f90 for purged filenames extensions ]

====> SOLUTION of the SLE -----------------------------------------------------
130    Iterations & mode of solution                              '${iterations}'    '${SL_mode}' 	    			 	
Modes:  1= Gravitationally self-consistent (GSC)
        2= Elastic GSC / 3="Eustatic" / 4="Woodward" / 5="No Ice" 

====> MAXIMUM HARMONIC DEGREE -------------------------------------------------
140    LMAX     				                       '${max_degree}'  

====> REFERENCE FRAME ---------------------------------------------------------
145    Includes degree 1 Love numbers (CM/CE frames)             '${include_degree_one}'  '${frame_center}'   

====> TEGMARK RESOLUTION ------------------------------------------------------
150    R                  				             '${tegmark_resolution}'
151    Prepare a new pixel table (y/n, filename)         '${new_pixel_file}' '${pixel_file_name}'

====> RHEOLOGICAL MODEL -------------------------------------------------------
160    Rheological profile info:                         '${number_mantle_layers}' '${code_value}' '${earth_model_filename}'  

====> ICE MODEL ---------------------------------------------------------------
170    Ice file name                                               '${ice_file}'  
171    Prepare a new SH ice file (y/n, filename)       '${prepare_ice_SH_file}'  '${ice_SH_file}'
172    Ice history time step (kyrs)                       '${ice_model_time_step}' 

====> SPHERICAL HARMONICS (SH) FILE AT PIXELS ---------------------------------
180    A new SH file (y/n, filename)                    '${prepare_pixel_SH_file}'  '${pixel_SH_file}'  

====> OCEAN FUNCTION (OF) -----------------------------------------------------
190    A new OF SH decomposition (y/n, filename)         '${prepare_ocean_SH_file}'  '${ocean_SH_file}'

====> REPOSITORY LABEL --------------------------------------------------------
195    The depot name  (four characters)                 '${depot_name}' 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 This is SECTION (2) of "config.dat: SELEN outputs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

====> EXECUTION of the GMT SCRIPTS -------------------------------------------- 
200    Execution of GMT scripts during the SELEN run (y/n)     'n'

====> PIXELIZATION & WINDOW ---------------------------------------------------
205    Pixelization maps (y/n)                                  'n'
206    Window function evaluation & plot (y/n)                     'n'

====> OCEAN FUNCTION (OF) -----------------------------------------------------
210    Present-day OF map & reconstruction (y/n)                'n'
215    Plot of OF degree variance (y/n)                            'n'

====> ICE MODEL ---------------------------------------------------------------
220    Maps of original ice sheets  (y/n)                	      'n' 
221    Plot of Equivalent Sea Level (ESL)  (y/n)       		     'n' 
222    Reconstruction & mapping of the ice sheets (y/n)  	    'n'

====> EARTH MODEL SPECTRAL PROPERTIES -----------------------------------------
230    Plot LDCs, relaxation spectrum & residues for normal modes  (y/n)   'n'      

====> RSL PREDICTIONS AT SPECIFIC SITES ---------------------------------------
240    RSL analysis, database & format                'n'  'sealevel.dat'   '0'
241    Plot of RSL sites distribution                         'n' 
242    Site-by-site RSL predictions vs data & plots          'n'  'n'
243    Scatterplot of RSL data & predictions       	    'n' 
244    Misfit between RSL data & predictions               'n'
245    Table with all RSL data & predictions              'n'

====> RSL REGIONS -------------------------------------------------------------
250    Gobal RSL zones  	                           'n'
251    Regional RSL contour lines                           '${Regional_RSL_contour}'   'rsl-region.dat'

====> SEA LEVEL CHANGE AT TIDE-GAUGE STATIONS --------------------------------- 
260    Tide-gauge (TG) analysis & database                    'n' 'rlr-trends.txt'      
261    Plot of TG stations distribution                      'n'
262    TG data scatterplot   	                            'n' 
263    Table of S, N, and U-dot predictions at TG sites    'n'  

====> GLOBAL PRESENT-DAY RATES ------------------------------------------------
270    Global maps of dot S, U & N               	  'n' 

====> 3D VELOCITY -------------------------------------------------------------
275    -Up, North, East, S, and N rates for sites in file       'n' 'NA_KK.txt'

====> REGIONAL PRESENT-DAY RATES ---------------------------------------------- 
280    Regional maps of dot S, U, & N 	           'n'	     
281      -1 Italy                                   'n'
282      -2 Mediterranean     		             'n'
283      -3 Europe     		                      'n'
284      -4 Fennoscandia   		               'n'
285      -5 Greenland                                   'n'
286      -6 North America  			         'n'
287      -7 Antarctica   		                  'n'

====> STOKES COEFFICIENTS (SC) ------------------------------------------------
290    Rate of change of SC & range of degrees for plot      'n'  '2'  '20'
END_CAT


make run


mv -f run/${ocean_SH_file} INPUTS/
mv -f run/${pixel_SH_file} INPUTS/
mv -f run/${ice_SH_file} INPUTS/



