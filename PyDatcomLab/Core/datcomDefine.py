#!/usr/bin/env python


configurationType = [u'常规布局', 
                         u'双垂尾布局', 
                         u'高超声速布局']        
        
speedRegime = ['Subsoinc', 'Transoinc', 'Supersonic', 'Hypersonic']

reserved_NAMELISTS = [
    'FLTCON',#
    'SYNTHS',#
    'BODY',#
    'SYMFLP',#
    'ASYFLP', #
    'OPTINS',#
    'WGPLNF', #    
    'HTPLNF',#
    'VTPLNF',#
    'VFPLNF',#
    'WGSCHR',#
    'HTSCHR',#
    'VTSCHR',#
    'VFSCHR',#
    'PROPWR',#linger
    'JETPWR', #
    'CONTAB', #
    'EXPR', #
    'GRNDEF', #
    'HYPEFF', #
    'LARWB', #
    'TRNJET', #
    'TVTPAN', #   
    ]
#dictNamelist ={
#{'ASYFLP', 'DELTAL', 9 , 'deflection angle for left hand plain flap aileron or left hand panel all moveable horizontal tail, measured in vertical plane of symmetry', 'deg'}, 
#{'ASYFLP2', 'DELTAR', 9 , 'deflection angle for left hand plain flap aileron or left hand panel all moveable horizontal tail, measured in vertical plane of symmetry', 'deg'}, 
# 
#}

#'ASYFLP'
dict_ASYFLP=['DELTAL', 'DELTAR', 'DELTAD', 'DELTAS', 'XSOC', 'HSOC', 'STYPE', 'XSPRME', 'NDELTA', 'CHRDFI', 
'CHRDFO', 'SPANFI', 'SPANFO', 'PHETE']




dtInputFile = """\
 $FLTCON NMACH = 1.0, MACH(1)=0.60, NALPHA = 11.0,
         ALSCHD(1) = -6.0, -4.0, -2.0, 0.0, 2.0,
                      4.0, 8.0, 12.0, 16.0, 20.0, 24.0,
         RNNUB=4.28E6$
 $OPTINS SREF=8.85, CBARR=2.48, BLREF=4.28$
 $SYNTHS XCG=4.14, ZCG=-0.20$
 $BODY NX = 10.0,
   X(1)=0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,
   R(1)=0.0,0.186,0.286,0.424,0.533,0.533,0.533,0.533,0.533,0.533,
   S(1)=0.0,0.080,0.160,0.323,0.751,0.883,0.939,1.032,1.032,1.032,
   P(1)=0.0,1.00,1.42,2.01,3.08,3.34,3.44,3.61,3.61,3.61$
 $BODY BNOSE=1.0, BLN=2.59, BLA=3.67$
CASEID APPROXIMATE AXISYMMETRIC BODY SOLUTION, EXAMPLE PROBLEM 1, CASE 1
SAVE
DUMP CASE
NEXT CASE
 $BODY ZU(1)= -.595,-.476,-.372,-.138, .200,
               .334, .343, .343, .343, .343,
       ZL(1)= -.595,-.715,-.754,-.805,-.868, 
              -.868,-.868,-.868,-.868,-.868$
CASEID ASYMMETRIC (CAMBERED) BODY SOLUTION, EXAMPLE PROBLEM 1, CASE 2
SAVE
NEXT CASE
 $FLTCON NMACH=3.0, MACH(1)=0.9,1.4,2.5, RNNUB=6.4E6, 9.96E6, 17.0E6$
SAVE
CASEID ASYMMETRIC (CAMBERED) BODY SOLUTION, EXAMPLE PROBLEM 1, CASE 3
NEXT CASE
 $FLTCON NMACH=1.0, MACH(1)=2.5, RNNUB=17.86E6, HYPERS=.TRUE.$
 $BODY DS=0.0$
CASEID HYPERSONIC BODY SOLUTION,  EXAMPLE PROBLEM 1, CASE 4
NEXT CASE
"""    
dtNmlstExmple ={
    'FLTCON':{
    'NMACH':1, 'MACH':{'Index':1, 'Value':[0.60, ]},
    'NALPHA':11.0, 
    'ALSCHD':{'Index':1, 'Value':[-6.0, -4.0, -2.0, 0.0, 2.0,4.0, 8.0, 12.0, 16.0, 20.0, 24.0]}, 
    'RNNUB':4.28E6
    },
    'OPTINS':{
    'SREF':8.85, 'CBARR':2.48, 'BLREF':4.28
    }, 
    'SYNTHS':{
    'XCG':4.14, 'ZCG':-0.20
    }, 
    
    'BODY':{
    'NX':10.0, 
    'X':{'Index':1, 'Value':[0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,]}, 
    'R':{'Index':1, 'Value':[0.0,0.186,0.286,0.424,0.533,0.533,0.533,0.533,0.533,0.533,]}, 
    'S':{'Index':1, 'Value':[0.0,0.080,0.160,0.323,0.751,0.883,0.939,1.032,1.032,1.032,]}, 
    'P':{'Index':1, 'Value':[0.0,1.00,1.42,2.01,3.08,3.34,3.44,3.61,3.61,3.61, ]}, 
    'BNOSE':1.0, 
    'BLN':2.59, 
    'BLA':3.67
    }, 
    'CASEID':{
    'CASEID':'APPROXIMATE AXISYMMETRIC BODY SOLUTION, EXAMPLE PROBLEM 1', 
    'CASE':1   
    }
}

dtNmlstExmple2 ={
    'FLTCON':{
    'NMACH':[1,], 'MACH':[0.60, ], 'NALPHA':[11.0, ], 
    'ALSCHD':[-6.0, -4.0, -2.0, 0.0, 2.0,4.0, 8.0, 12.0, 16.0, 20.0, 24.0], 
    'RNNUB':[4.28E6, ]
    },
    'OPTINS':{
    'SREF':[8.85, ], 'CBARR':[2.48, ], 'BLREF':[4.28, ]
    }, 
    'SYNTHS':{
    'XCG':[4.14, ], 'ZCG':[-0.20, ]
    }, 
    
    'BODY':{
    'NX':[10.0, ], 
    'X':[0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,], 
    'R':[0.0,0.186,0.286,0.424,0.533,0.533,0.533,0.533,0.533,0.533,], 
    'S':[0.0,0.080,0.160,0.323,0.751,0.883,0.939,1.032,1.032,1.032,], 
    'P':[0.0,1.00,1.42,2.01,3.08,3.34,3.44,3.61,3.61,3.61, ], 
    'BNOSE':[1.0, ], 
    'BLN':[2.59, ], 
    'BLA':[3.67, ]
    }, 
    'CASEID':{
    'CASEID':['APPROXIMATE AXISYMMETRIC BODY SOLUTION, EXAMPLE PROBLEM 1'], 
    'CASE':[1, ]    
    }
}
    

