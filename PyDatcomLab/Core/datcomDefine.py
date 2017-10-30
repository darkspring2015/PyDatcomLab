#!/usr/bin/env python

class datcomDefine(object):
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
    dictNamelist ={
    {'ASYFLP', 'DELTAL', 9 , 'deflection angle for left hand plain flap aileron or left hand panel all moveable horizontal tail, measured in vertical plane of symmetry', 'deg'}, 
    {'ASYFLP', 'DELTAR', 9 , 'deflection angle for left hand plain flap aileron or left hand panel all moveable horizontal tail, measured in vertical plane of symmetry', 'deg'}, 
     
    }
    #'ASYFLP'
    dict_ASYFLP=['DELTAL', 'DELTAR', 'DELTAD', 'DELTAS', 'XSOC', 'HSOC', 'STYPE', 'XSPRME', 'NDELTA', 'CHRDFI', 
    'CHRDFO', 'SPANFI', 'SPANFO', 'PHETE']
    
    
    
    
    
    
if __name__ == "__main__":
    pass
