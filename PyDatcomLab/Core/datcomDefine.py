#!/usr/bin/env python

class datcom(object):
    '''
    常量定义空间
    
    
    需要注意的Datcom中量的规定
    
    
    '''
    configurationType = [u'常规布局', 
                         u'双垂尾布局', 
                         u'高超声速布局']
    
    def __init__(self):
        pass


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
