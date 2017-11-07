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

#定义datcon的常量说明
definition = """\
<Idenity Name = 'FLTCON', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'SYNTHS', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'BODY'  , IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'SYMFLP', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'ASYFLP', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'OPTINS', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'WGPLNF', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'HTPLNF', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'VTPLNF', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'VFPLNF', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'WGSCHR', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'HTSCHR', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'VTSCHR', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'VFSCHR', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'PROPWR', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'JETPWR', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'CONTAB', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'EXPR'   , IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'GRNDEF', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'HYPEFF', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'LARWB', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'TRNJET', IDType = 'NAMELIST',Parent = ''></Idenity>
<Idenity Name = 'TVTPAN', IDType = 'NAMELIST',Parent = ''></Idenity>

<Idenity Name = 'TVTPAN', IDType = 'NAMELIST',Parent = ''></Idenity>
"""

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
