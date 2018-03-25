#!/usr/bin/env python
#from PyDatcomLab.Core.datcomDimension import Dimension

# 认为第一个单位是默认单位
Dimension = {
            'A':['cm2', 'm2', 'ft2', 'in2'],  #A denotes units of area: ft2, in2, m2, or cm2
            'DEG':['deg', 'rad'],  #Deg denotes angular measure in degrees, or 
            'TDEG':['℉', '℃', 'K', '°R'] ,  #temperature in degrees Rankine or degrees Kelvin
            'L':['feet', 'inch', 'M', 'CM'],  #denotes units of length: feet, inches, meters, or centimeters
            'F':['pounds', 'N'], #F denotes units of force; pounds or Newtons
            'W':['Kg', 'g', 'p'], #W 质量
            'T':['s', 'min', 'ms'], #t denotes units of time; seconds
}

def unitTransformation(tVar, targetUnit):
   """
   tVar = {'Dimension':tVar['Dimension'],  #量纲 L2 1/L  ,etc
           'Value':0 ,                     #值数据
           'Unit':'',                      #值的单位   
   }
   targetUnit   #目标单位
   """ 
   if 'Dimension' not in tVar.keys() :return tVar
   #开始执行单位变换
   tNew = tVar   
   tNew['Unit'] = targetUnit   #值的单位   
    #在这里执行对应的坐标转换
   return tNew
   
def getUnitListByDimension(tDimension):
    """
    返回Dimension对应的
    """
    if tDimension in Dimension.keys():
        return Dimension[tDimension]
    else:
        return []
        
def getMainUnitByDimension(tDimension):
    """
    返回Dimension对应的主要单位
    """
    if tDimension in Dimension.keys():
        return Dimension[tDimension][0]
    else:
        return ''
        
