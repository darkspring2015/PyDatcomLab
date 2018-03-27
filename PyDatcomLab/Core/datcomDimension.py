#!/usr/bin/env python
#from PyDatcomLab.Core.datcomDimension import Dimension

# 认为第一个单位是默认单位
Dimension = {
            'A':[ 'm2', 'cm2', 'ft2', 'in2'],  #A denotes units of area: ft2, in2, m2, or cm2
            'DEG':['deg', 'rad'],  #Deg denotes angular measure in degrees, or 
            'TDEG':['K','℉', '℃',  '°R'] ,  #temperature in degrees Rankine or degrees Kelvin
            'L':['m','ft', 'in',  'cm'],  #denotes units of length: feet, inches, meters, or centimeters
            'F':['N','lbf', 'kip'], #F denotes units of force; pounds or Newtons
            'W':['kg', 'g', 'lb'], #W 质量
            'T':['s', 'min', 'ms'], #t denotes units of time; seconds
}

#参考了 https://www.baidu.com/baidu?wd=%E5%88%86%E7%B1%B3&tn=monline_dg&ie=utf-8
# [X->SI  SI->X]
UnitMap ={
'A':{'SI':'m2', 'HowTo':{
'm2':[ lambda m2:  m2              , lambda m2:  m2] , 
'cm2':[lambda cm2:  1e-4*cm2       , lambda m2:  1e4*m2 ], 
'ft2':[lambda ft2:  0.92903e-1*ft2 , lambda m2:  m2*10.7639104 ],
'in2':[lambda in2:  0.6452e-3*in2  , lambda m2:  m2*1550.0031 ],
}}, #'A'
'L':{'SI':'m', 'HowTo':{
'm':[ lambda m:  m              , lambda m:  m] ,
'ft':[lambda ft:  0.3048*ft     , lambda m:  m*3.2808399 ] ,
'in':[lambda inch:  0.0254*inch , lambda m:  m*39.3700787 ] ,
'cm':[lambda cm:  1e-2*cm       , lambda m:  m*1e2 ] ,
}}, #'L'
'DEG':{'SI':'deg', 'HowTo':{
'deg':[lambda deg:  deg           , lambda deg:  deg] ,
'rad':[lambda rad:  0.0174533*rad , lambda deg:  deg*57.29578 ] ,
}}, #'DEG'
#https://baike.baidu.com/item/%E6%B8%A9%E5%BA%A6%E5%8D%95%E4%BD%8D/9460959?fr=aladdin
#华氏度(fahrenheit)和摄氏度(Centigrade)都是用来计量温度的单位
#华氏度：F=32+1.8×C 
# F=9℃/5+32，或 ℃=5（F-32）/9 
#绝对零度的时候（即0开尔文），兰氏度也为零。但是兰氏度定义为华氏度加上相应值而不像开尔文是定义为摄氏度减去相应值。[°R] = [°F] + 459.67， 也可以 [°R] = [K] × 1.8。
'TDEG':{'SI':'K', 'HowTo':{
'K':[lambda K:  K             , lambda K:  K] ,
'°R':[lambda R: R/1.8         , lambda K:  K*1.8 ] ,
'℉':[lambda F: (F -32)/1.8 + 273.15 , lambda K:  32 + 1.8*(K -  273.15) ] ,
#'℉':[lambda F: F/1.8 - 330.75 , lambda K:  523.67 + 1.8*K ] ,
'℃':[lambda C: C +273.15      , lambda K:  K - 273.15 ] ,
}}, #'TDEG'
#英镑原名pound（在一二八○年前为punde）sterling，前者为磅，九二五白银（一千份中九百二十五份白银、七十五份黄铜）称“史他令”，即一英镑等于一磅纯度九二五的白银—准此，英镑是“贴金”之译，英磅才是正译。
#磅（pound）的简写l（或lb），来自拉丁文的libra（一磅重），l加上先令的s（shilling，从罗马的货币单位solidus衍化而来）成为￡（显然是美化版）。英国货币在一九七一年前分为镑、先令和便士“三级制”，英镑的符号没有便士，此一代表最小货币单位的铜币遂被弃用；便士的原文pence，为penny的复数，其符号d则来自罗马时期最小货币单位 denarius

'F':{'SI':'N', 'HowTo':{
'N':[lambda N:  N , lambda N:  N] ,
'lbf':[lambda lbf:  4.448222*lbf    , lambda N:  N*0.2248089 ] ,#pounds
'kip':[lambda kip:  4448.221615*kip , lambda N:  N*0.2248089e-3 ] ,
}}, #'F'

'W':{'SI':'kg', 'HowTo':{
'kg':[lambda kg:  kg , lambda kg:  kg] ,
'lb':[lambda lb:  0.4535924*lb    , lambda kg:  kg*2.2046226 ] ,#pounds
'g':[ lambda g:  1e-3*g , lambda kg:  kg*1e3 ] ,
}}, #'W'

'T':{'SI':'s', 'HowTo':{
's':[lambda s:  s             , lambda s:  s] ,
'min':[lambda min:  60*min    , lambda s:  s*0.0166667 ] ,#pounds
'ms':[ lambda ms:  1e-3*ms    , lambda s:  s*1e3 ] ,
}}, #'W'

}


def unitTransformation(tVar, targetUnit):
   """
   tVar = {'Dimension':tVar['Dimension'],  #量纲 L2 1/L  ,etc
           'Value':0 ,                     #值数据
           'Unit':'',                      #值的单位   
   }
   targetUnit   #目标单位
   """ 
#   if 'Dimension' not in tVar.keys() or 'Unit' not in tVar.keys() \
#        or targetUnit is None or tVar['Unit'] == targetUnit \
#        or tVar['Dimension'] in UnitMap.keys()\
#        or tVar['Unit'] not in UnitMap[tVar['Dimension'] ].keys()\
#        or 'Value' not in tVar.keys() or tVar['Value'] is None  :
#       return tVar
   #开始执行单位变换
   #tNewVar = tVar.copy()
   try:
       tHowto = UnitMap[tVar['Dimension']]['HowTo']  #返回转换字典
       tVar['Value'] = tHowto[targetUnit][1](tHowto[tVar['Unit']][0](float(tVar['Value'])))
       tVar['Unit']  = targetUnit
       #return tVar       
   except  Exception as e:
       print(e)
   finally:
       return tVar  
   
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
        
def getDimensionByUnit(tUnit):
    """
    利用Unit查询Dimension
    """
    if tUnit is None or tUnit in ['', '/']:
        return ''
    #tDim = ''
    for iD in Dimension.keys():
        tIndex = Dimension[iD].index(tUnit)
        if tIndex > -1 :
            return iD
    return ''
    
    
