PyDatcomLab 项目是用Python编写的Datcom 1976版的GUI界面项目。项目将提供飞机模型编辑，Datcom算例设定，运行和结果解析，曲线分析等功能。


1. Datcom 软件简介
Datcom是一款空气动力学分析软件，用Fortran语言编写

2.PyDatcomLab项目简介
本项目是对Datcom软件的封装。


2.1 项目的基本概念

项目的工程文件说明

工程文件的后缀名： *.dcprj |DatcomProject
包括的主要内容
project_name = u'项目名称'           #unicode string,项目的名称， 
create_time  = YYYY-MM-DD_HHmmSS_MS #timeString，
modify_time  = YYYY-MM-DD_HHmmSS_MS #tiemString
project_dir  = r'.'                 #defalt is the current directory，默认是工程文件所在目录，为多项目预留
version      = r'XXX.XX.XXX'        #verison code 

#aerocraft definition 飞行器定义
aerocraft_name         = r'the name of the aerocraft'        #string, 被计算的飞行器的名字
aerocraft_3dmodel      = r'3DModels/aerocraft_name.py'       #由Python定义的飞行器3D模型的绘制模块
aerocraft_datcomModel  = r'aerocraft_name.xml'               #飞行器的基本构型定义文件


#case definition  算例定义
case_group_name        = u'算例组名称'               # 算例组名称
case_identifier        = u'case identifier name'    #算例名称




directory_structure = [ #主要的目录结构
              3DModels， #存放该工程下面用到的3D显示模型
              Reprots,  #存放该工程下面的结算结果文件
                        #命令规则： caseName-time-Index.xml
              Problems, #计算任务的顶级分布包
              Problems/caseName , #存放具体的算例 ，一组casename构成一个Problem
 
 """
 For example,a base Project directory may be contain these files like below:
 ./project_name.dcprj   #xml
 ./3DModels/
 
 """
              
              
              
    
file_list  = {   #dictory type ，包括主要的配置文件和结果
            configuration_file:r'configuration.xml'
            flight_conditions: and reference dimensions


