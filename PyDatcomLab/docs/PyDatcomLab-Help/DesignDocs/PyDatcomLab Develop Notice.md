# PyDatcomLab 说明文件

###  变量属性定义

变量定义表包括如下信息：

```mssql
CREATE TABLE `VariableDefine` (
                              `VarName` varchar(10) NOT NULL,
                              `NameList` varchar(10) NOT NULL,
                              `ShowName` varchar(20) NOT NULL,
                              `ToolTips` varchar(100) DEFAULT NULL,
                              `TYPE` varchar(10) DEFAULT `REAL`,
                              `Group` varchar(20) DEFAULT NULL,
                              `Range` varchar(100) DEFAULT NULL,
                              `Default` varchar(100) DEFAULT NULL,                              
                              `Limit` varchar(20) DEFAULT NULL,    
                              `Dimension` varchar(20) DEFAULT NULL,  
                              `Relation` varchar(20) DEFAULT NULL,                               
                               PRIMARY KEY (`id`,`NameList`) 
```
```python
datcomVarAttriList = [
    'VarName',   #Datcom变量名
    'NameList',  #Datcom NAMELIST
    'ShowName',  #显示名
    'ToolTips',  #提示信息
    'TYPE',      #变量类型  ['INT','REAL','Array','List']
    'SubType',   #变量子类型 对于['INT','REAL']为空,对于['Array','List']指定元素的类型 ['REAL','STR',]
    'Group',     #分组名   对于['INT','REAL','List']对应Group控件，对于'Array'对应table控件
    'Range',     #取值范围 对于['INT','REAL','Array'] 限制为 min,max，对于['List']为所有可能的值
    'Default',   #默认值   对于['INT','REAL','Array']为item的默认值，对于['List']为默认索引
    'Limit',     #数量     对于['INT','REAL','List']为空，对 ['Array']为min，max
    'Dimension', #量纲     ['L','DEG',...]
    'MustInput'  #是否是必须输入的量 [True,False]
    'Relation',  #其他关联信息 rule规则暂定
              ]
```

### 项目结构约定

1.项目以目录管理方式为基本样式

目录下存放*.dcprj文件作为项目的整体描述文件

​	在项目文件中包含若干的CASE Group ，对应命名为CaseGroupName的文件夹

​	每个CASE group包括若干的CASE， 对应若干命名为CaseName的文件夹

​	每个CASE 一个dcModel模型和计算目录，每个Case包括 dcModel.dcxml文件，*.inp文件和*.out文件

2.项目文件*.dcprj中记录项目的基本信息

​	项目名称：

​	项目描述：

​	修改时间：

​	创建时间：

​	飞行器基本信息：

​	CASE group信息

​		CASE 信息



# dcModel 类的说明

dcModel类是承载datcom所用计算数据的底层数据结构，从dcxml的数据文件中解析加载配置参数，并将其转换为UI界面使用的信息。

项目计划使用Python的dict作为内部存储结构，并实现了第一个版本，但是效果并不是特别的理想

作为第二个版本基本实现成QtModel的子类，利用Qt的Model/View结构来提供更加灵活高效的顶层设计能力。

现将基本的设计需要记录如下：

### datcom算例文件格式说明

新的项目文件采用更加简洁的结构层次

```xml
<CASE CASEID='1' Describe='' AerocraftName="AircraftName" Configuration="常规布局" createTime="2018-01-12 22:23:49" modifyTime="2018-01-12 22:23:49" >
    <VARIABLE VarName='LOOP'  Namelist='FLTCON' Url='FLTCON/LOOP' Unit='/'>['1.0']</VARIABLE>
    <VARIABLE VarName='NMACH' Namelist='FLTCON' Url='FLTCON/NMACH' Unit='/'>[3]</VARIABLE>
    <VARIABLE VarName='MACH'  Namelist='FLTCON' Url='FLTCON/MACH' Unit='/' SIndex ='1'>[0.15,0.25,0.25]</VARIABLE>
    <VARIABLE VarName='X'  Namelist='BODY' Url='BODY/X' Unit='L/m' SIndex ='1'>[0.15,0.25,0.25]</VARIABLE>
</CASE>

```

以上的XML将遵循以下的约束条件：

1. VarName，Namelist，Url是必须的
2. Unit的取值将包括："","/"表示无量纲量，有量纲量将表示为：“Dimension/Unit”,量纲和单位的信息见约定 []
3. SIndex 用来标示Array类型数据的起始位置，虽然程序能从变量定义表中进行推定，但是建议不省略。
4. \<VARIABLE>节点的其他信息从datcom定义文件中进行推定，Datcom定义结构见<a>
5. \<VARIABLE>节点的内容包括：\[int]\[float][float array]\[string]等不同的形式，将进行Python的eval评估
6. ​

### dcModel类的基本接口说明

dcModel类主要接口包括：

​	

| 接口名称                                     | 功能描述                            | 参数说明                                     | 约束条件                                     | 其、他  |
| ---------------------------------------- | ------------------------------- | ---------------------------------------- | ---------------------------------------- | ---- |
| \__init__(self，dtDefine = DtDefine)      | 初始化函数                           | dtDefine是datcom的整体定义对象，常驻实例 DtDefine     | 创建空的没有任何信息的实例<br />约束信息根据dtDefine生成      |      |
| \__init__(self,path = None，dtDefine = DtDefine) | 初始化函数，从文件path加载数据               | path str<br />*.dcXML,*.xml类型            | path无效将创建在 "~/.PyDatcomLab/temp/"下的随机文件<br />path有效将加载对应文件<br />path不存在将尝试创建对应的文件，并创建空白实例 |      |
| loadCASE(path)                           | 从文件path加载数据                     | path str<br />*.dcXML,*.xml类型<br />加载出现错误将引发异常 |                                          |      |
| saveCASE(path)                           | 将结果保存到文件                        |                                          |                                          |      |
| setProperties(pDict)                     | 修改属性字典定义的属性                     | pDict是Python的dict类型修改基本信息                | 对于超过datcom算例文件的属性信息将作为属性写入，但不承若行为        |      |
| getProperties()                          | 以dict形式返回属性                     | 返回值为Python的dict类型                        | 将返回所有的属性                                 |      |
| getNamelistCollection()                  | 返回实例包含的所有的Namelist和对应的Variaable | 返回值为dict<br />etc：{‘FLTCON’:['NMACH']}   | 返回的结果不包括值                                |      |
| addNamelist(self, namelist, variables = []) | 向实例添加一个选项卡和对应的变量                | namelist str ：控制卡的名称<br />variables [str] 变量组合 ，可选 | 当添加的变量省略时，或者变量未达到基本要求时，将根据datcom定义文件进行推定补充 |      |
| setVariable(variable)                    | 设置实例的某一个变量的值                    | variable是Python的dict类型，包括：{url，unit，value} | 对于不存在的namelist，将导致创建对应选项卡的操作；<br />对于存在的变量，将修改值和单位等信息<br />如果变量的值为None，将从集合中删除该变量 |      |
| deleteNamelist(namelist)                 | 从实例移除一个选项卡                      | namelist str：选项卡的名称                      | 不承诺数据的完整性                                |      |
| validate()                               | 验证当前的配置是否是一个可以执行的配置，并返回报告信息     | 返回值为当前配置的错误报告                            |                                          |      |
| buildDatcomInputFile(path)               | 根据当前的CASE配置创建datcom的计算文件        | 过程出错将引发异常                                |                                          |      |

## datcomConstraint 类

datcomConstraint 用来加载和记录与基本计算构型相关的datcom配置规则的信息

配置规则由特定的xml文件描述

###  constraint condition 描述文件

描述文件示例如下：

```XML
<ConfigurationCollection CreateTime="2018-03-26 19:32:33" Describe="some" ModifyTime="2018-03-27 19:26:09" modelPath=".\case3.xml">
    <Configuration CName='Wing-Body' DisplayName='机身机翼分析' HelpUrl='' Namelists="FLTCON ，OPTINS">        
        <VARIABLE Url='FLTCON/LOOP' VarName='LOOP'  Namelist='FLTCON'  Unit='/'>'1.0'</VARIABLE>
    </Configuration>
</ConfigurationCollection>
```



描述文件遵循如下的约定：

​	 1.根节点Tag ：ConfigurationCollection

       	2. 其下包含多个Configuration，每个Configuration由若干VARIABLE作为子节点
        	3. VARIABLE的指定了特殊的初始化规则,其值将被用来修改模型
         	4. Configuration的Namelists属性指定了该配置必需包含的选项卡的名称



 #### datcomConstraint  主要接口



## 开发过程

项目主要由Linger独立完成。

项目采用了Bug驱动开发的模式，在丰富的预判断日志机制下，系统的错误信息非常有效

### 1. 主要功能的开发

2018-7-13  Bug ：WT 、GAMMA、STMACH 、TSMACH 没有在界面激活，但是写入了最终文件，需要检查逻辑

​                    Bug： NX 不可见 &radic;

​		    Bug：编辑控件不可用&radic;

​		    Bug：添加选项卡无效&radic;

​		    Bug ：选项卡添加逻辑异常&radic;



2018-7-14 Bug： X的值加载不完整  &radic;

​			     _setArrayItembyIndex 修改了内部逻辑问题      &radic;

​		  Bug：界面未输入的表单项出现在最终的输入文件中    &radic;

​		  Bug ：没有按照分组顺序写入。不美观

​		  Bug ：Datcom的最终输入文件中数组列最后应该换行，不美观 &hearts;

​		  Bug ：Runing的报告信息总是提示未完成！    &radic;

​		  Bug ： 没有正确的推定 combo的组合关系  &radic;

​		  Bug :  在Input文件中array最后的，和$之间应该不同时出现&radic;

2018-07-15 

​                  Bug : onItemChanged()检查出错   这是逻辑判断错误导致  &radic; 

​                  Bug：表格FLTCON/Speed_Atmospheric在R：0，C：5的输入不应为空 

​			     主要问题，隐藏表格问题   &radic;

​                   Bug: 加载机翼的多个CARD错误	'WGSCHR/None'  给了一个不稳定的补丁&radic;

​                   Bug: 翼型截面的坐标模式没有选项卡，但是却被错误禁用&radic;

​                   	Task：添加定长表格规则描述&radic;

1. ​                                修改datcomDefine file的模板 增加 FixedRowsTable &radic;

2. ​                                增加了读写函数的FixedRowsTable 节&radic;

3. ​                                增加checkFixedRowTable()&radic;

4. 修改getRuleNumToCountByGroup的返回逻辑，从VarName到Url &radic;

   ​             Bug ： 修复了重新加载CASEUI时的打印不美观问题       &radic;

   ​             Bug  ： 修改DatcomDefine编辑器没有合适标题的问题  &radic;

   Bug ： 'InUsed'与‘MustInput'逻辑冲突  修复了List中的函数调用参数错误问题 &radic;

   Bug   :  NTPS 和表格的联系没有生效 &radic;

​       Bug   : 表格限制列的无法在10位数的情况进行正确输出, 将Single控件获得窗口的基本行为修改为setFocus + setCursor &radic;

Bug： 可选的Single如数控件没有正确的显示默认值、提示值  &radic; 不是bug，缺失没有定义值

Task ：帮助文档系统，改进 主要功能

- 系统刷新Define是能够自动刷新帮助文档
- 支持Define定义的Helpdoc使用本地相对路径  raletionPath + baseUrl

Task :  修改Datcom编辑器系统系统

- 支持规则节点添加，自动识别
- 支持属性节点添加，自动识别
- 支持git备份功能



Bug： 表格的表头自适应宽度+ 左对齐  &radic;

Bug  : 表格控件，空值不保存的问题。正确的行为应该是创建空值后写入到模型！ &radic;

Bug  ：可以错误的删除SLOPE列的错误    &radic;

Bug  ： 删除所有行的修复逻辑出错 &radic;

Bug   ：添加所有行没有初始化 &radic;



Task :修改新建模型的界面，使其支持最新的分组和必要性配置选项



2018-07-29

Bug：帮助浏览器始终覆盖主窗口 &radic;   去掉self可以防止出现这个问题

Bug： 更改目录结构后，docs功能异常  No module named 'mdx_mdx_math'

```shell
pip install python-markdown-math
```

安装之后就好了&radic;

Bug：添加选项卡时SYSTH的初始化状态不应该为选中

Bug:  机翼界面斜率没有合适的初始值，没有列数据出来

Bug:  NMACHLink异常

Bug: 小屏幕情况下界面下溢 

Bug: 小屏幕情况下长列表界面无法正常比例显示的问题

Bug：combo2Stuats状态异常的问题



Task： 将MathJax的位置改动到~/.PyDatcomLab/3rdParty中，减少每次更新和测试的时间消耗

Task ：设计3rdParty的配置文件设置方式

Task ：新建模型窗口不支持正确分组的问题



Bug ：错误发布已经删除的docs .md &radic; 

​            没有执行setup clean

Bug ：没有发布Helper系统 &radic; 

​            find_packages()无法识别没有"__init__()"的目录

Bug： WGPLNF/TYPE 的类型不同问题



TASK ： 增加直接读取Datcom Input文件的能力

TASK ： 增加绘制各种曲线的能力



Bug ： 曲面上下表面和中线表面的录入关系没有执行其一



Bug ： 删除分组选项卡中的一个，另一个并没有被删除&radic;增加了具体的功能