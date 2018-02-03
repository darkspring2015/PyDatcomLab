# PyDatcomLab 说明文件

###  变量属性定义

变量定义表包括如下信息：

```sql lite
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

