# Datcom程序的主要使用约束

本文档记录Datcom程序的主要使用约束条件，参考文献



## 1 .增升和控制面分析的限制

每次只能分析一个增升或者控制面，其他的信息需要通过EXPER选项卡进行指定





### Datcom 模型系统的约定

1. Datcom模型包括总模型和用于计算的模型，两个部分
   1. 对于总模型，应当包含定义一个飞机模型所需要的全部信息
   2. 对于计算模型，只包含用于分析的部分参数，在Datcom中对应CASE
2. Datcom中CASE既包含模型参数也包含控制参数
   1. 模型参数如上所述
   2. 控制参数包含了Datcom中特定的约定信息



### 基本设计构想

#### 设计目的 

​    实现项目基本目录结构的控件

​	项目模型

​	项目模型的界面

 #### 总体设计

     	1. 由逻辑树形结构承担
     	2. 逻辑树的内容由存储在框架中的python字典承担

#####  逻辑树

逻辑树包含项目的内部信息

##### 筛选逻辑

​	在NewModel中加入了模型筛选的基本逻辑

​		产生一个list指定模型包含的说明卡选项

