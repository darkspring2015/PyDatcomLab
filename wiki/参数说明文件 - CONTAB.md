
# NAMELIST CONTAB选项卡 
## 控制片和配平片的输入项
<br> 
<br>
### 参数示意图
![参数示意图](fig/CONTAB-para1.png) 
### 工程量含义表11
![参数示意图](fig/CONTAB-para2.png) 
### 工程量含义
![参数示意图](fig/CONTAB-para3.png) 
### 工程量含义表12
![参数示意图](fig/CONTAB-para4.png) 
<br>
<br>

## 参数表

|   工程量   | VariableName | 别名        | 长度限制 | Definition                               | 参数说明                         |
| :-----: | :----------: | :-------- | :--: | :--------------------------------------- | :--------------------------- |
|         |    TTYPE     | 类型        |  0   | = 1 Tab control<br> =2 TRIM tab<br> =3 Both | = 1 控制片<br> =2 配平片<br> =3 均是 |
| (Cfi)tc |    CFITC     | 控制片内侧弦长   |  0   | inboard chord,control tab                | 控制片内侧弦长                      |
| (Cfo)tc |    CFOTC     | 控制片外侧弦长   |  0   | outboard chord,control tab               | 控制片外侧弦长                      |
| (bi)tc  |     BITC     | 控制片内侧区域位置 |  0   | inboard span location control tab        | 控制片内侧区域位置                    |
| (bo)tc  |     BOTC     | 控制片外侧区域位置 |  0   | outboard span location control tab       | 控制片外侧区域位置                    |
| (Cfi)tt |    CFITT     | 配平片内侧弦长   |  0   | inboard chord,trim tab                   | 配平片内侧弦长                      |
| (Cfo)tt |    CFOTT     | 配平片外侧弦长   |  0   | outboard chord,trim tab                  | 配平片外侧弦长                      |
| (bi)tc  |     BITT     | 配平片内侧区域位置 |  0   | inboard span location trim tab           | 配平片内侧区域位置                    |
| (bo)tc  |     BOTT     | 配平片外侧区域位置 |  0   | outboard span location trim tab          | 配平片外侧区域位置                    |
|   B1    |      B1      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   B2    |      B2      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   B3    |      B3      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   B4    |      B4      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   D1    |      D1      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   D2    |      D2      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   D3    |      D3      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|  Gcmax  |    CGMAX     | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|    k    |      KS      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   Rl    |      RL      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|    β    |     BGR      | 见表11      |  0   | see table11 for definitions              | 见表11                         |
|   Δr    |     DELR     | 见表11      |  0   | see table11 for definitions              | 见表11                         |

<br>
## tips1 KS . if the system has a spring ,KS input ,then free stream dynamic pressure is required<br>如果输入中有KS这个字符串，则必须输入自由流动压.





# 分析表格的具体内容

### 1 $A_C$

## $ A_c=\frac{S_\text{tc}\overline{C}_\text{tc}}{S_c\overline{C}_c}$

其中：

 $S()$表示表面积 ，可动表面积定义为铰链之后的面积<br>surface area （movable surface are defined by their area aft of the hinge line）

 $\overline{C}()$ 表示表面平均气动弦长，可动表面定义铰链之后的面积，用$MAC$表示<br>surface mean aerodynamic chord (movalbe surfaces are defined by their area aft of the hinge line,and the MAC is of that area)





下标含义：

c 表示主控制面 main control surface

s 表示主控制面 附属的面 ，例如平尾、垂尾、机翼<br>surface to which the main control surface is attached，i，e，horizontal tail，vertical tail or wing。

tc  控制片 control tab

tt  配平片 trim tab



###  $B_1$

### $B_1 = \left ( \frac{\partial C_{h_c}}{\partial \delta _{c}} \right )_{\delta _{tc},a_s,\delta _{tt}}   = \left ( C _{h _{\delta} } \right )_{{c}'}$  ,$1/Deg$ 

参见 6.1.6.2章 (Datcom Section 6.1.6.2)

## $B_2$

### $B_2 = \left ( \frac{\partial C_{h_c}}{\partial \delta _{tc}} \right )_{\delta _{{c}'{a_s}'},\delta _{tt}}   $  ,$1/Deg$ 

  用户输入 。user input                   

##  $B_3$

### $B_3 = \left ( \frac{\partial C _{h_c}}{\partial \delta _{a_{s}}} \right ) _{\delta _{{c}'} \delta _{t{c}'} \delta _{tt}}    = \left ( C _{h _{a } }\right )_{{c}'}$

参见 6.1.6.1章 (Datcom Section 6.1.6.1)

## $B_4$



###  $B_4 = \left ( \frac{\partial C _{h_c}}{\partial \delta _{tt}} \right ) _{\delta _{{c}'} \delta _{t{c}'} a_s} $  ,$1/Deg$    

用户输入 。user input   



## $F_c$  

$F_c$      控制柱力(拉力为正)  control-column force (pull force is positive) 



## $G _{c _{max}}$

###  

### $G _{c _{max}} =\frac {1}{57.3\left ( \frac{\partial x_c}{\partial \delta _c} \right )_{max}}$



maximum stick gearing user input. <br>if $R_L$=0,$G_{c _{max}}$ also is zero.In this case input $G_{tc_{max}}$ and $\Delta r = 1.0 ( G_{tc_{max}} = G_{c_{max}} * \Delta r )$ .



## K

###  $k = -\left ( \frac{\partial M _{tc}}{\partial \delta  _{tc}} \right ) _{spring} \frac{1}{S _{tc} \overline c_{tc}}$   tab spring effectiveness

### $q$  local dynamic pressure

###  $R_1,R_2$  

shorthand notation for tab and main surface hinge moments and key linkage parameters,obtained from Table 12<br> 

###  $R_L$ 

aerodynamic boost link ratio ,user input . $(R_L \geqslant 0)$ . To input $R_L = \infty $ , set $R_L <  0$ .

<br>

##  $a_s$

angle of attack of the surface to which the main control surface is attached, Deg

附加控制面的曲面的攻角，Deg

## $\beta$ 

### $ \beta = \left ( \frac{\partial \delta _{tc}}{\partial \delta _{c}} \right ) _{\begin{matrix}stick\\ free \end{matrix}}$ 

  with $k = \infty$  control-tab gear ratio



