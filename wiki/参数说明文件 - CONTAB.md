
# NAMELIST CONTABѡ� 
## ����Ƭ����ƽƬ��������
<br> 
<br>
### ����ʾ��ͼ
![����ʾ��ͼ](fig/CONTAB-para1.png) 
### �����������11
![����ʾ��ͼ](fig/CONTAB-para2.png) 
### ����������
![����ʾ��ͼ](fig/CONTAB-para3.png) 
### �����������12
![����ʾ��ͼ](fig/CONTAB-para4.png) 
<br>
<br>

## ������

|   ������   | VariableName | ����        | �������� | Definition                               | ����˵��                         |
| :-----: | :----------: | :-------- | :--: | :--------------------------------------- | :--------------------------- |
|         |    TTYPE     | ����        |  0   | = 1 Tab control<br> =2 TRIM tab<br> =3 Both | = 1 ����Ƭ<br> =2 ��ƽƬ<br> =3 ���� |
| (Cfi)tc |    CFITC     | ����Ƭ�ڲ��ҳ�   |  0   | inboard chord,control tab                | ����Ƭ�ڲ��ҳ�                      |
| (Cfo)tc |    CFOTC     | ����Ƭ����ҳ�   |  0   | outboard chord,control tab               | ����Ƭ����ҳ�                      |
| (bi)tc  |     BITC     | ����Ƭ�ڲ�����λ�� |  0   | inboard span location control tab        | ����Ƭ�ڲ�����λ��                    |
| (bo)tc  |     BOTC     | ����Ƭ�������λ�� |  0   | outboard span location control tab       | ����Ƭ�������λ��                    |
| (Cfi)tt |    CFITT     | ��ƽƬ�ڲ��ҳ�   |  0   | inboard chord,trim tab                   | ��ƽƬ�ڲ��ҳ�                      |
| (Cfo)tt |    CFOTT     | ��ƽƬ����ҳ�   |  0   | outboard chord,trim tab                  | ��ƽƬ����ҳ�                      |
| (bi)tc  |     BITT     | ��ƽƬ�ڲ�����λ�� |  0   | inboard span location trim tab           | ��ƽƬ�ڲ�����λ��                    |
| (bo)tc  |     BOTT     | ��ƽƬ�������λ�� |  0   | outboard span location trim tab          | ��ƽƬ�������λ��                    |
|   B1    |      B1      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   B2    |      B2      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   B3    |      B3      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   B4    |      B4      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   D1    |      D1      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   D2    |      D2      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   D3    |      D3      | ����11      |  0   | see table11 for definitions              | ����11                         |
|  Gcmax  |    CGMAX     | ����11      |  0   | see table11 for definitions              | ����11                         |
|    k    |      KS      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   Rl    |      RL      | ����11      |  0   | see table11 for definitions              | ����11                         |
|    ��    |     BGR      | ����11      |  0   | see table11 for definitions              | ����11                         |
|   ��r    |     DELR     | ����11      |  0   | see table11 for definitions              | ����11                         |

<br>
## tips1 KS . if the system has a spring ,KS input ,then free stream dynamic pressure is required<br>�����������KS����ַ����������������������ѹ.





# �������ľ�������

### 1 $A_C$

## $ A_c=\frac{S_\text{tc}\overline{C}_\text{tc}}{S_c\overline{C}_c}$

���У�

 $S()$��ʾ����� ���ɶ����������Ϊ����֮������<br>surface area ��movable surface are defined by their area aft of the hinge line��

 $\overline{C}()$ ��ʾ����ƽ�������ҳ����ɶ����涨�����֮����������$MAC$��ʾ<br>surface mean aerodynamic chord (movalbe surfaces are defined by their area aft of the hinge line,and the MAC is of that area)





�±꺬�壺

c ��ʾ�������� main control surface

s ��ʾ�������� �������� ������ƽβ����β������<br>surface to which the main control surface is attached��i��e��horizontal tail��vertical tail or wing��

tc  ����Ƭ control tab

tt  ��ƽƬ trim tab



###  $B_1$

### $B_1 = \left ( \frac{\partial C_{h_c}}{\partial \delta _{c}} \right )_{\delta _{tc},a_s,\delta _{tt}}   = \left ( C _{h _{\delta} } \right )_{{c}'}$  ,$1/Deg$ 

�μ� 6.1.6.2�� (Datcom Section 6.1.6.2)

## $B_2$

### $B_2 = \left ( \frac{\partial C_{h_c}}{\partial \delta _{tc}} \right )_{\delta _{{c}'{a_s}'},\delta _{tt}}   $  ,$1/Deg$ 

  �û����� ��user input                   

##  $B_3$

### $B_3 = \left ( \frac{\partial C _{h_c}}{\partial \delta _{a_{s}}} \right ) _{\delta _{{c}'} \delta _{t{c}'} \delta _{tt}}    = \left ( C _{h _{a } }\right )_{{c}'}$

�μ� 6.1.6.1�� (Datcom Section 6.1.6.1)

## $B_4$



###  $B_4 = \left ( \frac{\partial C _{h_c}}{\partial \delta _{tt}} \right ) _{\delta _{{c}'} \delta _{t{c}'} a_s} $  ,$1/Deg$    

�û����� ��user input   



## $F_c$  

$F_c$      ��������(����Ϊ��)  control-column force (pull force is positive) 



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

���ӿ����������Ĺ��ǣ�Deg

## $\beta$ 

### $ \beta = \left ( \frac{\partial \delta _{tc}}{\partial \delta _{c}} \right ) _{\begin{matrix}stick\\ free \end{matrix}}$ 

  with $k = \infty$  control-tab gear ratio



