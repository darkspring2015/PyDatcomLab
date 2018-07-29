
# NAMELIST CONTABѡ� 
## ����Ƭ����ƽƬ��������

### ����ʾ��ͼ
![����ʾ��ͼ](figs/CONTAB-para1.png) 
### �����������11
![����ʾ��ͼ](figs/CONTAB-para2.png) 
### ����������
![����ʾ��ͼ](figs/CONTAB-para3.png) 
### �����������12
![����ʾ��ͼ](figs/CONTAB-para4.png) 


## ������

|        ������        | VariableName | ����        | �������� | Definition                               | ����˵��                           |
| :---------------: | :----------: | :-------- | :--: | :--------------------------------------- | :----------------------------- |
|                   |    TTYPE     | ����        |  0   | = 1 Tab control<br> =2 TRIM tab<br> =3 Both | = 1 ����Ƭ<br /> =2 ��ƽƬ<br> =3 ���� |
| $$(C_{fi})_{tc}$$ |    CFITC     | ����Ƭ�ڲ��ҳ�   |  0   | inboard chord,control tab                | ����Ƭ�ڲ��ҳ�                        |
| $$(C_{fo})_{tc}$$ |    CFOTC     | ����Ƭ����ҳ�   |  0   | outboard chord,control tab               | ����Ƭ����ҳ�                        |
|  $$(b_i)_{tc}$$   |     BITC     | ����Ƭ�ڲ�����λ�� |  0   | inboard span location control tab        | ����Ƭ�ڲ�����λ��                      |
|  $$(b_o)_{tc}$$   |     BOTC     | ����Ƭ�������λ�� |  0   | outboard span location control tab       | ����Ƭ�������λ��                      |
| $$(C_{fi})_{tt}$$ |    CFITT     | ��ƽƬ�ڲ��ҳ�   |  0   | inboard chord,trim tab                   | ��ƽƬ�ڲ��ҳ�                        |
| $$(C_{fo})_{tt}$$ |    CFOTT     | ��ƽƬ����ҳ�   |  0   | outboard chord,trim tab                  | ��ƽƬ����ҳ�                        |
|  $$(b_i)_{tc}$$   |     BITT     | ��ƽƬ�ڲ�����λ�� |  0   | inboard span location trim tab           | ��ƽƬ�ڲ�����λ��                      |
|  $$(b_o)_{tc}$$   |     BOTT     | ��ƽƬ�������λ�� |  0   | outboard span location trim tab          | ��ƽƬ�������λ��                      |
|       $B_1$       |      B1      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $B_2$       |      B2      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $B_3$       |      B3      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $B_4$       |      B4      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $D_1$       |      D1      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $D_2$       |      D2      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $D_3$       |      D3      | ����11      |  0   | see table11 for definitions              | ����11                           |
|   $$G_{cmax}$$    |    CGMAX     | ����11      |  0   | see table11 for definitions              | ����11                           |
|         k         |      KS      | ����11      |  0   | see table11 for definitions              | ����11                           |
|       $R_l$       |      RL      | ����11      |  0   | see table11 for definitions              | ����11                           |
|      $\beta$      |     BGR      | ����11      |  0   | see table11 for definitions              | ����11                           |
|    $\Delta_r$     |     DELR     | ����11      |  0   | see table11 for definitions              | ����11                           |


## Tips1 ��

KS . if the system has a spring ,KS input ,then free stream dynamic pressure is required.

�����������KS����ַ����������������������ѹ.



# �������ľ�������

### 1 $A_C$

$$ A_c=\frac{S_\text{tc}\overline{C}_\text{tc}}{S_c\overline{C}_c}$$

**���У�**

 $S()$��ʾ����� ���ɶ����������Ϊ����֮������.surface area ��movable surface are defined by their area aft of the hinge line��

 $\overline{C}()$��ʾ����ƽ�������ҳ����ɶ����涨�����֮����������$MAC$��ʾ;

 surface mean aerodynamic chord (movable surfaces are defined by their area aft of the hinge line,and the MAC is of that area )

**�±꺬�壺**

$c$ 	��ʾ�������� main control surface

$s$	��ʾ�������� �������� ������ƽβ����β������. 

 surface to which the main control surface is attached��i��e��horizontal tail��vertical tail or wing��

$t_c$  	����Ƭ control tab

$t_t$  	��ƽƬ trim tab



###  2. $B_1$

$$B_1 = \left ( \frac{\partial C_{h_c}}{\partial \delta _{c}} \right )_{\delta _{tc},a_s,\delta _{tt}}   = \left ( C _{h _{\delta} } \right )_{{c}'} ��  1/Deg$$

�μ� 6.1.6.2�� (Datcom Section 6.1.6.2)

## 3. $B_2$

$$B_2 = \left ( \frac{\partial C_{h_c}}{\partial \delta _{tc}} \right )_{\delta _{{c}'{a_s}'},\delta _{tt}}    ,1/Deg$$

�û����� ��user input                   

##  4. $B_3$

$$B_3 = \left ( \frac{\partial C _{h_c}}{\partial \delta _{a_{s}}} \right ) _{\delta _{{c}'} \delta _{t{c}'} \delta _{tt}}    = \left ( C _{h _{a } }\right )_{{c}'}$$

�μ� 6.1.6.1�� (Datcom Section 6.1.6.1)

## 5. $B_4$

$$B_4 = \left ( \frac{\partial C _{h_c}}{\partial \delta _{tt}} \right ) _{\delta _{{c}'} \delta _{t{c}'} a_s} $$  ,$$1/Deg$$

�û����� ��user input   



## 6. $F_c$  

$F_c$      ��������(����Ϊ��)  control-column force (pull force is positive) 

## 7.  $G_{c_{max}}$

$$G _{c _{max}} =\frac {1}{57.3\left ( \frac{\partial x_c}{\partial \delta _c} \right )_{max}}$$

maximum stick gearing user input. 

if $R_L$=0,$G_{c _{max}}$ also is zero.

In this case input $G_{tc_{max}}$ and $\Delta r = 1.0 ( G_{tc_{max}} = G_{c_{max}} * \Delta r )$ .



## 8. K

$k = -\left ( \frac{\partial M _{tc}}{\partial \delta  _{tc}} \right ) _{spring} \frac{1}{S _{tc} \overline c_{tc}}$   tab spring effectiveness

$q$  local dynamic pressure

### 9.  $R_1,R_2$

shorthand notation for tab and main surface hinge moments and key linkage parameters,obtained from Table 12 

###  10.  $R_L$ 

aerodynamic boost link ratio ,user input . $$(R_L \ge 0)$$ .

 To input $R_L = \infty $ , set $R_L <  0$ .

### 11.  $a_s$

angle of attack of the surface to which the main control surface is attached, Deg

���ӿ����������Ĺ��ǣ�Deg

###12.  $\beta$

$$ \beta = \left ( \frac{\partial \delta _{tc}}{\partial \delta _{c}} \right ) _{\begin{matrix}stick\\ free \end{matrix}}$$

 with $k = \infty$  control-tab gear ratio



