
# NAMELIST SYMFLP选项卡 
##    symetrical flap deflection inputs  对称襟翼偏转的输入项
<br>
## 参数表

|工程量   | VariableName|别名|长度限制| Definition|参数说明|
|:-------:|:-------:|:----------------------------|:-:|:-----------------------------------------------------------|:------------------------------------------|
|           | FTYPE  |   襟翼类别                 | 0 |  =1.0 plain flaps <br> =2.0 single slotted flaps <br> =3.0 fowler flaps <br> =4.0 double slotted flaps<br> =5.0 split flaps <br> =6.0 leading edge flap <br> =7.0 leading edge slats <br> =8.0 krueger |= 1平瓣<BR> = 2单缝襟翼<BR> = 3福勒皮瓣<BR> = 4双缝襟翼<BR> = 5分瓣<BR> = 6前缘襟翼<BR> = 7前缘缝翼<BR> = 8克鲁格 |
|           | NDELTA |   襟翼或slat的偏转角数量   | 0 | number of flap or slat deflection angles ,max 9 | 襟翼或slat的偏转角数量，最大9                        |
| δf        | DELTA  |   襟翼偏转角               | 9 | flap deflection angles measured steamwise       | 从流向（stramwise）测量的襟翼偏转角                  |
| tan(OTE/2)| PHETE  |   翼型后缘角正切值         | 0 | tangent of airfoil trailine edge angle based on ordinates at 90 and 99 percent chord |  翼型后缘角正切值 在95和99%弦长处      | 
| tan(OTE/2)| PHETEP |   翼型后缘角正切值         | 0 | tangent of airfoil trailine edge angle based on ordinates at 95 and 99 percent chord |  翼型后缘角正切值 在95和99%弦长处      | 
| Cfi       | CHRDFI |   襟翼内侧襟翼弦           | 0 | flap chord at inboard end of flap，measured parallel to longitudinal axis            |  平行于纵轴的襟翼内侧襟翼弦       | 
| Cfo       | CHRDFO |   襟翼外侧襟翼弦           | 0 | flap chord at outboard end of flap，measured parallel to longitudinal axis           | 平行于纵轴的襟翼外侧襟翼弦       | 
| bi        | SPANFI |   襟翼内侧端的横向距离     | 0 | span location of inborad end of flap ,measured perpendicular to vertical plane of symmetry      | 襟翼内侧端的横向距离，测量垂直于纵向对称面        | 
| bo        | SPANFO |   襟翼外侧端的横向距离     | 0 | span location of outborad end of flap ,measured perpendicular to vertical plane of symmetry      | 襟翼外侧端的横向距离，测量垂直于纵向对称面        | 
| Ci'       | CPRMEI |   襟翼内侧端的翼型弦长     | 9 | total wing chord at inboard end of flap (translating devices only) measured parallel to longitudinal axis      | 襟翼内侧端的弦长(translating devices only)  测量平行于纵向轴线        | 
| Co'       | CPRMEO |   襟翼外侧端的翼型弦长     | 9 | total wing chord at outboard end of flap (translating devices only) measured parallel to longitudinal axis     | 襟翼外侧端的弦长(translating devices only)  测量平行于纵向轴线        | 
| C'ai      | CAPINB |                            | 9 |      |  双缝襟翼需要      | 
| Cao       | CAPOUT |                            | 9 |      |        | 
| (δf)2     | DOBDEF |                            | 9 |      |        | 
| C2f       | DOBCIN |                            | 0 |      |        | 
| C2o       | DOBCOT |                            | 0 |      |        | 
| ΔCf       | SCLD   |   截面升力系数增量         | 9 | increment in section lift coefficient due to deflecting flap to the angle δf    | 偏转襟翼δf产生的面积升力系数增量        | 
| ΔCmf      | SCMD   |   截面俯仰力矩系数增量     | 9 | increment in section pitching moment coefficient due to deflecting flap to the angle δf    | 偏转襟翼δf产生的面积俯仰力矩系数增量        | 
| Cb        | CB     |   平衡的平均弦长           | 0 | average chord of the balance             | 平衡的平均弦长        | 
| tc        | TC     |   铰链处平均厚度           | 0 | average thickness of the control at hinge line             | 铰链线控制的平均厚度        | 
|           | NTYPE  |   鼻型                     | 0 | =1.0 round nose flap <br> =2.0 elliptic nose flap <br> =3.0 sharp nose flap         | ＝1.0 圆形鼻襟翼<br> ＝2.0 椭圆形鼻襟翼<br> ＝3.0 尖鼻襟翼         | 
|           | JETFLP |   喷气襟翼类型             | 0 | =1.0 pure jet flap <br> =2.0 IBF <br> =3.0 EBF <br> =4.0 combination mechanical and pure jet flap   | =1.0 纯喷气襟翼 <br> =2.0 IBF <br> =3.0 EBF <br> =4.0 机械喷气组合襟翼       | 
| Cμ        | CMU    |   二维射流系数             | 0 | two dimensional jet efflux coefficient    | 二维射流系数     | 
| δj        | DELJET |   射流偏转角度             | 9 | jet deflection angle    | 射流偏转角度     | 
| δjeff     | EFFJET |   EBF等效射流偏转角度      | 9 | EBF effective jet deflection angle    | EBF等效射流偏转角度      | 

### Tips 1：标注1的选项对于所有类型襟翼是可选的
<br><br>
### Tips 2: mechanical flap type if jetflp =4 .<br>	在jetflp =4是为机械瓣型


