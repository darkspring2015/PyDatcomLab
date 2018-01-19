
# NAMELIST ASYFLP选项卡 
##   asymmetrical  control deflection input  差动控制偏转的输入项
<br>
## 参数表

|工程量   | VariableName|别名|长度限制| Definition|参数说明|
|:-------:|:-------:|:----------------------------|:-:|:-----------------------------------------------------------|:------------------------------------------|
|           | STYPE  |   类别                     | 0 |  =1.0 flap spoiler on wing<br> =2.0 plus spoiler on wing<br> =3.0 spoiler-slot-deflection on wing<br> =4.0 plain flap aileron<br> =5.0 differentially deflected moveable horizontal tail | =1.0 瓣扰流翼<BR> =2.0 加扰流翼 <BR> =3.0 扰流板槽偏转翼 <BR> =4.0 简单襟翼副翼 <BR> =5.0 差动平尾 |           | NDELTA |   襟翼或slat的偏转角数量   | 0 | number of flap or slat deflection angles ,max 9 | 襟翼或slat的偏转角数量，最大9                        |
|           | NDELTA |   控制面偏转角数量         | 0 | number of control deflection angles；required for all controls，max.of 9       |  控制面偏转角数量                  |
| bi        | SPANFI |   内侧端的横向距离         | 0 | span location of inborad end of flap or spoiler control.<br>measured perpendicular to vertical plane of symmetry      | 内侧端的横向距离，测量垂直于纵向对称面        | 
| bo        | SPANFO |   襟翼外侧端的横向距离     | 0 | span location of outborad end of flap or spoiler control.<br>measured to perpendicular to vertical plane of symmetry      | 外侧端的横向距离，测量垂直于纵向对称面        | 
| tan(OTE/2)| PHETE  |   翼型后缘角正切值(90,99)  | 0 | tangent of airfoil trailine edge angle based on ordinates at 90 and 99 percent chord |  翼型后缘角正切值 在90和99%弦长处      | 
| δL        | DELTAL |   左偏转角                 | 9 | deflection angles for left hand plain flap aileron or left hand panel all moveable horizontal tail<br> measured in vertical plane of symmetry  | 左侧简单襟副翼或左侧全动平尾的偏转角度<br>在垂直对称面内测量      |
| δR        | DELTAR |   右偏转角                 | 9 | deflection angles for right hand plain flap aileron or right hand panel all moveable horizontal tail<br> measured in vertical plane of symmetry  | 右侧简单襟副翼或右侧全动平尾的偏转角度<br>在垂直对称面内测量      |
| Cfi       | CHRDFI |   内侧弦长                 | 0 | aileron chord at inboard end of plain flap aileron ，measured parallel to longitudinal axis            | 简单襟翼副翼内侧端的副翼弦长<br>测量平行于纵轴       | 
| Cfo       | CHRDFO |   外侧弦长                 | 0 | aileron chord at outboard end of plain flap aileron，measured parallel to longitudinal axis           | 简单襟翼副翼外侧端的副翼弦长<br>测量平行于纵轴        | 
| δd/c      | DELTAD |   偏转器投影高度           | 9 | projected height of deflector ,spoiler-slot-deflector control;fraction of chord  | 偏转器的投影高度，扰流板槽偏转控制<br>弦长的分数      |
| δs/c      | DELTAS |   扰流板投影高度           | 9 | projected height of spoiler,flap spoiler,plug spoiler and spoiler-slot deflector control；fraction of chord   | 扰流板、襟翼扰流板、塞扰流板和扰流槽偏转板控制的投射高度<br>弦长的分数     |
| Xs/c      | XSOC   |   扰流板开口到机翼前延的距离   | 9 | distance from wing leading edge to spoiler lip measured parallel to streamwise wing chord,flap and plug spoilers.<br>fraction of chord   | 扰流板开口到机翼前缘的距离，平行于流向翼弦、襟翼和塞扰流板进行测量<br>弦长的分数     |
| X's/c     | XSPRME |   扰流板铰链到机翼前延的距离   | 0 | distance from wing leading edge to spoiler hinge line  measured parallel to streamwise wing chord,flap spoiler, plug spoiler and spoiler-slot deflector control.<br>fraction of chord   | 扰流板铰链线到机翼前缘的距离，平行于流向翼弦、襟翼扰流板、塞扰流板和扰流槽偏转控制进行测量<br>弦长的分数     |
| hs/c      | HSOC   |   扰流板到翼型中线的投影高度   | 9 | projected height of spoiler measured from and normal to airfoil mean line, flap spoiler,plug spoiler and spoiler-slot reflector.<br>fraction of chord   | 扰流板到翼型中线的投影高度，襟翼扰流板、塞扰流板和扰流槽偏转控制进行测量<br>弦长的分数     |





