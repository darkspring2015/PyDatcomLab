

# 气动参数表

###  适用：WGSCHR、HTSCHR、VTSCHR、VFSCHR

| VariableName | 别名             | 长度限制 | 说明或Tips                                  |
| -----------: | :------------- | :--: | :--------------------------------------- |
|         TOVC | 最大机翼截面厚度       |  0   | maximum airfoil section thickness ,fraction of chord <br> 最大机翼截面厚度  最大机翼厚度是弦长的分数 |
|       DELTAY | 6.0到.15%弦长处翼型差 |  0   | difference between airfoil ordinates at 6.0 AND .15% chord ,percent chord <br> 6.0到.15%弦长处翼型差 |
|         XOVC | 最大机翼厚度的纵坐标     |  0   | chord location of maximum airfoil thickness, fraction of chord  <br> 最大机翼厚度的纵坐标 |
|          CLI | 翼型设计升力系数       |  0   | airfoil section design lift coefficient  <br>翼型设计升力系数 |
|       ALPHAI | 设计攻角           |  0   | angle of attack at section design lift coefficient,edg <br>翼型设计升力系数对应的攻角 |
|       CLALPA | 翼型升力曲线         |  20  | airfoil section lift curve slope dCl/dα ,per DEG<br> 翼型升力曲线 斜率 dCl/dα 每度 |
|        CLMAX | 翼型最大升力系数       |  20  | airfoil section maximum lift coefficient        <br>  翼型最大升力系数 |
|      CMO CM0 | 截面零升俯仰力矩系数     |  0   | section zero lift pitching moment coefficient   <br>  截面零升俯仰力矩系数 |
|         LERI | 翼型前缘半径         |  0   | airfoil leading edge radius fraction of chard    <br> 翼型前缘半径 ，参考弦长 |
|         LERO | 舷外板RLE         |  0   | RLE for outboard panel  fraction of chard    <br>  舷外板RLE 参考弦长 |
|       CAMBER | 弧形翼型截面标志       |  0   | cambered airfoil section flag <br>弧形翼型截面标志 |
|        TOVCO |                |  0   | t/c for outboard panel                   |
|        XOVCO |                |  0   | (x/c)max for outboard panel              |
|    CMOT CM0T |                |  0   | Cmo for outboard panel                   |
|       CLMAXL |                |  0   | airfoil maximum lift coefficient at MACH equal Zero |
|  CLAMO CLAM0 |                |  0   | airfoil section lift curve slope at MACH equal Zero |
|        TCEFF |                |  0   | planform effective thickness ratio，fraction of chord |
|       KSHARP | 波阻系数           |  0   | wave drag factor for sharp-nosed airfoil section，not input for round nosed airfoil |
|        SLOPE |                |  0   | airfoil surface slope at 0 ，20,40,60,80,100% chord ，DEG. <br> Positive when the tangent intersects the chord plane forward of the reference chord point <br>只有Supersonic下使用NACA CARD的翼型计算 |
|         ARCL |                |  0   | aspect ratio classification              |
|          XAC |                |  20  | section aerodynamic center，fraction of chord (see vol II for default) |
|        DWASH |                |  0   | subsonic downwash method flag <br>   =1. use datcom method 1  <br>  =2. use datcom method 2  <br>  =3. use datcom method 3  <br>  Supersonic ,use datcom method 2 if dwash =1 or 2 (see figure 9) |
|          YCM |                |  0   | airfoil maximum camber， fraction of chord |
|          CLD |                |  0   | 0.0 conical camber design lift coefficient for M=1.0 Design. <br> See-NACA RM A55G19.(Default To 0.0) |
|       TYPEIN | 翼型截面坐标模式       |  0   | type of airfoil section coordinate input for airfoil section module <br>     = 1.0 upper and lower surface coordinates(Y Upper and Y Lower) <br>    = 2.0 mean line and thickness distribution (mean and thick) |
|         NPTS | 截面点数           |  50  | number of section point input,Max =50    |
|        XCORD | 输入点横坐标         |  50  | abscissas of input points，<br>  TYPEIN =1.0 or 2.0 ,XCORD（1）=0.0，XCORD（NPTS）=1.0 |
|       YUPPER | 上表面            |  50  | ordinates of upper surface. <br>  TYPEIN =1.0 FRACTION OF CHORD ,and requires YUPPER(1)=0.0 YUPPER(NPTS)=0.0 |
|       YLOWER | 下表面            |  50  | ordinates of lower surface. <br>   TYPEIN =1.0 FRACTION OF CHORD ,and requires YLOWER(1)=0.0 YLOWER(NPTS)=0.0 |
|         MEAN | 中线             |  50  | ordinates of mean line .TYPEIN =2.0 <br>  FRACTION OF CHORD ,and requires MEAN(1)=0.0 MEAN(NPTS)=0.0 |
|        THICK | 厚度             |  50  | thikness distribution .TYPEIN =2.0 <br>  FRACTION OF CHORD ,and requires THICK(1)=0.0 THICK(NPTS)=0.0 |

## 需要注意的是:

速度域和攻角数对于后续数据的录入具有很大的影响作用