

# NAMELIST GRNDEF 地效
## 参数卡说明 

| VariableName | 别名        | 长度限制 | 原文说明或Tips                                | 说明或Tips               |
| :----------: | :-------- | :--: | :--------------------------------------- | :-------------------- |
|     NGH      | 地面高度的计算点数 |  0   | number of ground heights to be run      . | 参与计算的地面高度点数           |
|    GRDHT     | 地面高度值     |  0   | values of ground heights. ground heights equal altitude of REF.PLANE relative to Ground。 | 地面高度值。地面高度指参考平面到地面的告诉 |



# NAMELIST TVTPAN  双垂面输入
## 参数卡说明 
<br> effects of twin vertical panels only reflected in subsoinc lateral stabillty results
<br> 双垂直板影响仅体现在亚音速横向稳定性结果


| VariableName | 别名             | 长度限制 | 原文说明或Tips                                | 说明或Tips                              |
| :----------: | :------------- | :--: | :--------------------------------------- | :----------------------------------- |
|     BVP      | 升力面之上的垂直面跨度    |  0   | vertical panel span above lifting surface . | 垂直面在升力面之上的高度                         |
|      BV      | 垂直面跨度          |  0   | vertical panel span                      | 垂直面高度                                |
|     BDV      | 垂直面四分弦长点处的机身深度 |  0   | fuselage depth at quarter chord point of vertical panel mean aerodynamic chord | 垂直面平均气动弦长的四分之一点处的机身深度                |
|      BH      | 垂直面间的距离        |  0   | distance between vertical panels         | 垂直面间的距离                              |
|      SV      | 单垂直面设计型面积      |  0   | plan form area of one vertical panel     | 单垂直面设计型面积                            |
|    VPHITE    | 垂直面翼型截面总后缘角    |  0   | total trailing edge angle of vertical panel airfoil section | 垂直板翼型截面总后缘角                          |
|     VLP      | 四分弦长点到CG的纵向距离  |  0   | distance parallel to long.axis between the CG and the quarter chord point of the mac of the panel.<br> positive if aft of CG. | 垂直面四分之一气动弦长处联线到气动中心的垂直距离.<br> 中心之后为正 |
|      ZP      | 四分弦长点到CG的垂向距离  |  0   | distance in the Z direction between the CG and the mac of the panel,<br> positive for panel above CG. | 垂直面四分之一气动弦长处联线到气动中心的Z向距离.<br> 中心之上为正 |
