# PROPWR  动力定义卡

## 本节主要录入动力的参数

| VariableName | 别名           | 长度限制 | 说明或Tips                                  |
| -----------: | :----------- | :--: | :--------------------------------------- |
|       AIETLP | 引擎推力轴的入射角    |  0   | angle of incidence of engine thrust axis。<br>引擎推力轴的入射角 |
|       NENGSP | 引擎数          |  0   | number of engines(1 or 2)。<br>引擎数        |
|       THSTCP | 推力系数         |  0   | thrus coefficient = 2T/(P∞V∞^2Sref)，<br>推力系数 |
|       PHALOC | 螺旋桨轮毂轴向位置    |  0   | axial location of propeller hub，<br>螺旋桨轮毂轴向位置 |
|       PHVLOC | 螺旋桨轮毂垂直位置    |  0   | vertical location of propeller hub，<br>螺旋桨轮毂垂直位置 |
|       PRPRAD | 螺旋桨半径        |  0   | propeller radius ，<br>螺旋桨半径              |
|       ENGFCT | 经验法向力因子      |  0   | empirical normal force factor ，<br>经验法向力因子 |
|       BWAPR3 | 0.3螺旋桨半径叶片宽度 |  0   | blade width at 0.3 propeller radius ，<br>0.3螺旋桨半径叶片宽度 |
|       BWAPR6 | 0.6螺旋桨半径叶片宽度 |  0   | blade width at 0.6 propeller radius ，<br>0.6螺旋桨半径叶片宽度 |
|       BWAPR9 | 0.9螺旋桨半径叶片宽度 |  0   | blade width at 0.9 propeller radius ，<br>0.9螺旋桨半径叶片宽度 |
|       NOPBPE | 发动机的螺旋桨叶片数   |  0   | number of propeller blades per engine ，<br>发动机的螺旋桨叶片数 |
|       BAPR75 | 0.75螺旋桨半径叶片角 |  0   | blade angle at 0.75 propeller radius ，<br>0.75螺旋桨半径叶片角 |
|           YP | 发动机横向位置      |  0   | lateral location of engine ，<br>发动机横向位置  |
|         CROT | 螺旋桨转向        |  0   | .TRUE. counter rotating propeller 反向螺旋桨; <br>.FALSE. non counter rotating propeller  非对旋螺旋桨 |

## Tips 1：

Kn is not required as input if (bp)'s are input and conversely (bp)'s are not required if Kn is input. (See section 4.6.1 of DATCOM)
<br>kn ; ENGFCT,  (bp)'s  : BWAPR3,BWAPR6,BWAPR9,NOPBPE
<br>Kn 和 (bp)'s 参数不需要同时输入

## Tips 2：

propeller power effect methods are only applicable to longitudinal stability parameters in the subsonic speed regime.
<br>螺旋桨功率效应方法只适用于亚音速情况下的纵向稳定参数。
<br>
<br>
<br>

# JETPWR  喷气动力

<br>

## 参数卡说明

| VariableName | 别名           | 长度限制 | 原文说明或Tips                                | 说明或Tips      |
| :----------: | :----------- | :--: | :--------------------------------------- | :----------- |
|    AIETLJ    | 发动机推力线入射角    |  0   | angle of incidence of engine thrust line. | 发动机推力线入射角    |
|    NENGSJ    | 引擎数          |  0   | number of engines(1 or 2)。               | 引擎数          |
|    THSTCJ    | 推力系数         |  0   | thrus coefficient = 2T/(P∞V∞^2Sref)，     | 推力系数         |
|    JIALOC    | 喷气发动机进气道轴向位置 |  0   | axial location of jet engine inlet。      | 喷气发动机进气道轴向位置 |
|    JEVLOC    | 喷气发动机出口垂直位置  |  0   | vertical location of jet engine exit 。   | 喷气发动机出口垂直位置  |
|    JEALOC    | 喷气发动机出口轴向位置  |  0   | axial location of jet engine exit        | 喷气发动机出口轴向位置  |
|    JINLTA    | 喷气发动机进气道面积   |  0   | jet engine inlet area.                   | 喷气发动机进气道面积   |
|    JEANGL    | 射流出射角        |  0   | jet exit angle                           | 射流出射角        |
|    JEVEJO    | 射流出射速度       |  0   | jet exit velocity                        | 射流出射速度       |
|    AMBTMP    | 环境温度         |  0   | ambient temperature                      | 环境温度         |
|    JESTMP    | 喷口静温         |  0   | jet exit static temperature              | 喷口静温         |
|    JELLOC    | 喷气发动机的横向位置   |  0   | lateral location of jet engine           | 喷气发动机的横向位置   |
|    JETOTP    | 射流出射总压       |  0   | jet exit total pressure                  | 射流出射总压       |
|    AMBSTP    | 环境静压         |  0   | ambient static pressure                  | 环境静压         |
|    JERAD     | 射流出口半径       |  0   | radius of jet exit                       | 射流出口半径       |



## Tips1 ：

<br>

```
Jet power effect methods are only applicable to longitudinal stability parameters in the subsonic speed regime <br>
射流功率效应方法只适用于亚音速的纵向稳定参数 <br>
Jet power inputs are required for externally blown jet FLAP(EBF) configurations.not required pure jet flaps or internally blown flaps (IBF)<br>
喷射功率输入项对于外喷式襟翼(EBF)布局是必须的，对于纯喷气襟翼或内吹襟翼(IBF)不是必须的 <br>
```

