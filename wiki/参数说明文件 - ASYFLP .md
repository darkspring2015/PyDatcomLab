
# NAMELIST ASYFLPѡ� 
##   asymmetrical  control deflection input  �����ƫת��������
<br>
## ������

|������   | VariableName|����|��������| Definition|����˵��|
|:-------:|:-------:|:----------------------------|:-:|:-----------------------------------------------------------|:------------------------------------------|
|           | STYPE  |   ���                     | 0 |  =1.0 flap spoiler on wing<br> =2.0 plus spoiler on wing<br> =3.0 spoiler-slot-deflection on wing<br> =4.0 plain flap aileron<br> =5.0 differentially deflected moveable horizontal tail | =1.0 ��������<BR> =2.0 �������� <BR> =3.0 �������ƫת�� <BR> =4.0 �򵥽����� <BR> =5.0 �ƽβ |           | NDELTA |   �����slat��ƫת������   | 0 | number of flap or slat deflection angles ,max 9 | �����slat��ƫת�����������9                        |
|           | NDELTA |   ������ƫת������         | 0 | number of control deflection angles��required for all controls��max.of 9       |  ������ƫת������                  |
| bi        | SPANFI |   �ڲ�˵ĺ������         | 0 | span location of inborad end of flap or spoiler control.<br>measured perpendicular to vertical plane of symmetry      | �ڲ�˵ĺ�����룬������ֱ������Գ���        | 
| bo        | SPANFO |   �������˵ĺ������     | 0 | span location of outborad end of flap or spoiler control.<br>measured to perpendicular to vertical plane of symmetry      | ���˵ĺ�����룬������ֱ������Գ���        | 
| tan(OTE/2)| PHETE  |   ���ͺ�Ե������ֵ(90,99)  | 0 | tangent of airfoil trailine edge angle based on ordinates at 90 and 99 percent chord |  ���ͺ�Ե������ֵ ��90��99%�ҳ���      | 
| ��L        | DELTAL |   ��ƫת��                 | 9 | deflection angles for left hand plain flap aileron or left hand panel all moveable horizontal tail<br> measured in vertical plane of symmetry  | ���򵥽�������ȫ��ƽβ��ƫת�Ƕ�<br>�ڴ�ֱ�Գ����ڲ���      |
| ��R        | DELTAR |   ��ƫת��                 | 9 | deflection angles for right hand plain flap aileron or right hand panel all moveable horizontal tail<br> measured in vertical plane of symmetry  | �Ҳ�򵥽�����Ҳ�ȫ��ƽβ��ƫת�Ƕ�<br>�ڴ�ֱ�Գ����ڲ���      |
| Cfi       | CHRDFI |   �ڲ��ҳ�                 | 0 | aileron chord at inboard end of plain flap aileron ��measured parallel to longitudinal axis            | �򵥽������ڲ�˵ĸ����ҳ�<br>����ƽ��������       | 
| Cfo       | CHRDFO |   ����ҳ�                 | 0 | aileron chord at outboard end of plain flap aileron��measured parallel to longitudinal axis           | �򵥽��������˵ĸ����ҳ�<br>����ƽ��������        | 
| ��d/c      | DELTAD |   ƫת��ͶӰ�߶�           | 9 | projected height of deflector ,spoiler-slot-deflector control;fraction of chord  | ƫת����ͶӰ�߶ȣ��������ƫת����<br>�ҳ��ķ���      |
| ��s/c      | DELTAS |   ������ͶӰ�߶�           | 9 | projected height of spoiler,flap spoiler,plug spoiler and spoiler-slot deflector control��fraction of chord   | �����塢���������塢���������������ƫת����Ƶ�Ͷ��߶�<br>�ҳ��ķ���     |
| Xs/c      | XSOC   |   �����忪�ڵ�����ǰ�ӵľ���   | 9 | distance from wing leading edge to spoiler lip measured parallel to streamwise wing chord,flap and plug spoilers.<br>fraction of chord   | �����忪�ڵ�����ǰԵ�ľ��룬ƽ�����������ҡ����������������в���<br>�ҳ��ķ���     |
| X's/c     | XSPRME |   ���������������ǰ�ӵľ���   | 0 | distance from wing leading edge to spoiler hinge line  measured parallel to streamwise wing chord,flap spoiler, plug spoiler and spoiler-slot deflector control.<br>fraction of chord   | ����������ߵ�����ǰԵ�ľ��룬ƽ�����������ҡ����������塢���������������ƫת���ƽ��в���<br>�ҳ��ķ���     |
| hs/c      | HSOC   |   �����嵽�������ߵ�ͶӰ�߶�   | 9 | projected height of spoiler measured from and normal to airfoil mean line, flap spoiler,plug spoiler and spoiler-slot reflector.<br>fraction of chord   | �����嵽�������ߵ�ͶӰ�߶ȣ����������塢���������������ƫת���ƽ��в���<br>�ҳ��ķ���     |





