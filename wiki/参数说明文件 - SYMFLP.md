
# NAMELIST SYMFLPѡ� 
##    symetrical flap deflection inputs  �Գƽ���ƫת��������
<br>
## ������

|������   | VariableName|����|��������| Definition|����˵��|
|:-------:|:-------:|:----------------------------|:-:|:-----------------------------------------------------------|:------------------------------------------|
|           | FTYPE  |   �������                 | 0 |  =1.0 plain flaps <br> =2.0 single slotted flaps <br> =3.0 fowler flaps <br> =4.0 double slotted flaps<br> =5.0 split flaps <br> =6.0 leading edge flap <br> =7.0 leading edge slats <br> =8.0 krueger |= 1ƽ��<BR> = 2�������<BR> = 3����Ƥ��<BR> = 4˫�����<BR> = 5�ְ�<BR> = 6ǰԵ����<BR> = 7ǰԵ����<BR> = 8��³�� |
|           | NDELTA |   �����slat��ƫת������   | 0 | number of flap or slat deflection angles ,max 9 | �����slat��ƫת�����������9                        |
| ��f        | DELTA  |   ����ƫת��               | 9 | flap deflection angles measured steamwise       | ������stramwise�������Ľ���ƫת��                  |
| tan(OTE/2)| PHETE  |   ���ͺ�Ե������ֵ         | 0 | tangent of airfoil trailine edge angle based on ordinates at 90 and 99 percent chord |  ���ͺ�Ե������ֵ ��95��99%�ҳ���      | 
| tan(OTE/2)| PHETEP |   ���ͺ�Ե������ֵ         | 0 | tangent of airfoil trailine edge angle based on ordinates at 95 and 99 percent chord |  ���ͺ�Ե������ֵ ��95��99%�ҳ���      | 
| Cfi       | CHRDFI |   �����ڲ������           | 0 | flap chord at inboard end of flap��measured parallel to longitudinal axis            |  ƽ��������Ľ����ڲ������       | 
| Cfo       | CHRDFO |   ������������           | 0 | flap chord at outboard end of flap��measured parallel to longitudinal axis           | ƽ��������Ľ�����������       | 
| bi        | SPANFI |   �����ڲ�˵ĺ������     | 0 | span location of inborad end of flap ,measured perpendicular to vertical plane of symmetry      | �����ڲ�˵ĺ�����룬������ֱ������Գ���        | 
| bo        | SPANFO |   �������˵ĺ������     | 0 | span location of outborad end of flap ,measured perpendicular to vertical plane of symmetry      | �������˵ĺ�����룬������ֱ������Գ���        | 
| Ci'       | CPRMEI |   �����ڲ�˵������ҳ�     | 9 | total wing chord at inboard end of flap (translating devices only) measured parallel to longitudinal axis      | �����ڲ�˵��ҳ�(translating devices only)  ����ƽ������������        | 
| Co'       | CPRMEO |   �������˵������ҳ�     | 9 | total wing chord at outboard end of flap (translating devices only) measured parallel to longitudinal axis     | �������˵��ҳ�(translating devices only)  ����ƽ������������        | 
| C'ai      | CAPINB |                            | 9 |      |  ˫�������Ҫ      | 
| Cao       | CAPOUT |                            | 9 |      |        | 
| (��f)2     | DOBDEF |                            | 9 |      |        | 
| C2f       | DOBCIN |                            | 0 |      |        | 
| C2o       | DOBCOT |                            | 0 |      |        | 
| ��Cf       | SCLD   |   ��������ϵ������         | 9 | increment in section lift coefficient due to deflecting flap to the angle ��f    | ƫת�����f�������������ϵ������        | 
| ��Cmf      | SCMD   |   ���温������ϵ������     | 9 | increment in section pitching moment coefficient due to deflecting flap to the angle ��f    | ƫת�����f�����������������ϵ������        | 
| Cb        | CB     |   ƽ���ƽ���ҳ�           | 0 | average chord of the balance             | ƽ���ƽ���ҳ�        | 
| tc        | TC     |   ������ƽ�����           | 0 | average thickness of the control at hinge line             | �����߿��Ƶ�ƽ�����        | 
|           | NTYPE  |   ����                     | 0 | =1.0 round nose flap <br> =2.0 elliptic nose flap <br> =3.0 sharp nose flap         | ��1.0 Բ�αǽ���<br> ��2.0 ��Բ�αǽ���<br> ��3.0 ��ǽ���         | 
|           | JETFLP |   ������������             | 0 | =1.0 pure jet flap <br> =2.0 IBF <br> =3.0 EBF <br> =4.0 combination mechanical and pure jet flap   | =1.0 ���������� <br> =2.0 IBF <br> =3.0 EBF <br> =4.0 ��е������Ͻ���       | 
| C��        | CMU    |   ��ά����ϵ��             | 0 | two dimensional jet efflux coefficient    | ��ά����ϵ��     | 
| ��j        | DELJET |   ����ƫת�Ƕ�             | 9 | jet deflection angle    | ����ƫת�Ƕ�     | 
| ��jeff     | EFFJET |   EBF��Ч����ƫת�Ƕ�      | 9 | EBF effective jet deflection angle    | EBF��Ч����ƫת�Ƕ�      | 

### Tips 1����ע1��ѡ������������ͽ����ǿ�ѡ��
<br><br>
### Tips 2: mechanical flap type if jetflp =4 .<br>	��jetflp =4��Ϊ��е����


