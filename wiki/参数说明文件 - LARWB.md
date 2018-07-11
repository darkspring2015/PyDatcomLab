
# NAMELIST LARWB ѡ� 
## Input for NAMELIST LARWB - low aspect ratio wing ,wing-body input
## LARWB������:�����ұȻ��������ں���

### Sharp leading edge ʾ��ͼ
![Sharp leading edge](fig/LARWB-01.png) 
<br>
### Round leading edge ʾ��ͼ
![Round leading edge](fig/LARWB-02.png) 
<br>
### ��������ʾ��ͼ
![��������ʾ��ͼ](fig/LARWB-03.png) 
<br>
<br>
## ������

|������   | VariableName|����|��������| Definition|����˵��|
|:-------:|:-------:|:----------------------------|:-:|:-----------------------------------------------------------|:------------------------------------------|
| Zbase   | ZB      |   ���Ĵ�ֱ����              | 0 | vertical distance between centroid of base area and body ref plane | ��׼�����ĺͻ���ο���Ļ��Ĵ�ֱ���� |          
| S       | SREF    |   ƽ�����                  | 0 | planform area used as reference area      |  ƽ������������ο����                  |
| ��e��    | DELTEP  |   ��ǰԵ����                | 0 | sharp leading edge parameter              |  ����ǰԵ����                  |
| Sf      | SFRONT  |   ���ͶӰ�����(0������)   | 0 | projected frontal area perpendicular to zero normal force ref plane        | ��ֱ���㷨�����ο����ͶӰ�������                  |
| A       | AR      |   �����ݺ��                | 0 | aspect ratio of surface                | �����ݺ��                  |
| (R1/3 LE)/b   | R3LEOB   |   ԲǰԵ����         | 0 | round leading edge parameter           | ԲǰԵ����                  |
| ��L      | DELTAL  |   ԲǰԵ����                | 0 | round leading edge parameter           | ԲǰԵ����                  |
| LB      | L       |   ������                  | 0 | length of body used as longitudinal ref length           | �����ȣ���������ο�����               |
| Swet    | SWET    |   ʪ��                      | 0 | wetted area��excluding base area       | ʪ��������������               |
| P       | PERBAS  |   �����ܳ�                  | 0 | perimeter of base                      | �����ܳ�               |
| Sb      | SBASE   |   �������                  | 0 | base area                              | �������               |
| hb      | HB      |   �������߶�              | 0 | maximum height of base                 | �������߶�               |
| bb      | BB      |   ���������              | 0 | maximum span of base used as lateral ref length                | ��������ȣ���������ο�����               |
| BASE loaction designator   | BLF      |   �ײ����������ϵ              | 0 | .TRUE. Portions of base are aft of NON-lifting surface<br>  .FALSE. Total base aft of lifing surface      | .TRUE. ���ֵײ��ڷ�������ĺ�<br>  .FALSE. �����ײ����������             |
| Xm      | XCG     |   �����������              | 0 | longitudinal location of CG from nose                 | ����������룬�ӻ�ͷ����               |
| ��       | THETAD  |   ����붥��                | 0 | wing semi-apex angle                   | ����붥��               |
| NOSE bluntness designator       | ROUNDN  |   ����                | 0 | .TRUE. rounded nose<br>  .FALSE. pointed nose      | .TRUE. Բ��<br>  .FALSE. ���             |
| Sbs     | SBS     |   ͶӰ�����                | 0 | projected side area of configuration                    | ���͵�ͶӰ�����               |
| (SBS).2LB   | SBSLB     |   ǰ20%�����ͶӰ�����        | 0 | projected side area of configuration forward of .2LB               | ǰ20%�����ͶӰ�����               |
| XcentroidSBS  | XCENSB  |   ͶӰ�������ĵ���ͷ�ľ���     | 0 | distance from nose of vehicle to centroid of projected side area   | �ӷ������ǲ���ͶӰ��������ĵľ���               |
| XcentroidW    | XCENW   |   ͶӰƽ�����ĵ����ͱǲ��ľ��� | 0 | distance from nose of configuration to centroid of projected plain area   | �ӹ��ͱǲ���ͶӰƽ���������ĵľ���               |


### ������
![������](fig/LARWB-para.png) 
<br>



