# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\BODY.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName("Form")
        Form.resize(1121, 569)
        self.groupBox_Sup_Hyp = QtWidgets.QGroupBox(Form)
        self.groupBox_Sup_Hyp.setGeometry(QtCore.QRect(10, 230, 331, 166))
        self.groupBox_Sup_Hyp.setObjectName("groupBox_Sup_Hyp")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout(self.groupBox_Sup_Hyp)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.label_BNOSE = QtWidgets.QLabel(self.groupBox_Sup_Hyp)
        self.label_BNOSE.setObjectName("label_BNOSE")
        self.horizontalLayout_4.addWidget(self.label_BNOSE)
        spacerItem = QtWidgets.QSpacerItem(48, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem)
        self.comboBox_BNOSE = QtWidgets.QComboBox(self.groupBox_Sup_Hyp)
        self.comboBox_BNOSE.setObjectName("comboBox_BNOSE")
        self.comboBox_BNOSE.addItem("")
        self.comboBox_BNOSE.addItem("")
        self.horizontalLayout_4.addWidget(self.comboBox_BNOSE)
        self.verticalLayout_2.addLayout(self.horizontalLayout_4)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.label_BTAIL = QtWidgets.QLabel(self.groupBox_Sup_Hyp)
        self.label_BTAIL.setObjectName("label_BTAIL")
        self.horizontalLayout_5.addWidget(self.label_BTAIL)
        spacerItem1 = QtWidgets.QSpacerItem(48, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem1)
        self.comboBox_BTAIL = QtWidgets.QComboBox(self.groupBox_Sup_Hyp)
        self.comboBox_BTAIL.setObjectName("comboBox_BTAIL")
        self.comboBox_BTAIL.addItem("")
        self.comboBox_BTAIL.addItem("")
        self.horizontalLayout_5.addWidget(self.comboBox_BTAIL)
        self.verticalLayout_2.addLayout(self.horizontalLayout_5)
        self.horizontalLayout_6 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_6.setObjectName("horizontalLayout_6")
        self.label_BLN = QtWidgets.QLabel(self.groupBox_Sup_Hyp)
        self.label_BLN.setObjectName("label_BLN")
        self.horizontalLayout_6.addWidget(self.label_BLN)
        spacerItem2 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem2)
        self.lineEdit_BLN = QtWidgets.QLineEdit(self.groupBox_Sup_Hyp)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.lineEdit_BLN.sizePolicy().hasHeightForWidth())
        self.lineEdit_BLN.setSizePolicy(sizePolicy)
        self.lineEdit_BLN.setObjectName("lineEdit_BLN")
        self.horizontalLayout_6.addWidget(self.lineEdit_BLN)
        self.verticalLayout_2.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_7 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_7.setObjectName("horizontalLayout_7")
        self.label_BLA = QtWidgets.QLabel(self.groupBox_Sup_Hyp)
        self.label_BLA.setObjectName("label_BLA")
        self.horizontalLayout_7.addWidget(self.label_BLA)
        self.lineEdit_BLA = QtWidgets.QLineEdit(self.groupBox_Sup_Hyp)
        self.lineEdit_BLA.setObjectName("lineEdit_BLA")
        self.horizontalLayout_7.addWidget(self.lineEdit_BLA)
        self.verticalLayout_2.addLayout(self.horizontalLayout_7)
        self.horizontalLayout_8 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_8.setObjectName("horizontalLayout_8")
        self.checkBox_DS = QtWidgets.QCheckBox(self.groupBox_Sup_Hyp)
        self.checkBox_DS.setObjectName("checkBox_DS")
        self.horizontalLayout_8.addWidget(self.checkBox_DS)
        spacerItem3 = QtWidgets.QSpacerItem(58, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_8.addItem(spacerItem3)
        self.lineEdit_DS = QtWidgets.QLineEdit(self.groupBox_Sup_Hyp)
        self.lineEdit_DS.setObjectName("lineEdit_DS")
        self.horizontalLayout_8.addWidget(self.lineEdit_DS)
        self.verticalLayout_2.addLayout(self.horizontalLayout_8)
        self.groupBox_Sub = QtWidgets.QGroupBox(Form)
        self.groupBox_Sub.setGeometry(QtCore.QRect(10, 100, 338, 110))
        self.groupBox_Sub.setObjectName("groupBox_Sub")
        self.verticalLayout_3 = QtWidgets.QVBoxLayout(self.groupBox_Sub)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_3 = QtWidgets.QLabel(self.groupBox_Sub)
        self.label_3.setObjectName("label_3")
        self.horizontalLayout_3.addWidget(self.label_3)
        spacerItem4 = QtWidgets.QSpacerItem(28, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem4)
        self.comboBox_VarCombo = QtWidgets.QComboBox(self.groupBox_Sub)
        self.comboBox_VarCombo.setObjectName("comboBox_VarCombo")
        self.comboBox_VarCombo.addItem("")
        self.comboBox_VarCombo.addItem("")
        self.comboBox_VarCombo.addItem("")
        self.horizontalLayout_3.addWidget(self.comboBox_VarCombo)
        self.verticalLayout_3.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_12 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_12.setObjectName("horizontalLayout_12")
        self.checkBox_ELLIP = QtWidgets.QCheckBox(self.groupBox_Sub)
        self.checkBox_ELLIP.setObjectName("checkBox_ELLIP")
        self.horizontalLayout_12.addWidget(self.checkBox_ELLIP)
        spacerItem5 = QtWidgets.QSpacerItem(88, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_12.addItem(spacerItem5)
        self.ELLIP = QtWidgets.QLineEdit(self.groupBox_Sub)
        self.ELLIP.setObjectName("ELLIP")
        self.horizontalLayout_12.addWidget(self.ELLIP)
        self.verticalLayout_3.addLayout(self.horizontalLayout_12)
        self.horizontalLayout_9 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_9.setObjectName("horizontalLayout_9")
        self.label_8 = QtWidgets.QLabel(self.groupBox_Sub)
        self.label_8.setObjectName("label_8")
        self.horizontalLayout_9.addWidget(self.label_8)
        spacerItem6 = QtWidgets.QSpacerItem(13, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_9.addItem(spacerItem6)
        self.comboBox_METHED = QtWidgets.QComboBox(self.groupBox_Sub)
        self.comboBox_METHED.setObjectName("comboBox_METHED")
        self.comboBox_METHED.addItem("")
        self.comboBox_METHED.addItem("")
        self.horizontalLayout_9.addWidget(self.comboBox_METHED)
        self.verticalLayout_3.addLayout(self.horizontalLayout_9)
        self.label_3.raise_()
        self.Tab_ComboVariables = QtWidgets.QTableWidget(Form)
        self.Tab_ComboVariables.setGeometry(QtCore.QRect(360, 0, 331, 571))
        self.Tab_ComboVariables.setObjectName("Tab_ComboVariables")
        self.Tab_ComboVariables.setColumnCount(0)
        self.Tab_ComboVariables.setRowCount(0)
        self.layoutWidget = QtWidgets.QWidget(Form)
        self.layoutWidget.setGeometry(QtCore.QRect(710, 10, 664, 551))
        self.layoutWidget.setObjectName("layoutWidget")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.layoutWidget)
        self.verticalLayout.setContentsMargins(0, 0, 0, 0)
        self.verticalLayout.setObjectName("verticalLayout")
        self.image_BODY = QtWidgets.QLabel(self.layoutWidget)
        self.image_BODY.setText("")
        self.image_BODY.setPixmap(QtGui.QPixmap(":/card/rc_card/Body_SubSonic.png"))
        self.image_BODY.setScaledContents(True)
        self.image_BODY.setObjectName("image_BODY")
        self.verticalLayout.addWidget(self.image_BODY)
        self.plainTextEdit = QtWidgets.QPlainTextEdit(self.layoutWidget)
        self.plainTextEdit.setObjectName("plainTextEdit")
        self.verticalLayout.addWidget(self.plainTextEdit)
        self.layoutWidget1 = QtWidgets.QWidget(Form)
        self.layoutWidget1.setGeometry(QtCore.QRect(20, 30, 311, 22))
        self.layoutWidget1.setObjectName("layoutWidget1")
        self.horizontalLayout = QtWidgets.QHBoxLayout(self.layoutWidget1)
        self.horizontalLayout.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtWidgets.QLabel(self.layoutWidget1)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        spacerItem7 = QtWidgets.QSpacerItem(180, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem7)
        self.comboBox_SpeedRegime = QtWidgets.QComboBox(self.layoutWidget1)
        self.comboBox_SpeedRegime.setObjectName("comboBox_SpeedRegime")
        self.comboBox_SpeedRegime.addItem("")
        self.comboBox_SpeedRegime.addItem("")
        self.comboBox_SpeedRegime.addItem("")
        self.comboBox_SpeedRegime.addItem("")
        self.horizontalLayout.addWidget(self.comboBox_SpeedRegime)
        self.layoutWidget2 = QtWidgets.QWidget(Form)
        self.layoutWidget2.setGeometry(QtCore.QRect(20, 60, 311, 22))
        self.layoutWidget2.setObjectName("layoutWidget2")
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout(self.layoutWidget2)
        self.horizontalLayout_2.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.label_2 = QtWidgets.QLabel(self.layoutWidget2)
        self.label_2.setObjectName("label_2")
        self.horizontalLayout_2.addWidget(self.label_2)
        spacerItem8 = QtWidgets.QSpacerItem(18, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem8)
        self.NX = QtWidgets.QLineEdit(self.layoutWidget2)
        self.NX.setObjectName("NX")
        self.horizontalLayout_2.addWidget(self.NX)
        self.layoutWidget3 = QtWidgets.QWidget(Form)
        self.layoutWidget3.setGeometry(QtCore.QRect(20, 420, 303, 22))
        self.layoutWidget3.setObjectName("layoutWidget3")
        self.horizontalLayout_10 = QtWidgets.QHBoxLayout(self.layoutWidget3)
        self.horizontalLayout_10.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout_10.setObjectName("horizontalLayout_10")
        self.checkBox_ITYPE = QtWidgets.QCheckBox(self.layoutWidget3)
        self.checkBox_ITYPE.setObjectName("checkBox_ITYPE")
        self.horizontalLayout_10.addWidget(self.checkBox_ITYPE)
        spacerItem9 = QtWidgets.QSpacerItem(68, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_10.addItem(spacerItem9)
        self.layoutWidget4 = QtWidgets.QWidget(Form)
        self.layoutWidget4.setGeometry(QtCore.QRect(20, 450, 305, 22))
        self.layoutWidget4.setObjectName("layoutWidget4")
        self.horizontalLayout_11 = QtWidgets.QHBoxLayout(self.layoutWidget4)
        self.horizontalLayout_11.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout_11.setObjectName("horizontalLayout_11")
        spacerItem10 = QtWidgets.QSpacerItem(68, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_11.addItem(spacerItem10)
        self.comboBox_ITYPE = QtWidgets.QComboBox(self.layoutWidget4)
        self.comboBox_ITYPE.setObjectName("comboBox_ITYPE")
        self.comboBox_ITYPE.addItem("")
        self.comboBox_ITYPE.addItem("")
        self.comboBox_ITYPE.addItem("")
        self.horizontalLayout_11.addWidget(self.comboBox_ITYPE)

        self.retranslateUi(Form)
        QtCore.QMetaObject.connectSlotsByName(Form)
        Form.setTabOrder(self.comboBox_SpeedRegime, self.NX)
        Form.setTabOrder(self.NX, self.comboBox_VarCombo)
        Form.setTabOrder(self.comboBox_VarCombo, self.checkBox_ELLIP)
        Form.setTabOrder(self.checkBox_ELLIP, self.ELLIP)
        Form.setTabOrder(self.ELLIP, self.comboBox_BNOSE)
        Form.setTabOrder(self.comboBox_BNOSE, self.comboBox_BTAIL)
        Form.setTabOrder(self.comboBox_BTAIL, self.lineEdit_BLN)
        Form.setTabOrder(self.lineEdit_BLN, self.lineEdit_BLA)
        Form.setTabOrder(self.lineEdit_BLA, self.checkBox_DS)
        Form.setTabOrder(self.checkBox_DS, self.lineEdit_DS)
        Form.setTabOrder(self.lineEdit_DS, self.comboBox_METHED)
        Form.setTabOrder(self.comboBox_METHED, self.checkBox_ITYPE)
        Form.setTabOrder(self.checkBox_ITYPE, self.comboBox_ITYPE)
        Form.setTabOrder(self.comboBox_ITYPE, self.Tab_ComboVariables)
        Form.setTabOrder(self.Tab_ComboVariables, self.plainTextEdit)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", "机体参数"))
        self.groupBox_Sup_Hyp.setTitle(_translate("Form", "超音速/高超声速参数"))
        self.label_BNOSE.setText(_translate("Form", "BNOSE：机头样式"))
        self.comboBox_BNOSE.setItemText(0, _translate("Form", "1.0 圆锥型机头"))
        self.comboBox_BNOSE.setItemText(1, _translate("Form", "2.0 尖型机头"))
        self.label_BTAIL.setText(_translate("Form", "BTAIL：机尾样式"))
        self.comboBox_BTAIL.setItemText(0, _translate("Form", "1.0 圆锥型机尾"))
        self.comboBox_BTAIL.setItemText(1, _translate("Form", "2.0 尖型机尾"))
        self.label_BLN.setText(_translate("Form", "BLN：机身机头长度"))
        self.label_BLA.setText(_translate("Form", "BLA：圆柱状后机身部分长度"))
        self.checkBox_DS.setText(_translate("Form", "DS：机头钝度"))
        self.groupBox_Sub.setTitle(_translate("Form", "亚音速参数"))
        self.label_3.setText(_translate("Form", "机身横截面参数组合"))
        self.comboBox_VarCombo.setItemText(0, _translate("Form", "R-ZU-ZL"))
        self.comboBox_VarCombo.setItemText(1, _translate("Form", "S-ZU-ZL"))
        self.comboBox_VarCombo.setItemText(2, _translate("Form", "R-S-P-ZU-ZL  "))
        self.checkBox_ELLIP.setText(_translate("Form", "机身高宽比"))
        self.label_8.setText(_translate("Form", "METHED：计算方法"))
        self.comboBox_METHED.setItemText(0, _translate("Form", "1. 使用现有方法（系统默认）"))
        self.comboBox_METHED.setItemText(1, _translate("Form", "2. 使用Jorgensen方法"))
        self.plainTextEdit.setPlainText(_translate("Form", "本节主要录入机体的参数\n"
"NX : 机身定义位置的数量，最大20\n"
"X  : 机身截面的纵向距离，最大20\n"
"S  : 横截面面积（cross sectional area）\n"
"P  : 周长（periphery）\n"
"R  : 半径（local planform half width）\n"
"ELLIP : ELLIP = 高度A/宽度B,>1表示高而窄\n"
"ZU : 截面处机体上表面的Z坐标（中线以上为正）\n"
"ZL : 截面处机体下表面的Z坐标（中线以下为负）\n"
"\n"
"Tips 1:截面参数模式\n"
"A1. 只使用R或者S，其他参数按照圆截面估计；\n"
"A2. 如果ELLIP被定义，从R或者S估计其他参数，ELLIP = 高度A/宽度B,>1表示高而窄；此处必须使用METHOD=2.0\n"
"A3. R,S,P都定义\n"
"Tips 2;参考截面说明\n"
"参考平面 = 轴对称机体的堆成轴\n"
"Tips 3：\n"
"机体中线线处Z=0\n"
"Tips 4：\n"
"BNOSE BTAIL BLN BLA 这些参数在亚音速域不需要\n"
"Tips 5：\n"
"DS 只在高超声速需要 （Hypersonic Speed Regime Oonly）\n"
"\n"
"Tips 6：\n"
"在跨声速和超声速是由S得到等效半径 Req = sqrt(S/π)"))
        self.label.setText(_translate("Form", "布局选择"))
        self.comboBox_SpeedRegime.setItemText(0, _translate("Form", "亚音速"))
        self.comboBox_SpeedRegime.setItemText(1, _translate("Form", "跨音速"))
        self.comboBox_SpeedRegime.setItemText(2, _translate("Form", "超音速"))
        self.comboBox_SpeedRegime.setItemText(3, _translate("Form", "高超音速"))
        self.label_2.setText(_translate("Form", "NX：机身横截面的个数"))
        self.checkBox_ITYPE.setText(_translate("Form", "ITYPE:跨音速阻力分量马赫数计算方法"))
        self.comboBox_ITYPE.setItemText(0, _translate("Form", "1. 直翼，不计算面积效应(Area Rule)"))
        self.comboBox_ITYPE.setItemText(1, _translate("Form", "2. 掠翼，不计算面积效应(系统默认)"))
        self.comboBox_ITYPE.setItemText(2, _translate("Form", "3. 掠翼，计算面积效应"))

import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Form = QtWidgets.QWidget()
    ui = Ui_Form()
    ui.setupUi(Form)
    Form.show()
    sys.exit(app.exec_())

