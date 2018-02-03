# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\PROPWR.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_PROPWR(object):
    def setupUi(self, PROPWR):
        PROPWR.setObjectName("PROPWR")
        PROPWR.resize(989, 538)
        self.horizontalLayout_18 = QtWidgets.QHBoxLayout(PROPWR)
        self.horizontalLayout_18.setObjectName("horizontalLayout_18")
        self.verticalLayout_4 = QtWidgets.QVBoxLayout()
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label_AIETLP = QtWidgets.QLabel(PROPWR)
        self.label_AIETLP.setObjectName("label_AIETLP")
        self.horizontalLayout.addWidget(self.label_AIETLP)
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.AIETLP = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.AIETLP.sizePolicy().hasHeightForWidth())
        self.AIETLP.setSizePolicy(sizePolicy)
        self.AIETLP.setObjectName("AIETLP")
        self.horizontalLayout.addWidget(self.AIETLP)
        self.verticalLayout_4.addLayout(self.horizontalLayout)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.label_NENGSP = QtWidgets.QLabel(PROPWR)
        self.label_NENGSP.setObjectName("label_NENGSP")
        self.horizontalLayout_2.addWidget(self.label_NENGSP)
        spacerItem1 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem1)
        self.NENGSP = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.NENGSP.sizePolicy().hasHeightForWidth())
        self.NENGSP.setSizePolicy(sizePolicy)
        self.NENGSP.setObjectName("NENGSP")
        self.horizontalLayout_2.addWidget(self.NENGSP)
        self.verticalLayout_4.addLayout(self.horizontalLayout_2)
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_THSTCP = QtWidgets.QLabel(PROPWR)
        self.label_THSTCP.setObjectName("label_THSTCP")
        self.horizontalLayout_3.addWidget(self.label_THSTCP)
        spacerItem2 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem2)
        self.THSTCP = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.THSTCP.sizePolicy().hasHeightForWidth())
        self.THSTCP.setSizePolicy(sizePolicy)
        self.THSTCP.setObjectName("THSTCP")
        self.horizontalLayout_3.addWidget(self.THSTCP)
        self.verticalLayout_4.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.label_PHALOC = QtWidgets.QLabel(PROPWR)
        self.label_PHALOC.setObjectName("label_PHALOC")
        self.horizontalLayout_4.addWidget(self.label_PHALOC)
        spacerItem3 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem3)
        self.PHALOC = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.PHALOC.sizePolicy().hasHeightForWidth())
        self.PHALOC.setSizePolicy(sizePolicy)
        self.PHALOC.setObjectName("PHALOC")
        self.horizontalLayout_4.addWidget(self.PHALOC)
        self.verticalLayout_4.addLayout(self.horizontalLayout_4)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.label_PHVLOC = QtWidgets.QLabel(PROPWR)
        self.label_PHVLOC.setObjectName("label_PHVLOC")
        self.horizontalLayout_5.addWidget(self.label_PHVLOC)
        spacerItem4 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem4)
        self.PHVLOC = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.PHVLOC.sizePolicy().hasHeightForWidth())
        self.PHVLOC.setSizePolicy(sizePolicy)
        self.PHVLOC.setObjectName("PHVLOC")
        self.horizontalLayout_5.addWidget(self.PHVLOC)
        self.verticalLayout_4.addLayout(self.horizontalLayout_5)
        self.horizontalLayout_6 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_6.setObjectName("horizontalLayout_6")
        self.label_PRPRAD = QtWidgets.QLabel(PROPWR)
        self.label_PRPRAD.setObjectName("label_PRPRAD")
        self.horizontalLayout_6.addWidget(self.label_PRPRAD)
        spacerItem5 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem5)
        self.PRPRAD = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.PRPRAD.sizePolicy().hasHeightForWidth())
        self.PRPRAD.setSizePolicy(sizePolicy)
        self.PRPRAD.setObjectName("PRPRAD")
        self.horizontalLayout_6.addWidget(self.PRPRAD)
        self.verticalLayout_4.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_12 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_12.setObjectName("horizontalLayout_12")
        self.label_YP = QtWidgets.QLabel(PROPWR)
        self.label_YP.setObjectName("label_YP")
        self.horizontalLayout_12.addWidget(self.label_YP)
        spacerItem6 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_12.addItem(spacerItem6)
        self.YP = QtWidgets.QLineEdit(PROPWR)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.YP.sizePolicy().hasHeightForWidth())
        self.YP.setSizePolicy(sizePolicy)
        self.YP.setObjectName("YP")
        self.horizontalLayout_12.addWidget(self.YP)
        self.verticalLayout_4.addLayout(self.horizontalLayout_12)
        self.groupBox = QtWidgets.QGroupBox(PROPWR)
        self.groupBox.setObjectName("groupBox")
        self.verticalLayout_3 = QtWidgets.QVBoxLayout(self.groupBox)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.horizontalLayout_14 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_14.setObjectName("horizontalLayout_14")
        self.label_BAPR75 = QtWidgets.QLabel(self.groupBox)
        self.label_BAPR75.setObjectName("label_BAPR75")
        self.horizontalLayout_14.addWidget(self.label_BAPR75)
        spacerItem7 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_14.addItem(spacerItem7)
        self.BAPR75 = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BAPR75.sizePolicy().hasHeightForWidth())
        self.BAPR75.setSizePolicy(sizePolicy)
        self.BAPR75.setObjectName("BAPR75")
        self.horizontalLayout_14.addWidget(self.BAPR75)
        self.verticalLayout_3.addLayout(self.horizontalLayout_14)
        self.horizontalLayout_13 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_13.setObjectName("horizontalLayout_13")
        self.label_CROT = QtWidgets.QLabel(self.groupBox)
        self.label_CROT.setObjectName("label_CROT")
        self.horizontalLayout_13.addWidget(self.label_CROT)
        spacerItem8 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_13.addItem(spacerItem8)
        self.comboBox_CROT = QtWidgets.QComboBox(self.groupBox)
        self.comboBox_CROT.setObjectName("comboBox_CROT")
        self.comboBox_CROT.addItem("")
        self.comboBox_CROT.addItem("")
        self.horizontalLayout_13.addWidget(self.comboBox_CROT)
        self.verticalLayout_3.addLayout(self.horizontalLayout_13)
        self.groupBox_KN = QtWidgets.QGroupBox(self.groupBox)
        self.groupBox_KN.setCheckable(True)
        self.groupBox_KN.setObjectName("groupBox_KN")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.groupBox_KN)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout_7 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_7.setObjectName("horizontalLayout_7")
        self.label_ENGFCT = QtWidgets.QLabel(self.groupBox_KN)
        self.label_ENGFCT.setObjectName("label_ENGFCT")
        self.horizontalLayout_7.addWidget(self.label_ENGFCT)
        spacerItem9 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_7.addItem(spacerItem9)
        self.ENGFCT = QtWidgets.QLineEdit(self.groupBox_KN)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ENGFCT.sizePolicy().hasHeightForWidth())
        self.ENGFCT.setSizePolicy(sizePolicy)
        self.ENGFCT.setObjectName("ENGFCT")
        self.horizontalLayout_7.addWidget(self.ENGFCT)
        self.verticalLayout.addLayout(self.horizontalLayout_7)
        self.verticalLayout_3.addWidget(self.groupBox_KN)
        self.groupBox_propeller = QtWidgets.QGroupBox(self.groupBox)
        self.groupBox_propeller.setCheckable(True)
        self.groupBox_propeller.setChecked(False)
        self.groupBox_propeller.setObjectName("groupBox_propeller")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout(self.groupBox_propeller)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.horizontalLayout_8 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_8.setObjectName("horizontalLayout_8")
        self.label_BWAPR3 = QtWidgets.QLabel(self.groupBox_propeller)
        self.label_BWAPR3.setObjectName("label_BWAPR3")
        self.horizontalLayout_8.addWidget(self.label_BWAPR3)
        spacerItem10 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_8.addItem(spacerItem10)
        self.BWAPR3 = QtWidgets.QLineEdit(self.groupBox_propeller)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BWAPR3.sizePolicy().hasHeightForWidth())
        self.BWAPR3.setSizePolicy(sizePolicy)
        self.BWAPR3.setObjectName("BWAPR3")
        self.horizontalLayout_8.addWidget(self.BWAPR3)
        self.verticalLayout_2.addLayout(self.horizontalLayout_8)
        self.horizontalLayout_9 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_9.setObjectName("horizontalLayout_9")
        self.label_BWAPR6 = QtWidgets.QLabel(self.groupBox_propeller)
        self.label_BWAPR6.setObjectName("label_BWAPR6")
        self.horizontalLayout_9.addWidget(self.label_BWAPR6)
        spacerItem11 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_9.addItem(spacerItem11)
        self.BWAPR6 = QtWidgets.QLineEdit(self.groupBox_propeller)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BWAPR6.sizePolicy().hasHeightForWidth())
        self.BWAPR6.setSizePolicy(sizePolicy)
        self.BWAPR6.setObjectName("BWAPR6")
        self.horizontalLayout_9.addWidget(self.BWAPR6)
        self.verticalLayout_2.addLayout(self.horizontalLayout_9)
        self.horizontalLayout_10 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_10.setObjectName("horizontalLayout_10")
        self.label_BWAPR9 = QtWidgets.QLabel(self.groupBox_propeller)
        self.label_BWAPR9.setObjectName("label_BWAPR9")
        self.horizontalLayout_10.addWidget(self.label_BWAPR9)
        spacerItem12 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_10.addItem(spacerItem12)
        self.BWAPR9 = QtWidgets.QLineEdit(self.groupBox_propeller)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BWAPR9.sizePolicy().hasHeightForWidth())
        self.BWAPR9.setSizePolicy(sizePolicy)
        self.BWAPR9.setObjectName("BWAPR9")
        self.horizontalLayout_10.addWidget(self.BWAPR9)
        self.verticalLayout_2.addLayout(self.horizontalLayout_10)
        self.horizontalLayout_11 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_11.setObjectName("horizontalLayout_11")
        self.label_NOPBPE = QtWidgets.QLabel(self.groupBox_propeller)
        self.label_NOPBPE.setObjectName("label_NOPBPE")
        self.horizontalLayout_11.addWidget(self.label_NOPBPE)
        spacerItem13 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_11.addItem(spacerItem13)
        self.NOPBPE = QtWidgets.QLineEdit(self.groupBox_propeller)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.NOPBPE.sizePolicy().hasHeightForWidth())
        self.NOPBPE.setSizePolicy(sizePolicy)
        self.NOPBPE.setObjectName("NOPBPE")
        self.horizontalLayout_11.addWidget(self.NOPBPE)
        self.verticalLayout_2.addLayout(self.horizontalLayout_11)
        self.verticalLayout_3.addWidget(self.groupBox_propeller)
        spacerItem14 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_3.addItem(spacerItem14)
        self.verticalLayout_4.addWidget(self.groupBox)
        self.horizontalLayout_18.addLayout(self.verticalLayout_4)
        self.tabWidget = QtWidgets.QTabWidget(PROPWR)
        self.tabWidget.setObjectName("tabWidget")
        self.tab_2 = QtWidgets.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.horizontalLayout_15 = QtWidgets.QHBoxLayout(self.tab_2)
        self.horizontalLayout_15.setObjectName("horizontalLayout_15")
        self.textBrowser_Para = QtWidgets.QTextBrowser(self.tab_2)
        self.textBrowser_Para.setObjectName("textBrowser_Para")
        self.horizontalLayout_15.addWidget(self.textBrowser_Para)
        self.tabWidget.addTab(self.tab_2, "")
        self.horizontalLayout_18.addWidget(self.tabWidget)

        self.retranslateUi(PROPWR)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(PROPWR)

    def retranslateUi(self, PROPWR):
        _translate = QtCore.QCoreApplication.translate
        PROPWR.setWindowTitle(_translate("PROPWR", "螺旋桨"))
        self.label_AIETLP.setText(_translate("PROPWR", "AIETLP  引擎推力轴的入射角"))
        self.AIETLP.setToolTip(_translate("PROPWR", " angle of incidence of engine thrust axis。引擎推力轴的入射角"))
        self.label_NENGSP.setText(_translate("PROPWR", "NENGSP  引擎数"))
        self.NENGSP.setToolTip(_translate("PROPWR", "number of engines(1 or 2)。引擎数"))
        self.label_THSTCP.setText(_translate("PROPWR", "THSTCP  推力系数"))
        self.THSTCP.setToolTip(_translate("PROPWR", "thrus coefficient = 2T/(P∞V∞^2Sref)，推力系数"))
        self.label_PHALOC.setText(_translate("PROPWR", "PHALOC  螺旋桨轮毂轴向位置"))
        self.PHALOC.setToolTip(_translate("PROPWR", "axial location of propeller hub，螺旋桨轮毂轴向位置"))
        self.label_PHVLOC.setText(_translate("PROPWR", "PHVLOC  螺旋桨轮毂垂直位置"))
        self.PHVLOC.setToolTip(_translate("PROPWR", "vertical location of propeller hub，螺旋桨轮毂垂直位置 "))
        self.label_PRPRAD.setText(_translate("PROPWR", "PRPRAD  螺旋桨半径"))
        self.PRPRAD.setToolTip(_translate("PROPWR", "propeller radius ，螺旋桨半径"))
        self.label_YP.setText(_translate("PROPWR", "YP 发动机横向位置  "))
        self.YP.setToolTip(_translate("PROPWR", "lateral location of engine ，发动机横向位置 "))
        self.groupBox.setTitle(_translate("PROPWR", "螺旋浆信息"))
        self.label_BAPR75.setText(_translate("PROPWR", "BAPR75  0.75螺旋桨半径叶片角 "))
        self.BAPR75.setToolTip(_translate("PROPWR", "blade angle at 0.75 propeller radius ，0.75螺旋桨半径叶片角"))
        self.label_CROT.setText(_translate("PROPWR", "CROT 螺旋桨转向"))
        self.comboBox_CROT.setToolTip(_translate("PROPWR", "<html><head/><body><p> .TRUE. counter rotating propeller 反向螺旋桨;</p><p> .FALSE. non counter rotating propeller  非对旋螺旋桨</p></body></html>"))
        self.comboBox_CROT.setItemText(0, _translate("PROPWR", ".TRUE."))
        self.comboBox_CROT.setItemText(1, _translate("PROPWR", ".FALSE."))
        self.groupBox_KN.setTitle(_translate("PROPWR", "KN-经验法向力因子"))
        self.label_ENGFCT.setText(_translate("PROPWR", "ENGFCT  经验法向力因子"))
        self.ENGFCT.setToolTip(_translate("PROPWR", "empirical normal force factor ，经验法向力因子 "))
        self.groupBox_propeller.setTitle(_translate("PROPWR", "螺旋桨定义"))
        self.label_BWAPR3.setText(_translate("PROPWR", "BWAPR3 0.3螺旋桨半径叶片宽度"))
        self.BWAPR3.setToolTip(_translate("PROPWR", "blade width at 0.3 propeller radius ，0.3螺旋桨半径叶片宽度"))
        self.label_BWAPR6.setText(_translate("PROPWR", "BWAPR6 0.6螺旋桨半径叶片宽度"))
        self.BWAPR6.setToolTip(_translate("PROPWR", "blade width at 0.6 propeller radius ，0.6螺旋桨半径叶片宽度"))
        self.label_BWAPR9.setText(_translate("PROPWR", "BWAPR9 0.9螺旋桨半径叶片宽度"))
        self.BWAPR9.setToolTip(_translate("PROPWR", "blade width at 0.9 propeller radius ，0.9螺旋桨半径叶片宽度"))
        self.label_NOPBPE.setText(_translate("PROPWR", "NOPBPE 发动机的螺旋桨叶片数"))
        self.NOPBPE.setToolTip(_translate("PROPWR", "number of propeller blades per engine ，发动机的螺旋桨叶片数"))
        self.textBrowser_Para.setHtml(_translate("PROPWR", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:18px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:xx-large; font-weight:600;\">PROPWR 动力定义卡</span></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-本节主要录入动力的参数\"></a><span style=\" font-size:x-large; font-weight:600;\">本</span><span style=\" font-size:x-large; font-weight:600;\">节主要录入动力的参数</span></p>\n"
"<table border=\"0\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px;\" cellspacing=\"2\" cellpadding=\"0\"><thead>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">VariableName</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">别名</span></p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">长度限制</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">说明或Tips</span></p></td></tr></thead>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">AIETLP</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">引擎推力轴的入射角</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">angle of incidence of engine thrust axis。引擎推力轴的入射角</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">NENGSP</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">引擎数</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">number of engines(1 or 2)。引擎数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">THSTCP</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">推力系数</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">thrus coefficient = 2T/(P∞V∞^2Sref)，推力系数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">PHALOC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">螺旋桨轮毂轴向位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">axial location of propeller hub，螺旋桨轮毂轴向位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">PHVLOC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">螺旋桨轮毂垂直位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">vertical location of propeller hub，螺旋桨轮毂垂直位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">PRPRAD</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">螺旋桨半径</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">propeller radius ，螺旋桨半径</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">ENGFCT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">经验法向力因子</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">empirical normal force factor ，经验法向力因子</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BWAPR3</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0.3螺旋桨半径叶片宽度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">blade width at 0.3 propeller radius ，0.3螺旋桨半径叶片宽度</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BWAPR6</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0.6螺旋桨半径叶片宽度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">blade width at 0.6 propeller radius ，0.6螺旋桨半径叶片宽度</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BWAPR9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0.9螺旋桨半径叶片宽度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">blade width at 0.9 propeller radius ，0.9螺旋桨半径叶片宽度</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">NOPBPE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">发动机的螺旋桨叶片数</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">number of propeller blades per engine ，发动机的螺旋桨叶片数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BAPR75</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0.75螺旋桨半径叶片角</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">blade angle at 0.75 propeller radius ，0.75螺旋桨半径叶片角</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">YP</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">发动机横向位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">lateral location of engine ，发动机横向位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"right\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CROT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">螺旋桨转向</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">.TRUE. counter rotating propeller 反向螺旋桨; </p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">.FALSE. non counter rotating propeller 非对旋螺旋桨</p></td></tr></table>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-tips-1\"></a><span style=\" font-size:x-large; font-weight:600;\">T</span><span style=\" font-size:x-large; font-weight:600;\">ips 1：</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Kn is not required as input if (bp)\'s are input and conversely (bp)\'s are not required if Kn is input. (See section 4.6.1 of DATCOM) ENGFCT,BWAPR3,BWAPR6,BWAPR9,NOPBPE Kn 和 (bp)\'s 参数不需要同时输入</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-tips-2\"></a><span style=\" font-size:x-large; font-weight:600;\">T</span><span style=\" font-size:x-large; font-weight:600;\">ips 2：</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">propeller power effect methods are only applicable to longitudinal stability parameters in the subsonic speed regime. 螺旋桨功率效应方法只适用于亚音速情况下的纵向稳定参数。</p></body></html>"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_2), _translate("PROPWR", "参数说明"))

import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    PROPWR = QtWidgets.QWidget()
    ui = Ui_PROPWR()
    ui.setupUi(PROPWR)
    PROPWR.show()
    sys.exit(app.exec_())

