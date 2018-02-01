# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\ASYFLP.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_ASYFLP(object):
    def setupUi(self, ASYFLP):
        ASYFLP.setObjectName("ASYFLP")
        ASYFLP.resize(1061, 467)
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout(ASYFLP)
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.verticalLayout = QtWidgets.QVBoxLayout()
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtWidgets.QLabel(ASYFLP)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        spacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.comboBox_STYPE = QtWidgets.QComboBox(ASYFLP)
        self.comboBox_STYPE.setMinimumSize(QtCore.QSize(80, 0))
        self.comboBox_STYPE.setObjectName("comboBox_STYPE")
        self.comboBox_STYPE.addItem("")
        self.comboBox_STYPE.addItem("")
        self.comboBox_STYPE.addItem("")
        self.comboBox_STYPE.addItem("")
        self.comboBox_STYPE.addItem("")
        self.horizontalLayout.addWidget(self.comboBox_STYPE)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.label_2 = QtWidgets.QLabel(ASYFLP)
        self.label_2.setObjectName("label_2")
        self.horizontalLayout_2.addWidget(self.label_2)
        spacerItem1 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem1)
        self.NDELTA = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.NDELTA.sizePolicy().hasHeightForWidth())
        self.NDELTA.setSizePolicy(sizePolicy)
        self.NDELTA.setMinimumSize(QtCore.QSize(80, 0))
        self.NDELTA.setObjectName("NDELTA")
        self.horizontalLayout_2.addWidget(self.NDELTA)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_3 = QtWidgets.QLabel(ASYFLP)
        self.label_3.setObjectName("label_3")
        self.horizontalLayout_3.addWidget(self.label_3)
        spacerItem2 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem2)
        self.PHETE = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.PHETE.sizePolicy().hasHeightForWidth())
        self.PHETE.setSizePolicy(sizePolicy)
        self.PHETE.setMinimumSize(QtCore.QSize(80, 0))
        self.PHETE.setObjectName("PHETE")
        self.horizontalLayout_3.addWidget(self.PHETE)
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.label_5 = QtWidgets.QLabel(ASYFLP)
        self.label_5.setObjectName("label_5")
        self.horizontalLayout_5.addWidget(self.label_5)
        spacerItem3 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem3)
        self.CHRDFI = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CHRDFI.sizePolicy().hasHeightForWidth())
        self.CHRDFI.setSizePolicy(sizePolicy)
        self.CHRDFI.setMinimumSize(QtCore.QSize(80, 0))
        self.CHRDFI.setObjectName("CHRDFI")
        self.horizontalLayout_5.addWidget(self.CHRDFI)
        self.verticalLayout.addLayout(self.horizontalLayout_5)
        self.horizontalLayout_6 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_6.setObjectName("horizontalLayout_6")
        self.label_6 = QtWidgets.QLabel(ASYFLP)
        self.label_6.setObjectName("label_6")
        self.horizontalLayout_6.addWidget(self.label_6)
        spacerItem4 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem4)
        self.CHRDFO = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CHRDFO.sizePolicy().hasHeightForWidth())
        self.CHRDFO.setSizePolicy(sizePolicy)
        self.CHRDFO.setMinimumSize(QtCore.QSize(80, 0))
        self.CHRDFO.setObjectName("CHRDFO")
        self.horizontalLayout_6.addWidget(self.CHRDFO)
        self.verticalLayout.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_7 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_7.setObjectName("horizontalLayout_7")
        self.label_7 = QtWidgets.QLabel(ASYFLP)
        self.label_7.setObjectName("label_7")
        self.horizontalLayout_7.addWidget(self.label_7)
        spacerItem5 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_7.addItem(spacerItem5)
        self.SPANFI = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.SPANFI.sizePolicy().hasHeightForWidth())
        self.SPANFI.setSizePolicy(sizePolicy)
        self.SPANFI.setMinimumSize(QtCore.QSize(80, 0))
        self.SPANFI.setObjectName("SPANFI")
        self.horizontalLayout_7.addWidget(self.SPANFI)
        self.verticalLayout.addLayout(self.horizontalLayout_7)
        self.horizontalLayout_8 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_8.setObjectName("horizontalLayout_8")
        self.label_8 = QtWidgets.QLabel(ASYFLP)
        self.label_8.setObjectName("label_8")
        self.horizontalLayout_8.addWidget(self.label_8)
        spacerItem6 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_8.addItem(spacerItem6)
        self.SPANFO = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.SPANFO.sizePolicy().hasHeightForWidth())
        self.SPANFO.setSizePolicy(sizePolicy)
        self.SPANFO.setMinimumSize(QtCore.QSize(80, 0))
        self.SPANFO.setObjectName("SPANFO")
        self.horizontalLayout_8.addWidget(self.SPANFO)
        self.verticalLayout.addLayout(self.horizontalLayout_8)
        self.horizontalLayout_11 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_11.setObjectName("horizontalLayout_11")
        self.label_11 = QtWidgets.QLabel(ASYFLP)
        self.label_11.setObjectName("label_11")
        self.horizontalLayout_11.addWidget(self.label_11)
        spacerItem7 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_11.addItem(spacerItem7)
        self.XSPRME = QtWidgets.QLineEdit(ASYFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.XSPRME.sizePolicy().hasHeightForWidth())
        self.XSPRME.setSizePolicy(sizePolicy)
        self.XSPRME.setMinimumSize(QtCore.QSize(80, 0))
        self.XSPRME.setObjectName("XSPRME")
        self.horizontalLayout_11.addWidget(self.XSPRME)
        self.verticalLayout.addLayout(self.horizontalLayout_11)
        spacerItem8 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem8)
        self.horizontalLayout_4.addLayout(self.verticalLayout)
        self.tabWidget_input = QtWidgets.QTabWidget(ASYFLP)
        self.tabWidget_input.setObjectName("tabWidget_input")
        self.tab_4 = QtWidgets.QWidget()
        self.tab_4.setObjectName("tab_4")
        self.horizontalLayout_19 = QtWidgets.QHBoxLayout(self.tab_4)
        self.horizontalLayout_19.setObjectName("horizontalLayout_19")
        self.tableWidget_input = QtWidgets.QTableWidget(self.tab_4)
        self.tableWidget_input.setObjectName("tableWidget_input")
        self.tableWidget_input.setColumnCount(0)
        self.tableWidget_input.setRowCount(0)
        self.horizontalLayout_19.addWidget(self.tableWidget_input)
        self.tabWidget_input.addTab(self.tab_4, "")
        self.tab = QtWidgets.QWidget()
        self.tab.setObjectName("tab")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout(self.tab)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.scrollArea = QtWidgets.QScrollArea(self.tab)
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setObjectName("scrollArea")
        self.scrollAreaWidgetContents = QtWidgets.QWidget()
        self.scrollAreaWidgetContents.setGeometry(QtCore.QRect(0, 0, 1046, 1382))
        self.scrollAreaWidgetContents.setObjectName("scrollAreaWidgetContents")
        self.horizontalLayout_18 = QtWidgets.QHBoxLayout(self.scrollAreaWidgetContents)
        self.horizontalLayout_18.setObjectName("horizontalLayout_18")
        self.label_16 = QtWidgets.QLabel(self.scrollAreaWidgetContents)
        self.label_16.setText("")
        self.label_16.setPixmap(QtGui.QPixmap(":/card/rc_card/ASYFLP.png"))
        self.label_16.setScaledContents(True)
        self.label_16.setObjectName("label_16")
        self.horizontalLayout_18.addWidget(self.label_16)
        self.scrollArea.setWidget(self.scrollAreaWidgetContents)
        self.verticalLayout_2.addWidget(self.scrollArea)
        self.tabWidget_input.addTab(self.tab, "")
        self.tab_2 = QtWidgets.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.verticalLayout_3 = QtWidgets.QVBoxLayout(self.tab_2)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.scrollArea_2 = QtWidgets.QScrollArea(self.tab_2)
        self.scrollArea_2.setWidgetResizable(True)
        self.scrollArea_2.setObjectName("scrollArea_2")
        self.scrollAreaWidgetContents_2 = QtWidgets.QWidget()
        self.scrollAreaWidgetContents_2.setGeometry(QtCore.QRect(0, 0, 1214, 1398))
        self.scrollAreaWidgetContents_2.setObjectName("scrollAreaWidgetContents_2")
        self.verticalLayout_4 = QtWidgets.QVBoxLayout(self.scrollAreaWidgetContents_2)
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.label_17 = QtWidgets.QLabel(self.scrollAreaWidgetContents_2)
        self.label_17.setText("")
        self.label_17.setPixmap(QtGui.QPixmap(":/card/rc_card/ASYFLP-PARA.png"))
        self.label_17.setScaledContents(True)
        self.label_17.setObjectName("label_17")
        self.verticalLayout_4.addWidget(self.label_17)
        self.scrollArea_2.setWidget(self.scrollAreaWidgetContents_2)
        self.verticalLayout_3.addWidget(self.scrollArea_2)
        self.tabWidget_input.addTab(self.tab_2, "")
        self.tab_3 = QtWidgets.QWidget()
        self.tab_3.setObjectName("tab_3")
        self.horizontalLayout_17 = QtWidgets.QHBoxLayout(self.tab_3)
        self.horizontalLayout_17.setObjectName("horizontalLayout_17")
        self.textBrowser = QtWidgets.QTextBrowser(self.tab_3)
        self.textBrowser.setObjectName("textBrowser")
        self.horizontalLayout_17.addWidget(self.textBrowser)
        self.tabWidget_input.addTab(self.tab_3, "")
        self.horizontalLayout_4.addWidget(self.tabWidget_input)
        self.actionAddRow = QtWidgets.QAction(ASYFLP)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAddRow.setIcon(icon)
        self.actionAddRow.setObjectName("actionAddRow")
        self.actionDeleteRow = QtWidgets.QAction(ASYFLP)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/cardIco/rc_card/icos/DeletedIcon.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionDeleteRow.setIcon(icon1)
        self.actionDeleteRow.setObjectName("actionDeleteRow")

        self.retranslateUi(ASYFLP)
        self.tabWidget_input.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(ASYFLP)

    def retranslateUi(self, ASYFLP):
        _translate = QtCore.QCoreApplication.translate
        ASYFLP.setWindowTitle(_translate("ASYFLP", "差动控制面"))
        self.label.setText(_translate("ASYFLP", "STYPE  类别"))
        self.comboBox_STYPE.setToolTip(_translate("ASYFLP", " =1.0 flap spoiler on wing<br> =2.0 plus spoiler on wing<br> =3.0 spoiler-slot-deflection on wing<br> =4.0 plain flap aileron<br> =5.0 differentially deflected moveable horizontal tail <br> =1.0 瓣扰流翼<BR> =2.0 加扰流翼 <BR> =3.0 扰流板槽偏转翼 <BR> =4.0 简单襟翼副翼 <BR> =5.0 差动平尾 "))
        self.comboBox_STYPE.setItemText(0, _translate("ASYFLP", "1.0 机翼襟翼扰流板"))
        self.comboBox_STYPE.setItemText(1, _translate("ASYFLP", "2.0 机翼上的扰流板"))
        self.comboBox_STYPE.setItemText(2, _translate("ASYFLP", "3.0 扰流板槽偏转翼"))
        self.comboBox_STYPE.setItemText(3, _translate("ASYFLP", "4.0 简单襟翼副翼"))
        self.comboBox_STYPE.setItemText(4, _translate("ASYFLP", "5.0 差动平尾"))
        self.label_2.setText(_translate("ASYFLP", "NDELTA 偏转角数量"))
        self.NDELTA.setToolTip(_translate("ASYFLP", "number of control deflection angles；required for all controls，max.of 9  \n"
"控制面偏转角数量 "))
        self.label_3.setText(_translate("ASYFLP", "PHETE  翼型后缘角正切值(90,99)"))
        self.label_5.setText(_translate("ASYFLP", "CHRDFI  内侧弦长"))
        self.label_6.setText(_translate("ASYFLP", "CHRDFO  外侧弦长 "))
        self.label_7.setText(_translate("ASYFLP", "SPANFI  内侧端的横向距离"))
        self.label_8.setText(_translate("ASYFLP", "SPANFO  外侧端的横向距离"))
        self.label_11.setText(_translate("ASYFLP", "XSPRME  扰流板铰链到机翼前延的距离"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_4), _translate("ASYFLP", "可变参数"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab), _translate("ASYFLP", "示意图"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_2), _translate("ASYFLP", "参数表"))
        self.textBrowser.setHtml(_translate("ASYFLP", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">   </p>\n"
"<p style=\" margin-top:18px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-namelist-asyflp选项卡\"></a><span style=\" font-size:xx-large; font-weight:600;\">N</span><span style=\" font-size:xx-large; font-weight:600;\">AMELIST ASYFLP选项卡</span></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-asymmetrical--control-deflection-input--差动控制偏转的输入项\"></a><span style=\" font-size:x-large; font-weight:600;\">a</span><span style=\" font-size:x-large; font-weight:600;\">symmetrical  control deflection input  差动控制偏转的输入项</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br />## 参数表</p>\n"
"<table border=\"0\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px;\" cellspacing=\"2\" cellpadding=\"0\"><thead>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">工程量</span></p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">VariableName</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">别名</span></p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">长度限制</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">Definition</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">参数说明</span></p></td></tr></thead>\n"
"<tr>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">STYPE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">类别</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 flap spoiler on wing<br /> =2.0 plus spoiler on wing<br /> =3.0 spoiler-slot-deflection on wing<br /> =4.0 plain flap aileron<br /> =5.0 differentially deflected moveable horizontal tail</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 瓣扰流翼<br /> =2.0 加扰流翼 <br /> =3.0 扰流板槽偏转翼 <br /> =4.0 简单襟翼副翼 <br /> =5.0 差动平尾</p></td></tr>\n"
"<tr>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">NDELTA</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制面偏转角数量</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">number of control deflection angles；required for all controls，max.of 9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制面偏转角数量</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">bi</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">SPANFI</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">内侧端的横向距离</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">span location of inborad end of flap or spoiler control.<br />measured perpendicular to vertical plane of symmetry</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">内侧端的横向距离，测量垂直于纵向对称面</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">bo</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">SPANFO</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼外侧端的横向距离</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">span location of outborad end of flap or spoiler control.<br />measured to perpendicular to vertical plane of symmetry</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">外侧端的横向距离，测量垂直于纵向对称面</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tan(OTE/2)</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">PHETE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">翼型后缘角正切值(90,99)</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tangent of airfoil trailine edge angle based on ordinates at 90 and 99 percent chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">翼型后缘角正切值 在90和99%弦长处</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δL</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELTAL</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">左偏转角</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">deflection angles for left hand plain flap aileron or left hand panel all moveable horizontal tail<br /> measured in vertical plane of symmetry</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">左侧简单襟副翼或左侧全动平尾的偏转角度<br />在垂直对称面内测量</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δR</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELTAR</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">右偏转角</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">deflection angles for right hand plain flap aileron or right hand panel all moveable horizontal tail<br /> measured in vertical plane of symmetry</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">右侧简单襟副翼或右侧全动平尾的偏转角度<br />在垂直对称面内测量</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cfi</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CHRDFI</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">内侧弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">aileron chord at inboard end of plain flap aileron ，measured parallel to longitudinal axis</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">简单襟翼副翼内侧端的副翼弦长<br />测量平行于纵轴</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cfo</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CHRDFO</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">外侧弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">aileron chord at outboard end of plain flap aileron，measured parallel to longitudinal axis</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">简单襟翼副翼外侧端的副翼弦长<br />测量平行于纵轴</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δd/c</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELTAD</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">偏转器投影高度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">projected height of deflector ,spoiler-slot-deflector control;fraction of chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">偏转器的投影高度，扰流板槽偏转控制<br />弦长的分数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δs/c</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELTAS</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板投影高度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">projected height of spoiler,flap spoiler,plug spoiler and spoiler-slot deflector control；fraction of chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板、襟翼扰流板、塞扰流板和扰流槽偏转板控制的投射高度<br />弦长的分数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Xs/c</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">XSOC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板开口到机翼前延的距离</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">distance from wing leading edge to spoiler lip measured parallel to streamwise wing chord,flap and plug spoilers.<br />fraction of chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板开口到机翼前缘的距离，平行于流向翼弦、襟翼和塞扰流板进行测量<br />弦长的分数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">X\'s/c</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">XSPRME</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板铰链到机翼前延的距离</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">distance from wing leading edge to spoiler hinge line  measured parallel to streamwise wing chord,flap spoiler, plug spoiler and spoiler-slot deflector control.<br />fraction of chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板铰链线到机翼前缘的距离，平行于流向翼弦、襟翼扰流板、塞扰流板和扰流槽偏转控制进行测量<br />弦长的分数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">hs/c</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">HSOC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板到翼型中线的投影高度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">projected height of spoiler measured from and normal to airfoil mean line, flap spoiler,plug spoiler and spoiler-slot reflector.<br />fraction of chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">扰流板到翼型中线的投影高度，襟翼扰流板、塞扰流板和扰流槽偏转控制进行测量<br />弦长的分数</p></td></tr></table></body></html>"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_3), _translate("ASYFLP", "参数说明"))
        self.actionAddRow.setText(_translate("ASYFLP", "增加行"))
        self.actionAddRow.setToolTip(_translate("ASYFLP", "新增行"))
        self.actionDeleteRow.setText(_translate("ASYFLP", "删除行"))
        self.actionDeleteRow.setToolTip(_translate("ASYFLP", "删除一行"))

import card_ico_rc
import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    ASYFLP = QtWidgets.QWidget()
    ui = Ui_ASYFLP()
    ui.setupUi(ASYFLP)
    ASYFLP.show()
    sys.exit(app.exec_())

