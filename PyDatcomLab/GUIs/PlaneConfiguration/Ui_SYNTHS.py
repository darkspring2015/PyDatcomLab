# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\SYNTHS.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_SYNTHS(object):
    def setupUi(self, SYNTHS):
        SYNTHS.setObjectName("SYNTHS")
        SYNTHS.resize(1171, 632)
        self.horizontalLayout_18 = QtWidgets.QHBoxLayout(SYNTHS)
        self.horizontalLayout_18.setObjectName("horizontalLayout_18")
        self.verticalLayout_7 = QtWidgets.QVBoxLayout()
        self.verticalLayout_7.setObjectName("verticalLayout_7")
        self.groupBoxCG = QtWidgets.QGroupBox(SYNTHS)
        self.groupBoxCG.setObjectName("groupBoxCG")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.groupBoxCG)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtWidgets.QLabel(self.groupBoxCG)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        spacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.XCG = QtWidgets.QLineEdit(self.groupBoxCG)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.XCG.sizePolicy().hasHeightForWidth())
        self.XCG.setSizePolicy(sizePolicy)
        self.XCG.setObjectName("XCG")
        self.horizontalLayout.addWidget(self.XCG)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.label_2 = QtWidgets.QLabel(self.groupBoxCG)
        self.label_2.setObjectName("label_2")
        self.horizontalLayout_2.addWidget(self.label_2)
        spacerItem1 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem1)
        self.ZCG = QtWidgets.QLineEdit(self.groupBoxCG)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ZCG.sizePolicy().hasHeightForWidth())
        self.ZCG.setSizePolicy(sizePolicy)
        self.ZCG.setObjectName("ZCG")
        self.horizontalLayout_2.addWidget(self.ZCG)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.verticalLayout_7.addWidget(self.groupBoxCG)
        self.groupBox = QtWidgets.QGroupBox(SYNTHS)
        self.groupBox.setObjectName("groupBox")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout(self.groupBox)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_3 = QtWidgets.QLabel(self.groupBox)
        self.label_3.setObjectName("label_3")
        self.horizontalLayout_3.addWidget(self.label_3)
        spacerItem2 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem2)
        self.XW = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.XW.sizePolicy().hasHeightForWidth())
        self.XW.setSizePolicy(sizePolicy)
        self.XW.setObjectName("XW")
        self.horizontalLayout_3.addWidget(self.XW)
        self.verticalLayout_2.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.label_4 = QtWidgets.QLabel(self.groupBox)
        self.label_4.setObjectName("label_4")
        self.horizontalLayout_4.addWidget(self.label_4)
        spacerItem3 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem3)
        self.ZW = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ZW.sizePolicy().hasHeightForWidth())
        self.ZW.setSizePolicy(sizePolicy)
        self.ZW.setObjectName("ZW")
        self.horizontalLayout_4.addWidget(self.ZW)
        self.verticalLayout_2.addLayout(self.horizontalLayout_4)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.label_5 = QtWidgets.QLabel(self.groupBox)
        self.label_5.setObjectName("label_5")
        self.horizontalLayout_5.addWidget(self.label_5)
        spacerItem4 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem4)
        self.ALIW = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ALIW.sizePolicy().hasHeightForWidth())
        self.ALIW.setSizePolicy(sizePolicy)
        self.ALIW.setObjectName("ALIW")
        self.horizontalLayout_5.addWidget(self.ALIW)
        self.verticalLayout_2.addLayout(self.horizontalLayout_5)
        self.verticalLayout_7.addWidget(self.groupBox)
        self.groupBox_HorizontalTail = QtWidgets.QGroupBox(SYNTHS)
        self.groupBox_HorizontalTail.setObjectName("groupBox_HorizontalTail")
        self.verticalLayout_3 = QtWidgets.QVBoxLayout(self.groupBox_HorizontalTail)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.horizontalLayout_6 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_6.setObjectName("horizontalLayout_6")
        self.label_6 = QtWidgets.QLabel(self.groupBox_HorizontalTail)
        self.label_6.setObjectName("label_6")
        self.horizontalLayout_6.addWidget(self.label_6)
        spacerItem5 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem5)
        self.XH = QtWidgets.QLineEdit(self.groupBox_HorizontalTail)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.XH.sizePolicy().hasHeightForWidth())
        self.XH.setSizePolicy(sizePolicy)
        self.XH.setObjectName("XH")
        self.horizontalLayout_6.addWidget(self.XH)
        self.verticalLayout_3.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_7 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_7.setObjectName("horizontalLayout_7")
        self.label_7 = QtWidgets.QLabel(self.groupBox_HorizontalTail)
        self.label_7.setObjectName("label_7")
        self.horizontalLayout_7.addWidget(self.label_7)
        spacerItem6 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_7.addItem(spacerItem6)
        self.ZH = QtWidgets.QLineEdit(self.groupBox_HorizontalTail)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ZH.sizePolicy().hasHeightForWidth())
        self.ZH.setSizePolicy(sizePolicy)
        self.ZH.setObjectName("ZH")
        self.horizontalLayout_7.addWidget(self.ZH)
        self.verticalLayout_3.addLayout(self.horizontalLayout_7)
        self.horizontalLayout_8 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_8.setObjectName("horizontalLayout_8")
        self.label_8 = QtWidgets.QLabel(self.groupBox_HorizontalTail)
        self.label_8.setObjectName("label_8")
        self.horizontalLayout_8.addWidget(self.label_8)
        spacerItem7 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_8.addItem(spacerItem7)
        self.ALIH = QtWidgets.QLineEdit(self.groupBox_HorizontalTail)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ALIH.sizePolicy().hasHeightForWidth())
        self.ALIH.setSizePolicy(sizePolicy)
        self.ALIH.setObjectName("ALIH")
        self.horizontalLayout_8.addWidget(self.ALIH)
        self.verticalLayout_3.addLayout(self.horizontalLayout_8)
        self.horizontalLayout_9 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_9.setObjectName("horizontalLayout_9")
        self.checkBox_HINAX = QtWidgets.QCheckBox(self.groupBox_HorizontalTail)
        self.checkBox_HINAX.setObjectName("checkBox_HINAX")
        self.horizontalLayout_9.addWidget(self.checkBox_HINAX)
        spacerItem8 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_9.addItem(spacerItem8)
        self.HINAX = QtWidgets.QLineEdit(self.groupBox_HorizontalTail)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.HINAX.sizePolicy().hasHeightForWidth())
        self.HINAX.setSizePolicy(sizePolicy)
        self.HINAX.setObjectName("HINAX")
        self.horizontalLayout_9.addWidget(self.HINAX)
        self.verticalLayout_3.addLayout(self.horizontalLayout_9)
        self.verticalLayout_7.addWidget(self.groupBox_HorizontalTail)
        self.groupBox_VerticalTail = QtWidgets.QGroupBox(SYNTHS)
        self.groupBox_VerticalTail.setObjectName("groupBox_VerticalTail")
        self.verticalLayout_4 = QtWidgets.QVBoxLayout(self.groupBox_VerticalTail)
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.horizontalLayout_10 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_10.setObjectName("horizontalLayout_10")
        self.label_9 = QtWidgets.QLabel(self.groupBox_VerticalTail)
        self.label_9.setObjectName("label_9")
        self.horizontalLayout_10.addWidget(self.label_9)
        spacerItem9 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_10.addItem(spacerItem9)
        self.XV = QtWidgets.QLineEdit(self.groupBox_VerticalTail)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.XV.sizePolicy().hasHeightForWidth())
        self.XV.setSizePolicy(sizePolicy)
        self.XV.setObjectName("XV")
        self.horizontalLayout_10.addWidget(self.XV)
        self.verticalLayout_4.addLayout(self.horizontalLayout_10)
        self.horizontalLayout_11 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_11.setObjectName("horizontalLayout_11")
        self.label_10 = QtWidgets.QLabel(self.groupBox_VerticalTail)
        self.label_10.setObjectName("label_10")
        self.horizontalLayout_11.addWidget(self.label_10)
        spacerItem10 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_11.addItem(spacerItem10)
        self.ZV = QtWidgets.QLineEdit(self.groupBox_VerticalTail)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ZV.sizePolicy().hasHeightForWidth())
        self.ZV.setSizePolicy(sizePolicy)
        self.ZV.setObjectName("ZV")
        self.horizontalLayout_11.addWidget(self.ZV)
        self.verticalLayout_4.addLayout(self.horizontalLayout_11)
        self.verticalLayout_7.addWidget(self.groupBox_VerticalTail)
        self.groupBox_2 = QtWidgets.QGroupBox(SYNTHS)
        self.groupBox_2.setObjectName("groupBox_2")
        self.verticalLayout_5 = QtWidgets.QVBoxLayout(self.groupBox_2)
        self.verticalLayout_5.setObjectName("verticalLayout_5")
        self.horizontalLayout_12 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_12.setObjectName("horizontalLayout_12")
        self.label_11 = QtWidgets.QLabel(self.groupBox_2)
        self.label_11.setObjectName("label_11")
        self.horizontalLayout_12.addWidget(self.label_11)
        spacerItem11 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_12.addItem(spacerItem11)
        self.XVF = QtWidgets.QLineEdit(self.groupBox_2)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.XVF.sizePolicy().hasHeightForWidth())
        self.XVF.setSizePolicy(sizePolicy)
        self.XVF.setObjectName("XVF")
        self.horizontalLayout_12.addWidget(self.XVF)
        self.verticalLayout_5.addLayout(self.horizontalLayout_12)
        self.horizontalLayout_13 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_13.setObjectName("horizontalLayout_13")
        self.label_12 = QtWidgets.QLabel(self.groupBox_2)
        self.label_12.setObjectName("label_12")
        self.horizontalLayout_13.addWidget(self.label_12)
        spacerItem12 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_13.addItem(spacerItem12)
        self.ZVF = QtWidgets.QLineEdit(self.groupBox_2)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ZVF.sizePolicy().hasHeightForWidth())
        self.ZVF.setSizePolicy(sizePolicy)
        self.ZVF.setObjectName("ZVF")
        self.horizontalLayout_13.addWidget(self.ZVF)
        self.verticalLayout_5.addLayout(self.horizontalLayout_13)
        self.verticalLayout_7.addWidget(self.groupBox_2)
        self.groupBox_3 = QtWidgets.QGroupBox(SYNTHS)
        self.groupBox_3.setObjectName("groupBox_3")
        self.verticalLayout_6 = QtWidgets.QVBoxLayout(self.groupBox_3)
        self.verticalLayout_6.setObjectName("verticalLayout_6")
        self.horizontalLayout_15 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_15.setObjectName("horizontalLayout_15")
        self.label_14 = QtWidgets.QLabel(self.groupBox_3)
        self.label_14.setObjectName("label_14")
        self.horizontalLayout_15.addWidget(self.label_14)
        spacerItem13 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_15.addItem(spacerItem13)
        self.SCALE = QtWidgets.QLineEdit(self.groupBox_3)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.SCALE.sizePolicy().hasHeightForWidth())
        self.SCALE.setSizePolicy(sizePolicy)
        self.SCALE.setObjectName("SCALE")
        self.horizontalLayout_15.addWidget(self.SCALE)
        self.verticalLayout_6.addLayout(self.horizontalLayout_15)
        self.horizontalLayout_14 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_14.setObjectName("horizontalLayout_14")
        self.label_13 = QtWidgets.QLabel(self.groupBox_3)
        self.label_13.setObjectName("label_13")
        self.horizontalLayout_14.addWidget(self.label_13)
        spacerItem14 = QtWidgets.QSpacerItem(13, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_14.addItem(spacerItem14)
        self.VERTUP = QtWidgets.QComboBox(self.groupBox_3)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.VERTUP.sizePolicy().hasHeightForWidth())
        self.VERTUP.setSizePolicy(sizePolicy)
        self.VERTUP.setMaximumSize(QtCore.QSize(130, 16777215))
        self.VERTUP.setMouseTracking(False)
        self.VERTUP.setAutoFillBackground(False)
        self.VERTUP.setObjectName("VERTUP")
        self.VERTUP.addItem("")
        self.VERTUP.addItem("")
        self.horizontalLayout_14.addWidget(self.VERTUP)
        self.verticalLayout_6.addLayout(self.horizontalLayout_14)
        self.verticalLayout_7.addWidget(self.groupBox_3)
        spacerItem15 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_7.addItem(spacerItem15)
        self.horizontalLayout_18.addLayout(self.verticalLayout_7)
        self.tabWidget = QtWidgets.QTabWidget(SYNTHS)
        self.tabWidget.setObjectName("tabWidget")
        self.tab = QtWidgets.QWidget()
        self.tab.setObjectName("tab")
        self.horizontalLayout_16 = QtWidgets.QHBoxLayout(self.tab)
        self.horizontalLayout_16.setObjectName("horizontalLayout_16")
        self.textBrowser = QtWidgets.QTextBrowser(self.tab)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.textBrowser.sizePolicy().hasHeightForWidth())
        self.textBrowser.setSizePolicy(sizePolicy)
        self.textBrowser.setMinimumSize(QtCore.QSize(0, 120))
        self.textBrowser.setObjectName("textBrowser")
        self.horizontalLayout_16.addWidget(self.textBrowser)
        self.tabWidget.addTab(self.tab, "")
        self.horizontalLayout_18.addWidget(self.tabWidget)

        self.retranslateUi(SYNTHS)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(SYNTHS)

    def retranslateUi(self, SYNTHS):
        _translate = QtCore.QCoreApplication.translate
        SYNTHS.setWindowTitle(_translate("SYNTHS", "飞机综合参数"))
        self.groupBoxCG.setTitle(_translate("SYNTHS", "质心"))
        self.label.setText(_translate("SYNTHS", "XCG ：重心纵向位置"))
        self.label_2.setText(_translate("SYNTHS", "ZCG ：重心垂向位置"))
        self.groupBox.setTitle(_translate("SYNTHS", "机翼参数"))
        self.label_3.setText(_translate("SYNTHS", "XW  ：机翼理论顶端的纵向位置"))
        self.label_4.setText(_translate("SYNTHS", "ZW  ：机翼理论顶端的垂向位置"))
        self.label_5.setText(_translate("SYNTHS", "ALIW：机翼安装角"))
        self.groupBox_HorizontalTail.setTitle(_translate("SYNTHS", "平尾参数"))
        self.label_6.setText(_translate("SYNTHS", "XH  ：平尾理论顶端的纵向位置"))
        self.label_7.setText(_translate("SYNTHS", "ZH  ：平尾理论顶端的纵向位置"))
        self.label_8.setText(_translate("SYNTHS", "ALIH：平尾安装角"))
        self.checkBox_HINAX.setText(_translate("SYNTHS", "HINAX：平尾铰链轴的纵向位置"))
        self.groupBox_VerticalTail.setTitle(_translate("SYNTHS", "垂尾参数"))
        self.label_9.setText(_translate("SYNTHS", "XV  ：垂尾理论顶端的纵向位置"))
        self.label_10.setText(_translate("SYNTHS", "ZV  ：垂尾理论顶端的垂向位置"))
        self.groupBox_2.setTitle(_translate("SYNTHS", "腹鳍参数(Ventral Tail)"))
        self.label_11.setText(_translate("SYNTHS", "XVF ：腹鳍理论顶端的纵向位置"))
        self.label_12.setText(_translate("SYNTHS", "ZVF ：腹鳍理论顶端的垂向位置"))
        self.groupBox_3.setTitle(_translate("SYNTHS", "其他配置"))
        self.label_14.setText(_translate("SYNTHS", "SCALE：缩放因子"))
        self.label_13.setText(_translate("SYNTHS", "VERTUP：垂直面与参考面关系"))
        self.VERTUP.setCurrentText(_translate("SYNTHS", ".TRUE. 参考面上方"))
        self.VERTUP.setItemText(0, _translate("SYNTHS", ".TRUE. 参考面上方"))
        self.VERTUP.setItemText(1, _translate("SYNTHS", ".FALSE. 参考面下方"))
        self.textBrowser.setHtml(_translate("SYNTHS", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">本NAMELIST录入飞机的综合参数，主要包括：飞机质心位置，机翼、平尾、垂尾、腹尾、垂直尾翼等的几何位置。</p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">缩放因子用来放大风动试验结果。</p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Tips1：勾选HINAX时采用全动平尾</p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">1. HINAX只针对全动平尾布局</p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">2. 输入HINAX时，XH和ZH被认为是0安装角（iw=0）</p>\n"
"<p style=\"-qt-paragraph-type:empty; margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Tips2：使用双垂尾</p></body></html>"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab), _translate("SYNTHS", "参数说明"))

import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    SYNTHS = QtWidgets.QWidget()
    ui = Ui_SYNTHS()
    ui.setupUi(SYNTHS)
    SYNTHS.show()
    sys.exit(app.exec_())

