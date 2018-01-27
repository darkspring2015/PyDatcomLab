# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\CONTAB.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_CONTAB(object):
    def setupUi(self, CONTAB):
        CONTAB.setObjectName("CONTAB")
        CONTAB.resize(1112, 613)
        self.horizontalLayout = QtWidgets.QHBoxLayout(CONTAB)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout()
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.horizontalLayout_13 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_13.setObjectName("horizontalLayout_13")
        self.label_13 = QtWidgets.QLabel(CONTAB)
        self.label_13.setObjectName("label_13")
        self.horizontalLayout_13.addWidget(self.label_13)
        spacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_13.addItem(spacerItem)
        self.comboBox_TTYPE = QtWidgets.QComboBox(CONTAB)
        self.comboBox_TTYPE.setObjectName("comboBox_TTYPE")
        self.comboBox_TTYPE.addItem("")
        self.comboBox_TTYPE.addItem("")
        self.comboBox_TTYPE.addItem("")
        self.horizontalLayout_13.addWidget(self.comboBox_TTYPE)
        self.verticalLayout_2.addLayout(self.horizontalLayout_13)
        self.horizontalLayout_8 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_8.setObjectName("horizontalLayout_8")
        self.label_8 = QtWidgets.QLabel(CONTAB)
        self.label_8.setObjectName("label_8")
        self.horizontalLayout_8.addWidget(self.label_8)
        spacerItem1 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_8.addItem(spacerItem1)
        self.CFITC = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CFITC.sizePolicy().hasHeightForWidth())
        self.CFITC.setSizePolicy(sizePolicy)
        self.CFITC.setMinimumSize(QtCore.QSize(80, 0))
        self.CFITC.setObjectName("CFITC")
        self.horizontalLayout_8.addWidget(self.CFITC)
        self.verticalLayout_2.addLayout(self.horizontalLayout_8)
        self.horizontalLayout_9 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_9.setObjectName("horizontalLayout_9")
        self.label_9 = QtWidgets.QLabel(CONTAB)
        self.label_9.setObjectName("label_9")
        self.horizontalLayout_9.addWidget(self.label_9)
        spacerItem2 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_9.addItem(spacerItem2)
        self.CFOTC = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CFOTC.sizePolicy().hasHeightForWidth())
        self.CFOTC.setSizePolicy(sizePolicy)
        self.CFOTC.setMinimumSize(QtCore.QSize(80, 0))
        self.CFOTC.setObjectName("CFOTC")
        self.horizontalLayout_9.addWidget(self.CFOTC)
        self.verticalLayout_2.addLayout(self.horizontalLayout_9)
        self.horizontalLayout_10 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_10.setObjectName("horizontalLayout_10")
        self.label_10 = QtWidgets.QLabel(CONTAB)
        self.label_10.setObjectName("label_10")
        self.horizontalLayout_10.addWidget(self.label_10)
        spacerItem3 = QtWidgets.QSpacerItem(1, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_10.addItem(spacerItem3)
        self.BITC = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BITC.sizePolicy().hasHeightForWidth())
        self.BITC.setSizePolicy(sizePolicy)
        self.BITC.setMinimumSize(QtCore.QSize(80, 0))
        self.BITC.setObjectName("BITC")
        self.horizontalLayout_10.addWidget(self.BITC)
        self.verticalLayout_2.addLayout(self.horizontalLayout_10)
        self.horizontalLayout_11 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_11.setObjectName("horizontalLayout_11")
        self.label_11 = QtWidgets.QLabel(CONTAB)
        self.label_11.setObjectName("label_11")
        self.horizontalLayout_11.addWidget(self.label_11)
        spacerItem4 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_11.addItem(spacerItem4)
        self.BOTC = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BOTC.sizePolicy().hasHeightForWidth())
        self.BOTC.setSizePolicy(sizePolicy)
        self.BOTC.setMinimumSize(QtCore.QSize(80, 0))
        self.BOTC.setObjectName("BOTC")
        self.horizontalLayout_11.addWidget(self.BOTC)
        self.verticalLayout_2.addLayout(self.horizontalLayout_11)
        self.horizontalLayout_14 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_14.setObjectName("horizontalLayout_14")
        self.label_14 = QtWidgets.QLabel(CONTAB)
        self.label_14.setObjectName("label_14")
        self.horizontalLayout_14.addWidget(self.label_14)
        spacerItem5 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_14.addItem(spacerItem5)
        self.CFITT = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CFITT.sizePolicy().hasHeightForWidth())
        self.CFITT.setSizePolicy(sizePolicy)
        self.CFITT.setMinimumSize(QtCore.QSize(80, 0))
        self.CFITT.setObjectName("CFITT")
        self.horizontalLayout_14.addWidget(self.CFITT)
        self.verticalLayout_2.addLayout(self.horizontalLayout_14)
        self.horizontalLayout_15 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_15.setObjectName("horizontalLayout_15")
        self.label_15 = QtWidgets.QLabel(CONTAB)
        self.label_15.setObjectName("label_15")
        self.horizontalLayout_15.addWidget(self.label_15)
        spacerItem6 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_15.addItem(spacerItem6)
        self.CFOTT = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CFOTT.sizePolicy().hasHeightForWidth())
        self.CFOTT.setSizePolicy(sizePolicy)
        self.CFOTT.setMinimumSize(QtCore.QSize(80, 0))
        self.CFOTT.setObjectName("CFOTT")
        self.horizontalLayout_15.addWidget(self.CFOTT)
        self.verticalLayout_2.addLayout(self.horizontalLayout_15)
        self.horizontalLayout_20 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_20.setObjectName("horizontalLayout_20")
        self.label_17 = QtWidgets.QLabel(CONTAB)
        self.label_17.setObjectName("label_17")
        self.horizontalLayout_20.addWidget(self.label_17)
        spacerItem7 = QtWidgets.QSpacerItem(1, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_20.addItem(spacerItem7)
        self.BITT = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BITT.sizePolicy().hasHeightForWidth())
        self.BITT.setSizePolicy(sizePolicy)
        self.BITT.setMinimumSize(QtCore.QSize(80, 0))
        self.BITT.setObjectName("BITT")
        self.horizontalLayout_20.addWidget(self.BITT)
        self.verticalLayout_2.addLayout(self.horizontalLayout_20)
        self.horizontalLayout_16 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_16.setObjectName("horizontalLayout_16")
        self.label_16 = QtWidgets.QLabel(CONTAB)
        self.label_16.setObjectName("label_16")
        self.horizontalLayout_16.addWidget(self.label_16)
        spacerItem8 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_16.addItem(spacerItem8)
        self.BOTT = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BOTT.sizePolicy().hasHeightForWidth())
        self.BOTT.setSizePolicy(sizePolicy)
        self.BOTT.setMinimumSize(QtCore.QSize(80, 0))
        self.BOTT.setObjectName("BOTT")
        self.horizontalLayout_16.addWidget(self.BOTT)
        self.verticalLayout_2.addLayout(self.horizontalLayout_16)
        self.horizontalLayout_24 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_24.setObjectName("horizontalLayout_24")
        self.label_21 = QtWidgets.QLabel(CONTAB)
        self.label_21.setObjectName("label_21")
        self.horizontalLayout_24.addWidget(self.label_21)
        spacerItem9 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_24.addItem(spacerItem9)
        self.B1 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.B1.sizePolicy().hasHeightForWidth())
        self.B1.setSizePolicy(sizePolicy)
        self.B1.setMinimumSize(QtCore.QSize(80, 0))
        self.B1.setObjectName("B1")
        self.horizontalLayout_24.addWidget(self.B1)
        self.verticalLayout_2.addLayout(self.horizontalLayout_24)
        self.horizontalLayout_23 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_23.setObjectName("horizontalLayout_23")
        self.label_20 = QtWidgets.QLabel(CONTAB)
        self.label_20.setObjectName("label_20")
        self.horizontalLayout_23.addWidget(self.label_20)
        spacerItem10 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_23.addItem(spacerItem10)
        self.B2 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.B2.sizePolicy().hasHeightForWidth())
        self.B2.setSizePolicy(sizePolicy)
        self.B2.setMinimumSize(QtCore.QSize(80, 0))
        self.B2.setObjectName("B2")
        self.horizontalLayout_23.addWidget(self.B2)
        self.verticalLayout_2.addLayout(self.horizontalLayout_23)
        self.horizontalLayout_21 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_21.setObjectName("horizontalLayout_21")
        self.label_18 = QtWidgets.QLabel(CONTAB)
        self.label_18.setObjectName("label_18")
        self.horizontalLayout_21.addWidget(self.label_18)
        spacerItem11 = QtWidgets.QSpacerItem(1, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_21.addItem(spacerItem11)
        self.B3 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.B3.sizePolicy().hasHeightForWidth())
        self.B3.setSizePolicy(sizePolicy)
        self.B3.setMinimumSize(QtCore.QSize(80, 0))
        self.B3.setObjectName("B3")
        self.horizontalLayout_21.addWidget(self.B3)
        self.verticalLayout_2.addLayout(self.horizontalLayout_21)
        self.horizontalLayout_22 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_22.setObjectName("horizontalLayout_22")
        self.label_19 = QtWidgets.QLabel(CONTAB)
        self.label_19.setObjectName("label_19")
        self.horizontalLayout_22.addWidget(self.label_19)
        spacerItem12 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_22.addItem(spacerItem12)
        self.B4 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.B4.sizePolicy().hasHeightForWidth())
        self.B4.setSizePolicy(sizePolicy)
        self.B4.setMinimumSize(QtCore.QSize(80, 0))
        self.B4.setObjectName("B4")
        self.horizontalLayout_22.addWidget(self.B4)
        self.verticalLayout_2.addLayout(self.horizontalLayout_22)
        self.horizontalLayout_25 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_25.setObjectName("horizontalLayout_25")
        self.label_22 = QtWidgets.QLabel(CONTAB)
        self.label_22.setObjectName("label_22")
        self.horizontalLayout_25.addWidget(self.label_22)
        spacerItem13 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_25.addItem(spacerItem13)
        self.D1 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.D1.sizePolicy().hasHeightForWidth())
        self.D1.setSizePolicy(sizePolicy)
        self.D1.setMinimumSize(QtCore.QSize(80, 0))
        self.D1.setObjectName("D1")
        self.horizontalLayout_25.addWidget(self.D1)
        self.verticalLayout_2.addLayout(self.horizontalLayout_25)
        self.horizontalLayout_27 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_27.setObjectName("horizontalLayout_27")
        self.label_24 = QtWidgets.QLabel(CONTAB)
        self.label_24.setObjectName("label_24")
        self.horizontalLayout_27.addWidget(self.label_24)
        spacerItem14 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_27.addItem(spacerItem14)
        self.D2 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.D2.sizePolicy().hasHeightForWidth())
        self.D2.setSizePolicy(sizePolicy)
        self.D2.setMinimumSize(QtCore.QSize(80, 0))
        self.D2.setObjectName("D2")
        self.horizontalLayout_27.addWidget(self.D2)
        self.verticalLayout_2.addLayout(self.horizontalLayout_27)
        self.horizontalLayout_26 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_26.setObjectName("horizontalLayout_26")
        self.label_23 = QtWidgets.QLabel(CONTAB)
        self.label_23.setObjectName("label_23")
        self.horizontalLayout_26.addWidget(self.label_23)
        spacerItem15 = QtWidgets.QSpacerItem(1, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_26.addItem(spacerItem15)
        self.D3 = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.D3.sizePolicy().hasHeightForWidth())
        self.D3.setSizePolicy(sizePolicy)
        self.D3.setMinimumSize(QtCore.QSize(80, 0))
        self.D3.setObjectName("D3")
        self.horizontalLayout_26.addWidget(self.D3)
        self.verticalLayout_2.addLayout(self.horizontalLayout_26)
        self.horizontalLayout_29 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_29.setObjectName("horizontalLayout_29")
        self.label_26 = QtWidgets.QLabel(CONTAB)
        self.label_26.setObjectName("label_26")
        self.horizontalLayout_29.addWidget(self.label_26)
        spacerItem16 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_29.addItem(spacerItem16)
        self.CGMAX = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CGMAX.sizePolicy().hasHeightForWidth())
        self.CGMAX.setSizePolicy(sizePolicy)
        self.CGMAX.setMinimumSize(QtCore.QSize(80, 0))
        self.CGMAX.setObjectName("CGMAX")
        self.horizontalLayout_29.addWidget(self.CGMAX)
        self.verticalLayout_2.addLayout(self.horizontalLayout_29)
        self.horizontalLayout_30 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_30.setObjectName("horizontalLayout_30")
        self.label_27 = QtWidgets.QLabel(CONTAB)
        self.label_27.setObjectName("label_27")
        self.horizontalLayout_30.addWidget(self.label_27)
        spacerItem17 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_30.addItem(spacerItem17)
        self.KS = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.KS.sizePolicy().hasHeightForWidth())
        self.KS.setSizePolicy(sizePolicy)
        self.KS.setMinimumSize(QtCore.QSize(80, 0))
        self.KS.setObjectName("KS")
        self.horizontalLayout_30.addWidget(self.KS)
        self.verticalLayout_2.addLayout(self.horizontalLayout_30)
        self.horizontalLayout_28 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_28.setObjectName("horizontalLayout_28")
        self.label_25 = QtWidgets.QLabel(CONTAB)
        self.label_25.setObjectName("label_25")
        self.horizontalLayout_28.addWidget(self.label_25)
        spacerItem18 = QtWidgets.QSpacerItem(1, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_28.addItem(spacerItem18)
        self.RL = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.RL.sizePolicy().hasHeightForWidth())
        self.RL.setSizePolicy(sizePolicy)
        self.RL.setMinimumSize(QtCore.QSize(80, 0))
        self.RL.setObjectName("RL")
        self.horizontalLayout_28.addWidget(self.RL)
        self.verticalLayout_2.addLayout(self.horizontalLayout_28)
        self.horizontalLayout_31 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_31.setObjectName("horizontalLayout_31")
        self.label_28 = QtWidgets.QLabel(CONTAB)
        self.label_28.setObjectName("label_28")
        self.horizontalLayout_31.addWidget(self.label_28)
        spacerItem19 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_31.addItem(spacerItem19)
        self.BGR = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BGR.sizePolicy().hasHeightForWidth())
        self.BGR.setSizePolicy(sizePolicy)
        self.BGR.setMinimumSize(QtCore.QSize(80, 0))
        self.BGR.setObjectName("BGR")
        self.horizontalLayout_31.addWidget(self.BGR)
        self.verticalLayout_2.addLayout(self.horizontalLayout_31)
        self.horizontalLayout_32 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_32.setObjectName("horizontalLayout_32")
        self.label_29 = QtWidgets.QLabel(CONTAB)
        self.label_29.setObjectName("label_29")
        self.horizontalLayout_32.addWidget(self.label_29)
        spacerItem20 = QtWidgets.QSpacerItem(1, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_32.addItem(spacerItem20)
        self.DELR = QtWidgets.QLineEdit(CONTAB)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.DELR.sizePolicy().hasHeightForWidth())
        self.DELR.setSizePolicy(sizePolicy)
        self.DELR.setMinimumSize(QtCore.QSize(80, 0))
        self.DELR.setToolTip("")
        self.DELR.setObjectName("DELR")
        self.horizontalLayout_32.addWidget(self.DELR)
        self.verticalLayout_2.addLayout(self.horizontalLayout_32)
        spacerItem21 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_2.addItem(spacerItem21)
        self.horizontalLayout.addLayout(self.verticalLayout_2)
        self.tabWidget_input = QtWidgets.QTabWidget(CONTAB)
        self.tabWidget_input.setObjectName("tabWidget_input")
        self.tab_6 = QtWidgets.QWidget()
        self.tab_6.setObjectName("tab_6")
        self.horizontalLayout_18 = QtWidgets.QHBoxLayout(self.tab_6)
        self.horizontalLayout_18.setObjectName("horizontalLayout_18")
        self.textBrowser_2 = QtWidgets.QTextBrowser(self.tab_6)
        self.textBrowser_2.setObjectName("textBrowser_2")
        self.horizontalLayout_18.addWidget(self.textBrowser_2)
        self.tabWidget_input.addTab(self.tab_6, "")
        self.horizontalLayout.addWidget(self.tabWidget_input)

        self.retranslateUi(CONTAB)
        self.tabWidget_input.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(CONTAB)

    def retranslateUi(self, CONTAB):
        _translate = QtCore.QCoreApplication.translate
        CONTAB.setWindowTitle(_translate("CONTAB", "CONTAB"))
        self.label_13.setText(_translate("CONTAB", "TTYPE 类型"))
        self.comboBox_TTYPE.setItemText(0, _translate("CONTAB", "1. control tab"))
        self.comboBox_TTYPE.setItemText(1, _translate("CONTAB", "2. trim tab"))
        self.comboBox_TTYPE.setItemText(2, _translate("CONTAB", "3. both"))
        self.label_8.setText(_translate("CONTAB", "CFITC  控制片内侧弦长"))
        self.label_9.setText(_translate("CONTAB", "CFOTC  控制片外侧弦长"))
        self.label_10.setText(_translate("CONTAB", "BITC  控制片内侧区域位置"))
        self.label_11.setText(_translate("CONTAB", "BOTC  控制片外侧区域位置"))
        self.label_14.setText(_translate("CONTAB", "CFITT  配平片内侧弦长"))
        self.label_15.setText(_translate("CONTAB", "CFOTT  配平片外侧弦长"))
        self.label_17.setText(_translate("CONTAB", "BITT  配平片内侧区域位置"))
        self.label_16.setText(_translate("CONTAB", "BOTT  配平片外侧区域位置"))
        self.label_21.setText(_translate("CONTAB", "B1"))
        self.label_20.setText(_translate("CONTAB", "B2"))
        self.label_18.setText(_translate("CONTAB", "B3"))
        self.label_19.setText(_translate("CONTAB", "B4"))
        self.label_22.setText(_translate("CONTAB", "D1"))
        self.label_24.setText(_translate("CONTAB", "D2"))
        self.label_23.setText(_translate("CONTAB", "D3"))
        self.label_26.setText(_translate("CONTAB", "CGMAX"))
        self.label_27.setText(_translate("CONTAB", "KS"))
        self.label_25.setText(_translate("CONTAB", "RL"))
        self.label_28.setText(_translate("CONTAB", "BGR"))
        self.label_29.setText(_translate("CONTAB", "DELR"))
        self.textBrowser_2.setHtml(_translate("CONTAB", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"> </p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"readme\"></a>    </p>\n"
"<p style=\" margin-top:18px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-namelist-contab选项卡\"></a><span style=\" font-size:xx-large; font-weight:600;\">N</span><span style=\" font-size:xx-large; font-weight:600;\">AMELIST CONTAB选项卡</span></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-控制片和配平片的输入项\"></a><span style=\" font-size:x-large; font-weight:600;\">控</span><span style=\" font-size:x-large; font-weight:600;\">制片和配平片的输入项</span></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /> <br />### 参数示意图![参数示意图](fig/CONTAB-para1.png) ### 工程量含义表11![参数示意图](fig/CONTAB-para2.png) ### 工程量含义![参数示意图](fig/CONTAB-para3.png) ### 工程量含义表12![参数示意图](fig/CONTAB-para4.png) <br /><br /></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-参数表\"></a><span style=\" font-size:x-large; font-weight:600;\">参</span><span style=\" font-size:x-large; font-weight:600;\">数表</span></p>\n"
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
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">TTYPE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">类型</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">= 1 Tab control<br /> =2 TRIM tab<br /> =3 Both</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">= 1 控制片<br /> =2 配平片<br /> =3 均是</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(Cfi)tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CFITC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片内侧弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">inboard chord,control tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片内侧弦长</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(Cfo)tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CFOTC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片外侧弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">outboard chord,control tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片外侧弦长</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(bi)tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BITC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片内侧区域位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">inboard span location control tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片内侧区域位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(bo)tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BOTC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片外侧区域位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">outboard span location control tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">控制片外侧区域位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(Cfi)tt</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CFITT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片内侧弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">inboard chord,trim tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片内侧弦长</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(Cfo)tt</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CFOTT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片外侧弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">outboard chord,trim tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片外侧弦长</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(bi)tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BITT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片内侧区域位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">inboard span location trim tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片内侧区域位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(bo)tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BOTT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片外侧区域位置</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">outboard span location trim tab</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">配平片外侧区域位置</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B1</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B1</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B2</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B2</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B3</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B3</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B4</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">B4</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">D1</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">D1</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">D2</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">D2</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">D3</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">D3</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Gcmax</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CGMAX</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">k</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">KS</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Rl</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">RL</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">β</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">BGR</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Δr</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELR</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">see table11 for definitions</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">见表11</p></td></tr></table>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br />## tips1 KS . if the system has a spring ,KS input ,then free stream dynamic pressure is required<br />如果输入中有KS这个字符串，则必须输入自由流动压.</p>\n"
"<p style=\" margin-top:18px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-分析表格的具体内容\"></a><span style=\" font-size:xx-large; font-weight:600;\">分</span><span style=\" font-size:xx-large; font-weight:600;\">析表格的具体内容</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-1-a_c\"></a><span style=\" font-size:large; font-weight:600;\">1</span><span style=\" font-size:large; font-weight:600;\"> $A_C$</span></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content--a_cfracs_texttcoverlinec_texttcs_coverlinec_c\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\"> A_c=\\frac{S_\\text{tc}\\overline{C}_\\text{tc}}{S_c\\overline{C}_c}$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">其中：</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">$S()$表示表面积 ，可动表面积定义为铰链之后的面积<br />surface area （movable surface are defined by their area aft of the hinge line）</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">$\\overline{C}()$ 表示表面平均气动弦长，可动表面定义铰链之后的面积，用$MAC$表示<br />surface mean aerodynamic chord (movalbe surfaces are defined by their area aft of the hinge line,and the MAC is of that area)</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">下标含义：</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">c 表示主控制面 main control surface</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">s 表示主控制面 附属的面 ，例如平尾、垂尾、机翼<br />surface to which the main control surface is attached，i，e，horizontal tail，vertical tail or wing。</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tc  控制片 control tab</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tt  配平片 trim tab</p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_1\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">B_1$</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_1--left--fracpartial-c_h_cpartial-delta-c-right-delta-_tca_sdelta-_tt----left--c-_h-delta--right-c--1deg\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">B_1 = \\left ( \\frac{\\partial C_{h_c}}{\\partial \\delta </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">{c}} \\right )</span><span style=\" font-size:large; font-weight:600;\">{\\delta _{tc},a_s,\\delta _{tt}}   = \\left ( C _{h </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">{\\delta} } \\right )</span><span style=\" font-size:large; font-weight:600;\">{{c}\'}$  ,$1/Deg$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">参见 6.1.6.2章 (Datcom Section 6.1.6.2)</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_2\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">B_2$</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_2--left--fracpartial-c_h_cpartial-delta-tc-right-delta-_ca_sdelta-_tt-----1deg\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">B_2 = \\left ( \\frac{\\partial C_{h_c}}{\\partial \\delta </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">{tc}} \\right )</span><span style=\" font-size:large; font-weight:600;\">{\\delta _{{c}\'{a_s}\'},\\delta _{tt}}   $  ,$1/Deg$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">用户输入 。user input</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_3\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">B_3$</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_3--left--fracpartial-c-_h_cpartial-delta-as-right--_delta-_c-delta-_tc-delta-_tt-----left--c-_h-a--right-c\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">B_3 = \\left ( \\frac{\\partial C _{h_c}}{\\partial \\delta </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">{a</span><span style=\" font-size:large; font-weight:600;\">{s}}} \\right ) _{\\delta _{{c}\'} \\delta _{t{c}\'} \\delta _{tt}}    = \\left ( C _{h </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">{a } }\\right )</span><span style=\" font-size:large; font-weight:600;\">{{c}\'}$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">参见 6.1.6.1章 (Datcom Section 6.1.6.1)</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_4\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">B_4$</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-b_4--left--fracpartial-c-_h_cpartial-delta-_tt-right--_delta-_c-delta-_tc-a_s---1deg\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">B_4 = \\left ( \\frac{\\partial C _{h_c}}{\\partial \\delta _{tt}} \\right ) _{\\delta _{{c}\'} \\delta _{t{c}\'} a_s} $  ,$1/Deg$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">用户输入 。user input</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-f_c\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">F_c$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">$F_c$      控制柱力(拉力为正)  control-column force (pull force is positive)</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-g-_c-_max\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">G _{c _{max}}$</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-g-_c-_max-frac-1573left--fracpartial-x_cpartial-delta-c-right-max\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">G _{c _{max}} =\\frac {1}{57.3\\left ( \\frac{\\partial x_c}{\\partial \\delta </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">c} \\right )</span><span style=\" font-size:large; font-weight:600;\">{max}}$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">maximum stick gearing user input. <br />if $R_L$=0,$G_{c <span style=\" font-style:italic;\">{max}}$ also is zero.In this case input $G</span>{tc_{max}}$ and $\\Delta r = 1.0 ( G_{tc_{max}} = G_{c_{max}} * \\Delta r )$ .</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-k\"></a><span style=\" font-size:x-large; font-weight:600;\">K</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-k---left--fracpartial-m-_tcpartial-delta--_tc-right--_spring-frac1s-tc-overline-ctc---tab-spring-effectiveness\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">k = -\\left ( \\frac{\\partial M _{tc}}{\\partial \\delta  _{tc}} \\right ) _{spring} \\frac{1}{S </span><span style=\" font-size:large; font-weight:600; font-style:italic;\">{tc} \\overline c</span><span style=\" font-size:large; font-weight:600;\">{tc}}$   tab spring effectiveness</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-q--local-dynamic-pressure\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">q$  local dynamic pressure</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-r_1r_2\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">R_1,R_2$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">shorthand notation for tab and main surface hinge moments and key linkage parameters,obtained from Table 12<br /></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-r_l\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\">R_L$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">aerodynamic boost link ratio ,user input . $(R_L \\geqslant 0)$ . To input $R_L = \\infty $ , set $R_L &lt;  0$ .</p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-a_s\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">a_s$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">angle of attack of the surface to which the main control surface is attached, Deg</p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">附加控制面的曲面的攻角，Deg</p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-beta\"></a><span style=\" font-size:x-large; font-weight:600;\">$</span><span style=\" font-size:x-large; font-weight:600;\">\\beta$</span></p>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content--beta--left--fracpartial-delta-_tcpartial-delta-_c-right--_beginmatrixstick-free-endmatrix\"></a><span style=\" font-size:large; font-weight:600;\">$</span><span style=\" font-size:large; font-weight:600;\"> \\beta = \\left ( \\frac{\\partial \\delta _{tc}}{\\partial \\delta _{c}} \\right ) _{\\begin{matrix}stick\\ free \\end{matrix}}$</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">with $k = \\infty$  control-tab gear ratio  </p></body></html>"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_6), _translate("CONTAB", "参数说明"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    CONTAB = QtWidgets.QWidget()
    ui = Ui_CONTAB()
    ui.setupUi(CONTAB)
    CONTAB.show()
    sys.exit(app.exec_())

