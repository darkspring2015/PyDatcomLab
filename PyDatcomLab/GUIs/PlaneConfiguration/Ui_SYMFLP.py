# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\SYMFLP.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_SYMFLP(object):
    def setupUi(self, SYMFLP):
        SYMFLP.setObjectName("SYMFLP")
        SYMFLP.resize(1118, 531)
        self.horizontalLayout_16 = QtWidgets.QHBoxLayout(SYMFLP)
        self.horizontalLayout_16.setObjectName("horizontalLayout_16")
        self.verticalLayout = QtWidgets.QVBoxLayout()
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtWidgets.QLabel(SYMFLP)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        spacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.comboBox_FTYPE = QtWidgets.QComboBox(SYMFLP)
        self.comboBox_FTYPE.setMinimumSize(QtCore.QSize(80, 0))
        self.comboBox_FTYPE.setObjectName("comboBox_FTYPE")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.comboBox_FTYPE.addItem("")
        self.horizontalLayout.addWidget(self.comboBox_FTYPE)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.label_2 = QtWidgets.QLabel(SYMFLP)
        self.label_2.setObjectName("label_2")
        self.horizontalLayout_2.addWidget(self.label_2)
        spacerItem1 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem1)
        self.NDELTA = QtWidgets.QLineEdit(SYMFLP)
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
        self.label_3 = QtWidgets.QLabel(SYMFLP)
        self.label_3.setObjectName("label_3")
        self.horizontalLayout_3.addWidget(self.label_3)
        spacerItem2 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem2)
        self.PHETE = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.PHETE.sizePolicy().hasHeightForWidth())
        self.PHETE.setSizePolicy(sizePolicy)
        self.PHETE.setMinimumSize(QtCore.QSize(80, 0))
        self.PHETE.setObjectName("PHETE")
        self.horizontalLayout_3.addWidget(self.PHETE)
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.label_4 = QtWidgets.QLabel(SYMFLP)
        self.label_4.setObjectName("label_4")
        self.horizontalLayout_4.addWidget(self.label_4)
        spacerItem3 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem3)
        self.PHETEP = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.PHETEP.sizePolicy().hasHeightForWidth())
        self.PHETEP.setSizePolicy(sizePolicy)
        self.PHETEP.setMinimumSize(QtCore.QSize(80, 0))
        self.PHETEP.setObjectName("PHETEP")
        self.horizontalLayout_4.addWidget(self.PHETEP)
        self.verticalLayout.addLayout(self.horizontalLayout_4)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.label_5 = QtWidgets.QLabel(SYMFLP)
        self.label_5.setObjectName("label_5")
        self.horizontalLayout_5.addWidget(self.label_5)
        spacerItem4 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem4)
        self.CHRDFI = QtWidgets.QLineEdit(SYMFLP)
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
        self.label_6 = QtWidgets.QLabel(SYMFLP)
        self.label_6.setObjectName("label_6")
        self.horizontalLayout_6.addWidget(self.label_6)
        spacerItem5 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_6.addItem(spacerItem5)
        self.CHRDFO = QtWidgets.QLineEdit(SYMFLP)
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
        self.label_7 = QtWidgets.QLabel(SYMFLP)
        self.label_7.setObjectName("label_7")
        self.horizontalLayout_7.addWidget(self.label_7)
        spacerItem6 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_7.addItem(spacerItem6)
        self.SPANFI = QtWidgets.QLineEdit(SYMFLP)
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
        self.label_8 = QtWidgets.QLabel(SYMFLP)
        self.label_8.setObjectName("label_8")
        self.horizontalLayout_8.addWidget(self.label_8)
        spacerItem7 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_8.addItem(spacerItem7)
        self.SPANFO = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.SPANFO.sizePolicy().hasHeightForWidth())
        self.SPANFO.setSizePolicy(sizePolicy)
        self.SPANFO.setMinimumSize(QtCore.QSize(80, 0))
        self.SPANFO.setObjectName("SPANFO")
        self.horizontalLayout_8.addWidget(self.SPANFO)
        self.verticalLayout.addLayout(self.horizontalLayout_8)
        self.horizontalLayout_9 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_9.setObjectName("horizontalLayout_9")
        self.label_9 = QtWidgets.QLabel(SYMFLP)
        self.label_9.setObjectName("label_9")
        self.horizontalLayout_9.addWidget(self.label_9)
        spacerItem8 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_9.addItem(spacerItem8)
        self.DOBCIN = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.DOBCIN.sizePolicy().hasHeightForWidth())
        self.DOBCIN.setSizePolicy(sizePolicy)
        self.DOBCIN.setMinimumSize(QtCore.QSize(80, 0))
        self.DOBCIN.setObjectName("DOBCIN")
        self.horizontalLayout_9.addWidget(self.DOBCIN)
        self.verticalLayout.addLayout(self.horizontalLayout_9)
        self.horizontalLayout_10 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_10.setObjectName("horizontalLayout_10")
        self.label_10 = QtWidgets.QLabel(SYMFLP)
        self.label_10.setObjectName("label_10")
        self.horizontalLayout_10.addWidget(self.label_10)
        spacerItem9 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_10.addItem(spacerItem9)
        self.DOBCOT = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.DOBCOT.sizePolicy().hasHeightForWidth())
        self.DOBCOT.setSizePolicy(sizePolicy)
        self.DOBCOT.setMinimumSize(QtCore.QSize(80, 0))
        self.DOBCOT.setObjectName("DOBCOT")
        self.horizontalLayout_10.addWidget(self.DOBCOT)
        self.verticalLayout.addLayout(self.horizontalLayout_10)
        self.horizontalLayout_11 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_11.setObjectName("horizontalLayout_11")
        self.label_11 = QtWidgets.QLabel(SYMFLP)
        self.label_11.setObjectName("label_11")
        self.horizontalLayout_11.addWidget(self.label_11)
        spacerItem10 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_11.addItem(spacerItem10)
        self.CB = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CB.sizePolicy().hasHeightForWidth())
        self.CB.setSizePolicy(sizePolicy)
        self.CB.setMinimumSize(QtCore.QSize(80, 0))
        self.CB.setObjectName("CB")
        self.horizontalLayout_11.addWidget(self.CB)
        self.verticalLayout.addLayout(self.horizontalLayout_11)
        self.horizontalLayout_12 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_12.setObjectName("horizontalLayout_12")
        self.label_12 = QtWidgets.QLabel(SYMFLP)
        self.label_12.setObjectName("label_12")
        self.horizontalLayout_12.addWidget(self.label_12)
        spacerItem11 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_12.addItem(spacerItem11)
        self.TC = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.TC.sizePolicy().hasHeightForWidth())
        self.TC.setSizePolicy(sizePolicy)
        self.TC.setMinimumSize(QtCore.QSize(80, 0))
        self.TC.setObjectName("TC")
        self.horizontalLayout_12.addWidget(self.TC)
        self.verticalLayout.addLayout(self.horizontalLayout_12)
        self.horizontalLayout_13 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_13.setObjectName("horizontalLayout_13")
        self.label_13 = QtWidgets.QLabel(SYMFLP)
        self.label_13.setObjectName("label_13")
        self.horizontalLayout_13.addWidget(self.label_13)
        spacerItem12 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_13.addItem(spacerItem12)
        self.comboBox_NTYPE = QtWidgets.QComboBox(SYMFLP)
        self.comboBox_NTYPE.setMinimumSize(QtCore.QSize(80, 0))
        self.comboBox_NTYPE.setObjectName("comboBox_NTYPE")
        self.comboBox_NTYPE.addItem("")
        self.comboBox_NTYPE.addItem("")
        self.comboBox_NTYPE.addItem("")
        self.horizontalLayout_13.addWidget(self.comboBox_NTYPE)
        self.verticalLayout.addLayout(self.horizontalLayout_13)
        self.horizontalLayout_14 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_14.setObjectName("horizontalLayout_14")
        self.label_14 = QtWidgets.QLabel(SYMFLP)
        self.label_14.setObjectName("label_14")
        self.horizontalLayout_14.addWidget(self.label_14)
        spacerItem13 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_14.addItem(spacerItem13)
        self.comboBox_JETFLP = QtWidgets.QComboBox(SYMFLP)
        self.comboBox_JETFLP.setObjectName("comboBox_JETFLP")
        self.comboBox_JETFLP.addItem("")
        self.comboBox_JETFLP.addItem("")
        self.comboBox_JETFLP.addItem("")
        self.comboBox_JETFLP.addItem("")
        self.horizontalLayout_14.addWidget(self.comboBox_JETFLP)
        self.verticalLayout.addLayout(self.horizontalLayout_14)
        self.horizontalLayout_15 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_15.setObjectName("horizontalLayout_15")
        self.label_15 = QtWidgets.QLabel(SYMFLP)
        self.label_15.setObjectName("label_15")
        self.horizontalLayout_15.addWidget(self.label_15)
        spacerItem14 = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_15.addItem(spacerItem14)
        self.CMU = QtWidgets.QLineEdit(SYMFLP)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CMU.sizePolicy().hasHeightForWidth())
        self.CMU.setSizePolicy(sizePolicy)
        self.CMU.setMinimumSize(QtCore.QSize(80, 0))
        self.CMU.setObjectName("CMU")
        self.horizontalLayout_15.addWidget(self.CMU)
        self.verticalLayout.addLayout(self.horizontalLayout_15)
        self.horizontalLayout_16.addLayout(self.verticalLayout)
        self.tabWidget_input = QtWidgets.QTabWidget(SYMFLP)
        self.tabWidget_input.setObjectName("tabWidget_input")
        self.tab_4 = QtWidgets.QWidget()
        self.tab_4.setObjectName("tab_4")
        self.horizontalLayout_19 = QtWidgets.QHBoxLayout(self.tab_4)
        self.horizontalLayout_19.setObjectName("horizontalLayout_19")
        self.tableWidget_flap = QtWidgets.QTableWidget(self.tab_4)
        self.tableWidget_flap.setObjectName("tableWidget_flap")
        self.tableWidget_flap.setColumnCount(0)
        self.tableWidget_flap.setRowCount(0)
        self.horizontalLayout_19.addWidget(self.tableWidget_flap)
        self.tableWidget_delta = QtWidgets.QTableWidget(self.tab_4)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.tableWidget_delta.sizePolicy().hasHeightForWidth())
        self.tableWidget_delta.setSizePolicy(sizePolicy)
        self.tableWidget_delta.setMaximumSize(QtCore.QSize(200, 16777215))
        self.tableWidget_delta.setObjectName("tableWidget_delta")
        self.tableWidget_delta.setColumnCount(0)
        self.tableWidget_delta.setRowCount(0)
        self.horizontalLayout_19.addWidget(self.tableWidget_delta)
        self.tabWidget_input.addTab(self.tab_4, "")
        self.tab_5 = QtWidgets.QWidget()
        self.tab_5.setObjectName("tab_5")
        self.horizontalLayout_20 = QtWidgets.QHBoxLayout(self.tab_5)
        self.horizontalLayout_20.setObjectName("horizontalLayout_20")
        self.tableWidget_jet = QtWidgets.QTableWidget(self.tab_5)
        self.tableWidget_jet.setObjectName("tableWidget_jet")
        self.tableWidget_jet.setColumnCount(0)
        self.tableWidget_jet.setRowCount(0)
        self.horizontalLayout_20.addWidget(self.tableWidget_jet)
        self.tabWidget_input.addTab(self.tab_5, "")
        self.tab = QtWidgets.QWidget()
        self.tab.setObjectName("tab")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout(self.tab)
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.scrollArea = QtWidgets.QScrollArea(self.tab)
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setObjectName("scrollArea")
        self.scrollAreaWidgetContents = QtWidgets.QWidget()
        self.scrollAreaWidgetContents.setGeometry(QtCore.QRect(0, 0, 706, 578))
        self.scrollAreaWidgetContents.setObjectName("scrollAreaWidgetContents")
        self.horizontalLayout_18 = QtWidgets.QHBoxLayout(self.scrollAreaWidgetContents)
        self.horizontalLayout_18.setObjectName("horizontalLayout_18")
        self.label_16 = QtWidgets.QLabel(self.scrollAreaWidgetContents)
        self.label_16.setText("")
        self.label_16.setPixmap(QtGui.QPixmap(":/card/rc_card/SYMFLP-2.png"))
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
        self.scrollAreaWidgetContents_2.setGeometry(QtCore.QRect(0, 0, 919, 560))
        self.scrollAreaWidgetContents_2.setObjectName("scrollAreaWidgetContents_2")
        self.verticalLayout_4 = QtWidgets.QVBoxLayout(self.scrollAreaWidgetContents_2)
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.label_17 = QtWidgets.QLabel(self.scrollAreaWidgetContents_2)
        self.label_17.setText("")
        self.label_17.setPixmap(QtGui.QPixmap(":/card/rc_card/SYMFLP-para1.png"))
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
        self.horizontalLayout_16.addWidget(self.tabWidget_input)
        self.actionAddRow = QtWidgets.QAction(SYMFLP)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAddRow.setIcon(icon)
        self.actionAddRow.setObjectName("actionAddRow")
        self.actionDeleteRow = QtWidgets.QAction(SYMFLP)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/cardIco/rc_card/icos/DeletedIcon.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionDeleteRow.setIcon(icon1)
        self.actionDeleteRow.setObjectName("actionDeleteRow")

        self.retranslateUi(SYMFLP)
        self.tabWidget_input.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(SYMFLP)

    def retranslateUi(self, SYMFLP):
        _translate = QtCore.QCoreApplication.translate
        SYMFLP.setWindowTitle(_translate("SYMFLP", "对称襟翼 SYMFLP"))
        self.label.setText(_translate("SYMFLP", "FTYPE  襟翼类型"))
        self.comboBox_FTYPE.setItemText(0, _translate("SYMFLP", "1.0 平襟翼"))
        self.comboBox_FTYPE.setItemText(1, _translate("SYMFLP", "2.0 单缝襟翼"))
        self.comboBox_FTYPE.setItemText(2, _translate("SYMFLP", "3.0 福勒襟翼"))
        self.comboBox_FTYPE.setItemText(3, _translate("SYMFLP", "4.0 双缝襟翼"))
        self.comboBox_FTYPE.setItemText(4, _translate("SYMFLP", "5.0 分裂襟翼"))
        self.comboBox_FTYPE.setItemText(5, _translate("SYMFLP", "6.0 前缘襟翼"))
        self.comboBox_FTYPE.setItemText(6, _translate("SYMFLP", "7.0 前缘缝翼"))
        self.comboBox_FTYPE.setItemText(7, _translate("SYMFLP", "8.0 克鲁格襟翼 "))
        self.comboBox_FTYPE.setItemText(8, _translate("SYMFLP", "9.0 射流襟翼"))
        self.label_2.setText(_translate("SYMFLP", "NDELTA 偏转角数量"))
        self.label_3.setText(_translate("SYMFLP", "PHETE  翼型后缘角正切值(90,99)"))
        self.label_4.setText(_translate("SYMFLP", "PHETEP  翼型后缘角正切值(95,99)"))
        self.label_5.setText(_translate("SYMFLP", "CHRDFI  襟翼内侧襟翼弦 "))
        self.label_6.setText(_translate("SYMFLP", "CHRDFO  襟翼外侧襟翼弦"))
        self.label_7.setText(_translate("SYMFLP", "SPANFI  襟翼内侧端的横向距离"))
        self.label_8.setText(_translate("SYMFLP", "SPANFO  襟翼外侧端的横向距离"))
        self.label_9.setText(_translate("SYMFLP", "DOBCIN"))
        self.label_10.setText(_translate("SYMFLP", "DOBCOT"))
        self.label_11.setText(_translate("SYMFLP", "CB  平衡的平均弦长"))
        self.label_12.setText(_translate("SYMFLP", "TC  铰链处平均厚度"))
        self.label_13.setText(_translate("SYMFLP", "NTYPE  鼻型"))
        self.comboBox_NTYPE.setItemText(0, _translate("SYMFLP", "1.0 圆形鼻襟翼"))
        self.comboBox_NTYPE.setItemText(1, _translate("SYMFLP", "2.0 椭圆形鼻襟翼"))
        self.comboBox_NTYPE.setItemText(2, _translate("SYMFLP", "3.0 尖鼻襟翼"))
        self.label_14.setText(_translate("SYMFLP", "JETFLP  喷气襟翼类型 "))
        self.comboBox_JETFLP.setItemText(0, _translate("SYMFLP", "1.0 纯喷气襟翼"))
        self.comboBox_JETFLP.setItemText(1, _translate("SYMFLP", "2.0 IBF"))
        self.comboBox_JETFLP.setItemText(2, _translate("SYMFLP", "3.0 EBF"))
        self.comboBox_JETFLP.setItemText(3, _translate("SYMFLP", "4.0 机械喷气组合襟翼"))
        self.label_15.setText(_translate("SYMFLP", "CMU  二维射流系数"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_4), _translate("SYMFLP", "可变参数"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_5), _translate("SYMFLP", "射流襟翼参数"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab), _translate("SYMFLP", "示意图"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_2), _translate("SYMFLP", "参数表"))
        self.textBrowser.setHtml(_translate("SYMFLP", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-symetrical-flap-deflection-inputs--对称襟翼偏转的输入项\"></a><span style=\" font-size:x-large; font-weight:600;\">s</span><span style=\" font-size:x-large; font-weight:600;\">ymetrical flap deflection inputs  对称襟翼偏转的输入项</span></p>\n"
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
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">FTYPE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼类别</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 plain flaps <br /> =2.0 single slotted flaps <br /> =3.0 fowler flaps <br /> =4.0 double slotted flaps<br /> =5.0 split flaps <br /> =6.0 leading edge flap <br /> =7.0 leading edge slats <br /> =8.0 krueger</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 平襟翼<br /> =2.0 单缝襟翼 <br /> =3.0 福勒襟翼 <br /> =4.0 双缝襟翼 <br /> =5.0 分裂襟翼 <br /> =6.0 前缘襟翼 <br /> =7.0 前缘缝翼 <br /> =8.0 克鲁格襟翼</p></td></tr>\n"
"<tr>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">NDELTA</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼或slat的偏转角数量</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">number of flap or slat deflection angles ,max 9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼或slat的偏转角数量，最大9</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δf</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELTA</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼偏转角</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">flap deflection angles measured steamwise</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">从流向（stramwise）测量的襟翼偏转角</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tan(OTE/2)</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">PHETE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">翼型后缘角正切值</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tangent of airfoil trailine edge angle based on ordinates at 90 and 99 percent chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">翼型后缘角正切值 在95和99%弦长处</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tan(OTE/2)</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">PHETEP</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">翼型后缘角正切值</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tangent of airfoil trailine edge angle based on ordinates at 95 and 99 percent chord</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">翼型后缘角正切值 在95和99%弦长处</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cfi</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CHRDFI</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼内侧襟翼弦</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">flap chord at inboard end of flap，measured parallel to longitudinal axis</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">平行于纵轴的襟翼内侧襟翼弦</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cfo</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CHRDFO</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼外侧襟翼弦</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">flap chord at outboard end of flap，measured parallel to longitudinal axis</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">平行于纵轴的襟翼外侧襟翼弦</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">bi</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">SPANFI</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼内侧端的横向距离</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">span location of inborad end of flap ,measured perpendicular to vertical plane of symmetry</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼内侧端的横向距离，测量垂直于纵向对称面</p></td></tr>\n"
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
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">span location of outborad end of flap ,measured perpendicular to vertical plane of symmetry</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼外侧端的横向距离，测量垂直于纵向对称面</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Ci\'</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CPRMEI</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼内侧端的翼型弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">total wing chord at inboard end of flap (translating devices only) measured parallel to longitudinal axis</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼内侧端的弦长(translating devices only)  测量平行于纵向轴线</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Co\'</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CPRMEO</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼外侧端的翼型弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">total wing chord at outboard end of flap (translating devices only) measured parallel to longitudinal axis</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">襟翼外侧端的弦长(translating devices only)  测量平行于纵向轴线</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">C\'ai</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CAPINB</p></td>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">双缝襟翼需要</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cao</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CAPOUT</p></td>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td></td>\n"
"<td></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">(δf)2</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DOBDEF</p></td>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td></td>\n"
"<td></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">C2f</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DOBCIN</p></td>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td></td>\n"
"<td></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">C2o</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DOBCOT</p></td>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td></td>\n"
"<td></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">ΔCf</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">SCLD</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">截面升力系数增量</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">increment in section lift coefficient due to deflecting flap to the angle δf</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">偏转襟翼δf产生的面积升力系数增量</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">ΔCmf</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">SCMD</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">截面俯仰力矩系数增量</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">increment in section pitching moment coefficient due to deflecting flap to the angle δf</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">偏转襟翼δf产生的面积俯仰力矩系数增量</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cb</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CB</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">平衡的平均弦长</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">average chord of the balance</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">平衡的平均弦长</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">tc</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">TC</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">铰链处平均厚度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">average thickness of the control at hinge line</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">铰链线控制的平均厚度</p></td></tr>\n"
"<tr>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">NTYPE</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">鼻型</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 round nose flap <br /> =2.0 elliptic nose flap <br /> =3.0 sharp nose flap</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">＝1.0 圆形鼻襟翼<br /> ＝2.0 椭圆形鼻襟翼<br /> ＝3.0 尖鼻襟翼</p></td></tr>\n"
"<tr>\n"
"<td></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">JETFLP</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">喷气襟翼类型</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 pure jet flap <br /> =2.0 IBF <br /> =3.0 EBF <br /> =4.0 combination mechanical and pure jet flap</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">=1.0 纯喷气襟翼 <br /> =2.0 IBF <br /> =3.0 EBF <br /> =4.0 机械喷气组合襟翼</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Cμ</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">CMU</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">二维射流系数</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">two dimensional jet efflux coefficient</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">二维射流系数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δj</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">DELJET</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">射流偏转角度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">jet deflection angle</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">射流偏转角度</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">δjeff</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">EFFJET</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">EBF等效射流偏转角度</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">9</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">EBF effective jet deflection angle</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">EBF等效射流偏转角度</p></td></tr></table>\n"
"<p style=\" margin-top:14px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-tips-1标注1的选项对于所有类型襟翼是可选的\"></a><span style=\" font-size:large; font-weight:600;\">T</span><span style=\" font-size:large; font-weight:600;\">ips 1：标注1的选项对于所有类型襟翼是可选的</span></p>\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /><br /></p>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Tips 2: mechanical flap type if jetflp =4 .<br />    在jetflp =4是为机械瓣型</p></body></html>"))
        self.tabWidget_input.setTabText(self.tabWidget_input.indexOf(self.tab_3), _translate("SYMFLP", "参数说明"))
        self.actionAddRow.setText(_translate("SYMFLP", "增加行"))
        self.actionAddRow.setToolTip(_translate("SYMFLP", "新增行"))
        self.actionDeleteRow.setText(_translate("SYMFLP", "删除行"))
        self.actionDeleteRow.setToolTip(_translate("SYMFLP", "删除一行"))

import card_ico_rc
import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    SYMFLP = QtWidgets.QWidget()
    ui = Ui_SYMFLP()
    ui.setupUi(SYMFLP)
    SYMFLP.show()
    sys.exit(app.exec_())

