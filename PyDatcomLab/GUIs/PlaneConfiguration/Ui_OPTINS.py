# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\OPTINS.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_OPTINS(object):
    def setupUi(self, OPTINS):
        OPTINS.setObjectName("OPTINS")
        OPTINS.resize(736, 381)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout(OPTINS)
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        self.verticalLayout_2 = QtWidgets.QVBoxLayout()
        self.verticalLayout_2.setObjectName("verticalLayout_2")
        self.groupBox = QtWidgets.QGroupBox(OPTINS)
        self.groupBox.setTitle("")
        self.groupBox.setObjectName("groupBox")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.groupBox)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.checkBox_ROUGFC = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_ROUGFC.setObjectName("checkBox_ROUGFC")
        self.horizontalLayout.addWidget(self.checkBox_ROUGFC)
        spacerItem = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.ROUGFC = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ROUGFC.sizePolicy().hasHeightForWidth())
        self.ROUGFC.setSizePolicy(sizePolicy)
        self.ROUGFC.setObjectName("ROUGFC")
        self.horizontalLayout.addWidget(self.ROUGFC)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.checkBox_SREF = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_SREF.setObjectName("checkBox_SREF")
        self.horizontalLayout_2.addWidget(self.checkBox_SREF)
        spacerItem1 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem1)
        self.SREF = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.SREF.sizePolicy().hasHeightForWidth())
        self.SREF.setSizePolicy(sizePolicy)
        self.SREF.setObjectName("SREF")
        self.horizontalLayout_2.addWidget(self.SREF)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.checkBox_CBARR = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_CBARR.setObjectName("checkBox_CBARR")
        self.horizontalLayout_3.addWidget(self.checkBox_CBARR)
        spacerItem2 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem2)
        self.CBARR = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.CBARR.sizePolicy().hasHeightForWidth())
        self.CBARR.setSizePolicy(sizePolicy)
        self.CBARR.setObjectName("CBARR")
        self.horizontalLayout_3.addWidget(self.CBARR)
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.checkBox_BLREF = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_BLREF.setObjectName("checkBox_BLREF")
        self.horizontalLayout_4.addWidget(self.checkBox_BLREF)
        spacerItem3 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem3)
        self.BLREF = QtWidgets.QLineEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.BLREF.sizePolicy().hasHeightForWidth())
        self.BLREF.setSizePolicy(sizePolicy)
        self.BLREF.setObjectName("BLREF")
        self.horizontalLayout_4.addWidget(self.BLREF)
        self.verticalLayout.addLayout(self.horizontalLayout_4)
        self.verticalLayout_2.addWidget(self.groupBox)
        self.tableView = QtWidgets.QTableView(OPTINS)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.tableView.sizePolicy().hasHeightForWidth())
        self.tableView.setSizePolicy(sizePolicy)
        self.tableView.setObjectName("tableView")
        self.verticalLayout_2.addWidget(self.tableView)
        self.horizontalLayout_5.addLayout(self.verticalLayout_2)
        self.tabWidget = QtWidgets.QTabWidget(OPTINS)
        self.tabWidget.setObjectName("tabWidget")
        self.tab = QtWidgets.QWidget()
        self.tab.setObjectName("tab")
        self.formLayout = QtWidgets.QFormLayout(self.tab)
        self.formLayout.setObjectName("formLayout")
        self.label = QtWidgets.QLabel(self.tab)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label.sizePolicy().hasHeightForWidth())
        self.label.setSizePolicy(sizePolicy)
        self.label.setText("")
        self.label.setPixmap(QtGui.QPixmap(":/card/rc_card/Roughness.png"))
        self.label.setObjectName("label")
        self.formLayout.setWidget(0, QtWidgets.QFormLayout.LabelRole, self.label)
        self.tabWidget.addTab(self.tab, "")
        self.tab_2 = QtWidgets.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.tabWidget.addTab(self.tab_2, "")
        self.horizontalLayout_5.addWidget(self.tabWidget)

        self.retranslateUi(OPTINS)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(OPTINS)

    def retranslateUi(self, OPTINS):
        _translate = QtCore.QCoreApplication.translate
        OPTINS.setWindowTitle(_translate("OPTINS", "附加参数"))
        self.checkBox_ROUGFC.setText(_translate("OPTINS", "ROUGFC 表面粗糙度"))
        self.checkBox_SREF.setText(_translate("OPTINS", "SREF 参考面积"))
        self.checkBox_CBARR.setText(_translate("OPTINS", "CBARR 纵向参考长度值"))
        self.checkBox_BLREF.setText(_translate("OPTINS", "BLREF 横向参考长度值"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab), _translate("OPTINS", "粗糙度"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_2), _translate("OPTINS", "Tab 2"))

import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    OPTINS = QtWidgets.QWidget()
    ui = Ui_OPTINS()
    ui.setupUi(OPTINS)
    OPTINS.show()
    sys.exit(app.exec_())

