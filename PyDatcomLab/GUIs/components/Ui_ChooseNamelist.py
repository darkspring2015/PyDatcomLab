# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\ChooseNamelist.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dlg_ChooseNamelist(object):
    def setupUi(self, Dlg_ChooseNamelist):
        Dlg_ChooseNamelist.setObjectName("Dlg_ChooseNamelist")
        Dlg_ChooseNamelist.resize(310, 82)
        Dlg_ChooseNamelist.setSizeGripEnabled(True)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dlg_ChooseNamelist)
        self.verticalLayout.setObjectName("verticalLayout")
        spacerItem = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem)
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label = QtWidgets.QLabel(Dlg_ChooseNamelist)
        self.label.setObjectName("label")
        self.horizontalLayout.addWidget(self.label)
        spacerItem1 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem1)
        self.comboBox_Namelist = QtWidgets.QComboBox(Dlg_ChooseNamelist)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.comboBox_Namelist.sizePolicy().hasHeightForWidth())
        self.comboBox_Namelist.setSizePolicy(sizePolicy)
        self.comboBox_Namelist.setObjectName("comboBox_Namelist")
        self.horizontalLayout.addWidget(self.comboBox_Namelist)
        self.verticalLayout.addLayout(self.horizontalLayout)
        spacerItem2 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem2)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        spacerItem3 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem3)
        self.pushButton_Add = QtWidgets.QPushButton(Dlg_ChooseNamelist)
        self.pushButton_Add.setObjectName("pushButton_Add")
        self.horizontalLayout_2.addWidget(self.pushButton_Add)
        spacerItem4 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem4)
        self.pushButton_Cancel = QtWidgets.QPushButton(Dlg_ChooseNamelist)
        self.pushButton_Cancel.setObjectName("pushButton_Cancel")
        self.horizontalLayout_2.addWidget(self.pushButton_Cancel)
        spacerItem5 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem5)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        spacerItem6 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem6)

        self.retranslateUi(Dlg_ChooseNamelist)
        QtCore.QMetaObject.connectSlotsByName(Dlg_ChooseNamelist)

    def retranslateUi(self, Dlg_ChooseNamelist):
        _translate = QtCore.QCoreApplication.translate
        Dlg_ChooseNamelist.setWindowTitle(_translate("Dlg_ChooseNamelist", "选择Namelist"))
        self.label.setText(_translate("Dlg_ChooseNamelist", "添加Namelist"))
        self.pushButton_Add.setText(_translate("Dlg_ChooseNamelist", "添加"))
        self.pushButton_Cancel.setText(_translate("Dlg_ChooseNamelist", "取消"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dlg_ChooseNamelist = QtWidgets.QDialog()
    ui = Ui_Dlg_ChooseNamelist()
    ui.setupUi(Dlg_ChooseNamelist)
    Dlg_ChooseNamelist.show()
    sys.exit(app.exec_())

