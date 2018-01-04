# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\PlaneConfiguration.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(1056, 542)
        Dialog.setSizeGripEnabled(True)
        self.horizontalLayout = QtWidgets.QHBoxLayout(Dialog)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.tabWidget_Configuration = QtWidgets.QTabWidget(Dialog)
        self.tabWidget_Configuration.setObjectName("tabWidget_Configuration")
        self.tab = QtWidgets.QWidget()
        self.tab.setObjectName("tab")
        self.tabWidget_Configuration.addTab(self.tab, "")
        self.tab_2 = QtWidgets.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.tabWidget_Configuration.addTab(self.tab_2, "")
        self.horizontalLayout.addWidget(self.tabWidget_Configuration)

        self.retranslateUi(Dialog)
        self.tabWidget_Configuration.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.tabWidget_Configuration.setTabText(self.tabWidget_Configuration.indexOf(self.tab), _translate("Dialog", "Tab 1"))
        self.tabWidget_Configuration.setTabText(self.tabWidget_Configuration.indexOf(self.tab_2), _translate("Dialog", "Tab 2"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

