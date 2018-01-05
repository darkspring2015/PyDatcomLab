# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\Reference.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(1003, 795)
        Dialog.setSizeGripEnabled(True)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.tabWidget_Reference = QtWidgets.QTabWidget(Dialog)
        self.tabWidget_Reference.setTabPosition(QtWidgets.QTabWidget.West)
        self.tabWidget_Reference.setTabShape(QtWidgets.QTabWidget.Triangular)
        self.tabWidget_Reference.setElideMode(QtCore.Qt.ElideNone)
        self.tabWidget_Reference.setObjectName("tabWidget_Reference")
        self.tab_general = QtWidgets.QWidget()
        self.tab_general.setObjectName("tab_general")
        self.tabWidget_Reference.addTab(self.tab_general, "")
        self.tab_2 = QtWidgets.QWidget()
        self.tab_2.setObjectName("tab_2")
        self.label = QtWidgets.QLabel(self.tab_2)
        self.label.setGeometry(QtCore.QRect(70, 70, 91, 16))
        self.label.setObjectName("label")
        self.textEdit = QtWidgets.QTextEdit(self.tab_2)
        self.textEdit.setGeometry(QtCore.QRect(160, 70, 271, 21))
        self.textEdit.setObjectName("textEdit")
        self.toolButton = QtWidgets.QToolButton(self.tab_2)
        self.toolButton.setGeometry(QtCore.QRect(440, 70, 37, 18))
        self.toolButton.setObjectName("toolButton")
        self.tabWidget_Reference.addTab(self.tab_2, "")
        self.verticalLayout.addWidget(self.tabWidget_Reference)

        self.retranslateUi(Dialog)
        self.tabWidget_Reference.setCurrentIndex(1)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.tabWidget_Reference.setTabText(self.tabWidget_Reference.indexOf(self.tab_general), _translate("Dialog", "通用配置"))
        self.label.setText(_translate("Dialog", "工程目录(默认)"))
        self.toolButton.setText(_translate("Dialog", "..."))
        self.tabWidget_Reference.setTabText(self.tabWidget_Reference.indexOf(self.tab_2), _translate("Dialog", "Tab 2"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

