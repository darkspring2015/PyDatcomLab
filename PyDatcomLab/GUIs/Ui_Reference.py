# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'C:\Users\lingo\Documents\workspace\DatcomSolution\PyDatcomLab\PyDatcomLab\GUIs\Reference.ui'
#
# Created by: PyQt5 UI code generator 5.9
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(400, 300)
        Dialog.setSizeGripEnabled(True)
        self.pushButton_test = QtWidgets.QPushButton(Dialog)
        self.pushButton_test.setGeometry(QtCore.QRect(110, 130, 75, 23))
        self.pushButton_test.setObjectName("pushButton_test")

        self.retranslateUi(Dialog)
        self.pushButton_test.clicked.connect(Dialog.slotClicked)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.pushButton_test.setText(_translate("Dialog", "点击测试"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

