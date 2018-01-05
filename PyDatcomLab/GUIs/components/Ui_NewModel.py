# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\NewModel.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(400, 300)
        Dialog.setSizeGripEnabled(True)
        self.pushButton_New = QtWidgets.QPushButton(Dialog)
        self.pushButton_New.setGeometry(QtCore.QRect(120, 240, 75, 21))
        self.pushButton_New.setObjectName("pushButton_New")
        self.textEdit_DirPath = QtWidgets.QTextEdit(Dialog)
        self.textEdit_DirPath.setGeometry(QtCore.QRect(72, 90, 243, 21))
        self.textEdit_DirPath.setObjectName("textEdit_DirPath")
        self.label_ModelDir = QtWidgets.QLabel(Dialog)
        self.label_ModelDir.setGeometry(QtCore.QRect(19, 90, 48, 21))
        self.label_ModelDir.setObjectName("label_ModelDir")
        self.pushButton_ChoiseDir = QtWidgets.QPushButton(Dialog)
        self.pushButton_ChoiseDir.setGeometry(QtCore.QRect(320, 90, 75, 21))
        self.pushButton_ChoiseDir.setObjectName("pushButton_ChoiseDir")
        self.splitter = QtWidgets.QSplitter(Dialog)
        self.splitter.setGeometry(QtCore.QRect(20, 20, 304, 31))
        self.splitter.setOrientation(QtCore.Qt.Horizontal)
        self.splitter.setObjectName("splitter")
        self.label_ModelName = QtWidgets.QLabel(self.splitter)
        self.label_ModelName.setObjectName("label_ModelName")
        self.textEdit_ModelName = QtWidgets.QTextEdit(self.splitter)
        self.textEdit_ModelName.setObjectName("textEdit_ModelName")

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "新建模型"))
        self.pushButton_New.setText(_translate("Dialog", "新建模型"))
        self.label_ModelDir.setText(_translate("Dialog", "模型目录"))
        self.pushButton_ChoiseDir.setText(_translate("Dialog", "..."))
        self.label_ModelName.setText(_translate("Dialog", "模型名称"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

