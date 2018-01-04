# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\BrowseModels.ui'
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
        self.listWidget_Models = QtWidgets.QListWidget(Dialog)
        self.listWidget_Models.setGeometry(QtCore.QRect(15, 40, 376, 251))
        self.listWidget_Models.setObjectName("listWidget_Models")
        self.splitter = QtWidgets.QSplitter(Dialog)
        self.splitter.setGeometry(QtCore.QRect(15, 9, 376, 21))
        self.splitter.setOrientation(QtCore.Qt.Horizontal)
        self.splitter.setObjectName("splitter")
        self.label_ModelDir = QtWidgets.QLabel(self.splitter)
        self.label_ModelDir.setObjectName("label_ModelDir")
        self.textEdit_Dir = QtWidgets.QTextEdit(self.splitter)
        self.textEdit_Dir.setObjectName("textEdit_Dir")
        self.pushButton_ChoiseDir = QtWidgets.QPushButton(self.splitter)
        self.pushButton_ChoiseDir.setObjectName("pushButton_ChoiseDir")

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.label_ModelDir.setText(_translate("Dialog", "模型目录"))
        self.pushButton_ChoiseDir.setText(_translate("Dialog", "..."))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

