# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\OPTIONS\OPTIONS.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName("Form")
        Form.resize(704, 559)
        self.checkBox_4 = QtWidgets.QCheckBox(Form)
        self.checkBox_4.setGeometry(QtCore.QRect(51, 130, 71, 16))
        self.checkBox_4.setObjectName("checkBox_4")
        self.lineEdit_BLREF = QtWidgets.QLineEdit(Form)
        self.lineEdit_BLREF.setGeometry(QtCore.QRect(463, 128, 181, 20))
        self.lineEdit_BLREF.setObjectName("lineEdit_BLREF")
        self.checkBox = QtWidgets.QCheckBox(Form)
        self.checkBox.setGeometry(QtCore.QRect(50, 70, 83, 16))
        self.checkBox.setObjectName("checkBox")
        self.tableView = QtWidgets.QTableView(Form)
        self.tableView.setGeometry(QtCore.QRect(50, 191, 481, 331))
        self.tableView.setObjectName("tableView")
        self.lineEdit_ROUGFC = QtWidgets.QLineEdit(Form)
        self.lineEdit_ROUGFC.setGeometry(QtCore.QRect(139, 68, 180, 20))
        self.lineEdit_ROUGFC.setObjectName("lineEdit_ROUGFC")
        self.lineEdit_SREF = QtWidgets.QLineEdit(Form)
        self.lineEdit_SREF.setGeometry(QtCore.QRect(140, 128, 181, 20))
        self.lineEdit_SREF.setObjectName("lineEdit_SREF")
        self.checkBox_2 = QtWidgets.QCheckBox(Form)
        self.checkBox_2.setGeometry(QtCore.QRect(350, 70, 107, 16))
        self.checkBox_2.setObjectName("checkBox_2")
        self.lineEdit_CBARR = QtWidgets.QLineEdit(Form)
        self.lineEdit_CBARR.setGeometry(QtCore.QRect(463, 68, 180, 20))
        self.lineEdit_CBARR.setObjectName("lineEdit_CBARR")
        self.checkBox_3 = QtWidgets.QCheckBox(Form)
        self.checkBox_3.setGeometry(QtCore.QRect(350, 130, 107, 16))
        self.checkBox_3.setObjectName("checkBox_3")

        self.retranslateUi(Form)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", "Form"))
        self.checkBox_4.setText(_translate("Form", "参考面积"))
        self.checkBox.setText(_translate("Form", "表面粗糙度"))
        self.checkBox_2.setText(_translate("Form", "纵向参考长度值"))
        self.checkBox_3.setText(_translate("Form", "横向参考长度值"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Form = QtWidgets.QWidget()
    ui = Ui_Form()
    ui.setupUi(Form)
    Form.show()
    sys.exit(app.exec_())

