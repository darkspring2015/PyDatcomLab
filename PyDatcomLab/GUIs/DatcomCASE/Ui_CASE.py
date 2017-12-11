# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\DatcomCASE\CASE.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Form(object):
    def setupUi(self, Form):
        Form.setObjectName("Form")
        Form.resize(867, 726)
        self.groupBox = QtWidgets.QGroupBox(Form)
        self.groupBox.setGeometry(QtCore.QRect(10, 10, 999, 645))
        self.groupBox.setObjectName("groupBox")
        self.layoutWidget = QtWidgets.QWidget(self.groupBox)
        self.layoutWidget.setGeometry(QtCore.QRect(30, 40, 77, 471))
        self.layoutWidget.setObjectName("layoutWidget")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.layoutWidget)
        self.verticalLayout.setContentsMargins(0, 0, 0, 0)
        self.verticalLayout.setObjectName("verticalLayout")
        self.pushButtonFLTCON = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButtonFLTCON.setObjectName("pushButtonFLTCON")
        self.verticalLayout.addWidget(self.pushButtonFLTCON)
        self.pushButtonSYNTHS = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButtonSYNTHS.setObjectName("pushButtonSYNTHS")
        self.verticalLayout.addWidget(self.pushButtonSYNTHS)
        self.pushButtonOPTIONS = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButtonOPTIONS.setObjectName("pushButtonOPTIONS")
        self.verticalLayout.addWidget(self.pushButtonOPTIONS)
        self.pushButtonBODY = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButtonBODY.setObjectName("pushButtonBODY")
        self.verticalLayout.addWidget(self.pushButtonBODY)
        self.pushButtonWGPLNF = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButtonWGPLNF.setObjectName("pushButtonWGPLNF")
        self.verticalLayout.addWidget(self.pushButtonWGPLNF)
        self.pushButton_6 = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButton_6.setObjectName("pushButton_6")
        self.verticalLayout.addWidget(self.pushButton_6)
        self.pushButton_7 = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButton_7.setObjectName("pushButton_7")
        self.verticalLayout.addWidget(self.pushButton_7)
        self.pushButton_8 = QtWidgets.QPushButton(self.layoutWidget)
        self.pushButton_8.setObjectName("pushButton_8")
        self.verticalLayout.addWidget(self.pushButton_8)
        self.textBrowser = QtWidgets.QTextBrowser(self.groupBox)
        self.textBrowser.setGeometry(QtCore.QRect(190, 40, 551, 531))
        self.textBrowser.setObjectName("textBrowser")
        self.label = QtWidgets.QLabel(self.groupBox)
        self.label.setGeometry(QtCore.QRect(230, 20, 101, 16))
        self.label.setObjectName("label")
        self.pushButton_9 = QtWidgets.QPushButton(self.groupBox)
        self.pushButton_9.setGeometry(QtCore.QRect(30, 530, 75, 71))
        self.pushButton_9.setObjectName("pushButton_9")

        self.retranslateUi(Form)
        self.pushButtonFLTCON.clicked.connect(Form.slotFLTCON)
        self.pushButtonSYNTHS.clicked.connect(Form.slotSYNTHS)
        self.pushButtonOPTIONS.clicked.connect(Form.slotOPTIONS)
        self.pushButtonBODY.clicked.connect(Form.slotBODY)
        self.pushButtonWGPLNF.clicked.connect(Form.slotWGPLNF)
        self.pushButton_6.clicked.connect(Form.slotVTPLNF)
        self.pushButton_7.clicked.connect(Form.slotVFPLNF)
        self.pushButton_8.clicked.connect(Form.slotHTPLNF)
        self.pushButton_9.clicked.connect(Form.slotSAVE)
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", "Form"))
        self.groupBox.setTitle(_translate("Form", "输入参数"))
        self.pushButtonFLTCON.setText(_translate("Form", "FLTCON"))
        self.pushButtonSYNTHS.setText(_translate("Form", "SYNTHS"))
        self.pushButtonOPTIONS.setText(_translate("Form", "OPTIONS"))
        self.pushButtonBODY.setText(_translate("Form", "BODY"))
        self.pushButtonWGPLNF.setText(_translate("Form", "WGPLNF"))
        self.pushButton_6.setText(_translate("Form", "VTPLNF"))
        self.pushButton_7.setText(_translate("Form", "VFPLNF"))
        self.pushButton_8.setText(_translate("Form", "HTPLNF"))
        self.label.setText(_translate("Form", "飞机形态图"))
        self.pushButton_9.setText(_translate("Form", "SAVE"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Form = QtWidgets.QWidget()
    ui = Ui_Form()
    ui.setupUi(Form)
    Form.show()
    sys.exit(app.exec_())

