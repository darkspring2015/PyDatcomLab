# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\NewModel.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(399, 498)
        Dialog.setSizeGripEnabled(True)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label_ModelName = QtWidgets.QLabel(Dialog)
        self.label_ModelName.setObjectName("label_ModelName")
        self.horizontalLayout.addWidget(self.label_ModelName)
        spacerItem = QtWidgets.QSpacerItem(30, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.lineEdit_ModelName = QtWidgets.QLineEdit(Dialog)
        self.lineEdit_ModelName.setObjectName("lineEdit_ModelName")
        self.horizontalLayout.addWidget(self.lineEdit_ModelName)
        self.verticalLayout.addLayout(self.horizontalLayout)
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.pushButton_GetFileName = QtWidgets.QPushButton(Dialog)
        self.pushButton_GetFileName.setObjectName("pushButton_GetFileName")
        self.horizontalLayout_2.addWidget(self.pushButton_GetFileName)
        spacerItem1 = QtWidgets.QSpacerItem(3, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem1)
        self.lineEdit_DirPath = QtWidgets.QLineEdit(Dialog)
        self.lineEdit_DirPath.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
        self.lineEdit_DirPath.setObjectName("lineEdit_DirPath")
        self.horizontalLayout_2.addWidget(self.lineEdit_DirPath)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.label_templete = QtWidgets.QLabel(Dialog)
        self.label_templete.setObjectName("label_templete")
        self.horizontalLayout_3.addWidget(self.label_templete)
        spacerItem2 = QtWidgets.QSpacerItem(20, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem2)
        self.comboBox_template = QtWidgets.QComboBox(Dialog)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.comboBox_template.sizePolicy().hasHeightForWidth())
        self.comboBox_template.setSizePolicy(sizePolicy)
        self.comboBox_template.setObjectName("comboBox_template")
        self.comboBox_template.addItem("")
        self.horizontalLayout_3.addWidget(self.comboBox_template)
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.textEdit_Describe = QtWidgets.QTextEdit(Dialog)
        self.textEdit_Describe.setObjectName("textEdit_Describe")
        self.verticalLayout.addWidget(self.textEdit_Describe)
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
        self.groupBox_2 = QtWidgets.QGroupBox(Dialog)
        self.groupBox_2.setObjectName("groupBox_2")
        self.gridLayout_3 = QtWidgets.QGridLayout(self.groupBox_2)
        self.gridLayout_3.setObjectName("gridLayout_3")
        self.checkBox_FLTCON = QtWidgets.QCheckBox(self.groupBox_2)
        self.checkBox_FLTCON.setObjectName("checkBox_FLTCON")
        self.gridLayout_3.addWidget(self.checkBox_FLTCON, 0, 0, 1, 1)
        self.checkBox_OPTINS = QtWidgets.QCheckBox(self.groupBox_2)
        self.checkBox_OPTINS.setObjectName("checkBox_OPTINS")
        self.gridLayout_3.addWidget(self.checkBox_OPTINS, 1, 0, 1, 1)
        spacerItem3 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.gridLayout_3.addItem(spacerItem3, 2, 0, 1, 1)
        self.horizontalLayout_4.addWidget(self.groupBox_2)
        self.groupBox = QtWidgets.QGroupBox(Dialog)
        self.groupBox.setObjectName("groupBox")
        self.gridLayout = QtWidgets.QGridLayout(self.groupBox)
        self.gridLayout.setObjectName("gridLayout")
        self.checkBox_SYNTHS = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_SYNTHS.setObjectName("checkBox_SYNTHS")
        self.gridLayout.addWidget(self.checkBox_SYNTHS, 0, 0, 1, 1)
        self.checkBox_BODY = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_BODY.setObjectName("checkBox_BODY")
        self.gridLayout.addWidget(self.checkBox_BODY, 0, 1, 1, 1)
        self.checkBox_WGPLNF = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_WGPLNF.setObjectName("checkBox_WGPLNF")
        self.gridLayout.addWidget(self.checkBox_WGPLNF, 1, 0, 1, 1)
        self.checkBox_WGSCHR = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_WGSCHR.setObjectName("checkBox_WGSCHR")
        self.gridLayout.addWidget(self.checkBox_WGSCHR, 1, 1, 1, 1)
        self.checkBox_HTPLNF = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_HTPLNF.setObjectName("checkBox_HTPLNF")
        self.gridLayout.addWidget(self.checkBox_HTPLNF, 2, 0, 1, 1)
        self.checkBox_HTSCHR = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_HTSCHR.setObjectName("checkBox_HTSCHR")
        self.gridLayout.addWidget(self.checkBox_HTSCHR, 2, 1, 1, 1)
        self.checkBox_VTPLNF = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_VTPLNF.setObjectName("checkBox_VTPLNF")
        self.gridLayout.addWidget(self.checkBox_VTPLNF, 3, 0, 1, 1)
        self.checkBox_VTSCHR = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_VTSCHR.setObjectName("checkBox_VTSCHR")
        self.gridLayout.addWidget(self.checkBox_VTSCHR, 3, 1, 1, 1)
        self.checkBox_VFPLNF = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_VFPLNF.setObjectName("checkBox_VFPLNF")
        self.gridLayout.addWidget(self.checkBox_VFPLNF, 4, 0, 1, 1)
        self.checkBox_VFSCHR = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_VFSCHR.setObjectName("checkBox_VFSCHR")
        self.gridLayout.addWidget(self.checkBox_VFSCHR, 4, 1, 1, 1)
        self.checkBox_EXPR = QtWidgets.QCheckBox(self.groupBox)
        self.checkBox_EXPR.setObjectName("checkBox_EXPR")
        self.gridLayout.addWidget(self.checkBox_EXPR, 5, 0, 1, 1)
        self.horizontalLayout_4.addWidget(self.groupBox)
        self.verticalLayout.addLayout(self.horizontalLayout_4)
        self.groupBox_CARD3 = QtWidgets.QGroupBox(Dialog)
        self.groupBox_CARD3.setObjectName("groupBox_CARD3")
        self.gridLayout_2 = QtWidgets.QGridLayout(self.groupBox_CARD3)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.checkBox_PROPWR = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_PROPWR.setObjectName("checkBox_PROPWR")
        self.gridLayout_2.addWidget(self.checkBox_PROPWR, 0, 0, 1, 1)
        self.checkBox_JETPWR = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_JETPWR.setObjectName("checkBox_JETPWR")
        self.gridLayout_2.addWidget(self.checkBox_JETPWR, 0, 1, 1, 1)
        self.checkBox_GRNDEF = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_GRNDEF.setObjectName("checkBox_GRNDEF")
        self.gridLayout_2.addWidget(self.checkBox_GRNDEF, 0, 2, 1, 1)
        self.checkBox_TRNJET = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_TRNJET.setObjectName("checkBox_TRNJET")
        self.gridLayout_2.addWidget(self.checkBox_TRNJET, 1, 0, 1, 1)
        self.checkBox_HYPEFF = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_HYPEFF.setObjectName("checkBox_HYPEFF")
        self.gridLayout_2.addWidget(self.checkBox_HYPEFF, 1, 1, 1, 1)
        self.checkBox_LARWB = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_LARWB.setObjectName("checkBox_LARWB")
        self.gridLayout_2.addWidget(self.checkBox_LARWB, 1, 2, 1, 1)
        self.checkBox_CONTAB = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_CONTAB.setObjectName("checkBox_CONTAB")
        self.gridLayout_2.addWidget(self.checkBox_CONTAB, 2, 0, 1, 1)
        self.checkBox_SYMFLP = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_SYMFLP.setObjectName("checkBox_SYMFLP")
        self.gridLayout_2.addWidget(self.checkBox_SYMFLP, 2, 1, 1, 1)
        self.checkBox_ASYFLP = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_ASYFLP.setObjectName("checkBox_ASYFLP")
        self.gridLayout_2.addWidget(self.checkBox_ASYFLP, 2, 2, 1, 1)
        self.checkBox_TVTPAN = QtWidgets.QCheckBox(self.groupBox_CARD3)
        self.checkBox_TVTPAN.setObjectName("checkBox_TVTPAN")
        self.gridLayout_2.addWidget(self.checkBox_TVTPAN, 3, 0, 1, 1)
        self.verticalLayout.addWidget(self.groupBox_CARD3)
        self.horizontalLayout_5 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_5.setObjectName("horizontalLayout_5")
        spacerItem4 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem4)
        self.pushButton_New = QtWidgets.QPushButton(Dialog)
        self.pushButton_New.setObjectName("pushButton_New")
        self.horizontalLayout_5.addWidget(self.pushButton_New)
        spacerItem5 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_5.addItem(spacerItem5)
        self.verticalLayout.addLayout(self.horizontalLayout_5)

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "新建模型"))
        self.label_ModelName.setText(_translate("Dialog", "模型名称"))
        self.lineEdit_ModelName.setPlaceholderText(_translate("Dialog", "模型的名称（对应AerocraftName）"))
        self.pushButton_GetFileName.setText(_translate("Dialog", "模型文件"))
        self.lineEdit_DirPath.setPlaceholderText(_translate("Dialog", "模型的文件名"))
        self.label_templete.setText(_translate("Dialog", "模型模板"))
        self.comboBox_template.setItemText(0, _translate("Dialog", "常规模型"))
        self.textEdit_Describe.setToolTip(_translate("Dialog", "模型的描述，CASE的描述"))
        self.textEdit_Describe.setStatusTip(_translate("Dialog", "模型的描述，CASE的描述"))
        self.textEdit_Describe.setPlaceholderText(_translate("Dialog", "在此输入模型的描述"))
        self.groupBox_2.setTitle(_translate("Dialog", "CARD1"))
        self.checkBox_FLTCON.setText(_translate("Dialog", "飞行条件"))
        self.checkBox_OPTINS.setText(_translate("Dialog", "可选项"))
        self.groupBox.setTitle(_translate("Dialog", "CARD2"))
        self.checkBox_SYNTHS.setText(_translate("Dialog", "总体参数"))
        self.checkBox_BODY.setText(_translate("Dialog", "机体"))
        self.checkBox_WGPLNF.setText(_translate("Dialog", "机翼外形参数"))
        self.checkBox_WGSCHR.setText(_translate("Dialog", "机翼气动参数"))
        self.checkBox_HTPLNF.setText(_translate("Dialog", "平尾外形参数"))
        self.checkBox_HTSCHR.setText(_translate("Dialog", "平尾气动参数"))
        self.checkBox_VTPLNF.setText(_translate("Dialog", "垂尾外形参数"))
        self.checkBox_VTSCHR.setText(_translate("Dialog", "垂尾气动参数"))
        self.checkBox_VFPLNF.setText(_translate("Dialog", "腹鳍外形参数"))
        self.checkBox_VFSCHR.setText(_translate("Dialog", "腹鳍气动参数"))
        self.checkBox_EXPR.setText(_translate("Dialog", "实验结果参数"))
        self.groupBox_CARD3.setTitle(_translate("Dialog", "CARD3"))
        self.checkBox_PROPWR.setText(_translate("Dialog", "螺旋桨动力项"))
        self.checkBox_JETPWR.setText(_translate("Dialog", "喷气动力"))
        self.checkBox_GRNDEF.setText(_translate("Dialog", "地效"))
        self.checkBox_TRNJET.setText(_translate("Dialog", "横向射流控制"))
        self.checkBox_HYPEFF.setText(_translate("Dialog", "高超声速襟翼"))
        self.checkBox_LARWB.setText(_translate("Dialog", "翼身融合体"))
        self.checkBox_CONTAB.setText(_translate("Dialog", "控制面"))
        self.checkBox_SYMFLP.setText(_translate("Dialog", "襟翼"))
        self.checkBox_ASYFLP.setText(_translate("Dialog", "副翼"))
        self.checkBox_TVTPAN.setText(_translate("Dialog", "双垂面"))
        self.pushButton_New.setText(_translate("Dialog", "新建模型"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

