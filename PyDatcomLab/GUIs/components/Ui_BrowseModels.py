# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\BrowseModels.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_Dialog(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("Dialog")
        Dialog.resize(460, 374)
        Dialog.setSizeGripEnabled(True)
        self.verticalLayout = QtWidgets.QVBoxLayout(Dialog)
        self.verticalLayout.setObjectName("verticalLayout")
        self.groupBox = QtWidgets.QGroupBox(Dialog)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.groupBox.sizePolicy().hasHeightForWidth())
        self.groupBox.setSizePolicy(sizePolicy)
        self.groupBox.setMinimumSize(QtCore.QSize(0, 0))
        self.groupBox.setMaximumSize(QtCore.QSize(16777215, 50))
        self.groupBox.setTitle("")
        self.groupBox.setObjectName("groupBox")
        self.horizontalLayout = QtWidgets.QHBoxLayout(self.groupBox)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.label_ModelDir = QtWidgets.QLabel(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label_ModelDir.sizePolicy().hasHeightForWidth())
        self.label_ModelDir.setSizePolicy(sizePolicy)
        self.label_ModelDir.setObjectName("label_ModelDir")
        self.horizontalLayout.addWidget(self.label_ModelDir)
        self.textEdit_Dir = QtWidgets.QTextEdit(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.textEdit_Dir.sizePolicy().hasHeightForWidth())
        self.textEdit_Dir.setSizePolicy(sizePolicy)
        self.textEdit_Dir.setObjectName("textEdit_Dir")
        self.horizontalLayout.addWidget(self.textEdit_Dir)
        self.pushButton_ChoiseDir = QtWidgets.QPushButton(self.groupBox)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Fixed, QtWidgets.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.pushButton_ChoiseDir.sizePolicy().hasHeightForWidth())
        self.pushButton_ChoiseDir.setSizePolicy(sizePolicy)
        self.pushButton_ChoiseDir.setMinimumSize(QtCore.QSize(0, 0))
        self.pushButton_ChoiseDir.setObjectName("pushButton_ChoiseDir")
        self.horizontalLayout.addWidget(self.pushButton_ChoiseDir)
        self.verticalLayout.addWidget(self.groupBox)
        self.ModelSet = QtWidgets.QTableWidget(Dialog)
        self.ModelSet.setObjectName("ModelSet")
        self.ModelSet.setColumnCount(0)
        self.ModelSet.setRowCount(0)
        self.verticalLayout.addWidget(self.ModelSet)
        self.actionNewModel = QtWidgets.QAction(Dialog)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/icos/pluginInstall.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionNewModel.setIcon(icon)
        self.actionNewModel.setObjectName("actionNewModel")
        self.actionAddModel = QtWidgets.QAction(Dialog)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/icos/pluginRepository.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAddModel.setIcon(icon1)
        self.actionAddModel.setObjectName("actionAddModel")
        self.actionRemoveModel = QtWidgets.QAction(Dialog)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/icos/syncToc.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionRemoveModel.setIcon(icon2)
        self.actionRemoveModel.setObjectName("actionRemoveModel")

        self.retranslateUi(Dialog)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        Dialog.setWindowTitle(_translate("Dialog", "Dialog"))
        self.label_ModelDir.setText(_translate("Dialog", "模型目录"))
        self.pushButton_ChoiseDir.setText(_translate("Dialog", "..."))
        self.actionNewModel.setText(_translate("Dialog", "新建模型"))
        self.actionNewModel.setToolTip(_translate("Dialog", "新建一个模型"))
        self.actionAddModel.setText(_translate("Dialog", "添加模型"))
        self.actionRemoveModel.setText(_translate("Dialog", "移除模型"))

import MainWindow_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    Dialog = QtWidgets.QDialog()
    ui = Ui_Dialog()
    ui.setupUi(Dialog)
    Dialog.show()
    sys.exit(app.exec_())

