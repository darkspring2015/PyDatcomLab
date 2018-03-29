# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\components\PlaneConfiguration.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets
from PyDatcomLab.GUIs.InputCard.DatcomWidgetBase import DatcomWidgetBase as dtCARD


class DatcomCASEEditerUi(object):
    def setupUi(self, Dialog):
        Dialog.setObjectName("DatcomCASEEditerUi")
        Dialog.resize(900, 500)
        Dialog.setSizeGripEnabled(True)
        self.horizontalLayout = QtWidgets.QHBoxLayout(Dialog)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.tabWidget_Configuration = QtWidgets.QTabWidget(Dialog)
        self.tabWidget_Configuration.setObjectName("tabWidget_Configuration")
        tNamelistCollection = self.dcModel.getNamelistCollection()
        for iC in tNamelistCollection.keys():
            tTab = dtCARD(tCARD = iC, tModel = self.dcModel, parent = Dialog) 
            tTab.setObjectName("tabCARD_%s"%iC)
            tNDf = Dialog.dtDefine.getNamelistDefineByName(iC)
            if tNDf is None:
                Dialog.logger.error("%s的定义无效"%iC)
                continue
            #检查信息
            if 'DisplayName' in tNDf.keys():
                tDisplay = tNDf['DisplayName']
            else :
                tDisplay = iC
            self.tabWidget_Configuration.addTab(tTab, tDisplay)
        self.horizontalLayout.addWidget(self.tabWidget_Configuration)

        self.retranslateUi(Dialog)
        self.tabWidget_Configuration.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(Dialog)

    def retranslateUi(self, Dialog):
        _translate = QtCore.QCoreApplication.translate
        if self.dcModel.getProperties()['CName'] is not None:
            Dialog.setWindowTitle(_translate( self.dcModel.getProperties()['CName'],  self.dcModel.getProperties()['CName']))
        else:
            Dialog.setWindowTitle(_translate("DatcomCASEEditer", "DatcomCASEEditer"))
        #self.tabWidget_Configuration.setTabText(self.tabWidget_Configuration.indexOf(self.tab), _translate("Dialog", "Tab 1"))
        #self.tabWidget_Configuration.setTabText(self.tabWidget_Configuration.indexOf(self.tab_2), _translate("Dialog", "Tab 2"))


if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
#    Dialog = QtWidgets.QDialog()
#    ui = DatcomCASEEditerUI()
#    ui.setupUi(Dialog)
#    Dialog.show()
    sys.exit(app.exec_())

