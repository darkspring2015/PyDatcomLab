# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\GRNDEF.ui'
#
# Created by: PyQt5 UI code generator 5.10
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_GRNDEF(object):
    def setupUi(self, GRNDEF):
        GRNDEF.setObjectName("GRNDEF")
        GRNDEF.resize(840, 428)
        self.horizontalLayout = QtWidgets.QHBoxLayout(GRNDEF)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.verticalLayout = QtWidgets.QVBoxLayout()
        self.verticalLayout.setObjectName("verticalLayout")
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.label_NGH = QtWidgets.QLabel(GRNDEF)
        self.label_NGH.setObjectName("label_NGH")
        self.horizontalLayout_2.addWidget(self.label_NGH)
        spacerItem = QtWidgets.QSpacerItem(55, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem)
        self.NGH = QtWidgets.QLineEdit(GRNDEF)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.NGH.sizePolicy().hasHeightForWidth())
        self.NGH.setSizePolicy(sizePolicy)
        self.NGH.setObjectName("NGH")
        self.horizontalLayout_2.addWidget(self.NGH)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.tableWidget_GHeight = QtWidgets.QTableWidget(GRNDEF)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.tableWidget_GHeight.sizePolicy().hasHeightForWidth())
        self.tableWidget_GHeight.setSizePolicy(sizePolicy)
        self.tableWidget_GHeight.setObjectName("tableWidget_GHeight")
        self.tableWidget_GHeight.setColumnCount(0)
        self.tableWidget_GHeight.setRowCount(0)
        self.verticalLayout.addWidget(self.tableWidget_GHeight)
        self.horizontalLayout.addLayout(self.verticalLayout)
        self.tabWidget = QtWidgets.QTabWidget(GRNDEF)
        self.tabWidget.setObjectName("tabWidget")
        self.tab_3 = QtWidgets.QWidget()
        self.tab_3.setObjectName("tab_3")
        self.horizontalLayout_17 = QtWidgets.QHBoxLayout(self.tab_3)
        self.horizontalLayout_17.setObjectName("horizontalLayout_17")
        self.textBrowser = QtWidgets.QTextBrowser(self.tab_3)
        self.textBrowser.setObjectName("textBrowser")
        self.horizontalLayout_17.addWidget(self.textBrowser)
        self.tabWidget.addTab(self.tab_3, "")
        self.horizontalLayout.addWidget(self.tabWidget)
        self.actionAddRow = QtWidgets.QAction(GRNDEF)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAddRow.setIcon(icon)
        self.actionAddRow.setObjectName("actionAddRow")
        self.actionDeleteRow = QtWidgets.QAction(GRNDEF)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/cardIco/rc_card/icos/DeletedIcon.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionDeleteRow.setIcon(icon1)
        self.actionDeleteRow.setObjectName("actionDeleteRow")

        self.retranslateUi(GRNDEF)
        self.tabWidget.setCurrentIndex(0)
        QtCore.QMetaObject.connectSlotsByName(GRNDEF)

    def retranslateUi(self, GRNDEF):
        _translate = QtCore.QCoreApplication.translate
        GRNDEF.setWindowTitle(_translate("GRNDEF", "GRNDEF"))
        self.label_NGH.setText(_translate("GRNDEF", "NGH  地面高度的计算点数"))
        self.textBrowser.setHtml(_translate("GRNDEF", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'SimSun\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><br /></p>\n"
"<p style=\" margin-top:18px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-namelist-grndef-地效\"></a><span style=\" font-size:xx-large; font-weight:600;\">N</span><span style=\" font-size:xx-large; font-weight:600;\">AMELIST GRNDEF 地效</span></p>\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><a name=\"user-content-参数卡说明-1\"></a><span style=\" font-size:x-large; font-weight:600;\">参</span><span style=\" font-size:x-large; font-weight:600;\">数卡说明</span></p>\n"
"<table border=\"0\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px;\" cellspacing=\"2\" cellpadding=\"0\"><thead>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">VariableName</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">别名</span></p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">长度限制</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">原文说明或Tips</span></p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-weight:600;\">说明或Tips</span></p></td></tr></thead>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">NGH</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">地面高度的计算点数</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">number of ground heights to be run      .</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">参与计算的地面高度点数</p></td></tr>\n"
"<tr>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">GRDHT</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">地面高度值</p></td>\n"
"<td>\n"
"<p align=\"center\" style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">0</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">values of ground heights. ground heights equal altitude of REF.PLANE relative to Ground。</p></td>\n"
"<td>\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">地面高度值。地面高度指参考平面到地面的告诉</p></td></tr></table></body></html>"))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_3), _translate("GRNDEF", "参数说明"))
        self.actionAddRow.setText(_translate("GRNDEF", "增加行"))
        self.actionAddRow.setToolTip(_translate("GRNDEF", "新增行"))
        self.actionDeleteRow.setText(_translate("GRNDEF", "删除行"))
        self.actionDeleteRow.setToolTip(_translate("GRNDEF", "删除一行"))

import card_ico_rc
import card_rc_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    GRNDEF = QtWidgets.QWidget()
    ui = Ui_GRNDEF()
    ui.setupUi(GRNDEF)
    GRNDEF.show()
    sys.exit(app.exec_())

