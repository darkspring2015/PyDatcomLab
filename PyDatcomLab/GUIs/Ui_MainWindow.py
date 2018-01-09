# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\MainWindow.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName("MainWindow")
        MainWindow.resize(1202, 900)
        self.centralWidgetLay = QtWidgets.QWidget(MainWindow)
        self.centralWidgetLay.setObjectName("centralWidgetLay")
        self.verticalLayout = QtWidgets.QVBoxLayout(self.centralWidgetLay)
        self.verticalLayout.setObjectName("verticalLayout")
        self.Main_widget = QtWidgets.QWidget(self.centralWidgetLay)
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.Main_widget.sizePolicy().hasHeightForWidth())
        self.Main_widget.setSizePolicy(sizePolicy)
        self.Main_widget.setObjectName("Main_widget")
        self.verticalLayout.addWidget(self.Main_widget)
        MainWindow.setCentralWidget(self.centralWidgetLay)
        self.menuBar = QtWidgets.QMenuBar(MainWindow)
        self.menuBar.setGeometry(QtCore.QRect(0, 0, 1202, 23))
        self.menuBar.setObjectName("menuBar")
        self.menuProject = QtWidgets.QMenu(self.menuBar)
        self.menuProject.setObjectName("menuProject")
        self.menu_Model = QtWidgets.QMenu(self.menuBar)
        self.menu_Model.setObjectName("menu_Model")
        self.menu_Problem = QtWidgets.QMenu(self.menuBar)
        self.menu_Problem.setObjectName("menu_Problem")
        self.menu_Windows = QtWidgets.QMenu(self.menuBar)
        self.menu_Windows.setObjectName("menu_Windows")
        self.menu_Help = QtWidgets.QMenu(self.menuBar)
        self.menu_Help.setObjectName("menu_Help")
        self.menu_Analyze = QtWidgets.QMenu(self.menuBar)
        self.menu_Analyze.setObjectName("menu_Analyze")
        self.menu_8 = QtWidgets.QMenu(self.menu_Analyze)
        self.menu_8.setObjectName("menu_8")
        self.menu_7 = QtWidgets.QMenu(self.menu_Analyze)
        self.menu_7.setObjectName("menu_7")
        MainWindow.setMenuBar(self.menuBar)
        self.statusBar = QtWidgets.QStatusBar(MainWindow)
        self.statusBar.setObjectName("statusBar")
        MainWindow.setStatusBar(self.statusBar)
        self.toolBar = QtWidgets.QToolBar(MainWindow)
        self.toolBar.setObjectName("toolBar")
        MainWindow.addToolBar(QtCore.Qt.TopToolBarArea, self.toolBar)
        self.actionDocs = QtWidgets.QAction(MainWindow)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/icos/ircEditTopic.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionDocs.setIcon(icon)
        self.actionDocs.setObjectName("actionDocs")
        self.actionHelp = QtWidgets.QAction(MainWindow)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap(":/icos/help.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionHelp.setIcon(icon1)
        self.actionHelp.setObjectName("actionHelp")
        self.actionAbout = QtWidgets.QAction(MainWindow)
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap(":/icos/preferences-mail_generic.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAbout.setIcon(icon2)
        self.actionAbout.setObjectName("actionAbout")
        self.actionNewProject = QtWidgets.QAction(MainWindow)
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap(":/icos/projectUserProps.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionNewProject.setIcon(icon3)
        self.actionNewProject.setObjectName("actionNewProject")
        self.actionOpenProject = QtWidgets.QAction(MainWindow)
        icon4 = QtGui.QIcon()
        icon4.addPixmap(QtGui.QPixmap(":/icos/dirOpen.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionOpenProject.setIcon(icon4)
        self.actionOpenProject.setObjectName("actionOpenProject")
        self.actionSaveProject = QtWidgets.QAction(MainWindow)
        icon5 = QtGui.QIcon()
        icon5.addPixmap(QtGui.QPixmap(":/icos/fileSave.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionSaveProject.setIcon(icon5)
        self.actionSaveProject.setObjectName("actionSaveProject")
        self.actionSettings = QtWidgets.QAction(MainWindow)
        icon6 = QtGui.QIcon()
        icon6.addPixmap(QtGui.QPixmap(":/icos/configureShortcuts.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionSettings.setIcon(icon6)
        self.actionSettings.setObjectName("actionSettings")
        self.actionImportSettings = QtWidgets.QAction(MainWindow)
        icon7 = QtGui.QIcon()
        icon7.addPixmap(QtGui.QPixmap(":/icos/bookmarkPrevious.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionImportSettings.setIcon(icon7)
        self.actionImportSettings.setObjectName("actionImportSettings")
        self.actionExportSettings = QtWidgets.QAction(MainWindow)
        icon8 = QtGui.QIcon()
        icon8.addPixmap(QtGui.QPixmap(":/icos/bookmarkNext.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionExportSettings.setIcon(icon8)
        self.actionExportSettings.setObjectName("actionExportSettings")
        self.actionExit = QtWidgets.QAction(MainWindow)
        icon9 = QtGui.QIcon()
        icon9.addPixmap(QtGui.QPixmap(":/icos/close.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionExit.setIcon(icon9)
        self.actionExit.setObjectName("actionExit")
        self.actionBrowse = QtWidgets.QAction(MainWindow)
        icon10 = QtGui.QIcon()
        icon10.addPixmap(QtGui.QPixmap(":/icos/pluginRepository.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionBrowse.setIcon(icon10)
        self.actionBrowse.setObjectName("actionBrowse")
        self.actionNewModel = QtWidgets.QAction(MainWindow)
        icon11 = QtGui.QIcon()
        icon11.addPixmap(QtGui.QPixmap(":/icos/pluginInstall.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionNewModel.setIcon(icon11)
        self.actionNewModel.setObjectName("actionNewModel")
        self.actionSaveModel = QtWidgets.QAction(MainWindow)
        icon12 = QtGui.QIcon()
        icon12.addPixmap(QtGui.QPixmap(":/icos/plugin.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionSaveModel.setIcon(icon12)
        self.actionSaveModel.setObjectName("actionSaveModel")
        self.actionExportModel = QtWidgets.QAction(MainWindow)
        icon13 = QtGui.QIcon()
        icon13.addPixmap(QtGui.QPixmap(":/icos/bookmarkToggle.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionExportModel.setIcon(icon13)
        self.actionExportModel.setObjectName("actionExportModel")
        self.actionImportModel = QtWidgets.QAction(MainWindow)
        icon14 = QtGui.QIcon()
        icon14.addPixmap(QtGui.QPixmap(":/icos/outgoingBookmark.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionImportModel.setIcon(icon14)
        self.actionImportModel.setObjectName("actionImportModel")
        self.actionCopyModel = QtWidgets.QAction(MainWindow)
        icon15 = QtGui.QIcon()
        icon15.addPixmap(QtGui.QPixmap(":/icos/moveBookmark.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionCopyModel.setIcon(icon15)
        self.actionCopyModel.setObjectName("actionCopyModel")
        self.actionManageModelTemplete = QtWidgets.QAction(MainWindow)
        icon16 = QtGui.QIcon()
        icon16.addPixmap(QtGui.QPixmap(":/icos/preferences-api.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionManageModelTemplete.setIcon(icon16)
        self.actionManageModelTemplete.setObjectName("actionManageModelTemplete")
        self.actionNewCASE = QtWidgets.QAction(MainWindow)
        icon17 = QtGui.QIcon()
        icon17.addPixmap(QtGui.QPixmap(":/icos/tabNew.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionNewCASE.setIcon(icon17)
        self.actionNewCASE.setObjectName("actionNewCASE")
        self.actionCopyCASE = QtWidgets.QAction(MainWindow)
        icon18 = QtGui.QIcon()
        icon18.addPixmap(QtGui.QPixmap(":/icos/importShortcuts.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionCopyCASE.setIcon(icon18)
        self.actionCopyCASE.setObjectName("actionCopyCASE")
        self.actionDeleteCASE = QtWidgets.QAction(MainWindow)
        icon19 = QtGui.QIcon()
        icon19.addPixmap(QtGui.QPixmap(":/icos/cBreak.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionDeleteCASE.setIcon(icon19)
        self.actionDeleteCASE.setObjectName("actionDeleteCASE")
        self.actionRunning = QtWidgets.QAction(MainWindow)
        icon20 = QtGui.QIcon()
        icon20.addPixmap(QtGui.QPixmap(":/icos/1rightarrow.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionRunning.setIcon(icon20)
        self.actionRunning.setObjectName("actionRunning")
        self.actionRunAllCASE = QtWidgets.QAction(MainWindow)
        icon21 = QtGui.QIcon()
        icon21.addPixmap(QtGui.QPixmap(":/icos/2rightarrow.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionRunAllCASE.setIcon(icon21)
        self.actionRunAllCASE.setObjectName("actionRunAllCASE")
        self.actionProjectManager = QtWidgets.QAction(MainWindow)
        icon22 = QtGui.QIcon()
        icon22.addPixmap(QtGui.QPixmap(":/icos/projectOpen.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionProjectManager.setIcon(icon22)
        self.actionProjectManager.setObjectName("actionProjectManager")
        self.actionNACA = QtWidgets.QAction(MainWindow)
        icon23 = QtGui.QIcon()
        icon23.addPixmap(QtGui.QPixmap(":/icos/fileIDL.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionNACA.setIcon(icon23)
        self.actionNACA.setObjectName("actionNACA")
        self.actionModelPreview = QtWidgets.QAction(MainWindow)
        icon24 = QtGui.QIcon()
        icon24.addPixmap(QtGui.QPixmap(":/icos/preferences-search.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionModelPreview.setIcon(icon24)
        self.actionModelPreview.setObjectName("actionModelPreview")
        self.actionReportWindow = QtWidgets.QAction(MainWindow)
        icon25 = QtGui.QIcon()
        icon25.addPixmap(QtGui.QPixmap(":/icos/drawFill.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionReportWindow.setIcon(icon25)
        self.actionReportWindow.setObjectName("actionReportWindow")
        self.actionExportUserLayout = QtWidgets.QAction(MainWindow)
        icon26 = QtGui.QIcon()
        icon26.addPixmap(QtGui.QPixmap(":/icos/pullBookmark.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionExportUserLayout.setIcon(icon26)
        self.actionExportUserLayout.setObjectName("actionExportUserLayout")
        self.actionReloadLayout = QtWidgets.QAction(MainWindow)
        icon27 = QtGui.QIcon()
        icon27.addPixmap(QtGui.QPixmap(":/icos/updateLocal.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionReloadLayout.setIcon(icon27)
        self.actionReloadLayout.setObjectName("actionReloadLayout")
        self.actionImportUserLayout = QtWidgets.QAction(MainWindow)
        icon28 = QtGui.QIcon()
        icon28.addPixmap(QtGui.QPixmap(":/icos/pushBookmark.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionImportUserLayout.setIcon(icon28)
        self.actionImportUserLayout.setObjectName("actionImportUserLayout")
        self.actionNewSubPlot = QtWidgets.QAction(MainWindow)
        icon29 = QtGui.QIcon()
        icon29.addPixmap(QtGui.QPixmap(":/icos/preferences-shell.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionNewSubPlot.setIcon(icon29)
        self.actionNewSubPlot.setObjectName("actionNewSubPlot")
        self.actionDeletePlot = QtWidgets.QAction(MainWindow)
        icon30 = QtGui.QIcon()
        icon30.addPixmap(QtGui.QPixmap(":/icos/deleteShape.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionDeletePlot.setIcon(icon30)
        self.actionDeletePlot.setObjectName("actionDeletePlot")
        self.actionOnlyOnePlot = QtWidgets.QAction(MainWindow)
        icon31 = QtGui.QIcon()
        icon31.addPixmap(QtGui.QPixmap(":/icos/plot1.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionOnlyOnePlot.setIcon(icon31)
        self.actionOnlyOnePlot.setObjectName("actionOnlyOnePlot")
        self.actionTwoOneLayout = QtWidgets.QAction(MainWindow)
        icon32 = QtGui.QIcon()
        icon32.addPixmap(QtGui.QPixmap(":/icos/plot1-2.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionTwoOneLayout.setIcon(icon32)
        self.actionTwoOneLayout.setObjectName("actionTwoOneLayout")
        self.actionTwoTwoPlot = QtWidgets.QAction(MainWindow)
        icon33 = QtGui.QIcon()
        icon33.addPixmap(QtGui.QPixmap(":/icos/icons.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionTwoTwoPlot.setIcon(icon33)
        self.actionTwoTwoPlot.setObjectName("actionTwoTwoPlot")
        self.actionCustomPlot = QtWidgets.QAction(MainWindow)
        icon34 = QtGui.QIcon()
        icon34.addPixmap(QtGui.QPixmap(":/icos/grid.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionCustomPlot.setIcon(icon34)
        self.actionCustomPlot.setObjectName("actionCustomPlot")
        self.actionLogWindow = QtWidgets.QAction(MainWindow)
        icon35 = QtGui.QIcon()
        icon35.addPixmap(QtGui.QPixmap(":/icos/compareFiles.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionLogWindow.setIcon(icon35)
        self.actionLogWindow.setObjectName("actionLogWindow")
        self.actionImageTips = QtWidgets.QAction(MainWindow)
        icon36 = QtGui.QIcon()
        icon36.addPixmap(QtGui.QPixmap(":/icos/siteinfo-media.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionImageTips.setIcon(icon36)
        self.actionImageTips.setObjectName("actionImageTips")
        self.menuProject.addAction(self.actionNewProject)
        self.menuProject.addAction(self.actionOpenProject)
        self.menuProject.addAction(self.actionSaveProject)
        self.menuProject.addSeparator()
        self.menuProject.addAction(self.actionSettings)
        self.menuProject.addAction(self.actionImportSettings)
        self.menuProject.addAction(self.actionExportSettings)
        self.menuProject.addSeparator()
        self.menuProject.addAction(self.actionExit)
        self.menu_Model.addAction(self.actionBrowse)
        self.menu_Model.addAction(self.actionNewModel)
        self.menu_Model.addAction(self.actionCopyModel)
        self.menu_Model.addAction(self.actionSaveModel)
        self.menu_Model.addSeparator()
        self.menu_Model.addAction(self.actionExportModel)
        self.menu_Model.addAction(self.actionImportModel)
        self.menu_Model.addSeparator()
        self.menu_Model.addAction(self.actionManageModelTemplete)
        self.menu_Model.addSeparator()
        self.menu_Model.addAction(self.actionNACA)
        self.menu_Problem.addAction(self.actionNewCASE)
        self.menu_Problem.addAction(self.actionCopyCASE)
        self.menu_Problem.addAction(self.actionDeleteCASE)
        self.menu_Problem.addSeparator()
        self.menu_Problem.addAction(self.actionRunning)
        self.menu_Problem.addAction(self.actionRunAllCASE)
        self.menu_Problem.addSeparator()
        self.menu_Windows.addAction(self.actionProjectManager)
        self.menu_Windows.addAction(self.actionModelPreview)
        self.menu_Windows.addAction(self.actionReportWindow)
        self.menu_Windows.addAction(self.actionLogWindow)
        self.menu_Windows.addAction(self.actionImageTips)
        self.menu_Windows.addSeparator()
        self.menu_Windows.addAction(self.actionReloadLayout)
        self.menu_Windows.addAction(self.actionImportUserLayout)
        self.menu_Windows.addAction(self.actionExportUserLayout)
        self.menu_Windows.addSeparator()
        self.menu_Help.addAction(self.actionDocs)
        self.menu_Help.addAction(self.actionHelp)
        self.menu_Help.addSeparator()
        self.menu_Help.addAction(self.actionAbout)
        self.menu_8.addAction(self.actionNewSubPlot)
        self.menu_8.addAction(self.actionDeletePlot)
        self.menu_7.addAction(self.actionOnlyOnePlot)
        self.menu_7.addAction(self.actionTwoOneLayout)
        self.menu_7.addAction(self.actionTwoTwoPlot)
        self.menu_7.addSeparator()
        self.menu_7.addAction(self.actionCustomPlot)
        self.menu_Analyze.addAction(self.menu_8.menuAction())
        self.menu_Analyze.addSeparator()
        self.menu_Analyze.addAction(self.menu_7.menuAction())
        self.menuBar.addAction(self.menuProject.menuAction())
        self.menuBar.addAction(self.menu_Model.menuAction())
        self.menuBar.addAction(self.menu_Problem.menuAction())
        self.menuBar.addAction(self.menu_Analyze.menuAction())
        self.menuBar.addAction(self.menu_Windows.menuAction())
        self.menuBar.addAction(self.menu_Help.menuAction())
        self.toolBar.addAction(self.actionNewProject)
        self.toolBar.addAction(self.actionOpenProject)
        self.toolBar.addAction(self.actionSaveProject)
        self.toolBar.addSeparator()
        self.toolBar.addAction(self.actionBrowse)
        self.toolBar.addAction(self.actionNewModel)
        self.toolBar.addAction(self.actionSaveModel)
        self.toolBar.addAction(self.actionCopyModel)
        self.toolBar.addAction(self.actionExportModel)
        self.toolBar.addAction(self.actionImportModel)
        self.toolBar.addSeparator()
        self.toolBar.addAction(self.actionNewCASE)
        self.toolBar.addAction(self.actionDeleteCASE)
        self.toolBar.addAction(self.actionCopyCASE)
        self.toolBar.addAction(self.actionRunning)
        self.toolBar.addAction(self.actionRunAllCASE)
        self.toolBar.addSeparator()
        self.toolBar.addAction(self.actionNACA)
        self.toolBar.addSeparator()
        self.toolBar.addAction(self.actionNewSubPlot)
        self.toolBar.addAction(self.actionDeletePlot)
        self.toolBar.addAction(self.actionOnlyOnePlot)
        self.toolBar.addAction(self.actionTwoOneLayout)
        self.toolBar.addAction(self.actionTwoTwoPlot)
        self.toolBar.addAction(self.actionCustomPlot)

        self.retranslateUi(MainWindow)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)

    def retranslateUi(self, MainWindow):
        _translate = QtCore.QCoreApplication.translate
        MainWindow.setWindowTitle(_translate("MainWindow", "Datcom辅助分析软件"))
        self.menuProject.setTitle(_translate("MainWindow", "项目"))
        self.menu_Model.setTitle(_translate("MainWindow", "模型"))
        self.menu_Problem.setTitle(_translate("MainWindow", "算例"))
        self.menu_Windows.setTitle(_translate("MainWindow", "窗口"))
        self.menu_Help.setTitle(_translate("MainWindow", "帮助"))
        self.menu_Analyze.setTitle(_translate("MainWindow", "分析"))
        self.menu_8.setTitle(_translate("MainWindow", "分析向导"))
        self.menu_7.setTitle(_translate("MainWindow", "曲线窗口布局"))
        self.toolBar.setWindowTitle(_translate("MainWindow", "toolBar"))
        self.actionDocs.setText(_translate("MainWindow", "文档"))
        self.actionHelp.setText(_translate("MainWindow", "帮助"))
        self.actionAbout.setText(_translate("MainWindow", "关于我们"))
        self.actionNewProject.setText(_translate("MainWindow", "新建项目"))
        self.actionOpenProject.setText(_translate("MainWindow", "打开项目"))
        self.actionSaveProject.setText(_translate("MainWindow", "保存项目"))
        self.actionSettings.setText(_translate("MainWindow", "设置"))
        self.actionImportSettings.setText(_translate("MainWindow", "导入设置"))
        self.actionExportSettings.setText(_translate("MainWindow", "导出设置"))
        self.actionExit.setText(_translate("MainWindow", "退出"))
        self.actionBrowse.setText(_translate("MainWindow", "浏览"))
        self.actionNewModel.setText(_translate("MainWindow", "新建模型"))
        self.actionSaveModel.setText(_translate("MainWindow", "保存模型"))
        self.actionExportModel.setText(_translate("MainWindow", "导出模型"))
        self.actionImportModel.setText(_translate("MainWindow", "导入模型"))
        self.actionCopyModel.setText(_translate("MainWindow", "复制模型"))
        self.actionManageModelTemplete.setText(_translate("MainWindow", "管理模型模板"))
        self.actionNewCASE.setText(_translate("MainWindow", "新建CASE"))
        self.actionCopyCASE.setText(_translate("MainWindow", "复制CASE"))
        self.actionDeleteCASE.setText(_translate("MainWindow", "删除CASE"))
        self.actionRunning.setText(_translate("MainWindow", "运行当前CASE"))
        self.actionRunAllCASE.setText(_translate("MainWindow", "运行所有CASE"))
        self.actionProjectManager.setText(_translate("MainWindow", "项目管理器"))
        self.actionNACA.setText(_translate("MainWindow", "NACA定义"))
        self.actionModelPreview.setText(_translate("MainWindow", "模型预览窗口"))
        self.actionReportWindow.setText(_translate("MainWindow", "报表窗口"))
        self.actionExportUserLayout.setText(_translate("MainWindow", "导出用户布局"))
        self.actionReloadLayout.setText(_translate("MainWindow", "恢复默认布局"))
        self.actionImportUserLayout.setText(_translate("MainWindow", "导入用户布局"))
        self.actionNewSubPlot.setText(_translate("MainWindow", "新建图表"))
        self.actionDeletePlot.setText(_translate("MainWindow", "删除图表"))
        self.actionOnlyOnePlot.setText(_translate("MainWindow", "1行1列"))
        self.actionTwoOneLayout.setText(_translate("MainWindow", "1行2列"))
        self.actionTwoTwoPlot.setText(_translate("MainWindow", "2行2列"))
        self.actionCustomPlot.setText(_translate("MainWindow", "自定义"))
        self.actionLogWindow.setText(_translate("MainWindow", "日志窗口"))
        self.actionImageTips.setText(_translate("MainWindow", "可视化提示窗口"))

import MainWindow_rc

if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    MainWindow = QtWidgets.QMainWindow()
    ui = Ui_MainWindow()
    ui.setupUi(MainWindow)
    MainWindow.show()
    sys.exit(app.exec_())

