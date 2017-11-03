#！
#PyDatcom的主程序
import os, sys
from PyQt5 import QtCore, QtWidgets


#确保相对的导入能够起到作用，需要导入相对路径
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'Core')))

#导入主要的窗体
from PyDatcomLab.GUIs.MainWindow import DatcomMainWindow

#执行主要脚本
app = QtWidgets.QApplication(sys.argv)
mainWin = DatcomMainWindow()
mainWin.show()
sys.exit(app.exec_())





