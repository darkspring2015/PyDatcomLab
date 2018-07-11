QTabWidget的 doubleclick问题



```python
self.tabWidget_Configuration = QtWidgets.QTabWidget(Dialog)
self.tabWidget_Configuration.setObjectName("tabWidget_Configuration")
self.tabWidget_Configuration.setMovable(True)
self.tabWidget_Configuration.setTabsClosable(True)
self.tabWidget_Configuration.setTabBarAutoHide(True)
self.tabWidget_Configuration.setUsesScrollButtons(True)
self.tabWidget_Configuration.setDocumentMode(True)  #使得可以点击到空白区域，否则无法响应
```



日常的bug修复



修正项
1.需要修正综合参数全部为可选
2.table在选择时没有初值的问题
3.删除选项卡有错误，多删除了一个，仍然无法解释
4.录入的数据没有正确的保存

删除选项卡有错误，多删除了一个：因为TabWidget的函数中触发了新的删除



5.表格无法再新添加的过程中，对NMACH的做出正确的响应！需要修改加载逻辑



6.长度单位制转换逻辑异常，存在转换误差 ！ 这个误差不是编程误差，而是舍入误差，因为界面定义了一个显示精度。处理方法，将界面的精度和DatcomInput使用的精度分离开来

7.FLTCON输入界面的变量的Enable状态判断错误，是因为调用on_EnabledStatusChanged的输入从BOOL变成了INT，但是条用端没有修改，已修复

8.控件没有在开始时自动的判断NMACH的逻辑状态 ，on_Singal_NMACHChanged槽函数没有被正确的引接





# Emit（）

```
Qt支持6种连接方式，其中3中最主要：
    Qt::DirectConnection（直连方式）
      当信号发出后，相应的槽函数将立即被调用。emit语句后的代码将在所有槽函数执行完毕后被执行。（信号与槽函数关系类似于函数调用，同步执行）
    Qt::QueuedConnection（排队方式）
      当信号发出后，排队到信号队列中，需等到接收对象所属线程的事件循环取得控制权时才取得该信号，调用相应的槽函数。emit语句后的代码将在发出信号后立即被执行，无需等待槽函数执行完毕。（此时信号被塞到信号队列里了，信号与槽函数关系类似于消息通信，异步执行）
    Qt::AutoConnection（自动方式）
      Qt的默认连接方式，如果信号的发出和接收这个信号的对象同属一个线程，那个工作方式与直连方式相同；否则工作方式与排队方式相同。
```



开始一次非常巨大的底层重构！

将数据同步逻辑写入到一个Wrapper类中，所有的修改操作的结果触发关系变换到dtModel类中，实现类似QtModel类的识别层——一个伟大的造轮子的过程。