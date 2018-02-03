#!/usr/bin/env python
"""
本模块负责封装Datcom中需要用到的数据库部分的函数
1.创建Datcom基础信息的定义
2.创建PyDatcomLab程序运行需要的配置库
"""

#Author : Hongten
#MailTo : hongtenzone@foxmail.com
#QQ     : 648719819
#Blog   : http://www.cnblogs.com/hongten
#Create : 2013-08-09
#Version: 1.0

#DB-API 2.0 interface for SQLite databases

import sqlite3
import os
'''SQLite数据库是一款非常小巧的嵌入式开源数据库软件，也就是说
没有独立的维护进程，所有的维护都来自于程序本身。
在python中，使用sqlite3创建数据库的连接，当我们指定的数据库文件不存在的时候
连接对象会自动创建数据库文件；如果数据库文件已经存在，则连接对象不会再创建
数据库文件，而是直接打开该数据库文件。
    连接对象可以是硬盘上面的数据库文件，也可以是建立在内存中的，在内存中的数据库
    执行完任何操作后，都不需要提交事务的(commit)

    创建在硬盘上面： conn = sqlite3.connect('c:\\test\\test.db')
    创建在内存上面： conn = sqlite3.connect('"memory:')

    下面我们一硬盘上面创建数据库文件为例来具体说明：
    conn = sqlite3.connect('c:\\test\\hongten.db')
    其中conn对象是数据库链接对象，而对于数据库链接对象来说，具有以下操作：

        commit()            --事务提交
        rollback()          --事务回滚
        close()             --关闭一个数据库链接
        cursor()            --创建一个游标

    cu = conn.cursor()
    这样我们就创建了一个游标对象：cu
    在sqlite3中，所有sql语句的执行都要在游标对象的参与下完成
    对于游标对象cu，具有以下具体操作：

        execute()           --执行一条sql语句
        executemany()       --执行多条sql语句
        close()             --游标关闭
        fetchone()          --从结果中取出一条记录
        fetchmany()         --从结果中取出多条记录
        fetchall()          --从结果中取出所有记录
        scroll()            --游标滚动

'''

import logging

class datcomInfoDB(object):
    """
    datcomInfoDB 封装Datcom本身的定义信息
    主要表包括： 变量定义表
    """
    def __init__(self, dbFile = r'~\.PyDatcom\DB\datcom.db'):
        """
        类初始化函数
        """
        #超类
        #日志
        self.logger = logging.getLogger(r'Datcomlogger')
        
        #保存数据库目录
        self.dbFile = dbFile
        if dbFile == ':memory:':
            self.dbFile = ':memory:'
        else:
            self.dbFile = os.path.expanduser(dbFile)
        #表格常量定义
        self.sqlVariableDefine ="""\
CREATE TABLE `VariableDefine` (
                              `VarName` varchar(10) NOT NULL,
                              `NameList` varchar(10) NOT NULL,
                              `ShowName` varchar(20) NOT NULL,
                              `ToolTips` varchar(100) DEFAULT NULL,
                              `TYPE` varchar(10) DEFAULT `REAL`,
                              `Group` varchar(20) DEFAULT NULL,
                              `Range` varchar(100) DEFAULT NULL,
                              `Default` varchar(100) DEFAULT NULL,                              
                              `Limit` varchar(20) DEFAULT NULL,    
                              `Dimension` varchar(20) DEFAULT NULL,  
                              `Relation` varchar(20) DEFAULT NULL,                                
                               PRIMARY KEY (`id`,`NameList`)        
        """
        self.sqlNamelistDefine ="""\
CREATE TABLE `NamelistDefine` (
                              `NameList` varchar(10) NOT NULL,
                              `ShowName` varchar(20) NOT NULL,
                              `ToolTips` varchar(100) DEFAULT NULL,
                              `CardGroup` varchar(10) DEFAULT `Group 2`,                                               
                               PRIMARY KEY (`NameList`)        
        """
        #创建数据库
        self.createEmptyDB(self.dbFile)
        
        
        #内部状态量
        self.SHOW_SQL = True
        
    def createEmptyDB(self,dbFile = r'~\.PyDatcom\DB\datcom.db' ):
        """
        建表建库 
        
        @param dbFile 数据库路径
        @type str
        """
        if dbFile == ':memory:':
            self.dbFile = ':memory:'
        else:
            dbFile = os.path.expanduser(dbFile)
            if not os.path.exists(dbFile):
                os.mkdirs(os.path.dirname(dbFile))
                os.mknod(dbFile) #创建空文件
        #python sqlite
        self.create_table()

        
        
    def get_conn(self, path):
        '''获取到数据库的连接对象，参数为数据库文件的绝对路径
        如果传递的参数是存在，并且是文件，那么就返回硬盘上面改
        路径下的数据库文件的连接对象；否则，返回内存中的数据接
        连接对象'''

        if os.path.exists(path) and os.path.isfile(path):
            self.logger.info('硬盘上面:[{}]'.format(path))
            return sqlite3.connect(path)
        else:
            self.logger.info('内存上面:[:memory:]')
            return sqlite3.connect(':memory:')

    def get_cursor(self, conn):
        '''该方法是获取数据库的游标对象，参数为数据库的连接对象
        如果数据库的连接对象不为None，则返回数据库连接对象所创
        建的游标对象；否则返回一个游标对象，该对象是内存中数据
        库连接对象所创建的游标对象'''
        if conn is not None:
            return conn.cursor()
        else:
            return self.get_conn('').cursor()
            
    ###############################################################
    ####            创建|删除表操作     START
    ###############################################################
    def drop_table(self, conn, table):
        '''如果表存在,则删除表，如果表中存在数据的时候，使用该
        方法的时候要慎用！'''
        if table is not None and table != '':
            sql = 'DROP TABLE IF EXISTS ' + table
            if self.SHOW_SQL:
                self.logger.info('执行sql:[{}]'.format(sql))
            cu = self.get_cursor(conn)
            cu.execute(sql)
            conn.commit()
            self.logger.info('删除数据库表[{}]成功!'.format(table))
            self.close_all(conn, cu)
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql))

    def create_table(self, conn, sql):
        '''创建数据库表：'''
        if sql is not None and sql != '':
            cu = self.get_cursor(conn)
            if self.SHOW_SQL:
                self.logger.info('执行sql:[{}]'.format(sql))
            cu.execute(sql)
            conn.commit()
            self.logger.info('创建数据库表[student]成功!')
            self.close_all(conn, cu)
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql))
    
    ###############################################################
    ####            创建|删除表操作     END
    ###############################################################

    def close_all(self, conn, cu):
        '''关闭数据库游标对象和数据库连接对象'''
        try:
            if cu is not None:
                cu.close()
        finally:
            if cu is not None:
                cu.close()

    ###############################################################
    ####            数据库操作CRUD     START
    ###############################################################
    
    def save(self, conn, sql, data):
        '''插入数据'''
        if sql is not None and sql != '':
            if data is not None:
                cu = self.get_cursor(conn)
                for d in data:
                    if self.SHOW_SQL:
                        self.logger.info('执行sql:[{}],参数:[{}]'.format(sql, d))
                    cu.execute(sql, d)
                    conn.commit()
                self.close_all(conn, cu)
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql))

    def fetchall(self, conn, sql):
        '''查询所有数据'''
        if sql is not None and sql != '':
            cu = self.get_cursor(conn)
            if self.SHOW_SQL:
                self.logger.info('执行sql:[{}]'.format(sql))
            cu.execute(sql)
            r = cu.fetchall()
            if len(r) > 0:
                for e in range(len(r)):
                    self.logger.info(r[e])
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql)) 

    def fetchone(self, conn, sql, data):
        '''查询一条数据'''
        if sql is not None and sql != '':
            if data is not None:
                #Do this instead
                d = (data,) 
                cu = self.get_cursor(conn)
                if self.SHOW_SQL:
                    self.logger.info('执行sql:[{}],参数:[{}]'.format(sql, data))
                cu.execute(sql, d)
                r = cu.fetchall()
                if len(r) > 0:
                    for e in range(len(r)):
                        self.logger.info(r[e])
            else:
                self.logger.error('the [{}] equal None!'.format(data))
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql))

    def update(self, conn, sql, data):
        '''更新数据'''
        if sql is not None and sql != '':
            if data is not None:
                cu = self.get_cursor(conn)
                for d in data:
                    if self.SHOW_SQL:
                        self.logger.info('执行sql:[{}],参数:[{}]'.format(sql, d))
                    cu.execute(sql, d)
                    conn.commit()
                self.close_all(conn, cu)
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql))

    def delete(self, conn, sql, data):
        '''删除数据'''
        if sql is not None and sql != '':
            if data is not None:
                cu = self.get_cursor(conn)
                for d in data:
                    if self.SHOW_SQL:
                        self.logger.info('执行sql:[{}],参数:[{}]'.format(sql, d))
                    cu.execute(sql, d)
                    conn.commit()
                self.close_all(conn, cu)
        else:
            self.logger.error('the [{}] is empty or equal None!'.format(sql))
    ###############################################################
    ####            数据库操作CRUD     END
    ###############################################################


    ###############################################################
    ####            测试操作     START
    ###############################################################
    def drop_table_test(self):
        '''删除数据库表测试'''
        print('删除数据库表测试...')
        conn = self.get_conn(self.dbFile)
        self.drop_table(conn, 'VariableDefine')
    
    def create_table_VariableDefine(self):
        '''创建数据库表测试'''
        self.logger.info('创建数据库表:VariableDefine...')
        create_table_sql = '''CREATE TABLE `VariableDefine` (
                              `id` int(11) NOT NULL,
                              `VarName` varchar(10) NOT NULL,
                              `NameList` varchar(10) NOT NULL,
                              `ShowName` varchar(20) NOT NULL,
                              `ToolTips` varchar(100) DEFAULT NULL,
                              `TYPE` varchar(10) DEFAULT `REAL`,
                              `Group` varchar(20) DEFAULT NULL,
                              `Range` varchar(100) DEFAULT NULL,
                              `Default` varchar(100) DEFAULT NULL,                              
                              `Limit` varchar(20) DEFAULT NULL,    
                              `Dimension` varchar(20) DEFAULT NULL,                                                
                               PRIMARY KEY (`id`,`NameList`)
                            )'''
        conn = self.get_conn(self.dbFile)
        self.create_table(conn, create_table_sql)
    
    def save_test(self):
        '''保存数据测试...'''
        print('保存数据测试...')
        save_sql = '''INSERT INTO student values (?, ?, ?, ?, ?, ?)'''
        data = [(1, 'Hongten', '男', 20, '广东省广州市', '13423****62'),
                (2, 'Tom', '男', 22, '美国旧金山', '15423****63'),
                (3, 'Jake', '女', 18, '广东省广州市', '18823****87'),
                (4, 'Cate', '女', 21, '广东省广州市', '14323****32')]
        conn = self.get_conn(self.dbFile)
        self.save(conn, save_sql, data)
    
    def fetchall_test(self):
        '''查询所有数据...'''
        self.logger.info('查询所有数据...')
        fetchall_sql = '''SELECT * FROM student'''
        conn = self.get_conn(self.dbFile)
        self.fetchall(conn, fetchall_sql)
    
    def fetchone_test(self):
        '''查询一条数据...'''
        self.logger.info('查询一条数据...')
        fetchone_sql = 'SELECT * FROM student WHERE ID = ? '
        data = 1
        conn = self.get_conn(self.dbFile)
        self.fetchone(conn, fetchone_sql, data)
    
    def update_test(self):
        '''更新数据...'''
        self.logger.info('更新数据...')
        update_sql = 'UPDATE student SET name = ? WHERE ID = ? '
        data = [('HongtenAA', 1),
                ('HongtenBB', 2),
                ('HongtenCC', 3),
                ('HongtenDD', 4)]
        conn = self.get_conn(self.dbFile)
        self.update(conn, update_sql, data)
    
    def delete_test(self):
        '''删除数据...'''
        self.logger.info('删除数据...')
        delete_sql = 'DELETE FROM student WHERE NAME = ? AND ID = ? '
        data = [('HongtenAA', 1),
                ('HongtenCC', 3)]
        conn = self.get_conn(self.dbFile)
        self.delete(conn, delete_sql, data)
    
    ###############################################################
    ####            测试操作     END
    ###############################################################
    


if __name__ == '__main__':
    tDB = datcomInfoDB(r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\DB\1.db')
    tDB.create_table_VariableDefine()


