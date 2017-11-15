import unittest
from testpp import TestPreprocessor

class TestPreprocessorTest(unittest.TestCase):
  def setUp(self):
    self.pp = TestPreprocessor()

  def testParsesFunctionsIfOne(self):
    fun = open('data/fun1.ml').read()
    self.assertEquals([fun], self.pp.ParseFunctions(fun))

  def testParsesFunctionsIfSeveral(self):
    fun1 = open('data/fun1.ml').read()
    fun2 = open('data/fun2.ml').read()
    self.assertEquals([fun1, fun2], self.pp.ParseFunctions(fun1 + fun2))

  def testParsesFunctionsIfOneWithModuleHeader(self):
    module_header = open('data/module_header.ml').read()
    fun = open('data/fun1.ml').read()
    self.assertEquals([fun], self.pp.ParseFunctions(module_header + fun))

  def testAddsFunctionInfo(self):
    fun_name = 'fun_name'
    header = 'let ' + fun_name + ' param ='
    fun =  header + 'body'
    self.pp.AddFunctionInfo(fun)
    fun_info = {'header': header, 'full': fun}
    self.assertEquals(fun_info, self.pp.GetFunctionInfo(fun_name))

  def testAddsFunctionInfoWithTupleArg(self):
    fun_name = 'fun_name'
    header = 'let ' + fun_name + ' (a, b) ='
    fun =  header + 'body'
    self.pp.AddFunctionInfo(fun)
    fun_info = {'header': header, 'full': fun}
    self.assertEquals(fun_info, self.pp.GetFunctionInfo(fun_name))

  def testAddsFunctionInfoWithOptionalArg(self):
    fun_name = 'fun_name'
    header = 'let ' + fun_name + ' ?param:(p=val) p='
    fun =  header + 'body'
    self.pp.AddFunctionInfo(fun)
    fun_info = {'header': header, 'full': fun}
    self.assertEquals(fun_info, self.pp.GetFunctionInfo(fun_name))

  def testAddsRecursiveFunctionInfo(self):
    fun_name = 'fun_name'
    header = 'let rec ' + fun_name + ' param ='
    fun =  header + 'body'
    self.pp.AddFunctionInfo(fun)
    fun_info = {'header': header, 'full': fun}
    self.assertEquals(fun_info, self.pp.GetFunctionInfo(fun_name))

  def testAddsRecursiveFunctionInfoWithOptionalArg(self):
    fun_name = 'fun_name'
    header = 'let rec ' + fun_name + ' ?optional:(name=value) param ='
    fun =  header + 'body'
    self.pp.AddFunctionInfo(fun)
    fun_info = {'header': header, 'full': fun}
    self.assertEquals(fun_info, self.pp.GetFunctionInfo(fun_name))

  def testAddsFunctionWithDepsInfo(self):
    fun_name = 'fun_name'
    header = 'let ' + fun_name + ' param ='
    dep_name = 'dep_name'
    and_dep_header = 'and ' + dep_name + ' dep_param ='
    let_dep_header = 'let ' + dep_name + ' dep_param ='
    and_dep = and_dep_header + 'dep_body\n'
    let_dep = let_dep_header + 'dep_body\n'
    let_fun = header + 'body\n'
    fun = let_fun  + and_dep
    self.pp.AddFunctionInfo(fun)
    fun_info = {'header': header, 'full': let_fun}
    dep_info = {'header': let_dep_header, 'full': let_dep}
    self.assertEquals(fun_info, self.pp.GetFunctionInfo(fun_name))
    self.assertEquals(dep_info, self.pp.GetFunctionInfo(dep_name))

  def testProcessesStub(self):
    fun_header = 'let fun args ='
    fun_name = 'fun'
    self.pp.functions[fun_name] = {'header': fun_header}
    ret_value = 'ret_value'
    stub = '#stub ' + fun_name + ' ' + ret_value + '\n'
    saved_fun_name = fun_name + '__'
    saved_fun = 'let ' + saved_fun_name + ' = ' + fun_name + '\n'
    stub_fun = fun_header + ' ' + ret_value + '\n'
    processed = saved_fun + stub_fun
    self.assertEquals(processed, self.pp.ProcessLine(stub))
    self.assertEquals(1, len(self.pp.stubs))
    self.assertEquals(fun_name, self.pp.stubs[0])

  def testProcessesStubWhenReturnValueIsStringWithSpaces(self):
    fun_header = 'let fun args ='
    fun_name = 'fun'
    self.pp.functions[fun_name] = {'header': fun_header}
    ret_value = '"string with spaces"'
    stub = '#stub ' + fun_name + ' ' + ret_value + '\n'
    saved_fun_name = fun_name + '__'
    saved_fun = 'let ' + saved_fun_name + ' = ' + fun_name
    stub_fun = fun_header + ' ' + ret_value + '\n'
    processed = saved_fun + '\n' + stub_fun
    self.assertEquals(processed, self.pp.ProcessLine(stub))
    self.assertEquals(1, len(self.pp.stubs))
    self.assertEquals(fun_name, self.pp.stubs[0])

  def testProcessesSetup(self):
    fun_name = 'fun_name'
    setup = '#setup ' + fun_name + '\n'
    fun_full = 'fun_full'
    self.pp.functions[fun_name] = {'full': fun_full}
    self.assertEquals(fun_full, self.pp.ProcessLine(setup))
    self.assertEquals(fun_name, self.pp.setup_fun)

  def testProcessesTearDown(self):
    fun1_name = 'fun1'
    fun2_name = 'fun2'
    saved_fun1_name = fun1_name + '__'
    saved_fun2_name = fun2_name + '__'
    self.pp.stubs = [fun1_name, fun2_name]
    restore_stubs = 'let ' + fun1_name + ' = ' + saved_fun1_name + '\n'
    restore_stubs += 'let ' + fun2_name + ' = ' + saved_fun2_name + '\n'
    setup_fun_full = 'setup_fun_full'
    setup_fun_name = 'setup_fun_name'
    self.pp.setup_fun = setup_fun_name
    self.pp.functions[setup_fun_name] = {'full': setup_fun_full}
    restore_funs = restore_stubs + setup_fun_full
    self.assertEquals(restore_funs, self.pp.ProcessLine('#teardown\n'))
    self.assertEquals(0, len(self.pp.stubs))
    self.assertEquals('', self.pp.setup_fun)

  def testSkipsRegularLineProcessing(self):
    line = 'regular_line'
    self.assertEquals(line, self.pp.ProcessLine(line))

unittest.main()
