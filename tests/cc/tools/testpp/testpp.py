import re
import sys

class TestPreprocessor:
  def __init__(self):
    self.functions = {}
    self.stubs = []
    self.setup_fun = ''

  def ParseFunctions(self, text):
    funs = []
    pos = text.find("\nlet")
    # skip module header
    if text.startswith('let') == False:
      text = text[pos + 1:]
      pos = text.find("\nlet")
    while pos <> -1:
      funs.append(text[:pos + 1])
      text = text[pos + 1:]
      pos = text.find("\nlet")
    funs.append(text[pos + 1:])
    return funs

  def AddFunctionInfo(self, fun_text):
    let_fun_text = fun_text.replace('\nand', '\nlet')
    if let_fun_text <> fun_text:
      self.AddFunctions(let_fun_text)
    else:
      fun_text_parts = fun_text.split(" ")
      name = fun_text_parts[1]
      if name == "rec":
        name = fun_text_parts[2]
      pattern = r"let\s+(rec\s+)?\w+\s+(\?\w+(\s*:\(.+?=.+?\))?\s*)*.*?="
      match = re.search(pattern, fun_text)
      header = match.group()
      self.functions[name] = {'header': header, 'full': fun_text}

  def AddFunctions(self, text):
    funs_text = self.ParseFunctions(text)
    for fun_text in funs_text:
      self.AddFunctionInfo(fun_text)

  def GetFunctionInfo(self, name):
    return self.functions[name]

  def ProcessLine(self, line):
    splitted_line = line.split(' ')
    if len(splitted_line) > 1:
      fun_name = splitted_line[1].strip()
    if line.startswith('#stub'):
      fun_header = self.GetFunctionInfo(fun_name)['header']
      ret_value_pos = line.find(' ')
      ret_value_pos = line.find(' ', ret_value_pos + 1)
      ret_value = line[ret_value_pos + 1:]
      saved_fun_name = fun_name + '__'
      saved_fun = 'let ' + saved_fun_name + ' = ' + fun_name + '\n'
      stub_fun = fun_header + ' ' + ret_value
      self.stubs.append(fun_name)
      return saved_fun + stub_fun
    if line.startswith('#setup'):
      self.setup_fun = fun_name
      return self.GetFunctionInfo(fun_name)['full']
    if line.startswith('#teardown'):
      restore_funs = ''
      for stub in self.stubs:
        restore_funs += 'let ' + stub + ' = ' + stub + '__\n'
      restore_funs += self.GetFunctionInfo(self.setup_fun)['full']
      self.stubs = []
      self.setup_fun = ''
      return restore_funs
    return line

  def GenTestSuiteCode(self):
    code = '\nlet suite = "Suite" >::: [\n'
    for name in self.functions:
      if name.startswith('test'):
        code += '\t"' + name + '" >:: ' + name + ';\n'
    code += ']\n'
    code += 'let _ = run_test_tt ~verbose:false suite'
    return code

  def ProcessText(self, text):
    processed = ''
    lines = text.splitlines(True)
    for line in lines:
      processed += self.ProcessLine(line)
    self.AddFunctions(text)
    processed += self.GenTestSuiteCode()
    return processed

if __name__ == '__main__':
  if (len(sys.argv) < 2):
    sys.stdout.write('Usage: python testpp.py')
    sys.stdout.write(' <funs_file> <tests_file> <output_file>')
  else:
    pp = TestPreprocessor()
    funs_file = sys.argv[1]
    tests_file = sys.argv[2]
    output_file = sys.argv[3]
    pp.AddFunctions(open(funs_file).read())
    processed_text = pp.ProcessText(open(tests_file).read())
    output_file_handle = open(output_file, 'w')
    output_file_handle.write(processed_text)
    output_file_handle.close()

