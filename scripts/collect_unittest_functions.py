'''
This script iterates through all LLVM transformation unit tests
and extracts each function in each unit test into a standalone
module (i.e. a LLVM-IR file). The script also compiles each
LLVM-IR file to LLVM bitcode

All unit tests are stored in a single flat output directory
and a CSV is created that stores basic information for every file
'''

import git
import os
import re
from pathlib import Path


class UnitTestCollector:
  def __init__(self, git_root,
               debug = False,
               skip_dir={},
               skip_file={},
               skip_substr={}):

    self.debug             = debug
    self.skip_dir          = skip_dir
    self.skip_file         = skip_file
    self.skip_substr       = skip_substr
    self.llvm_dir          = f"{git_root}/FLUX/"
    self.llvm_bin          = f"{self.llvm_dir}/build/bin/"
    self.llvm_unittest_dir = f"{self.llvm_dir}/llvm/test/Transforms"
    self.output_dir        = f"{git_root}/unit-tests"
    self.llvm_extract      = f"{self.llvm_bin}/llvm-extract"
    self.llvm_as           = f"{self.llvm_bin}/llvm-as"
    self.output_ll_full    = f"{self.output_dir}/ll_files"
    self.output_ll_extract = f"{self.output_dir}/ll_functions"
    self.output_bc_full    = f"{self.output_dir}/bc_files"
    self.output_bc_extract = f"{self.output_dir}/bc_functions"
    self.unittest_csv      = f"{self.output_dir}/unittests.csv"

    if not os.path.exists(self.output_ll_full):
        os.mkdir(self.output_ll_full)
    if not os.path.exists(self.output_ll_extract):
        os.mkdir(self.output_ll_extract)
    if not os.path.exists(self.output_bc_full):
        os.mkdir(self.output_bc_full)
    if not os.path.exists(self.output_bc_extract):
        os.mkdir(self.output_bc_extract)

  def log(self, s):
      if self.debug:
          print(s)

  def format_special_identifier(self, func_name):
      special_chars = ['\\', '$']
      seq_pos_list = [idx for idx, ch in enumerate(func_name) if ch in special_chars]
      replace_map = {}
      for seq_start in seq_pos_list:
          seq_end = seq_start + 1
          while func_name[seq_end].isdigit() and seq_end - seq_start < 4:
              seq_end += 1
          seq_str = func_name[seq_start:seq_end]
          seq_str_formatted = f"$(printf '{seq_str}')"
          replace_map[seq_str] = seq_str_formatted
      for old_str, new_str in replace_map.items():
          func_name = func_name.replace(old_str, new_str)
      return func_name
          
  def get_function_name(self, line):
      if '(' not in line or '@' not in line:
          return None
      
      # start of identifier
      func_name = line[line.find('@')+1:].strip()
      
      # location to start searching for the start of
      # function arguments
      arg_loc_search = 0
      
      # if the identifier is wrapped in quotes, then
      # make the arg search start location equal to the
      # end of the matching quotations
      if func_name[0] == '"':
          arg_loc_search = func_name.find('"', 1)
          assert(arg_loc_search != -1)

      arg_start_loc = func_name.find('(', arg_loc_search)
      assert(arg_start_loc != -1)
      func_name = func_name[:arg_start_loc]

      return func_name.strip()

  def write_test_to_csv(self, test_id, ll_path, bc_path, file_path,
                        transform, unittest, function):
      with open(self.unittest_csv, "a") as f:
          f.write(f"{test_id},{ll_path},{bc_path},{file_path},{transform},{unittest},{function}\n")

  def expand_file(self, filename, output_file):
      self.log(f"File: {filename.path}")

      _, file_extension = os.path.splitext(filename)
      filename_noext = Path(filename).stem
      if file_extension != ".ll":
          self.log("    Skipping...")
          return

      # skip unit tests that contain certain substrings
      with open(filename) as fp:
          filestr = fp.read()
          for substr in self.skip_substr:
              if substr in filestr:
                  self.log("    Skipping on substr match...")
                  return

      # copy the full unit test and its assembled .bc to separate directories
      ll_full_out_path = f"{self.output_ll_full}/{output_file}{filename_noext}.ll"
      cp_cmd = f"cp {filename.path} {ll_full_out_path}"
      os.system(cp_cmd)
      bc_full_out_path = f"{self.output_bc_full}/{output_file}{filename_noext}.bc"
      llvm_as_cmd = f"{self.llvm_as} {ll_full_out_path} -o {bc_full_out_path}"
      ret_code = os.system(llvm_as_cmd)
      if ret_code != 0:
          self.log(f"    llvm-as failed to convert {filename.path} to a .bc file")

      # collect all functions names
      functions = []
      with open(filename) as fp:
          line = fp.readline()
          while line:
              if line.strip():
                  line_tokens = line.split()
                  if line_tokens[0] == "define":
                      func_name = self.get_function_name(line)
                      assert(func_name)

                      # identifier requirement for name values, taken
                      # from the LLVM language reference
                      if re.search("[-a-zA-Z$._]", func_name):
                          functions.append(func_name)
              line = fp.readline()

      for func in functions:
          # watch out for special characters in the identifier name
          if '\\' in func or '$' in func:
              func = self.format_special_identifier(func)

          ll_out_path = f"{self.output_ll_extract}/{output_file}{filename_noext}___{func}.ll"
          llvm_extract_cmd = f"{self.llvm_extract} -S -func {func} {filename.path} -o {ll_out_path}"
          ret_code = os.system(llvm_extract_cmd)
          if ret_code != 0:
              self.log(f"    llvm-extract failed to extract func {func} from file {filename.path}")
              continue

          bc_out_path = f"{self.output_bc_extract}/{output_file}{filename_noext}___{func}.bc"
          llvm_as_cmd = f"{self.llvm_as} {ll_out_path} -o {bc_out_path}"
          ret_code = os.system(llvm_as_cmd)
          if ret_code != 0:
              self.log(f"    llvm-as failed to convert {ll_out_path} to a .bc file")
              continue

          path_tokens = ll_out_path.split('/')
          self.write_test_to_csv(path_tokens[-1][:-3],
                                 ll_out_path,
                                 bc_out_path,
                                 ll_full_out_path,
                                 path_tokens[-1].split('___')[0],
                                 filename.name,
                                 func)

  def expand_dir(self, all_dir, output_file):
      for entry in os.scandir(all_dir):
          if entry.is_dir():
              cur_dir_name = os.path.basename(entry)
              if cur_dir_name in self.skip_dir:
                  continue
              self.expand_dir(entry, output_file + "___" + cur_dir_name)
          elif entry.is_file():
              cur_file_name = os.path.basename(entry)
              if cur_file_name in self.skip_file:
                  continue
              self.expand_file(entry, output_file + "___")

  def expand_all_dir(self):

      with open(self.unittest_csv, "w") as csv_file:
          csv_file.write("id,ll_path,bc_path,file_path,transform,unittest,function\n")

      for entry in os.scandir(self.llvm_unittest_dir):
          if entry.is_dir():
              cur_dir_name = os.path.basename(entry)
              if cur_dir_name in self.skip_dir:
                  continue
              self.expand_dir(entry, cur_dir_name)

def main():
    git_repo = git.Repo(os.getcwd(), search_parent_directories=True)
    git_root = git_repo.git.rev_parse("--show-toplevel")

    skip_dir = {
        "GCOVProfiling",
        "AArch64",
        "AMDGPU",
        "ARM",
        "Hexagon",
        "PowerPC",
        "RISCV",
        "WebAssembly",
        "Coroutines"
    }

    skip_file = {
        "vscale_cmp.ll",
        "distribution-remarks-missed.ll",
        "optnone.ll",
        "freeze_callbr_use_after_phi.ll"
    }

    skip_substr = {
        "\"target-cpu\"=\"sm",
        "\"target-cpu\"=\"cortex",
        "\"target-cpu\"=\"v13",
        "\"target-cpu\"=\"z13",
        "\"target-cpu\"=\"arm7tdmi",
        "\"target-cpu\"=\"ppc64",
        "\"target-cpu\"=\"gfx908",
        "\"target-cpu\"=\"nocona",
        "\"target-cpu\"=\"pentium-m",
        "systemz",
        "vscale"
    }

    U = UnitTestCollector(git_root,
                          debug=True,
                          skip_dir=skip_dir,
                          skip_file=skip_file,
                          skip_substr=skip_substr)
    U.expand_all_dir()

if __name__ == "__main__":
    main()
