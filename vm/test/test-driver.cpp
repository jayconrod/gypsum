// Copyright 2014,2016 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cerrno>
#include <cstring>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "error.h"

#ifndef TEST_PROGRAMS
  #define TEST_PROGRAMS ""
#endif

using namespace std;

static string nameFilter;
static bool shouldFork = true;
static bool verbose = false;

void usage(const char* name) {
  cerr << "usage: " << name << ": [-nofork] [-verbose] [name]" << endl;
  _exit(0);
}


void processArguments(int argc, char* argv[]) {
  for (int i = 1; i < argc; i++) {
    string arg(argv[i]);
    if (!arg.empty() && arg[0] == '-') {
      if (arg == "-nofork") {
        shouldFork = false;
      } else if (arg == "-verbose") {
        verbose = true;
      } else {
        usage(argv[0]);
      }
    } else {
      if (nameFilter.empty()) {
        nameFilter = arg;
      } else {
        usage(argv[0]);
      }
    }
  }
}


static vector<string> split(const string& str, char delim) {
  vector<string> pieces;
  size_t begin = 0;
  auto end = str.find(delim);
  while (end != string::npos) {
    auto len = end - begin;
    pieces.push_back(str.substr(begin, len));
    begin = end + 1;
    end = str.find(delim, begin);
  }
  auto len = str.size() - begin;
  pieces.push_back(str.substr(begin, len));
  return pieces;
}


static string basename(const string& path) {
  auto pos = path.find_last_of('/');
  if (pos == string::npos) {
    return path;
  } else if (pos == path.length() - 1) {
    return ".";
  } else {
    return path.substr(pos + 1, path.length() - (pos + 1));
  }
}


class SeparateTest: public TestBase {
 public:
  explicit SeparateTest(const string& executablePath)
      : TestBase(basename(executablePath).c_str()),
        executablePath_(executablePath) { }

  virtual void test() {
    auto pid = fork();
    if (pid == 0) {
      execl(executablePath_.c_str(), executablePath_.c_str(), nullptr);
      cerr << '\n' << executablePath_ << ": error: " << strerror(errno) << endl;
      _exit(4);
    } else {
      int status;
      waitpid(pid, &status, 0);
      if (WIFEXITED(status)) {
        _exit(WEXITSTATUS(status));
      } else {
        _exit(5);
      }
    }
  }

 private:
  string executablePath_;
};


int main(int argc, char* argv[]) {
  processArguments(argc, argv);

  if (shouldFork) {
    for (auto path : split(TEST_PROGRAMS, ' ')) {
      if (!path.empty())
        new SeparateTest(path);
    }
  } else {
    cerr << "skipping " << split(TEST_PROGRAMS, ' ').size()
         << " tests that require fork" << endl;
  }

  int testCount = 0;
  int passCount = 0;
  int failCount = 0;
  int errorCount = 0;
  for (TestBase* test = TestBase::head(); test != NULL; test = test->next()) {
    if (!nameFilter.empty() && nameFilter != test->name()) {
      continue;
    }
    testCount++;
    if (verbose) {
      cout << test->name() << "...";
      cout.flush();
    }
    char code = ' ';
    if (shouldFork) {
      pid_t pid = fork();
      if (pid == 0) {
        // Run test in child process.
        try {
          test->test();
        } catch (TestException& exn) {
          cerr << '\n' << test->name() << ": test error: " << exn.message() << endl;
          _exit(1);
        } catch (codeswitch::internal::Error& exn) {
          cerr << '\n' << test->name() << ": internal error: " << exn.message() << endl;
          _exit(2);
        } catch (...) {
          cerr << '\n' << test->name()
               << ": error: unknown exception" << endl;
          _exit(3);
        }
        _exit(0);
      }

      int status;
      waitpid(pid, &status, 0);
      if (WIFEXITED(status) && WEXITSTATUS(status) == 0) {
        passCount++;
        code = '.';
      } else if (WIFEXITED(status) && WEXITSTATUS(status) == 1) {
        failCount++;
        code = 'F';
      } else {
        errorCount++;
        code = 'E';
        if (WIFSIGNALED(status)) {
          cerr << '\n' << test->name()
               << ": error: terminated by signal " << WTERMSIG(status) << endl;
        } else {
          cerr << '\n' << test->name()
               << ": unknown error code: " << status << endl;
        }
      }
    } else {
      // Run test directly.
      try {
        test->test();
        code = '.';
        passCount++;
      } catch (TestException& exn) {
        cerr << '\n' << test->name() << ": test error: " << exn.message() << endl;
        failCount++;
        code = 'F';
      } catch (codeswitch::internal::Error& exn) {
        cerr << '\n' << test->name() << ": internal error: " << exn.message() << endl;
        errorCount++;
        code = 'E';
      } catch (...) {
        cerr << '\n' << test->name()
             << ": error: unknown exception" << endl;
        errorCount++;
        code = 'E';
      }
    }

    if (!verbose) {
      cout << code;
      cout.flush();
    } else {
      if (code == '.') {
        cout << "ok" << endl;
      } else if (code == 'F') {
        cout << "fail" << endl;
      } else {
        cout << "error" << endl;
      }
    }
  }

  cout << '\n'
       << setw(14) << "Tests Passed: " << setw(5) << passCount << '\n'
       << setw(14) << "Tests Failed: " << setw(5) << failCount << '\n'
       << setw(14) << "Test Errors: " << setw(5) << errorCount << '\n'
       << setw(14) << "Total Tests: " << setw(5) << testCount << endl;
  if (testCount == passCount)
    return 0;
  else
    return 1;
}
