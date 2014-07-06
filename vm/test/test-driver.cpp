// Copyright 2014 Jay Conrod. All rights reserved.

// This file is part of CodeSwitch. Use of this source code is governed by
// the 3-clause BSD license that can be found in the LICENSE.txt file.


#include "test.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "error.h"

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


int main(int argc, char* argv[]) {
  processArguments(argc, argv);

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
    pid_t pid;
    if (shouldFork)
      pid = fork();
    else
      pid = 0;

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
      if (shouldFork)
        _exit(0);
    }

    int status;
    if (shouldFork)
      waitpid(pid, &status, 0);
    else
      status = 0;
    char code = ' ';
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
