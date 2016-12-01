#include <string>
#include <Rcpp.h>
#include <locale>

using namespace Rcpp;

// [[Rcpp::export]]
List parse_array_definition(std::string x) {
  enum states {
    ARG,
    END_ARG,
    END_ENTRY,
    END_ARRAY
  };

  int brace_level = 0;

  std::string word;

  states state = ARG;
  List out;
  std::vector<std::string> args;

  for(std::string::const_iterator i = x.begin(); i != x.end(); ++i) {
    switch(state) {
      case ARG:
        {
          if (*i == '{') {
            Rcout << "{" << brace_level << "\n";
            ++brace_level;
            break;
          }
          if (*i == '}') {
            --brace_level;
            Rcout << "}" << brace_level <<"\n";
            if (brace_level == 1) {
              state = END_ENTRY;
              break;
            }
            if (brace_level == 0) {
              state = END_ARRAY;
              break;
            }
            break;
          }
          if (*i == ',') {
            state = END_ARG;
            break;
          }
          if (isspace(*i)) {
            word.clear();
          }
          if (isalnum(*i) || *i == '_' || *i == '.') {
            word.push_back(*i);
          }
          break;
        }
      case END_ARG:
        args.push_back(word);
        word.clear();
        state = ARG;
        break;
      case END_ENTRY:
        args.push_back(word);
        out.push_back(args);
        args.clear();
        word.clear();
        state = ARG;
        break;
      case END_ARRAY:
        return out;
    }
  }
  stop("Invalid input");
  return out;
}

// Find the end line of the function
// This returns the line number the end is on

// [[Rcpp::export]]
int find_function_end(const CharacterVector& x, int start = 0) {
  std::string line = as<std::string>(x[0]);
  int i = start;
  // Find first opening brace
  for(;i < line.size() && line[i] != '{';++i) { }

  int brace_level = 0;
  // Find closing brace
  for(;i < line.size();++i) {
    // TODO: handle block comments /* */
    if (line[i] == '/') {
      if (i + 1 < line.size() && line[i + 1] == '/') {
        continue;
      }
    }
    if (line[i] == '{') {
      ++brace_level;
    }
    if (line[i] == '}') {
      --brace_level;
    }
    if (brace_level == 0) {
      // R is 1 based, so add 1
      return i + 1;
    }
  }
  return NA_INTEGER;
}
