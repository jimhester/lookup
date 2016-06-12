#import <string>
#import <Rcpp.h>
#import <locale>

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

  std::string::const_iterator i = x.begin();
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
IntegerVector parse_c_function(CharacterVector x) {
  int brace_level = 0;
  int out = 0;
  while (out < x.length()) {
    std::string line = as<std::string>(x[out]);
    std::string::const_iterator i = line.begin();

    // Find first brace
    if (brace_level == 0) {
      while(*i != '{' && i != line.end()) {
        ++i;
      }
    }

    for(;i != line.end(); ++i) {
      if (*i == '{') {
        ++brace_level;
      }
      if (*i == '}') {
        --brace_level;
      }
      if (brace_level == 0) {
        return IntegerVector(1, out + 1);
      }
    }
    ++out;
  }
  return IntegerVector(1, NA_INTEGER);
}
