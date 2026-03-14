// ASTHandler.h
#ifndef AST_HANDLER_H
#define AST_HANDLER_H

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include "OperatorPos.h"
#include <R.h>
#include <Rinternals.h>

// Class to Handle AST Traversal and Operator Gathering
class ASTHandler
{
public:
    ASTHandler() = default;
    ~ASTHandler() = default;

    struct ParseToken
    {
        int line1;
        int col1;
        int line2;
        int col2;
    };

    // Gather all operators in the AST
    std::vector<OperatorPos> gatherOperators(SEXP expr, SEXP src_ref, bool is_inside_block,
                                             SEXP parse_data = R_NilValue);

private:
    int _start_line;
    int _start_col;
    int _end_line;
    int _end_col;
    std::string _file_path;
    bool _is_inside_block;
    std::unordered_map<std::string, std::vector<ParseToken>> _tokens_by_symbol;
    std::unordered_map<std::string, size_t> _token_cursor;

    void buildParseTokenIndex(SEXP parse_data);
    bool assignOperatorTokenRange(const std::string &symbol,
                                  int node_start_line,
                                  int node_start_col,
                                  int node_end_line,
                                  int node_end_col,
                                  bool prefer_node_span,
                                  int &out_start_line,
                                  int &out_start_col,
                                  int &out_end_line,
                                  int &out_end_col);
    // Recursive helper function
    void gatherOperatorsRecursive(SEXP expr, std::vector<int> path, std::vector<OperatorPos> &ops);

    bool isDeletable(SEXP expr);
};

#endif // AST_HANDLER_H