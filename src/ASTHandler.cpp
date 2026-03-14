// ASTHandler.cpp

#include <map>
#include <functional>
#include <iostream>
#include <algorithm>
#include <cstring>
#include <unordered_set>
#include "ASTHandler.h"
#include "PlusOperator.h"
#include "MinusOperator.h"
#include "DivideOperator.h"
#include "MultiplyOperator.h"
#include "EqualOperator.h"
#include "NotEqualOperator.h"
#include "LessThanOperator.h"
#include "MoreThanOperator.h"
#include "LessThanOrEqualOperator.h"
#include "MoreThanOrEqualOperator.h"
#include "AndOperator.h"
#include "OrOperator.h"
#include "LogicalOrOperator.h"
#include "LogicalAndOperator.h"
#include "DeleteOperator.h"

static struct CachedSyms
{
    SEXP s_lbrace = Rf_install("{");
    SEXP s_rbrace = Rf_install("}");
    SEXP s_plus = Rf_install("+");
    SEXP s_minus = Rf_install("-");
    SEXP s_mul = Rf_install("*");
    SEXP s_div = Rf_install("/");
    SEXP s_eq = Rf_install("==");
    SEXP s_neq = Rf_install("!=");
    SEXP s_lt = Rf_install("<");
    SEXP s_gt = Rf_install(">");
    SEXP s_le = Rf_install("<=");
    SEXP s_ge = Rf_install(">=");
    SEXP s_and = Rf_install("&");
    SEXP s_or = Rf_install("|");
    SEXP s_land = Rf_install("&&");
    SEXP s_lor = Rf_install("||");
    SEXP s_srcref = Rf_install("srcref");
    SEXP s_mutinfo = Rf_install("mutation_info");
} SYM;

static bool isKnownOperatorSymbol(const std::string &text)
{
    static const std::unordered_set<std::string> symbols = {
        "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&", "|", "&&", "||"};
    return symbols.find(text) != symbols.end();
}

static bool positionLE(int line_a, int col_a, int line_b, int col_b)
{
    return (line_a < line_b) || (line_a == line_b && col_a <= col_b);
}

static bool tokenWithinSpan(const ASTHandler::ParseToken &token,
                            int start_line,
                            int start_col,
                            int end_line,
                            int end_col)
{
    const bool starts_after_or_at = positionLE(start_line, start_col, token.line1, token.col1);
    const bool ends_before_or_at = positionLE(token.line2, token.col2, end_line, end_col);
    return starts_after_or_at && ends_before_or_at;
}

void ASTHandler::buildParseTokenIndex(SEXP parse_data)
{
    _tokens_by_symbol.clear();
    _token_cursor.clear();

    if (parse_data == R_NilValue || TYPEOF(parse_data) != VECSXP)
        return;

    SEXP col_names = Rf_getAttrib(parse_data, R_NamesSymbol);
    if (TYPEOF(col_names) != STRSXP)
        return;

    const int n_cols = Rf_length(parse_data);
    int text_idx = -1;
    int line1_idx = -1;
    int col1_idx = -1;
    int line2_idx = -1;
    int col2_idx = -1;
    int terminal_idx = -1;

    for (int i = 0; i < n_cols; ++i)
    {
        const char *name = CHAR(STRING_ELT(col_names, i));
        if (std::strcmp(name, "text") == 0)
            text_idx = i;
        else if (std::strcmp(name, "line1") == 0)
            line1_idx = i;
        else if (std::strcmp(name, "col1") == 0)
            col1_idx = i;
        else if (std::strcmp(name, "line2") == 0)
            line2_idx = i;
        else if (std::strcmp(name, "col2") == 0)
            col2_idx = i;
        else if (std::strcmp(name, "terminal") == 0)
            terminal_idx = i;
    }

    if (text_idx < 0 || line1_idx < 0 || col1_idx < 0 || line2_idx < 0 || col2_idx < 0)
        return;

    SEXP text_col = VECTOR_ELT(parse_data, text_idx);
    SEXP line1_col = VECTOR_ELT(parse_data, line1_idx);
    SEXP col1_col = VECTOR_ELT(parse_data, col1_idx);
    SEXP line2_col = VECTOR_ELT(parse_data, line2_idx);
    SEXP col2_col = VECTOR_ELT(parse_data, col2_idx);
    SEXP terminal_col = (terminal_idx >= 0) ? VECTOR_ELT(parse_data, terminal_idx) : R_NilValue;

    if (TYPEOF(text_col) != STRSXP || TYPEOF(line1_col) != INTSXP || TYPEOF(col1_col) != INTSXP ||
        TYPEOF(line2_col) != INTSXP || TYPEOF(col2_col) != INTSXP)
    {
        return;
    }

    const int n_rows = Rf_length(text_col);
    for (int i = 0; i < n_rows; ++i)
    {
        if (terminal_col != R_NilValue)
        {
            if (TYPEOF(terminal_col) == LGLSXP && LOGICAL(terminal_col)[i] != TRUE)
                continue;
            if (TYPEOF(terminal_col) == INTSXP && INTEGER(terminal_col)[i] != 1)
                continue;
        }

        if (STRING_ELT(text_col, i) == NA_STRING)
            continue;

        const std::string text = CHAR(STRING_ELT(text_col, i));
        if (!isKnownOperatorSymbol(text))
            continue;

        ParseToken token{
            INTEGER(line1_col)[i],
            INTEGER(col1_col)[i],
            INTEGER(line2_col)[i],
            INTEGER(col2_col)[i]};

        // Keep only tokens that belong to the current top-level expression span.
        if (!tokenWithinSpan(token, _start_line, _start_col, _end_line, _end_col))
            continue;

        _tokens_by_symbol[text].push_back(token);
    }

    for (auto &entry : _tokens_by_symbol)
    {
        auto &tokens = entry.second;
        std::sort(tokens.begin(), tokens.end(), [](const ParseToken &a, const ParseToken &b)
                  {
                      if (a.line1 != b.line1)
                          return a.line1 < b.line1;
                      if (a.col1 != b.col1)
                          return a.col1 < b.col1;
                      if (a.line2 != b.line2)
                          return a.line2 < b.line2;
                      return a.col2 < b.col2;
                  });
        _token_cursor[entry.first] = 0;
    }
}

bool ASTHandler::assignOperatorTokenRange(const std::string &symbol,
                                          int node_start_line,
                                          int node_start_col,
                                          int node_end_line,
                                          int node_end_col,
                                          bool prefer_node_span,
                                          int &out_start_line,
                                          int &out_start_col,
                                          int &out_end_line,
                                          int &out_end_col)
{
    auto it = _tokens_by_symbol.find(symbol);
    if (it == _tokens_by_symbol.end())
        return false;

    auto cur_it = _token_cursor.find(symbol);
    size_t cursor = (cur_it == _token_cursor.end()) ? 0 : cur_it->second;
    auto &tokens = it->second;

    auto claim = [&](size_t idx)
    {
        const ParseToken &tok = tokens[idx];
        out_start_line = tok.line1;
        out_start_col = tok.col1;
        out_end_line = tok.line2;
        out_end_col = tok.col2;
        _token_cursor[symbol] = idx + 1;
    };

    if (prefer_node_span)
    {
        for (size_t i = cursor; i < tokens.size(); ++i)
        {
            if (tokenWithinSpan(tokens[i], node_start_line, node_start_col, node_end_line, node_end_col))
            {
                claim(i);
                return true;
            }
        }
    }

    for (size_t i = cursor; i < tokens.size(); ++i)
    {
        if (tokenWithinSpan(tokens[i], _start_line, _start_col, _end_line, _end_col))
        {
            claim(i);
            return true;
        }
    }

    return false;
}

bool ASTHandler::isDeletable(SEXP expr)
{
    if (!_is_inside_block)
        return false;
    if (TYPEOF(expr) != LANGSXP)
        return true;

    SEXP head = CAR(expr);
    if (TYPEOF(head) == SYMSXP && (head == SYM.s_lbrace || head == SYM.s_rbrace))
        return false;
    return true;
}

std::vector<OperatorPos> ASTHandler::gatherOperators(SEXP expr, SEXP src_ref,
                                                     bool is_inside_block,
                                                     SEXP parse_data)
{
    if (TYPEOF(src_ref) != INTSXP || LENGTH(src_ref) < 4)
        Rf_error("src_ref must be an integer vector of length 4");

    const int *p = INTEGER(src_ref);
    _start_line = p[0];
    _start_col = p[1];
    _end_line = p[2];
    _end_col = p[3];
    _file_path.clear();

    SEXP srcfile = Rf_getAttrib(src_ref, Rf_install("srcfile"));
    if (srcfile != R_NilValue)
    {
        SEXP filename = Rf_getAttrib(srcfile, Rf_install("filename"));
        if (TYPEOF(filename) == STRSXP && LENGTH(filename) > 0)
        {
            _file_path = CHAR(STRING_ELT(filename, 0));
        }
        else if (TYPEOF(srcfile) == ENVSXP)
        {
            SEXP env_name = Rf_findVarInFrame(srcfile, Rf_install("filename"));
            if (env_name != R_UnboundValue && TYPEOF(env_name) == STRSXP && LENGTH(env_name) > 0)
            {
                _file_path = CHAR(STRING_ELT(env_name, 0));
            }
        }
    }

    _is_inside_block = is_inside_block;
    buildParseTokenIndex(parse_data);

    std::vector<OperatorPos> ops;
    std::vector<int> path;
    gatherOperatorsRecursive(expr, path, ops);
    return ops;
}

void ASTHandler::gatherOperatorsRecursive(SEXP expr, std::vector<int> path,
                                          std::vector<OperatorPos> &ops)
{
    if (TYPEOF(expr) != LANGSXP)
        return;

    int node_start_line = _start_line;
    int node_start_col = _start_col;
    int node_end_line = _end_line;
    int node_end_col = _end_col;

    SEXP node_srcref = Rf_getAttrib(expr, SYM.s_srcref);
    const bool has_node_srcref = (TYPEOF(node_srcref) == INTSXP && LENGTH(node_srcref) >= 4);
    if (TYPEOF(node_srcref) == INTSXP && LENGTH(node_srcref) >= 4)
    {
        const int *rp = INTEGER(node_srcref);
        node_start_line = rp[0];
        node_start_col = rp[1];
        node_end_line = rp[2];
        node_end_col = rp[3];
    }

    SEXP fun = CAR(expr);

    /* operator map – keys are the cached symbols */
    static const std::map<SEXP, std::function<std::unique_ptr<Operator>()>> op_map = {
        {SYM.s_plus, []
         { return std::make_unique<PlusOperator>(); }},
        {SYM.s_minus, []
         { return std::make_unique<MinusOperator>(); }},
        {SYM.s_mul, []
         { return std::make_unique<MultiplyOperator>(); }},
        {SYM.s_div, []
         { return std::make_unique<DivideOperator>(); }},
        {SYM.s_eq, []
         { return std::make_unique<EqualOperator>(); }},
        {SYM.s_neq, []
         { return std::make_unique<NotEqualOperator>(); }},
        {SYM.s_lt, []
         { return std::make_unique<LessThanOperator>(); }},
        {SYM.s_gt, []
         { return std::make_unique<MoreThanOperator>(); }},
        {SYM.s_le, []
         { return std::make_unique<LessThanOrEqualOperator>(); }},
        {SYM.s_ge, []
         { return std::make_unique<MoreThanOrEqualOperator>(); }},
        {SYM.s_and, []
         { return std::make_unique<AndOperator>(); }},
        {SYM.s_or, []
         { return std::make_unique<OrOperator>(); }},
        {SYM.s_land, []
         { return std::make_unique<LogicalAndOperator>(); }},
        {SYM.s_lor, []
         { return std::make_unique<LogicalOrOperator>(); }}};

    if (auto it = op_map.find(fun); it != op_map.end())
    {
        int op_start_line = node_start_line;
        int op_start_col = node_start_col;
        int op_end_line = node_end_line;
        int op_end_col = node_end_col;

        if (TYPEOF(fun) == SYMSXP)
        {
            const std::string symbol = CHAR(PRINTNAME(fun));
            assignOperatorTokenRange(symbol,
                                     node_start_line,
                                     node_start_col,
                                     node_end_line,
                                     node_end_col,
                                     has_node_srcref,
                                     op_start_line,
                                     op_start_col,
                                     op_end_line,
                                     op_end_col);
        }

        auto op = it->second();
        ops.push_back({path, std::move(op), op_start_line, op_start_col,
                       op_end_line, op_end_col, fun, _file_path});
    }

    const bool is_block = (fun == SYM.s_lbrace);

    // add delete operator if allowed
    if (isDeletable(expr))
    {
        auto del = std::make_unique<DeleteOperator>(expr);
        ops.push_back({path, std::move(del), node_start_line, node_start_col,
                       node_end_line, node_end_col, fun, _file_path});
    }

    // recurse into children (block or not)
    int idx = 0;
    for (SEXP next = CDR(expr); next != R_NilValue; next = CDR(next), ++idx)
    {
        auto child_path = path;
        child_path.push_back(idx);
        gatherOperatorsRecursive(CAR(next), child_path, ops);
    }
}
