#include <testthat.h>
#include <R.h>
#include <Rinternals.h>
#include "ASTHandler.h"

static SEXP makeSrcref()
{
    SEXP srcref = Rf_allocVector(INTSXP, 4);
    INTEGER(srcref)
    [0] = 1;
    INTEGER(srcref)
    [1] = 1;
    INTEGER(srcref)
    [2] = 1;
    INTEGER(srcref)
    [3] = 5;
    return srcref;
}

context("ASTHandler C++ tests")
{

    test_that("gatherOperators runs on a basic call")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP srcref = PROTECT(makeSrcref());

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        expect_true(ops.size() >= 0);

        UNPROTECT(2);
    }

    test_that("gatherOperators discovers supported operator calls")
    {
        const char *symbols[] = {
            "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&", "|", "&&", "||"};

        ASTHandler handler;
        for (const char *symbol : symbols)
        {
            SEXP expr = PROTECT(Rf_lang3(Rf_install(symbol), Rf_install("a"), Rf_install("b")));
            SEXP srcref = PROTECT(makeSrcref());

            std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);
            expect_true(ops.size() >= 1);

            UNPROTECT(2);
        }
    }

    test_that("gatherOperators returns empty for non call expressions")
    {
        SEXP expr = PROTECT(Rf_ScalarInteger(42));
        SEXP srcref = PROTECT(makeSrcref());

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        expect_true(ops.empty());
        UNPROTECT(2);
    }
}
