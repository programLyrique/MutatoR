#ifndef COMPARISON_OPERATOR_H
#define COMPARISON_OPERATOR_H

#include "ReplacementOperator.h"

class ComparisonOperator : public ReplacementOperator {
public:
    ComparisonOperator(SEXP symbol) : ReplacementOperator(symbol){}
    virtual ~ComparisonOperator() = default;

    virtual std::string getType() const override{
        return "ComparisonOperator";
    }
};

#endif