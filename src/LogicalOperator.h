#ifndef LOGICAL_OPERATOR_H
#define LOGICAL_OPERATOR_H

#include "ReplacementOperator.h"

class LogicalOperator : public ReplacementOperator {
public:
    LogicalOperator(SEXP symbol) : ReplacementOperator(symbol){}
    virtual ~LogicalOperator() = default;

    virtual std::string getType() const override{
        return "LogicalOperator";
    }
};

#endif