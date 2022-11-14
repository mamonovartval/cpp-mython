#include "statement.h"

#include <iostream>
#include <sstream>
#include <stdexcept>

using namespace std;

namespace ast {

    using runtime::Closure;
    using runtime::Context;
    using runtime::ObjectHolder;

    namespace {
        const string ADD_METHOD = "__add__"s;
        const string INIT_METHOD = "__init__"s;
    }  // namespace

    Assignment::Assignment(std::string var, std::unique_ptr<Statement> rv)
        :var_(std::move(var))
        , rv_(std::move(rv)) {

    }

    ObjectHolder Assignment::Execute(Closure& closure, Context& context) {
        closure[var_] = std::move(rv_->Execute(closure, context));

        return closure.at(var_);
    }

    VariableValue::VariableValue(const std::string& var_name) {
        dotted_ids_.push_back(var_name);
    }

    VariableValue::VariableValue(std::vector<std::string> dotted_ids)
        :dotted_ids_(std::move(dotted_ids)) {
    }

    ObjectHolder VariableValue::Execute(Closure& closure, Context& /*context*/) {
        if (dotted_ids_.size() == 1) {
            const auto it = closure.find(dotted_ids_[0]);
            if (it != closure.end()) {
                return  it->second;
            }
            else {
                throw std::runtime_error("Unknown name"s);
            }
        }

        ObjectHolder obj = closure.at(dotted_ids_[0]);
        auto* class_ptr = obj.TryAs<runtime::ClassInstance>();
        if (!class_ptr) {
            throw std::runtime_error("Accessing a non-existent field"s);
        }
        for (size_t i = 1; i + 1 < dotted_ids_.size(); ++i) {

            const auto item = class_ptr->Fields().find(dotted_ids_[i]);
            if (item == class_ptr->Fields().end())
                throw std::runtime_error("Accessing a non-existent field"s);
            obj = item->second;

        }
        const auto& fields = obj.TryAs<runtime::ClassInstance>()->Fields();
        if (const auto it = fields.find(dotted_ids_.back()); it != fields.end()) {
            return it->second;
        }
        throw std::runtime_error("Unknown name"s);
    }

    unique_ptr<Print> Print::Variable(const std::string& name) {
        return std::make_unique<Print>(std::make_unique<VariableValue>(name));
    }

    Print::Print(unique_ptr<Statement> argument) {
        args_.push_back(std::move(argument));
    }

    Print::Print(vector<unique_ptr<Statement>> args)
        :args_(std::move(args)) {
    }

    ObjectHolder Print::Execute(Closure& closure, Context& context) {
        bool first = true;
        for (const auto& arg : args_) {
            const ObjectHolder value = arg->Execute(closure, context);
            if (!first) {
                context.GetOutputStream() << ' ';
            }
            if (value) {
                value->Print(context.GetOutputStream(), context);
            }
            else {
                context.GetOutputStream() << "None";
            }
            if (args_.size() == 1)
                break;
            first = false;
        }
        context.GetOutputStream() << "\n";
        return ObjectHolder::None();
    }

    MethodCall::MethodCall(std::unique_ptr<Statement> object, std::string method,
        std::vector<std::unique_ptr<Statement>> args)
        :object_(std::move(object))
        , method_(std::move(method))
        , args_(std::move(args)) {
    }

    ObjectHolder MethodCall::Execute(Closure& closure, Context& context) {
        std::vector<runtime::ObjectHolder> object_args;
        for (const auto& arg : args_) {
            object_args.push_back(arg->Execute(closure, context));
        }

        auto* cls = object_->Execute(closure, context).TryAs<runtime::ClassInstance>();
        if (cls) {
           return cls->Call(method_, object_args, context);
        }
        throw std::runtime_error("Accessing a non-existent field");        
    }

    ObjectHolder Stringify::Execute(Closure& closure, Context& context) {
        const auto arg = GetArg()->Execute(closure, context);
        std::ostringstream out;
        if (const auto ptr = arg.TryAs<runtime::Number>()) {
            ptr->Print(out, context);
            return ObjectHolder::Own(runtime::String(out.str()));
        }
        else if (const auto ptr = arg.TryAs<runtime::String>()) {
            ptr->Print(out, context);
            return ObjectHolder::Own(runtime::String(out.str()));
        }
        else if (const auto ptr = arg.TryAs<runtime::Bool>()) {
            ptr->Print(out, context);
            return ObjectHolder::Own(runtime::String(out.str()));
        }
        else if (const auto ptr = arg.TryAs<runtime::ClassInstance>()) {

            ptr->Print(out, context);
            return ObjectHolder::Own(runtime::String(out.str()));
        }
        else {
            return ObjectHolder::Own(runtime::String("None"));
        }
    }

    ObjectHolder Add::Execute(Closure& closure, Context& context) {
        if (!GetRhs() || !GetLhs()) {
            throw std::runtime_error("Null operands are not supported"s);
        }
        // getting objects
        auto obj_lhs = GetLhs()->Execute(closure, context);
        auto obj_rhs = GetRhs()->Execute(closure, context);
        // getting pointers to Number objects
        auto ptr_lhs_n = obj_lhs.TryAs<runtime::Number>();
        auto ptr_rhs_n = obj_rhs.TryAs<runtime::Number>();
        // verify pointers on nullptr
        if (ptr_lhs_n && ptr_rhs_n) {
            auto l_val = ptr_lhs_n->GetValue();
            auto r_val = ptr_rhs_n->GetValue();

            return ObjectHolder::Own(runtime::Number{ l_val + r_val });
        }
        // gettting pointers to String objects
        auto ptr_lhs_s = obj_lhs.TryAs<runtime::String>();
        auto ptr_rhs_s = obj_rhs.TryAs<runtime::String>();
        // verify pointers on nullptr
        if (ptr_lhs_s && ptr_rhs_s) {
            auto l_val = ptr_lhs_s->GetValue();
            auto r_val = ptr_rhs_s->GetValue();

            return ObjectHolder::Own(runtime::String{ l_val + r_val });
        }
        // getting pointers to ClassInstance objects
        auto ptr_lhs_class_inst = obj_lhs.TryAs<runtime::ClassInstance>();
        // verify pointers on nullptr
        if (ptr_lhs_class_inst) {
            constexpr int counter_args = 1;

            if (ptr_lhs_class_inst->HasMethod(ADD_METHOD, counter_args)) {
                return ptr_lhs_class_inst->Call(ADD_METHOD, { obj_rhs }, context);
            }
        }

        throw std::runtime_error("Add operands are illegal"s);
    }

    ObjectHolder Sub::Execute(Closure& closure, Context& context) {
        if (!GetRhs() || !GetLhs()) {
            throw std::runtime_error("Null operands are not supported"s);
        }
        // getting objects
        auto obj_lhs = GetLhs()->Execute(closure, context);
        auto obj_rhs = GetRhs()->Execute(closure, context);
        // getting pointers to Number objects
        auto ptr_lhs_n = obj_lhs.TryAs<runtime::Number>();
        auto ptr_rhs_n = obj_rhs.TryAs<runtime::Number>();
        // verify pointers on nullptr
        if (ptr_lhs_n && ptr_rhs_n) {
            auto l_num = ptr_lhs_n->GetValue();
            auto r_num = ptr_rhs_n->GetValue();

            return ObjectHolder::Own(runtime::Number{ l_num - r_num });
        }

        throw std::runtime_error("Substrict operands are illegal"s);
    }

    ObjectHolder Mult::Execute(Closure& closure, Context& context) {
        if (!GetRhs() || !GetLhs()) {
            throw std::runtime_error("Null operands are not supported"s);
        }
        // getting objects
        auto obj_lhs = GetLhs()->Execute(closure, context);
        auto obj_rhs = GetRhs()->Execute(closure, context);
        // getting pointers to Number objects
        auto ptr_lhs_n = obj_lhs.TryAs<runtime::Number>();
        auto ptr_rhs_n = obj_rhs.TryAs<runtime::Number>();
        // verify pointers on nullptr
        if (ptr_lhs_n && ptr_rhs_n) {
            auto l_num = ptr_lhs_n->GetValue();
            auto r_num = ptr_rhs_n->GetValue();

            return ObjectHolder::Own(runtime::Number{ l_num * r_num });
        }

        throw std::runtime_error("Multiply operands are illegal"s);
    }

    ObjectHolder Div::Execute(Closure& closure, Context& context) {
        if (!GetRhs() || !GetLhs()) {
            throw std::runtime_error("Null operands are not supported"s);
        }
        // getting objects
        auto obj_lhs = GetLhs()->Execute(closure, context);
        auto obj_rhs = GetRhs()->Execute(closure, context);
        // getting pointers to Number objects
        auto ptr_lhs_n = obj_lhs.TryAs<runtime::Number>();
        auto ptr_rhs_n = obj_rhs.TryAs<runtime::Number>();
        // verify pointers on nullptr
        if (ptr_lhs_n && ptr_rhs_n) {
            auto l_num = ptr_lhs_n->GetValue();
            auto r_num = ptr_rhs_n->GetValue();

            if (r_num == 0) {
                throw std::runtime_error("Division by zero"s);
            }

            return ObjectHolder::Own(runtime::Number{ l_num / r_num });
        }

        throw std::runtime_error("Division operands are illegal"s);
    }

    void Compound::AddStatement(std::unique_ptr<Statement> stmt) {
        statements_.push_back(std::move(stmt));
    }

    ObjectHolder Compound::Execute(Closure& closure, Context& context) {
        for (const auto& statement : statements_) {
            statement->Execute(closure, context);
        }

        return {};
    }

    ObjectHolder Return::Execute(Closure& closure, Context& context) {
        throw statement_->Execute(closure, context);
    }

    ClassDefinition::ClassDefinition(ObjectHolder cls)
        : cls_(std::move(cls)) {
    }

    ObjectHolder ClassDefinition::Execute(Closure& closure, Context& /*context*/) {
        closure[cls_.TryAs<runtime::Class>()->GetName()] = cls_;
        return cls_;
    }

    FieldAssignment::FieldAssignment(VariableValue object, std::string field_name,
        std::unique_ptr<Statement> rv)
        :object_(std::move(object))
        , field_name_(std::move(field_name))
        , rv_(std::move(rv)) {
    }

    ObjectHolder FieldAssignment::Execute(Closure& closure, Context& context) {

        auto* cls = object_.Execute(closure, context).TryAs<runtime::ClassInstance>();

        if (cls) {
            auto& fields = cls->Fields();

            fields[field_name_] = rv_->Execute(closure, context);
            return fields[field_name_];
        }
        throw std::runtime_error("Attempting to access a non-instance class field");
    }

    IfElse::IfElse(std::unique_ptr<Statement> condition, std::unique_ptr<Statement> if_body,
        std::unique_ptr<Statement> else_body)
        : condition_(std::move(condition))
        , if_body_(std::move(if_body))
        , else_body_(std::move(else_body)) {
    }

    ObjectHolder IfElse::Execute(Closure& closure, Context& context) {
        auto bool_condition = condition_->Execute(closure, context);

        if (runtime::IsTrue(bool_condition)) {
            return if_body_->Execute(closure, context);
        }
        else if (else_body_) { 
            return else_body_->Execute(closure, context);
        }
        else {
            return {};
        }
    }

    ObjectHolder Or::Execute(Closure& closure, Context& context) {
        ObjectHolder lhs_arg = GetLhs()->Execute(closure, context);

        if (auto inst_Ptr = lhs_arg.TryAs<runtime::Bool>()) {
            return MakeOperation(inst_Ptr, closure, context);
        }
        else if (auto inst_Ptr = lhs_arg.TryAs<runtime::Number>()) {
            return MakeOperation(inst_Ptr, closure, context);
        }
        else {
            throw std::runtime_error("Value does not bool value");
        }

    }

    ObjectHolder And::Execute(Closure& closure, Context& context) {
        ObjectHolder lhs_arg = GetLhs()->Execute(closure, context);
        if (auto inst_Ptr = lhs_arg.TryAs<runtime::Bool>()) {
            return MakeOperation(inst_Ptr, closure, context);
        }
        else if (auto inst_Ptr = lhs_arg.TryAs<runtime::Number>()) {
            return MakeOperation(inst_Ptr, closure, context);
        }
        else {
            throw std::runtime_error("Value does not bool value");
        }

    }

    ObjectHolder Not::Execute(Closure& closure, Context& context) {
        const auto arg = GetArg()->Execute(closure, context);
        if (auto inst_Ptr = arg.TryAs<runtime::Bool>()) {
            return  ObjectHolder::Own(runtime::Bool{ !inst_Ptr->GetValue() });
        }
        else if (auto inst_Ptr = arg.TryAs<runtime::Number>()) {
            return  ObjectHolder::Own(runtime::Bool{ !inst_Ptr->GetValue() });
        }
        else {
            throw std::runtime_error("Value does not bool value");
        }
    }


    Comparison::Comparison(Comparator cmp, unique_ptr<Statement> lhs, unique_ptr<Statement> rhs)
        : BinaryOperation(std::move(lhs), std::move(rhs))
        , cmp_(std::move(cmp)) {
    }

    ObjectHolder Comparison::Execute(Closure& closure, Context& context) {
        return ObjectHolder::Own(runtime::Bool(cmp_(GetLhs()->Execute(closure, context), GetRhs()->Execute(closure, context), context)));
    }

    NewInstance::NewInstance(const runtime::Class& class_, std::vector<std::unique_ptr<Statement>> args)
        :class__(class_)
        , args_(std::move(args)) {
    }

    NewInstance::NewInstance(const runtime::Class& class_)
        : class__(class_) {
    }

    ObjectHolder NewInstance::Execute(Closure& closure, Context& context) {
        ObjectHolder oh = ObjectHolder::Own(runtime::ClassInstance(class__));
        auto class_inst_ = oh.TryAs<runtime::ClassInstance>();
        if (class_inst_->HasMethod(INIT_METHOD, args_.size())) {
            
            std::vector<runtime::ObjectHolder> new_args;
            for (const auto& arg : args_) {
                new_args.push_back(arg->Execute(closure, context));
            }
            class_inst_->Call(INIT_METHOD, new_args, context);
        }
        return oh;
    }

    MethodBody::MethodBody(std::unique_ptr<Statement>&& body)
        :body_(std::move(body)) {
    }

    ObjectHolder MethodBody::Execute(Closure& closure, Context& context) {
        ObjectHolder result = ObjectHolder::None();

        try {
            result = std::move(body_->Execute(closure, context));           
        }
        catch (ObjectHolder& obj) {
            result = std::move(obj);
        }
        return result;
    }

}  // namespace ast