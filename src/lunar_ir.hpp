#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_common.hpp"
#include "lunar_string.hpp"

#include <vector>


/*
 * TYPE := TYPE0 | ( OWNERSHIP TYPE0 )
 * TYPE0 := SCALAR | VECTOR | STRING | LIST | STRUCT | DICT | SET | DATA | FUNC | RSTREAM | WSTREAM | IDENTIFIER
 *
 * OWNERSHIP := unique | shared | ref | global
 *
 * SCALAR := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | binary | char | string | ATOM
 * ATOM := `IDENTIFIER
 *
 * VECTOR := ( vector TYPE SIZE)
 * SIZE := An integer greater than or equal to 0
 *
 * STRING := string
 *
 * LIST := ( list TYPE )
 *
 * STRUCT := ( struct IDENTIFIER ( TYPE IDENTIFIER )+ )
 *
 * DICT := ( dict TYPE TYPE )
 *
 * SET := ( set TYPE )
 *
 * DATA := ( data IDENTIFIER (TYPE IDENTIFIER)+ )
 *
 * FUNC := ( func (TYPE*) ( TYPE* ) )
 *
 * RSTREAM := (rstrm TYPE)
 * WSTREAM := (wstrm TYPE)
 */

namespace lunar {

enum OWNERSHIP {
    OWN_UNIQUE,
    OWN_SHARED,
    OWN_IMMOVABLE,
    OWN_REF,
    OWN_GLOBAL,
};

enum SCALAR {
    SC_BOOL,
    SC_U64,
    SC_S64,
    SC_U32,
    SC_S32,
    SC_U16,
    SC_S16,
    SC_U8,
    SC_S8,
    SC_DOUBLE,
    SC_FLOAT,
    SC_CHAR,
    SC_ATOM,
};

enum BASIC_TYPE {
    BT_SCALAR,
    BT_VECTOR,
    BT_STRING,
    BT_LIST,
    BT_STRUCT,
    BT_DICT,
    BT_SET,
    BT_DATA,
    BT_FUNCTYPE,
    BT_RSTREAM,
    BT_WSTREAM,
};

class lunar_ir_ast {
public:
    lunar_ir_ast() { }
    virtual ~lunar_ir_ast() { }
};

class lunar_ir_type {
public:
    lunar_ir_type(BASIC_TYPE type, OWNERSHIP owner_ship) : m_type(type), m_owner_ship(owner_ship) { }
    virtual ~lunar_ir_type() { }

protected:
    BASIC_TYPE m_type;
    OWNERSHIP  m_owner_ship;
};

class lunar_ir_scalar : public lunar_ir_type {
public:
    lunar_ir_scalar(OWNERSHIP owner_ship, SCALAR scalar)
        : lunar_ir_type(BT_SCALAR, owner_ship), m_scalar(scalar) { }

private:
    SCALAR m_scalar;
};

class lunar_ir_vector : public lunar_ir_type {
public:
    lunar_ir_vector(OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type, uint64_t size)
        : lunar_ir_type(BT_VECTOR, owner_ship),
          m_type(std::move(type)),
          m_size(size) { }
    
    virtual ~lunar_ir_vector() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
    uint64_t m_size;
};

class lunar_ir_list : public lunar_ir_type {
public:
    lunar_ir_list(OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_LIST, owner_ship),
          m_type(std::move(type)) { }
    
    virtual ~lunar_ir_list() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_struct : public lunar_ir_type {
public:
    lunar_ir_struct(OWNERSHIP owner_ship, const std::string &name)
        : lunar_ir_type(BT_STRUCT, owner_ship), m_name(name) { }
    
    virtual ~lunar_ir_struct() { }

    void add_member(std::unique_ptr<lunar_ir_type> type, const std::string &name)
    {
        m_member_types.push_back(std::move(type));
        m_member_names.push_back(name);
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_member_types;
    std::vector<std::string> m_member_names;
    std::string m_name;
};

class lunar_ir_dict : public lunar_ir_type {
public:
    lunar_ir_dict(OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> key, std::unique_ptr<lunar_ir_type> val)
        : lunar_ir_type(BT_DICT, owner_ship), m_key(std::move(key)), m_val(std::move(val)) { }

private:
    std::unique_ptr<lunar_ir_type> m_key;
    std::unique_ptr<lunar_ir_type> m_val;
};

class lunar_ir_set : public lunar_ir_type {
public:
    lunar_ir_set(OWNERSHIP owner_ship, std::unique_ptr<lunar_ir_type> val)
        : lunar_ir_type(BT_SET, owner_ship), m_val(std::move(val)) { }

private:
    std::unique_ptr<lunar_ir_type> m_val;
};

class lunar_ir_data : public lunar_ir_type {
public:
    lunar_ir_data(OWNERSHIP owner_ship, const std::string &name)
        : lunar_ir_type(BT_DATA, owner_ship), m_name(name) { }
    
    virtual ~lunar_ir_data() { }

    void add_member(std::unique_ptr<lunar_ir_type> type, const std::string &name)
    {
        m_member_types.push_back(std::move(type));
        m_member_names.push_back(name);
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_member_types;
    std::vector<std::string> m_member_names;
    std::string m_name;
};

class lunar_ir_functype : public lunar_ir_type {
public:
    lunar_ir_functype(OWNERSHIP owner_ship) : lunar_ir_type(BT_FUNCTYPE, owner_ship) { }
    virtual ~lunar_ir_functype() { }
    
    void add_ret(std::unique_ptr<lunar_ir_type> ret)
    {
        m_ret.push_back(std::move(ret));
    }

    void add_arg(std::unique_ptr<lunar_ir_type> arg)
    {
        m_arg.push_back(std::move(arg));
    }

private:
    std::vector<std::unique_ptr<lunar_ir_type>> m_ret;
    std::vector<std::unique_ptr<lunar_ir_type>> m_arg;
};

class lunar_ir_rstream : public lunar_ir_type {
public:
    lunar_ir_rstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_RSTREAM, OWN_UNIQUE), m_type(std::move(type)) { }

    virtual ~lunar_ir_rstream() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_wstream : public lunar_ir_type {
public:
    lunar_ir_wstream(std::unique_ptr<lunar_ir_type> type)
        : lunar_ir_type(BT_WSTREAM, OWN_SHARED), m_type(std::move(type)) { }

    virtual ~lunar_ir_wstream() { }

private:
    std::unique_ptr<lunar_ir_type> m_type;
};

class lunar_ir_string : public lunar_ir_type {
public:
    lunar_ir_string(OWNERSHIP owner_ship) : lunar_ir_type(BT_STRING, owner_ship) { }
    virtual ~lunar_ir_string() { }
};

}

#endif // LUNAR_IR_HPP