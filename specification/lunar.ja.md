# Lunar Language

# 構文

- LUNAR      := (DEFUN | STRUCT | UNION)*
- STATEMENTS := (STATEMENT EOL)* STATEMENT | e
- EOL        := \n | ;
- STATEMENT  := LET | ASSIGN | WHILE | IF | STRUCT | UNION
- EXPRIDLIT  := IDENTIFIER | LITERAL | EXPR
- EXPR       := CALLFUNC | OP | TUPLE
- LITERAL    := STR32 | STR8 | CHAR32 | CHAR8 | INT | FLOAT | HEX | OCT | BIN | ATOM | TRUE | FALSE

# IDENTIFIER

IDENTIFIERとは空白文字以外からなる、1文字以上の文字かつ、先頭が数字ではない文字列かつ、
予約文字（列）以外の文字列である。

# 型指定子

- TYPESPEC   := OWNERSHIP | UNSAFE | CONST | ONCE | ENDIAN
- TYPESPECS  := TYPESPEC*
- TYPESPECS1 := TYPESPEC+

## 所有権

所有権の概念があり、変数を利用する際には、その変数がどのような所有権で扱われるかを指定する。

構文：
- OWNERSHIP := uniq | shared | ref | immove

セマンティクス：
- unique
  - ただ唯一のオーナーのみから保持される変数である。所有権の移動はmoveにて行われる。
- shared
  - 複数のオーナーから保持される変数である。内部的には参照カウントを保持している。
- reference
  - unique、shared、immovalbe変数の所有権に影響を及ばさずに参照するための変数である。
- immovable
  - 所有権の変更が許されない変数である。

## unsafe指定子

referenceのみに指定可能。

- UNSAFE := unsafe

## const指定子

値の変更が出来ないオブジェクトに指定。

- CONST := const

## once指定子

一度しか束縛出来なくなる。unique, ref, sharedのみに指定可能。

- ONCE := once

## エンディアン指定子

u64など、特定の型にのみに有効。

- ENDIAN := big | little | native

# 型変数

型変数は変数の頭にバッククォートをつけて通常の変数名と区別する。

VARTYPE = `IDENTIFIER

# 型指定

- TYPE  := TYPE1 | \( (TYPE1,)+ TYPE1 \)
- TYPE1 := TYPESPECS TYPE2 | TYPESPECS VARTYPE | TYPESPECS1
- TYPE2 := SCALAR

# 第一級オブジェクト

## スカラ

- bool
- u64
- s64
- u32
- s32
- u16
- s16
- u8
- s8
- double
- float
- char (u32の別名、UTF-32）
- atom

構文：
- SCALAR := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | char | atom

# 関数

## 関数定義

構文：
- DEFUN    := fun IDENTIFIER \<TYPEARGS\>? \\( ARGS \\) { STATEMENTS }
- ARGS     := (TYPE? IDENTIFIER,)* TYPE? IDENTIFIER | e
- TYPEARGS := (VARTYPE (=TYPE)? ,)* VARTYPE (=TYPE)? | e

```
fun funcName <`return=int> () { }
fun funcName <`return=int> (shared `return arg) { }
fun funcName <`return=int, `a=shared `return> (`a arg) { }
fun funcName <`return=uniq, `a=(`return, int)> (`a arg) { }
fun funcName <'return=int, `a=shared, `b=(`return, `a)> (`a arg1, `b arg2) { }
```

# 型定義

## 構造体

- STRUCT  := struct \<TYPEARGS\>? name { MEMBERS }
- MEMBERS := (MEMBER,)* MEMBER ,?
- MEMBER  := TYPE: IDENTIFIER | STRUCT | UNION

## 多相型

- UNION    := union \<TYPEARGS\>? name { UMEMBERS }
- UMEMBERS := (UMEMBER,)* UMEMBER ,?
- UMEMBER  := (TYPE:)? IDENTIFIER | STRUCT | UNION

# 構文

## 変数定義・変数束縛

構文：
- LET      := let VARS BINDINGS?
- VARS     := (VAR,)? VAR
- VAR      := TYPE? IDENTIFIER | VTUPLE
- VTUPLE   := \\( (VTVAR,)* VTVAR \\) | \\( \\)
- VTVAR    := VTUPLE | VAR
- BINDINGS := = (EXPRIDLIT,)* EXPRIDLIT

```
let a = 100
let int: a
let int: a = 100
let int: a, bool: b = 100, true
let (int: a, int: b) = (100, 200)
```

## 代入文

- ASSIGN := AVARS = AEXPRS
- AVARS  := (AVAR,)? AVAR
- AVAR   := IDENTIFIER | ATUPLE
- ATUPLE := \\( (ATVAR,)* ATVAR \\) | \\( \\)
- VTVAR  := ATUPLE | AVAR

```
a = 100
a, b = 100, 200
(a, (b, c)) = (100, (200, true))
```

## while文

- WHILE := while EXPR { STATEMENTS }

## if文

- IF := if EXPR { STATEMENTS }

# 式

## タプル、名前無し構造体

- TUPLE := \\( (TEXPR,)* TEXPR \\) | \\( \\)
- TEXPR := TUPLE | EXPR

```
(int, bool)
```

## 演算

- OP := EXPRIDLIT OPERATOR2 EXPRIDLIT | OPERATOR1 EXPRIDLIT
- OPERATOR1 := not | - | ~
- OPERATOR2 := + | - | * | / | & | '|' | ^ | and | or | xor | < | >

## 関数呼び出し

- CALLFUNC := IDENTIFIER (<TARGS>)? \\( FARGS? \\)
- TARGS    := (TYPE,)* TYPE | (TYPE,)* (IDENTIFIER=TYPE,)* IDENTIFIER=TYPE
- FARGS    := (EXPR,)* EXPR | (EXPR,)* (IDENTIFIER=EXPR,)* IDENTIFIER=EXPR

## リテラル

### atom

- ATOM := \#IDENTIFIER

### 文字列

- STR32  := " CHARS* "
- STR8   := b " CHARS* "
- ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\\\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
- CHARS  := ESCAPE | ESCAPE以外の文字

### 文字

- CHAR32 := ' CHARS '
- CHAR8  := b ' CHARS '

### 整数

- INT     := -? DIGIT
- DIGIT   := NUM1to9 NUM0to9* | 0
- NUM1to9 := 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
- NUM0to9 := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

### 浮動小数

- FLOAT := INT . NUM0to9+ EXP? f?
- EXP   := EE SIGN NUM+
- EE    := e | E
- SIGN  := - | +

最後にfがついた場合は単精度で、つかない場合は倍精度となる。

### 16進数

- HEX     := 0x HEXNUM2\* | 0X HEXNUM2\*
- HEXNUM2 := HEXNUM HEXNUM
- HEXNUM  := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | A | b | B | c | C | d | D | f | F

### 8進数

- OCT    := 0 OCTNUM*
- OCTNUM := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

### 2進数

- BIN    := 0b BINNUM\* | 0B BINNUM\*
- BINNUM := 0 | 1

### 真偽値

- TRUE  := ture
- FALSE := false

# メモ

## 型システム

Lunarlangの型は、t(a, b, c, d, e, f) と表すことが出来る。

- 変数a
  - 所有権
  - a ∈ {unique, shared, ref, immovable}
- 変数b
  - unsafe指定子
  - b ∈ {true, false}
- 変数c
  - const指定子
  - c ∈ {true, false}
- 変数d
  - once指定子
  - d ∈ {true, false}
- 変数e
  - エンディアン指定子
  - e ∈ {big, little, native}
- 変数f
  - 実際の型を表す
  - f ∈ {bool, u64, s64, u32, s32, u16, s16, u8, s8, double, float, char, atom, struct(x, y), union(z, w)}
  - θ = t(a, b, c, d, e, f)で表される型の集合とする
  - structは以下のような型となる
    - struct(x, y)
    - x ∈ θ
    - y ∈ θ ∪ ∅
    - 例
      - struct(u64, struct(bool, ∅))
      - これは、{u64, bool} という直積型となる
  - unionは以下のような型となる
    - union(z, w)
    - z ∈ θ
    - w ∈ θ ∪ ∅
    - 例
      - union(u64, union(bool, ∅))
      - これは, u64 | bool という直和型となる

## 見た目はJavaScriptっぽく

```
bool foo() {
    f = 100 // 型推論
    shared u32 v = make shared u32
    somefunc = lambda u32 () { }

    *v = 200
    v = 200 // error

    if flag == true {
    }

    for n in d {
    }

    s32 = 0
    while s < 100 {
    }

    select {
    ch1:
        continue
    ch2:
        continue
    }

    case var {
    var < 100:

    typeof s32:

    }

    spawn(func2, arg, stacksize)
    thread(func2, arg, stacksize)

    return true
}
```

## Fault Tolerant

[Making reliable distributed systems in the presence of software errors](http://ftp.nsysu.edu.tw/FreeBSD/ports/distfiles/erlang/armstrong_thesis_2003.pdf "Making reliable distributed systems in the presence of software errors") の 5 Programming Fault-tolerant Systems (pp.115)を参照。

## 型推論

[Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system "Hindley–Milner type system") を参照。

## クロージャ

関数オブジェクトとして実現。暗黙的にthisをもつ関数。

## 左再帰のParser Combinatorの扱い

[Parser Combinators for Ambiguous Left-Recursive Grammars](http://richard.myweb.cs.uwindsor.ca/PUBLICATIONS/PADL_08.pdf "Parser Combinators for Ambiguous Left-Recursive Grammars")

## Parser Combinator

chain, choice, manyは汎用的な記述方法としてこちら側で定義！

## リアクティブプログラミング

グリーンスレッドはBehavior

## モナド、パイプ、programmable semicolon

do の第一引数が関数の第一引数に渡される。
第二引数以降も同じ。

```
do parser {
    func()
    func1() or func2()
    func3()

    if () {
    }

    return true
} fail {
}

parser -> func() -> func1() or func2() -> func3()

if func(parser) {
    if func1(parser) or func2(parser) {
        func3()
    }
}
```

```
do {
    let fd = socket()
    bind(fd)
}
```