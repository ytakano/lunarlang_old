# Lunar IR

Lunar言語の中間表現であり、ここからLLVM IRへ変換。

# 構文

- IR           := TOP*
- TOP          := FUNC | STRUCT | UNION | DATA | GLOBAL | EXPORT | IMPORT
- STATEMENT    := LET | IF | COND | WHILE | SELECT
- STEXPR       := STATMENT | EXPR
- LITERAL      := STR32 | STR8 | CHAR32 | CHAR8 | INT | FLOAT | HEX | OCT | BIN
- EXPRIDENT    := EXPR | IDENTIFIER
- EXPRIDENTLIT := EXPR | IDENTIFIER | LITERAL

# グローバル変数定義

- GLOBAL := ( global ( ( TYPE (IDENTIFIER+) EXPRIDENTLIT )+ ) )

# エクスポート

- EXPORT := ( export IDENTIFIER+ )

# インポート

- IMPORT := ( import STR32+ )

# 所有権

Lunar IRにはオーナーという概念があり、変数を利用する際には、その変数がどのような所有権で扱われるかを指定する。

構文：
- OWNERSHIP := unique | shared | ref

セマンティクス：
- unique
  - ただ唯一のオーナーのみから保持される変数である。所有権の移動はmoveにて行われる。
- shared
  - 複数のオーナーから保持される変数である。内部的には参照カウントを保持している。
- reference
  - unique、shared、immovalbe変数の所有権に影響を及ばさずに参照するための変数である。
- immovable
  - 所有権の変更が許されない変数である。

# 型指定

型指定は、所有権と実際の型を指定して行う。
所有権の指定が省略された場合はimmovable変数となる。

構文：
- TYPE  := TYPE0 | ( OWNERSHIP TYPE0 )
- TYPE0 := SCALAR | VECTOR | STRING | BINARY | LIST | STRUCT | DICT | SET | DATA | FUNCTYPE | RSTREAM | WSTREAM | PTR | UNION | PARSEC | MUTEX | CONDITION | IDENTIFIER

ここで、IDENTIFIERとは空白文字以外からなる、1文字以上の文字かつ、先頭が数字ではない文字列かつ、
予約文字（列）以外の文字列である。

# 第一級オブジェクト

## スカラ

以下の型がプリミティブなスカラオブジェクトとして利用可能。
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
- SCALAR := SCALARTYPE INITSCALAR | SCALARTYPE
- SCALARTYPE := bool | u64 | s64 | u32 | s32 | u16 | s16 | u8 | s8 | double | float | char |ATOM
- ATOM := `IDENTIFIER

ただしここで、INITSCALARは数値、真偽値、文字リテラル、atomリテラルのいずれかである。

## 配列

構文：
- VECTOR := ( vector TYPE SIZE )
- SIZE := 0以上の整数

例：
```lisp
(vector u64 10)
(vector (shared u32) 5)
(vector (unique u32) 5)
(vector (ref u32) 5)
```

## 文字列

構文：
- STRING := string

内部的にはUTF-32。

## バイナリ列

構文：
- BINARY := binary

## リスト

構文：
- LIST := ( list TYPE )

例：
```lisp
(list u64)
(list (shared u16))
(list (unique u16))
(list (ref u16))
```

## 構造体

構文：
- STRUCT := ( struct IDENTIFIER? ( TYPE IDENTIFIER )+ )

セマンティクス：
- ( struct 構造体の名前 ( 構造体メンバの型 構造体メンバの名前 )+ )

例：
```lisp
(struct my_data
  (u32 foo)
  ((shared u32) bar)
  ((unique u32) baz)
  ((ref u32) qux))
```

## 辞書（木）

構文
- DICT := ( dict TYPE TYPE )

セマンティクス
- ( dict Keyの型 Valueの型 )

例：
```lisp
(dict u32 (unique string))
```

## 集合（木）

構文
- SET := ( set TYPE )

セマンティクス
- ( set 値の型 )

```lisp
(set (unique u32))
```

## 多相型

構文：
- DATA := ( data IDENTIFIER? ( TYPE IDENTIFIER )+ )

セマンティクス：
- ( data 多相型の名前 ( 多相型となる型 型の名前 )+ )

## 関数型

構文：
- FUNCTYPE := ( func ( TYPE\* ) ( TYPE\* ) )

セマンティクス：
- ( func ( 戻り値の型\* ) ( 引数の型\* ) )

関数には所有権という概念はない。

## ストリーム

ストリームは読み込み用の端点と、書き込み用の端点から構成される。

構文：
- RSTREAM := ( rstrm TYPE )
- WSTREAM := ( wstrm TYPE )

セマンティクス：
- ( rstrm ストリームに渡すデータの型 )
- ( wstrm ストリームに渡すデータの型 )

ストリームには以下の制約がある。
- 読み込み用の端点は所有権がuniqueであり、書き込み用の端点は所有権がsharedでなければならない。
- ストリームが扱える値は、sharedもしくはunique変数か、プリミティブスカラ変数のみである。
- ストリームに送信する構造体内にある変数は、shared、unique、immovable変数のみである。ref変数を内部に持っている構造体は、ストリームで送信することは出来ない。

## ポインタ型

C関数と互換性を保つために利用され、それ以外での利用は非推奨である。

構文：
- PTR := ( ptr TYPE ) | ( ptr PTR )

セマンティクス：
- ( ptr ポインタの型 )

```lisp
(ptr u64)
(ptr (shared mystruct))
(ptr (ptr (unique u32)))
```

## 共用体

構文：
- UNION := ( union IDENTIFIER? ( TYPE IDENTIFIER )+ )

セマンティクス：
- ( union 構造体の名前 ( 構造体メンバの型 構造体メンバの名前 )+ )

C関数と互換性を保つために利用され、それ以外での利用は非推奨である。

## パーサ型

構文：
- PARSEC := parsec

## mutex型

構文：
- MUTEX := mutex

## condition型

構文：
- CONDITION := condition

# 関数

## 関数定義構文

構文：
- FUNC := ( defun IDENTIFIER ( TYPE\* ) ( TYPE IDENTIFIER )\* STEXPR\* )

## 関数呼び出し式

構文：
- CALLFUNC := ( EXPRIDENT EXPRIDENTLIT\* )

関数定義に応じた値を返す。

## 無名関数定義式

構文：
- LAMBDA := ( lambda ( TYPE\* ) ( TYPE IDENTIFIER )\* STEXPR\* )

関数型の値を返す。

# 変数

## 変数生成式

- NEW := ( new TYPE )

TYPE型の値を返す。

## 変数束縛構文

構文：
- LET := ( let ( ( TYPE (IDENTIFIER+) EXPRIDENTLIT )+ ) STEXPR\* )

セマンティクス：
- ( let ( ( 束縛 )+ ) 式\* ）
- ( let ( ( 型 (変数名+) 束縛する値 )+ ) 式\* )

関数は複数の値を返すこともあるため、複数の変数名を記述できるように。

## 変数の値書き換え文

構文：
- STORE := ( store! EXPRIDENT EXPRIDENTLIT )

セマンティクス：
- ( store! 書き換える変数 書き換える値 )

## 変数の束縛先変更文

構文：
- ASSOC := ( assoc! EXPRIDENT EXPRIDENT )

セマンティクス：
- ( assoc! 変数 束縛先 )

ただし、束縛先を変更できるのはuniqueかshared変数のみである。

# 制御式、制御文

## if 式

構文：
- IF := ( if EXPRIDENTLIT EXPRIDENTLIT EXPRIDENTLIT )

セマンティクス：
- ( if 条件 条件が真の時の値 条件が偽の時の値 )

if は式であり値を返す。C言語の?構文みたいなもの。

## cond 条件分岐構文

構文：
- COND := ( cond ( EXPRIDENTLIT STEXPR\* )+ ( else STEXPR\* )? )

セマンティクス：
- ( cond ( 条件 条件が真の時に実行する式\* )+ ( else どの条件にも当てはまらない場合に実行する式\* )? )

cond は制御構文であり、値は返さない。

## while ループ構文

構文：
- WHILE := ( while EXPRIDENTLIT STEXPR* )

セマンティクス：
- ( while 条件 条件が真の間実行する式\* )

## break 文

構文：
- BREAK := ( break )

while ループの制御から脱出するときに使う。

## return 文

構文：
- RETURN := ( return (EXPRIDENTLIT*) )

セマンティクス：
- ( return (返り値*) )

# 多相型

## type 式

構文：
- TYPEOF := ( type TYPE0 IDENTIFIER ) | ( type TYPE0 LITERAL )

セマンティクス：
- ( type 検査する形名 識別子 )

type 式は真偽値を返す式であり、多相型変数の型を動的に検査するために利用される。

例：
```lisp
(type u32 var)
```

# 入力選択

## select 構文

構文：
- SELECT := ( select ( EXPRIDENT STEXPR\* )\* ( timeout SIZE STEXPR* )? )

セマンティクス：
- ( select (ストリーム ストリームに入力があった時に実行する式) (timeout タイムアウトするまでの時間[ms] )? )

ストリームの入力待ちを行う。
入力待ちの際、他に実行可能なグリーンスレッドがある場合はそちらに処理が移行。

# 標準関数

## ストリーム

### ストリーム生成式

構文：
- MKSTREAM := ( mkstream TYPE SIZE )

(unique (rstrm TYPE))と(shared (wstrm TYPE))の2つの値を返す。

### push式

構文：
- PUSH := ( push! EXPRIDENTLIT )

ストリームの最後尾にデータを挿入する。
STRM_SUCCESS, STRM_CLOSED, STRM_NO_VACANCYのいずれかの値を返す。

### pop式

構文：
- POP := ( pop! EXPRIDENT )

セマンティクス：
- ( pop! ストリーム )

ストリームから先頭のデータを取り出す。
返り値は、(取り出した値 エラー)となり、エラーはSTRM_SUCCESS, STRM_CLOSED, 
STRM_NO_VACANCYのいずれかとなる。

## マルチタスキング

### spawn式

構文：
- SPAWN := ( spawn SIZE EXPRIDENT EXPRIDENTLIT SIZE)

セマンティクス：
- ( spawn スタックサイズ 呼び出す関数 関数へ渡す引数 スタックサイズ)

返り値はスレッド内で一意に識別されるs64型の整数値。

### schedule式

構文：
- SCHEDULE := ( schedule )

他のグリーンスレッドに制御を渡す。

### thread式

OSネイティブなデタッチスレッドを生成。

構文：
- THREAD := ( thread ATOM TYPE SIZE EXPRIDENT EXPRIDENTLIT )

セマンティクス：
- ( thread スレッドの名前 スレッドキューの型 キューのサイズ 呼び出す関数 関数へ渡す引数* )

ストリームと同じく、スレッドキューには以下の制約がある。
- スレッドキューが扱える値は、sharedもしくはunique変数か、プリミティブスカラ変数のみである。

返り値は、bool値。

## ロック・同期処理

### mutex_init式

- MUTEX_INIT := ( mutex_init EXPRIDENT )

### mutex_lock式

- MUTEX_LOCK := ( mutex_lock EXPRIDENT )

### mutex_try_lock式

- MUTEX_TRY_LOCK := ( mutex_try_lock EXPRIDENT )

### mutex_unlock式

- MUTEX_UNLOCK := ( mutex_unclock EXPRIDENT )

### mutex_cond_init式

- MUTEX_COND_INIT := ( mutex_cond_init EXPRIDENT )

### mutex_cond_wait式

- MUTEX_COND_WAIT := ( mutex_cond_wait EXPRIDENT EXPRIDENT SIZE? )

### spin_lock_init式

- SPIN_LOCK_INIT := ( spin_lock_init EXPRIDENT )

ゼロ代入。

### spin_lock式

- SPIN_LOCK := ( spin_lock EXPRIDENT )

### spin_try_lock式

- SPIN_TRY_LOCK := ( spin_try_lock EXPRIDENT )

### spin_unlock式

- SPIN_UNLOCK := ( spin_unlock EXPRIDENT )

### htm_lock_init式

- HTM_LOCK_INIT := ( htm_lock_init EXPRIDENT )

Hardware Transactional Memoryのロックハンドラを返す。

### htm_lock式

- HTM_LOCK := ( htm_lock EXPRIDENT )

### htm_unlock式

- HTM_UNCLOK := ( htm_unlock EXPRIDENT )

## Parser Combinator

- PARSECINIT   := (parser_init string EXPRIDENT) | (parser_init binary EXPRIDENT)
- PARSEC       := (parse EXPRIDENT PARSECOPS EXPRIDENTLIT*)
- PARSECOPS    := PARSECCHAR | PARSECMANY | PARSECMANY1 | PARSECTRY | PARSECTRYEND | PARSECLA | PARSECLAEND | PARSECDIGIT | PARSECHEX | PARSECOCT | PARSECSPACE | PARSECSATIS | PARSECSTR 
- PARSECCHAR   := character
- PARSECTRY    := try
- PARSERTRYEND := try_end
- PARSECLA     := look_ahead
- PARSECLAEND  := look_ahead_end
- PARSECDIGT   := digit
- PARSECHEX    := hex
- PARSECOCT    := oct
- PARSECSPACE  := space
- PARSECSATIS  := satisfy
- PARSECSTR    := string
- PARSECRESULT := result

```lisp
(let (parsec (p) (parsec_init string rstream))
  (p character 'a'))
```

## C関数呼び出し

### ccall

構文：
- CCALL := ( ccall IDENTIFIER EXPRIDENTLIT* )

セマンティクス：
- ( ccall C関数名 引数* )

shared、unique変数はポインタ渡し。immovableは値渡しとなる。
pointerのpointerはptr型を利用して実現する。

### dlopen式

モジュール読み込み

構文：
- DLOPEN := ( dlopen EXPRIDENTLIT )

セマンティクス：
- ( dlopen モジュールへのパス )

動的ライブラリ、.soファイルを読み込む。

返り値はbool値。

### deref式

PTR型の参照外し

構文：
- DEREF := ( deref EXPRIDENT )

セマンティクス：
- ( deref PTR型変数 )

返り値は、参照外しを行った値。

## 参照カウント

### inccnt式

- INCCNT := ( inccnt EXPRIDENT )

### deccnt式

- DECCNT := ( deccnt EXPRIDENT )

## プリミティブ演算

### 四則演算・剰余算

- ADD   := (+ EXPRIDENTLIT EXPRIDENTLIT+ )
- MINUS := (- EXPRIDENTLIT EXPRIDENTLIT+ )
- MULTI := (* EXPRIDENTLIT EXPRIDENTLIT+ )
- DIV   := (/ EXPRIDENTLIT EXPRIDENTLIT+ )
- MOD   := (mod EXPRIDENTLIT EXPRIDENTLIT+ )

## IO

### print式

- PRINT := ( print EXPRIDENTLIT )

文字列を標準出力へ出力。引数はstring型のみ。

## 文字列変換式

- TOSTR := ( tostr EXPRIDENTLIT )

引数を文字列へ変換。immovalなstring型を返す。 

# リテラル

## 文字列

### UTF-32

- STR32  := " CHARS* "
- STR8   := b " CHARS* "
- ESCAPE := \a | \b | \f | \r | \n | \t | \v | \\ | \? | \' | \" | \0 | \UXXXXXXXX | \uXXXX
- CHARS  := ESCAPE | ESCAPE以外の文字

## 文字

- CHAR32 := ' CHARS '
- CHAR8  := ' CHARS '

## 整数

- INT     := -? DIGIT
- DIGIT   := NUM1to9 NUM0to9* | 0
- NUM1to9 := 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
- NUM0to9 := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

## 浮動小数

- FLOAT := NUM . EXP
- EXP   := EE SIGN NUM+
- EE    := e | E
- SIGN  := - | +

## 16進数

- HEX     := 0x HEXNUM2\* | 0X HEXNUM2\*
- HEXNUM2 := HEXNUM HEXNUM
- HEXNUM  := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | A | b | B | c | C | d | D | f | F

## 8進数

- OCT    := 0 OCTNUM*
- OCTNUM := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

## 2進数

- BIN    := b BINNUM\* | B BINNUM\*
- BINNUM := 0 | 1

# Application Binary Interface (ABI)