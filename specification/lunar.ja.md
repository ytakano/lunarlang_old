# Lunar Language

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