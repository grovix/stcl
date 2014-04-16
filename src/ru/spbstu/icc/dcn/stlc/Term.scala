package ru.spbstu.icc.dcn.stlc

import scala.util.parsing.combinator._

abstract class Term {
  // Метод typeOf возвращает Some(ty), если терм имеет тип ty,
  // и None, если терму нельзя приписать никакой тип.  
  def typeOf: Option[Type] = Term.typeOf(this)
  
  // Метод valueOf возвращает Some(value), если результатом
  // вычисления терма является значение value, и None,
  // если при вычислении терма происходит ошибка времени выполнения.
  def valueOf: Option[Value] = Term.eval(this)
}

// Константы примитивных типов Int и Bool.
case class IntConst(value: Int) extends Term {
  override def toString = value.toString
}

case class BoolConst(value: Boolean) extends Term {
  override def toString = value.toString
}

// Базовое простое типизированное \lambda-исчисление.
// Переменные.
case class Var(name: String) extends Term {
  override def toString = name
}

// Функции.
case class Fun(argname: String, ty: Type, body: Term) extends Term {
  override def toString = s"[$argname: $ty => $body]"
}

// Применение функции к аргументу.
case class App(fun: Term, arg: Term) extends Term {
  override def toString = arg match {
    // Операция применения функции к аргументу левоассоциативна.
    // Если аргумент функции сам является применением функции к аргументу,
    // вокруг него нужно поставить скобки. 
    case App(_, _) => s"$fun ($arg)"
    
    // В остальных случаях избегаем лишних скобок.
    case _ => s"$fun $arg"
  }
}

// let x = e in t
case class Let(name: String, value: Term, body: Term) extends Term {
  override def toString =
    s"let $name = $value in $body"
}

// Упорядоченные пары и операции над ними
case class Pair(fst: Term, snd: Term) extends Term {
  override def toString = s"{$fst, $snd}"
}

// Операция "получить первый элемент пары".
case class Fst(pair: Term) extends Term {
  override def toString = s"fst $pair"
}

// Операция "получить второй элемент пары".
case class Snd(pair: Term) extends Term {
  override def toString = s"snd $pair"
}

// Сопровождающий объект, реализующий операции над термами.
object Term {
  // Парсер термов
  val termParser = new TermParsers()
  
  // Проверка типов.
  // Для проверки типов мы анализируем структуру терма. В зависимости от
  // того, чем является корень дерева, описывающего терм, мы применяем
  // соответствующее правило типизации. Если в наборе посылок (верхней части)
  // правила есть условия, мы должны проверить, что эти условия выполняются.
  
  // Типы приписываются к выражениям в определенном контексте, который задает
  // типы свободных переменных выражения. Мы реализуем такой контекст в виде
  // словаря (ассоциативного массива), сопоставляющего именам свободных
  // переменных их типы.
  type TypingContext = Map[String, Type]
  val emptyTypingContext: TypingContext = Map()
  
  // Чтобы вычислить тип замкнутого терма (то есть терма, не содержащего
  // свободных переменных), мы используем пустой контекст.
  def typeOf(term: Term): Option[Type] = typeOf(term, emptyTypingContext)
  
  // Алгоритм вычисления типа терма term в контексте env. Если выражению
  // нельзя приписать тип, функция typeOf возвращает значение None.
  def typeOf(term: Term, env: TypingContext): Option[Type] = term match {
    // Тип свободной переменной определяется контекстом.
    case Var(x) => env.get(x)
    
    // Тип целочисленной константы - Int.
    case IntConst(_) => Some(IntType)
    
    // Тип логической константы - Bool.
    case BoolConst(_) => Some(BoolType)
    
    // Чтобы проверить, что применение функции fn к аргументу arg допустимо,
    // и определить тип результата, нужно:
    case App(fn, arg) =>
      // 1) определить тип аргумента arg в текущем контексте;
      for (argType <- typeOf(arg, env);
           // 2) определить тип терма fn и убедиться, что это тип функции,
           FunType(dom, cod) <- typeOf(fn, env)
           // причем такой, что заявленный тип аргумента функции совпадает
           // с типом фактически переданного аргумента.
           if (dom == argType))
        // Если это так, то типом всего терма будет тип возвращаемого
        // значения функции.
        yield cod

    // Правило вычисления типа для функции.
    case Fun(arg, ty, body) =>
      // Мы знаем тип аргумента функции: он явно задан в нашем терме и равен ty.
      // Чтобы узнать тип возвращаемого функцией значения, нужно вычислить
      // тип тела функции в новом контексте, который получается путем добавления
      // к исходному контексту информации о типе аргумента функции.
      for (bodyTy <- typeOf(body, env + ((arg, ty))))
        yield FunType(ty, bodyTy)
    
    // let .. in ..
    case Let(name, value, body) =>
      for (valueTy <- typeOf(value, env);
           bodyTy <- typeOf(body, env + ((name, valueTy))))
        yield bodyTy
        
    // Тип упорядоченной пары вычисляется как пара типов ее элементов.
    case Pair(fst, snd) =>
      for (fstType <- typeOf(fst, env); sndType <- typeOf(snd, env))
        yield PairType(fstType, sndType)

    // Чтобы узнать тип выражения fst pair, мы должны сначала вычислить
    // тип самой пары pair, и взять из него только тип первого элемента пары.
    case Fst(pair) =>
      for (PairType(fstType, sndType) <- typeOf(pair, env))
        yield fstType
    
    // Тип выражения snd pair вычисляется аналогично.
    case Snd(pair) =>
      for (PairType(fstType, sndType) <- typeOf(pair, env))
        yield sndType
  }
  
  // Вычисление значения терма.
  
  // Чтобы вычислять значение терма, нам также нужен контекст. Этот контекст
  // будет сопоставлять свободным переменным уже не типы, а значения. 
  type EvalEnv = Map[String, Value]
  val emptyEnv: EvalEnv = Map()
  
  // Чтобы вычислить значение замкнутого терма, будем вычислять его
  // в пустом контексте.
  def eval(term: Term): Option[Value] = eval(term, emptyEnv)
  
  // Алгоритм вычисления значения терма.
  def eval(term: Term, env: EvalEnv): Option[Value] = term match {
    // Значением целочисленной константы является число, соответствующее этой константе.
    case IntConst(n)       => Some(IntVal(n))
    
    // Значением логической константы является соответствующая логическая величина.
    case BoolConst(n)      => Some(BoolVal(n))
    
    // Значение свободной переменной определяется контекстом ее вычисления.
    case Var(v)            => env.get(v)
    
    // Значением функции является замыкание, в котором сохраняется
    // тот контекст, в котором это замыкание было создано.
    // Благодаря этому мы можем представлять функции от нескольких
    // переменных как функцию от одной переменной, которая возвращает
    // функцию от остальных переменных. Чтобы мы могли в дальнейшем узнать,
    // какое именно значение было передано в качестве первого аргумента,
    // связку из имени аргумента и его значения надо сохранить в замыкании.
    case Fun(arg, _, body) => Some(ClosureVal(arg, body, env))
    
    // Чтобы вычислить значение терма, который описывает применение
    // выражения lhs к выражению rhs, нужно:
    case App(lhs, rhs) =>
      // 1) вычислить значение выражения lhs (так как выражение прошло проверку
      //    типов, это обязательно будет замыкание - если, конечно, в ходе вычисления
      //    не происходит ошибки времени выполнения);
      for (lhsValue <- eval(lhs, env);
           // 2) вычислить значение аргумента;
           rhsValue <- eval(rhs, env);
           // 3) применить полученное на первом шаге замыкание к значению аргумента.
           result <- applyClosure(lhsValue, rhsValue))
        yield result

    // let .. in ..
    case Let(name, value, body) =>
      for (
          valueV <- eval(value, env);
          bodyV <- eval(body, env + ((name, valueV))))
        yield bodyV
        
    // Упорядоченные пары. Чтобы построить упорядоченную пару, нужно
    // вычислить значение первого элемента пары, потом вычислить значение
    // второго элемента пары, и создать объект-пару, содержащую оба значения. 
    case Pair(fst, snd) =>
      for (fstValue <- eval(fst, env);
           sndValue <- eval(snd, env))
        yield PairVal(fstValue, sndValue)
    
    // Для вычисления первого элемента пары надо сначала вычислить значение самой
    // пары, а потом взять из нее первый элемент.
    case Fst(pair) =>
      for (PairVal(fstValue, sndValue) <- eval(pair, env))
        yield fstValue
    
    // Значение второго элемента пары определяется аналогично.
    case Snd(pair) =>
      for (PairVal(fstValue, sndValue) <- eval(pair, env))
        yield sndValue
  }
 
  // Вспомогательная функция: применение замыкания к аргументу.
  def applyClosure(closure: Value, argval: Value): Option[Value] = closure match {
    // Замыкание содержит три элемента:
    // 1) имя аргумента функции, 
    // 2) невычисленный терм, задающий тело функции;
    // 3) контекст, задающий значения остальных свободных переменных тела функции,
    //    кроме значения ее аргумента.
    // Чтобы применить замыкание к ранее вычисленному аргументу, нужно
    // вычислить тело функции в контексте, который получается из того контекста,
    // который был сохранен в контексте замыкания, путем добавления к нему связки
    // из имени и значения аргумента функции.
    case ClosureVal(v, body, env) => Term.eval(body, env + ((v, argval)))
  }
  
  // Функция синтаксического анализа терма.
  // Метод parse применяет к строке парсер термов, убеждается, что в строке не осталось
  // неразобранных символов и возвращает результат разбора, обернутый в контейнер Option[Term].
  def parse(text: String): Option[Term] = {
    val parseResult = termParser.parseAll(termParser.term, text)
    if (parseResult.successful) Some(parseResult.get) else None
  }
}

// Парсер термов 
class TermParsers extends RegexParsers {
  // Грамматика термов:
  // term ::= prim | prim term
  // prim ::= "true"
  //        | "false"
  //        | number
  //        | "[" ident ":" ty "=>" term "]"
  //        | "let" ident "=" term "in" term
  //        | "letrec" ident "=" term "in" term
  //        | "fst" term
  //        | "snd" term
  //        | identifier
  //        | "{" term "," term "}"
  //        | "(" term ")"
  //а теперь мое творчество
  //		|"if" "Bool" "then" term "else" term
  
  // Грамматику языка типов представим в следующем виде:
  // ty ::= typrim | typrim "->" ty
  // typrim ::= "Int" | "Bool" | "{" ty "," ty "}" | "(" ty ")"
  
  // Потенциальное имя переменной (может совпадать с ключевым словом)
  val name: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r
  
  // Целочисленная константа
  val number: Parser[String] = "[+-]?[0-9]+".r
  
  // Парсеры для каждого ключевого слова
  val kwdLet: Parser[String] = "let\\b".r
  val kwdIn: Parser[String] = "in\\b".r
  val kwdTrue: Parser[String] = "true\\b".r
  val kwdFalse: Parser[String] = "false\\b".r
  val kwdFst: Parser[String] = "fst\\b".r
  val kwdSnd: Parser[String] = "snd\\b".r
  val kwdInt: Parser[String] = "Int\\b".r
  val kwdBool: Parser[String] = "Bool\\b".r
  val kwdIf: Parser[String] = "if\\b".r
  val kwdThen: Parser[String] = "then\\b".r
  val kwdElse: Parser[String] = "else\\b".r
  
  // Парсер любого ключевого слова
  val reserved: Parser[String] =
    ( kwdLet | kwdIn
    | kwdTrue | kwdFalse
    | kwdFst | kwdSnd
    | kwdInt | kwdBool
    | kwdIf | kwdThen
    | kwdElse
    )
      
  // Идентификатор
  val identifier: Parser[String] = not(reserved) ~> name
  
  // Парсер ty разбирает цепочку из одной или нескольких конструкций
  // typrim, разделенных лексемой "->".
  // Комбинатор rep1sep возвращает список деревьев, соответствующих
  // каждому простому типу typrim. Мы должны свернуть этот список,
  // чтобы построить тип функции. Так как конструктор типов функций
  // правоассоциативен, список надо сворачивать справа.
  def ty: Parser[Type] =
    rep1sep(typrim, "->") ^^ {
      case ts => ts.reduceRight((dom,cod) => FunType(dom, cod))
    }
  
  // Парсер typrim разбирает простые типы. Сначала к неразобранной части
  // строки применяется парсер, ожидающий префикс "Int". Если парсер смог
  // извлечь из строки необходимый префикс, разбор считается успешным. Если
  // префикс не соответствует образцу, применяется следующий парсер, а если
  // не подошел ни один парсер, строка считается несоответствующей грамматике.
  def typrim: Parser[Type] =
    ( kwdInt ^^ (_ => IntType)
    | kwdBool ^^ (_ => BoolType)
    | "{"~> ty~ ","~ty <~ "}" ^^ { case fst~","~snd => PairType(fst, snd) }
    | "("~> ty <~")"
    )
  
  // Обратите внимание, что правило prim ::= ident идет после всех правил,
  // которые начинаются с "true", "false, "fst", "snd". Дело в том, что
  // парсер ident примет любую последовательность символов, которая удовлетворяет
  // определению идентификатора, то есть последовательность букв и цифр, которая
  // начинается с буквы. Если мы поместим правило ident раньше, чем правило "true",
  // то строка "true" будет сочтена идентификатором, а не логической константой.
  
  // Нетерминал term порождает цепочки из одной или нескольких конструкций prim.
  // Идею повторения некоторого парсера один или несколько раз реализует комбинатор rep1.
  // Результатом разбора такого составного терма будет список, элементы которого надо
  // преобразовать в дерево применений функции к аргументу. Это можно сделать, свернув
  // полученный список слева.
  def term: Parser[Term] =
	rep1(prim) ^^ (_.reduce((fn, arg) => App(fn, arg)))
	
  // Устройство парсера prim аналогично устройству парсера typrim для типов.
  def prim: Parser[Term] =
	( kwdTrue ^^ (_ => BoolConst(true))
	| kwdFalse ^^ (_ => BoolConst(false))
	| number ^^ (x => IntConst(x.toInt))
	| "["~> ((identifier <~":")~ty <~"=>")~term <~"]" ^^ {
	    case id~ty~body => Fun(id, ty, body)
	  }
	| kwdLet ~> identifier ~ ("=" ~> term) ~ (kwdIn ~> term) ^^ {
	    case  name ~ value ~ body => Let(name, value, body)
	  }
	| kwdFst ~> term ^^ { case term => Fst(term) }
	| kwdSnd ~> term ^^ { case term => Snd(term) }
	| identifier ^^ (x => Var(x))
	| "{"~> term~(","~> term) <~ "}" ^^ { case fst~snd => Pair(fst, snd) }
	| "("~> term <~")"
	)
}
