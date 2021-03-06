package ru.spbstu.icc.dcn.stlc

// Результатом вычисления терма является объект нового типа: значение.
// Значения соответствуют нормальным формам термов, однако концептуально
// они ближе к объектам, которые создаются в памяти компьютера при выполнении
// программы.
abstract class Value

// Значение может быть целочисленной константой,
case class IntVal(value: Int) extends Value {
  override def toString = value.toString
} 

// логической константой 
case class BoolVal(value: Boolean) extends Value {
  override def toString = value.toString
}

// или замыканием. Замыкание представляет собой функцию вместе с контекстом,
// в котором должно вычисляться ее тело.
//
// Рассмотрим в качестве примера терм (*) [x:Int => [y:Int => {y,x}]] 1 2.
// Чтобы получить нормальную форму этого терма, мы должны подставить значение 1
// в тело функции. 
//
// Это можно сделать двумя способами:
// 1) написать функцию подстановки
//    def subst(name: String, value: Term, term: Term): Term = ???
//    которая будет обходить дерево, представляющее терм term, и заменять все вхождения
//    переменной name на терм value. При этом надо учитывать области видимости
//    переменных: если в терм входит лямбда-абстракция, использующая то же самое имя,
//    она "скроет" переменную name, и в теле этой вложенной лямбда-абстракции проводить
//    подстановку нельзя.
//
// 2) Выбрать тот способ, который мы и реализовали в классе Term: ввести в функцию вычисления
//    термов дополнительный аргумент, который будет описывать контекст вычисления, то есть
//    хранить сведения о значениях свободных переменных терма. Таким образом мы избегаем
//    необходимости обходить весь терм: мы будем обращаться к контексту каждый раз, когда
//    встретим свободную переменную, и только в этих случаях.
//
// Второй способ представляется более простым, но скрывает серьезную проблему, которую можно
// увидеть, если по шагам рассмотреть процесс вычисления терма (*).
// 
// Предположим, что наша функция вычисления термов выглядит так:
// def eval(term: Term, env: Map[String, Term)): Term = ???
//
// Тогда ее применение к терму (*) можно описать следующей последовательностью
// подстановок:
// 
//   eval( ([x:Int => [y:Int => {y,x}]] 1) 2, Map())
// = eval( eval ([x:Int => [y:Int => {y,x}]] 1, Map()), eval (2, Map()))
// = eval( eval ([y:Int => {y,x}], Map(x -> 1)), 2)
//   -- обратите внимание, что выражение [y:Int => {y,x}] находится в нормальной форме,
//   -- поэтому функция eval просто возвращает его как результат.
// = eval( [y:Int => {y,x}], 2)
//   -- мы потеряли информацию о контексте, в котором вычислялась эта нормальная форма,
//   -- потому что тип данных, используемый для описания терма, не позволяет никаким
//   -- образом этот контекст сохранить.
//   -- Если бы мы выполнили подстановку и заменили x на 1, проблема была бы решена,
//   -- однако мы этого решили не делать. Раз так, продолжаем:
// = eval({y,x}, Map(y -> 2))
// = {eval(y, Map(y -> 2)), eval(x, Map(y -> 2))}
// = {2, eval(x, Map(y -> 2))}
// = runtime error
//   -- мы пытаемся вычислить значение свободной переменной x, но этого значения
//   -- в текущем контексте нет.
//
// Чтобы решить проблему, будем считать результатом вычисления терма не терм, а более
// сложную структуру, в которой для функций предусмотрим возможность сохранения контекста.
// Такая структура, в которой помимо тела функции (или, в реальных трансляторах, указателя
// на код функции) хранится контекст, в котором должно производиться вычисление, называется
// замыканием (а поскольку контекст в большей части современных языков определяется 
// статически, то есть на на основе не момента времени, когда происходит вычисление функции,
// а на основе того места в тексте программы, где был вызов функции был написан,
// то замыкание такого рода иногда называют лексическим замыканием).

case class ClosureVal(variable: String, term: Term, env: Term.EvalEnv) extends Value {
  override def toString = s"<$variable, $term, $formatEnv>"
  
  // Служебная функция форматирования словаря. Что она делает - загадка
  // для самостоятельного размышления.
  def formatEnv: String =
    "[" + env.map{ case (name, value) => s"$name/$value" }.mkString(", ") + "]"
}

// Для пар мы тоже предусмотрим отдельный вид значений - хотя и очень простой.
case class PairVal(fst: Value, snd: Value) extends Value {
  override def toString = s"{$fst, $snd}"
}
