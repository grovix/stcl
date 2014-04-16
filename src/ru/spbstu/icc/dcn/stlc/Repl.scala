package ru.spbstu.icc.dcn.stlc

// Пользовательский интерфейс интерпретатора.
// Интерпретатор читает выражение со стандартного ввода,
// анализирует его, проверяет его допустимость с точки
// зрения системы типов, вычисляет значение и печатает результат.

object Repl {
  def main(args: Array[String]) {  
    // Печать приглашения
    Console.print("> ")
    Console.flush()
    
    // Последовательно читаем строки со стандартного ввода,
    // пока не встретим команду ":quit" или конец файла.
    // Для каждой строки формируем и печатаем ответ интерпретатора.
    Iterator.continually(Console.readLine).
    	takeWhile(input => input != ":quit").
    	foreach(input => {
    	  Console.println(answer(input))
    	  Console.print("> ")
    	  Console.flush()
    	})
  }
  
  // Формирование ответа интерпретатора.
  def answer(input: String): String = {
    input.trim match {
      // Игнорируем пустые строки
      case "" => ""        
        
      // Если строка не пуста, пытаемся провести синтаксический анализ.
      case e => Term.parse(e) match {
        // Если получилось построить дерево разбора терма, проверяем его тип,
        // вычисляем и формируем строку, содержащую результат.
        case Some(term) => evaluatedTermRepr(term)
        
        // Если не получилось, возвращаем сообщение о синтаксической ошибке.
        //ff
        case None => "parse error"
      }
    }
  }
  
  // Формирование строки результата.
  def evaluatedTermRepr(term: Term): String = {
    term.typeOf match {
      // Если выражение корректно типизируется, пробуем его вычислить.
      case Some(ty) => term.valueOf match {
        // Выражение удалось вычислить, печатаем само выражение, его тип и результат.
        case Some(value) => term + " : " + ty + " = " + value
        
        // Выражение не удалось вычислить: ошибка времени выполнения.
        case None => term + " : " + ty + " = runtime error"
      }
      
      // Выражению невозможно приписать тип: ошибка типов.
      case None => term + " : type error"
    }
  }
}
