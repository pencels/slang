package slang.runtime

class NoMatchException(args: List[Value]) extends
    RuntimeException(s"Matchbox failed all matches for args ${args.map(_.toSlangString).mkString("[", ", ", "]")}")
