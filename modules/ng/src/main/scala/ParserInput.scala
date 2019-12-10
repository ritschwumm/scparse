package scparse.ng

trait ParserInput[S] {
	def index:Int
	def next:Option[(ParserInput[S],S)]
}
