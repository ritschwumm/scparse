package scparse.ng.binary

import java.io.File
import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel

import scutil.base.implicits._

import scparse.ng._

object MappedFileInput {
	def of(file:File):ParserInput[Byte]	= {
		val	mapped	= new RandomAccessFile(file, "r").getChannel use { fc =>
			fc map (FileChannel.MapMode.READ_ONLY, 0, file.length)
		}
		MappedFileInput(mapped, 0)
	}
}

final case class MappedFileInput(input:MappedByteBuffer, val index:Int) extends ParserInput[Byte] {
	def next:Option[(ParserInput[Byte], Byte)]	=
		if (index < input.remaining)	Some((MappedFileInput(input, index+1), input get index))
		else							None
}
