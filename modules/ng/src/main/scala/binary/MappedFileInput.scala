package scparse.ng.binary

import java.io.RandomAccessFile
import java.nio.file.*
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel

import scutil.core.implicits.*

import scparse.ng.*

object MappedFileInput {
	def of(file:Path):ParserInput[Byte]	={
		val	mapped	= new RandomAccessFile(file.toFile, "r").getChannel use { fc =>
			fc.map(FileChannel.MapMode.READ_ONLY, 0, Files.size(file))
		}
		MappedFileInput(mapped, 0)
	}
}

final case class MappedFileInput(input:MappedByteBuffer, val index:Int) extends ParserInput[Byte] {
	def next:Option[(ParserInput[Byte], Byte)]	=
		if (index < input.remaining)	Some((MappedFileInput(input, index+1), input get index))
		else							None
}
