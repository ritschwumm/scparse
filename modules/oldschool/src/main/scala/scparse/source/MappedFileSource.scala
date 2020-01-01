package scparse.oldschool

import java.io.File
import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel

import scutil.base.implicits._

object MappedFileSource {
	def of(file:File):MappedFileSource	= {
		val	mapped	= new RandomAccessFile(file, "r").getChannel use { fc =>
			fc map (FileChannel.MapMode.READ_ONLY, 0, file.length)
		}
		MappedFileSource(mapped, 0)
	}
}

final case class MappedFileSource(input:MappedByteBuffer, position:Int) extends Source[Byte] {
	def cata[X](empty: =>X, full:(Source[Byte],Byte)=>X):X	=
		if (position < input.remaining)	full(MappedFileSource(input, position+1), input get position)
		else							empty
}
