FILES=./ICFPC2015/Program.fs

target.exe: ${FILES}
	fsc.exe -o $@  ${FILES} -r FSharp.Core.dll -r ExtCore.dll -r FSharp.Data.dll -r UnionArgParser.dll -r ConcurrentPriorityQueue.dll
