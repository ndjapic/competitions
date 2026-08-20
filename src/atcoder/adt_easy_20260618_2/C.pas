program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	EE = 63;
var
	e, a: int8;
	ans: uint64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	ans := 0;
	for e := 0 to EE do begin
		read(a);
		if a = 1 then inc(ans, uint64(1) shl e);
	end;
	readln;

	writeln(ans);
end.
