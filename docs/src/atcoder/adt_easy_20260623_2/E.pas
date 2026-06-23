program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #min
uses
	math;
var
	ntc, tci: int32;
	na, nb, nc, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ntc);
	for tci := 1 to ntc do begin
		readln(na, nb, nc);

		ans := min(min(na, nc), (na + nb + nc) div 3);

		writeln(ans);
	end;
end.
