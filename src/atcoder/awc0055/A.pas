program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k, d: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	inc(k);

	ans := 0;
	for i := 1 to n do begin
		read(d);
		if i mod k > 0 then inc(ans, d);
	end;
	readln;

	writeln(ans);
end.
