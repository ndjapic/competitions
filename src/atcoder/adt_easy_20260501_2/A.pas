program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, h, ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := 0;
	for i := 1 to n do begin
		read(h);
		if h <= m then begin
			dec(m, h);
			inc(ans);
		end else
			m := 0;
	end;
	readln;

	writeln(ans);
end.
