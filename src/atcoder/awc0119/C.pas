program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	m, a, h: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, a);

	ans := 0;
	for i := 1 to n do begin
		read(h);
		if (h > a) and (ans > -1) then begin
			if h > m then
				ans := -1
			else begin
				inc(ans);
				m := m div 2;
			end;
		end;
	end;
	readln;

	writeln(ans);
end.
