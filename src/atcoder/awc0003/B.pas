program _B;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, ans: int32;
	l, r: array [1 .. nn] of char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(l[1], r[1], r[1]);
	ans := 0;

	for i := 2 to n do begin
		readln(l[i], r[i], r[i]);
		if r[i-1] = l[i] then inc(ans);
	end;

	writeln(ans);
end.
