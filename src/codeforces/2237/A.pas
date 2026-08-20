program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	notc, tci, n, i, j, ans: int32;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		for i := 1 to n do begin
			j := i+1;
			while (j <= n) and (a[j] <= a[i]) do inc(j);
			if j <= n then a[j] := a[i];
		end;

		ans := 0;
		for i := 1 to n do inc(ans, a[i]);
		writeln(ans);

	end;
end.
